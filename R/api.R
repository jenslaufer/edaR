.var_pairs <- function(data) {
  expand_grid(source = data %>% colnames(),
              target = data %>% colnames()) %>%
    filter(source %>% as.character() < target %>% as.character())
}

.title <-
  function(variable1,
           variable2,
           variable3 = NULL,
           variable4 = NULL,
           variable5 = NULL,
           outlier_treatment) {
    outlier_treatment %>% print()
    outlier_treatment_title <- ""
    if (outlier_treatment == "outlier_remove") {
      outlier_treatment_title <- "(Outlier Removed)"
    }
    else if (outlier_treatment == "outlier_only") {
      outlier_treatment_title <- "(Outlier Only)"
    }
    
    outlier_treatment_title %>% print()
    
    title <- "{variable1} vs. {variable2}" %>% glue(.null = "")
    
    if (!(variable3 %>% is.null())) {
      title <- "{title} vs. {variable3}" %>% glue()
    }
    if (!(variable4 %>% is.null())) {
      title <- "{title} vs. {variable4}" %>% glue()
    }
    if (!(variable5 %>% is.null())) {
      title <- "{title} vs {variable5}" %>% glue()
    }
    
    "{title} {outlier_treatment_title}" %>% glue()
  }

.is_outlier <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  
  ! ((quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr))
}

features <- function(data) {
  data %>% colnames()
}

index_features <- function(data) {
  data %>%
    select(!where(is.numeric)) %>%
    mutate(total = n()) %>%
    gather(-total, key = "variable", value = "value") %>%
    group_by(variable, value, total) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(variable, total) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(total == n) %>%
    ungroup() %>%
    pull(variable)
}

qualitive_features <- function(data) {
  data %>%
    select(!where(is.numeric)) %>%
    colnames()
}

quantitive_features <- function(data) {
  data %>%
    select(where(is.numeric)) %>%
    colnames()
}

rm_tukey_outliers <- function(data, .cols = NULL) {
  if (.cols %>% is.null()) {
    .cols <- data %>% colnames()
  }
  
  data %>%
    mutate(across(where(is.numeric) &
                    all_of(.cols), .is_outlier, .names = "{.col}_is_outlier")) %>%
    filter_at(vars(matches("_is_outlier$")), all_vars(. == F)) %>%
    select(!contains("is_outlier"))
}


data_summary <- function(data) {
  data %>%
    summarise(
      `Number Of Variables` = data %>% features() %>% length(),
      `Number Of Rows` = data %>% nrow(),
      `Number Of Quantitive Variables` = data %>% quantitive_features() %>% length(),
      `Number Of Qualitative Variables` = data %>% qualitive_features() %>% length(),
      `Number Of Date Variables` = data %>% select(where(is.numeric.Date)) %>% colnames() %>% length(),
      `Number Of Index Like Variables` = data %>% index_features() %>% length()
    )
}



empty_values_plot <- function(data) {
  plot1 <- data %>%
    mutate(rownum = row_number()) %>%
    gather(-rownum, key = "variable", value = "value") %>%
    mutate(isEmpty = value %>% is.na()) %>%
    mutate(isEmpty = if_else(isEmpty, "Missing", "Filled")) %>%
    ggplot(aes(x = rownum, y = variable, fill = isEmpty)) +
    geom_raster() +
    scale_fill_manual(values = c("Filled" = "#59A14F", "Missing" = "#E15759")) +
    bbc_style() +
    labs(title = "Dataset Rows")
  
  plot2 <-
    data %>%
    mutate(total = n()) %>%
    gather(-total, key = "feature", value = "value") %>%
    mutate(isNan = value %>% is.na()) %>%
    group_by(feature, total, isNan) %>%
    summarise(n = n(), .groups = "keep") %>%
    mutate(ratio = n / total,
           ratio_str = round(ratio * 100, 1) %>% format(1)) %>%
    mutate(label = if_else(ratio > .02, "{ratio_str}% ({n})" %>% glue(), "")) %>%
    mutate(isNan = if_else(isNan, "Missing", "Filled")) %>%
    select(-total) %>%
    ggplot(aes(x = feature, y = ratio, fill = isNan)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(aes(label = label), position = position_stack(vjust = .5)) +
    scale_fill_manual(values = c("Filled" = "#59A14F", "Missing" = "#E15759")) +
    coord_flip() +
    bbc_style() +
    labs(title = "Percentage/Number")
  
  grid.arrange(plot1,
               plot2,
               nrow = 1,
               top = textGrob("Missing Values", gp = gpar(fontsize = 40, font = 2)))
}

quantitive_plot <- function(data,
                            variable1,
                            variable2,
                            variable3 = NULL,
                            variable4 = NULL,
                            variable1Scale = "identity",
                            variable2Scale = "identity",
                            variable3Scale = "identity",
                            variable4Scale = "identity",
                            outlier_treatment) {
  if (variable3 %>% is.null() && variable4 %>% is.null()) {
    plot <- data %>%
      ggplot(aes(
        x = !!sym(variable1),
        y = !!sym(variable2)
      )) +
      scale_x_continuous(trans = variable1Scale) +
      scale_y_continuous(trans = variable2Scale)
  } else if (!(variable3 %>% is.null()) &&
             variable4 %>% is.null()) {
    plot <- data %>%
      ggplot(aes(
        x = !!sym(variable1),
        y = !!sym(variable2),
        color = !!sym(variable3)
      )) +
      scale_x_continuous(trans = variable1Scale) +
      scale_y_continuous(trans = variable2Scale)
    
    if (data %>% pull(!!sym(variable3)) %>% is.numeric()) {
      plot <- plot + scale_color_continuous(trans = variable3Scale)
    }
  } else if (variable3 %>% is.null() &&
             !(variable4 %>% is.null())) {
    plot <- data %>%
      ggplot(aes(
        x = !!sym(variable1),
        y = !!sym(variable2),
        shape = !!sym(variable4)
      )) +
      scale_x_continuous(trans = variable1Scale) +
      scale_y_continuous(trans = variable2Scale)
  } else {
    plot <- data %>% ggplot(aes(
      x = !!sym(variable1),
      y = !!sym(variable2),
      color = !!sym(variable3),
      shape = !!sym(variable4)
    )) +
      scale_x_continuous(trans = variable1Scale) +
      scale_y_continuous(trans = variable2Scale)
    if (data %>% pull(!!sym(variable3)) %>% is.numeric()) {
      plot <- plot + scale_color_continuous(trans = variable3Scale)
    }
  }
  
  
  plot +
    geom_point() +
    bbplot::bbc_style() +
    labs(title = .title(variable1, variable2, variable3, variable4, outlier_treatment = outlier_treatment))
}

qualitative_plot <-
  function(data,
           variable1,
           variable2 = NULL,
           variable3 = NULL,
           variable4 = NULL,
           outlier_treatment) {
    if (!(variable3 %>% is.null()) &&
        !(variable4 %>% is.null())) {
      data <- data %>%
        group_by(!!sym(variable2),!!sym(variable3),!!sym(variable4))
    } else if (!(variable3 %>% is.null()) &&
               variable4 %>% is.null()) {
      data <- data %>%
        group_by(!!sym(variable2),!!sym(variable3))
    } else if (!(variable2 %>% is.null()) &&
               variable3 %>% is.null() &&
               variable4 %>% is.null()) {
      data <- data %>% group_by(!!sym(variable2))
    }
    
    data <- data %>%
      mutate(total = n())
    
    data <-
      data %>% group_by(!!sym(variable1), total)
    if (!(variable3 %>% is.null()) &&
        !(variable4 %>% is.null())) {
      data <-
        data %>% group_by(!!sym(variable1),!!sym(variable2),!!sym(variable3),!!sym(variable4),
                          total)
    } else if (!(variable3 %>% is.null()) &&
               variable4 %>% is.null()) {
      data <-
        data %>% group_by(!!sym(variable1),!!sym(variable2),!!sym(variable3),
                          total)
    } else if (!(variable2 %>% is.null()) &&
               variable3 %>% is.null() &&
               variable4 %>% is.null()) {
      data <-
        data %>% group_by(!!sym(variable1),!!sym(variable2), total)
    }
    
    
    data <- data %>%
      summarise(n = n(), .groups = "drop") %>%
      ungroup() %>%
      mutate(
        ratio = n / total,
        ratio_str = round(ratio * 100, 1) %>% format(1),
        label = if_else(ratio > 0.05, "{ratio_str}% ({n})" %>% glue(), "")
      )
    
    
    
    if (!(variable2 %>% is.null())) {
      plot <- data %>%
        ggplot(aes(
          x = !!sym(variable2),
          y = ratio,
          fill = !!sym(variable1)
        ))
    } else {
      plot <- data %>%
        ggplot(aes(
          x = "",
          y = ratio,
          fill = !!sym(variable1)
        ))
    }
    
    plot <- plot +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(aes(label = label), position = position_stack(vjust = .5)) +
      coord_flip() +
      scale_fill_tableau() +
      bbc_style() +
      theme(axis.text.x = element_blank()) +
      labs(title = .title(variable1, variable2, variable3, variable4, outlier_treatment = outlier_treatment))
    
    if (!(variable3 %>% is.null()) &&
        !(variable4 %>% is.null())) {
      plot +
        facet_wrap("~ {variable3} ~ {variable4}" %>% glue() %>% formula())
    } else if (!(variable3 %>% is.null()) &&
               variable4 %>% is.null()) {
      plot +
        facet_wrap("~ {variable3}" %>% glue() %>% formula())
    } else {
      plot
    }
  }


quantitative_qualitative_plot <-
  function(data,
           quantitiveVariable,
           qualitativeVariable1,
           qualitativeVariable2 = NULL,
           qualitativeVariable3 = NULL,
           quantitiveVariableScale = "identity",
           outlier_treatment) {
    if (!(qualitativeVariable2 %>% is.null()) &
        qualitativeVariable3 %>% is.null()) {
      plot <- data %>%
        ggplot(aes(
          x = reorder_within(
            !!sym(qualitativeVariable1),!!sym(quantitiveVariable),!!sym(qualitativeVariable2),
            fun = median
          ),
          y = !!sym(quantitiveVariable)
        ))
    } else if (!(qualitativeVariable2 %>% is.null()) &
               !(qualitativeVariable3 %>% is.null())) {
      plot <- data %>%
        ggplot(aes(
          x = reorder_within(
            !!sym(qualitativeVariable1),!!sym(quantitiveVariable),
            list(
              !!sym(qualitativeVariable2),!!sym(qualitativeVariable3)
            ),
            fun = median
          ),
          y = !!sym(quantitiveVariable)
        ))
    } else {
      plot <- data %>%
        ggplot(aes(
          x = reorder(
            !!sym(qualitativeVariable1),!!sym(quantitiveVariable),
            fun = median
          ),
          y = !!sym(quantitiveVariable)
        ))
    }
    
    
    plot <- plot +
      geom_boxplot() +
      geom_jitter(alpha = .2) +
      scale_y_continuous(trans = quantitiveVariableScale) +
      scale_x_reordered() +
      bbc_style() +
      labs(
        title = .title(
          quantitiveVariable,
          qualitativeVariable1,
          qualitativeVariable2,
          qualitativeVariable3,
          outlier_treatment = outlier_treatment
        )
      )
    
    if (!(qualitativeVariable2 %>% is.null()) &
        qualitativeVariable3 %>% is.null()) {
      plot + facet_wrap("~{qualitativeVariable2}" %>% glue() %>% formula(),
                        scales = "free_x") +
        facet_wrap("~{qualitativeVariable2}" %>% glue() %>% formula(),
                   scales = "free")
    } else if (!(qualitativeVariable2 %>% is.null()) &
               !(qualitativeVariable3 %>% is.null())) {
      plot + facet_wrap(
        "~{qualitativeVariable2}~{qualitativeVariable3}" %>% glue() %>% formula(),
        scales = "free"
      )
    } else {
      plot
    }
  }

univariate_plot <-
  function(data, variable) {
    data <- data %>% .outlier_handling(c(variable), outlier_treatment)
    
    numeric <- data %>%
      pull(!!sym(variable)) %>%
      is.numeric()
    
    if (numeric) {
      data %>%
        ggplot(aes(x = !!sym(variable))) +
        geom_histogram(fill = "#4e79a7") +
        bbplot::bbc_style() +
        labs(title = "{variable}" %>% glue())
    } else {
      data %>%
        mutate(total = n()) %>%
        group_by(!!sym(variable), total) %>%
        summarise(n = n()) %>%
        mutate(ratio = round(n / total * 100, 1) %>% format(1)) %>%
        ggplot(aes(x = reorder(!!sym(variable), n), y = n)) +
        geom_bar(fill = "#4e79a7", stat = "identity") +
        geom_text(aes(label = "{ratio}%" %>% glue()), vjust = -0.25) +
        bbplot::bbc_style() +
        labs(title = "{variable}" %>% glue())
    }
  }

univariate_plots <- function(data, outlier_treatment = "all") {
  data %>%
    colnames() %>%
    map( ~ data %>% univariate_plot(..1, outlier_treatment = outlier_treatment))
}

bivariate_plot <-
  function(data,
           variable1,
           variable2,
           variable1Scale = "identity",
           variable2Scale = "identity",
           outlier_treatment) {
    data <-
      data %>% .outlier_handling(c(variable1, variable2), outlier_treatment)
    
    numeric1 <- data %>%
      pull(!!sym(variable1)) %>%
      is.numeric()
    numeric2 <- data %>%
      pull(!!sym(variable2)) %>%
      is.numeric()
    
    if (numeric1 && numeric2) {
      data %>% quantitive_plot(variable1,
                               variable2,
                               variable1Scale = variable1Scale,
                               variable2Scale = variable2Scale)
    } else if (!numeric1 && !numeric2) {
      data %>%
        qualitative_plot(variable1, variable2)
    } else if (numeric1 && !numeric2) {
      data %>%
        quantitative_qualitative_plot(variable1, variable2)
    } else if (!numeric1 && numeric2) {
      data %>%
        quantitative_qualitative_plot(variable2, variable1)
    }
  }

bivariate_plots <- function(data, outlier_treatment = "all") {
  data %>%
    .var_pairs() %>%
    pmap(
      ~ data %>%
        bivariate_plot(..1,
                       ..2,
                       outlier_treatment = outlier_treatment)
    )
}

.outlier_handling <-
  function(data, columns, outlier_treatment) {
    if (outlier_treatment == "outlier_remove") {
      data <- data %>% rm_tukey_outliers(columns)
    } else if (outlier_treatment == "outlier_only") {
      data_without_outliers <- data %>% rm_tukey_outliers(columns)
      data <- data %>% anti_join(data_without_outliers)
    }
    data
  }
