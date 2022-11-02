library(tidyverse)
library(glue)

var1 <- "test"
var2 <- "bla"
var3 <- NULL

"{var1} vs {var2} vs {var3}" %>% glue() %>% print()
