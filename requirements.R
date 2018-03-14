list.of.packages <- c("ggplot2", "magrittr", "data.table", "jsonlite", "httr", "curl", "tidyr", "purrr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) {install.packages(new.packages)}