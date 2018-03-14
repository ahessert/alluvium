require(tidyr)
require(purrr)
require(ggplot2)

tidy_plot_vars <- function(dt) {
  dt %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()
}
plot_state <- function(dt) {
  ggplot(data=dt, aes(x=as.factor(state))) +
    geom_histogram(stat="count")
}
