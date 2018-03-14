require(data.table)
require(magrittr)

source("./config/config.R")
source("./functions/plot_functions.R")
source("./functions/basic_cleaning.R")
source("./functions/windowed_features.R")
source("./functions/regression.R")

RESAMPLE_COUNT <- config$TOTAL_SAMPLES_FOR_TRAINING

train <- fread(TRAINING_DATA_PATH)
summary(train)

tidy_plot_vars(train)
plot_state(train)

tidy_plot_vars(train[rpm==0])
plot_state(train[rpm==0])


train %>%
  convert_to_unix_ts %>%
  fix_nulls %>%
  create_sin_cos_features %>%
  create_current_down_time_feature %>%
  create_state_dummies %>%
{.} -> train

create_training_sets <- function(unused, dt) {
  dt %>%
    create_rollup_features(runmode = "TRAIN") %>%
    create_trend_change_feature %>%
    create_outcome_var %>%
    create_preperiod %>%
  {.} -> reg_data  
  return(reg_data)
}

training_sets <- lapply(c(1:RESAMPLE_COUNT), create_training_sets, dt=train)
models <- lapply(training_sets, fit_regregression)
coefficients <- rowMeans(data.frame(lapply(models, function(x) {x$coefficients})), na.rm=T)

write(RJSONIO::toJSON(coefficients), COEFFICIENT_PATH)
