require(config)

config <- config::get(file = "./config/config.yml")

COEFFICIENT_PATH <- "./data/reg_coefficients.json"
TRAINING_DATA_PATH = "./data/train.csv"
REGRESSION_FEATURES <- c("wetBulbFahrenheit", "relativeHumidity", "sin_wind", "cos_wind", "rpm", 
                         "state_power_ramp_up", "state_running", "state_soft_start",
                         "sin_hour", "cos_hour","current_downtime", "current_downtime_sqd", "ten_min_wind_avg", 
                         "ten_min_wind_max", "one_hr_watt_avg", "one_hr_watt_trend", "trend_change_one_hr",
                         "trend_change_three_hr", "trend_continuation_three_hr", "trend_continuation_one_hr", 
                         "three_hr_watt_trend")

