make_prediction <- function(prediction_records, turbine_endpoint, coefficients) {
  prediction_records <- clean_records(prediction_records)
  formatted_predictors <- format_predictors(prediction_records)
  prediction <- apply_coefficients(formatted_predictors, coefficients)
  response <- GET(turbine_endpoint, query = list("ts"=prediction_records[1, ts], "value"=prediction))
}

format_new_row <- function(new_records, rownum) {
 new_row <- new_records[rownum, .(ts=ts,
                                  state=state,
                                  rpm=as.numeric(rpm), 
                                  wetBulbFahrenheit=as.numeric(wetBulbFarenheit),
                                  relativeHumidity=as.numeric(relativeHumidity),
                                  windSpeed=as.numeric(windSpeed),
                                  windDirection=as.numeric(windDirection),
                                  hourlyPrecip=as.numeric(hourlyPrecip),
                                  watts=as.numeric(watts))][order(ts)]
  return(new_row)
}

prepend_to_turbine_records <- function(new_row, turbine_records) {
   if (length(turbine_records)==0) {
    return(new_row)
  }
  else {
    return(rbind(new_row, turbine_records))
  }
}

read_coeficients <- function(coefficients_path) {
  coefficients <- unlist(read_json(coefficients_path))
}

clean_records <- function(dt) {
  dt %>%
    convert_to_unix_ts %>%
    fix_nulls %>%
    create_sin_cos_features %>%
    create_current_down_time_feature(runmode = "LIVE") %>%
    create_state_dummies %>%
    {.} -> dt
  return(dt)
}

format_predictors <- function(dt) {
  dt %>% 
    create_rollup_features(runmode = "LIVE") %>%
    create_trend_change_feature %>%
    {.} -> formatted_predictors
  
  formatted_predictors <- formatted_predictors[,REGRESSION_FEATURES, with=F]

  if (dt[,diff(range(unix_ts))] < 150) {
    formatted_predictors[, c("trend_change_three_hr", "trend_continuation_three_hr", "trend_continuation_one_hr", 
                             "three_hr_watt_trend") := 0]
  }
  return(unlist(formatted_predictors[1]))
}

apply_coefficients <- function(record, coefficients) {
  intercept <- 1
  weighted_values <- c(intercept, record) * coefficients
  prediction <- round(sum(weighted_values), digits = 3)
  if (prediction < 0) { return(0.001) }
  return(prediction)
}