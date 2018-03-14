require(data.table)
require(magrittr)
require(jsonlite)
require(httr)
require(curl)

source("./config/config.R")
source("./functions/basic_cleaning.R")
source("./functions/windowed_features.R")
source("./functions/realtime_predictions.R")


TURBINE_API_URL <- config$TURBINE_API_URL
API_DURATION <- config$DATA_STREAMING_DURATION
POLL_INTERVAL <- config$POLL_INTERVAL
START_URL <- paste(TURBINE_API_URL, "start?duration=", API_DURATION, sep="")
POLL_URL <- paste(TURBINE_API_URL, "turbine", sep="")
SUBMIT_URL <- paste(TURBINE_API_URL, "prediction", sep="")

COEFFICIENTS <- read_coeficients(COEFFICIENT_PATH)
  
poll_turbine_endpoint <- function(poll_url) {
  poll_url %>%
    fromJSON %>%
    data.table %>%
    {.} -> new_data
  return(new_data)
}

remove_redundant_records <- function(new_records, timestamps_received) {
  if (new_records[,.N] > 0) {
    return(new_records[!ts %in% timestamps_received])
  } else { return(new_records) }
}

run <- function() {
  
  start_ts <- Sys.time()
  execution_duration <- (API_DURATION+3)*60
  turbine_records <- data.table()
  timestamps_received <- c()
  
  curl(START_URL) %T>% open %>% close()
  
  while (Sys.time()-start_ts < execution_duration) {
    new_records <- poll_turbine_endpoint(POLL_URL)
    new_records <- remove_redundant_records(new_records, timestamps_received)
    if (new_records[,.N] > 0) {
      cat("Generating", new_records[,.N], "new predictions\n")
      timestamps_received <- c(timestamps_received, new_records[,ts])
      for (row_num in c(1:new_records[,.N])) {
        new_row <- format_new_row(new_records, row_num)
        turbine_records <- prepend_to_turbine_records(new_row, turbine_records)
        prediction_records <- copy(turbine_records[1:min(180,.N)])
        make_prediction(prediction_records, SUBMIT_URL, COEFFICIENTS)
      }
      cat("Complete ... wait for more data...\n")
    }
    Sys.sleep(POLL_INTERVAL)
  }
}

########
########
run()
########
########