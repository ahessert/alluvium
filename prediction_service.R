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
TOTAL_RUN_TIME <- config$PREDICTION_SERVICE_UP_TIME * 60
POLL_INTERVAL <- config$POLL_INTERVAL
START_URL <- paste(TURBINE_API_URL, "start?duration=", API_DURATION, sep="")
POLL_URL <- paste(TURBINE_API_URL, "turbine", sep="")
SUBMIT_URL <- paste(TURBINE_API_URL, "prediction", sep="")

COEFFICIENTS <- read_coeficients(COEFFICIENT_PATH)
  
poll_turbine_endpoint <- function(poll_url, timestamps_received) {
  poll_url %>%
    fromJSON %>% 
    data.table %>%
    remove_redundant_records(timestamps_received) %>%
  {.} -> new_records
  return(new_records)
}

remove_redundant_records <- function(new_records, timestamps_received) {
  if (new_records[,.N] > 0) {
    return(new_records[!ts %in% timestamps_received])
  } else { return(new_records) }
}

process_new_row <- function(row_num, new_records, turbine_records) {
  new_records %>%
    format_new_row(row_num) %>%
    prepend_to_turbine_records(turbine_records) %>%
    head(n=180) %>% copy %>%
    make_prediction(SUBMIT_URL, COEFFICIENTS)
}

run <- function() {
  
  start_ts <- as.numeric(Sys.time())
  turbine_records <- data.table()
  timestamps_received <- c()
  curl(START_URL) %T>% open %>% close()
  
  while (as.numeric(Sys.time()) - start_ts < TOTAL_RUN_TIME) {
    new_records  <- poll_turbine_endpoint(POLL_URL, timestamps_received)
    
    if (new_records[,.N] > 0) { 
      cat("Generating", new_records[,.N], "new predictions\n")
      timestamps_received <- c(timestamps_received, new_records[,ts])
      lapply(c(1:new_records[,.N]), process_new_row, new_records, turbine_records)
      cat("Complete ... wait for more data...\n")
    }
    else { Sys.sleep(POLL_INTERVAL) }
  }
}

########
########
run()
########
########