## Create minutes with zero watts for current observation
create_current_down_time_feature <- function(dt, runmode="TRAIN") {
  dt <- dt[order(unix_ts)]
  first_zero_ts <- dt[1, unix_ts]
  minutes_with_zero_watts <- c()
  for (record in c(1:dt[,.N])) {
    if (!dt[record, watts] == 0) {
      first_zero_ts <- dt[record, unix_ts]
    }
    current_downtime <- (dt[record, unix_ts]-first_zero_ts)/60
    minutes_with_zero_watts <- c(minutes_with_zero_watts, current_downtime)
  }
  dt[,`:=` (current_downtime=minutes_with_zero_watts,
            current_downtime_sqd=minutes_with_zero_watts^2)]
  if (runmode == "LIVE") {dt <- dt[order(-unix_ts)]}
  return(dt)
}

get_random_offset <- function(dt, runmode) {
  if (runmode=="TRAIN") {
    seconds_offset <- sample(10*60, 1)
    ten_min_offset <- sample(6, 1)
  }
  else if (runmode == "LIVE") {
    seconds_offset <- -max(dt$unix_ts)
    ten_min_offset <- 0
  }
  else {
    stop("Error: Please choose runmode TRAIN or LIVE")
  }
  return(list(seconds=seconds_offset, ten_min=ten_min_offset))
}

################################################################
##      Decorrelate observations to avoid overfitting         ##
## MAX Time will be used to join 10min, 1hr, and 3hr rollups  ##
################################################################

create_rollup_features <- function(dt, runmode="LIVE") {
  offset = get_random_offset(dt, runmode)
  ten_min_rollup <- dt[, .(ts=max(ts),
                         ten_min_watt_avg=mean(watts),
                         ten_min_watt_trend=diff(.SD[c(1,.N),watts]),
                         ten_min_wind_avg=mean(windSpeed),
                         ten_min_wind_max=max(windSpeed),
                         ten_min_temp_avg=mean(wetBulbFahrenheit),
                         ten_min_count=.N),
                      by=.(ten_min_index=abs(unix_ts+offset$seconds) %/% (60*10))]

  one_hr_rollup <- ten_min_rollup[, .(ts=max(ts),
                                      one_hr_watt_avg=mean(ten_min_watt_avg),
                                      one_hr_watt_trend=diff(.SD[c(1,.N), ten_min_watt_avg]),
                                      one_hr_temp_change=diff(.SD[c(1,.N), ten_min_temp_avg]),
                                      one_hr_count=.N),
                                  by=.(one_hr_index=(ten_min_index+offset$ten_min) %/% 6)]

  three_hr_rollup <- ten_min_rollup[, .(ts=max(ts),
                                        three_hr_watt_avg=mean(ten_min_watt_avg),
                                        three_hr_watt_trend=mean(ten_min_watt_trend),
                                        three_hr_count=.N%/%6),
                                    by=.(three_hr_index=(ten_min_index+offset$ten_min) %/% 18)]

  rollup_dt <- dt[ten_min_rollup[one_hr_rollup[three_hr_rollup, on="ts"], on="ts"], on="ts"]
  return(rollup_dt)
}
  

create_trend_change_feature <- function(dt) {
  trend_change_1 <- dt[,ten_min_watt_trend*one_hr_watt_trend]
  dt[,`:=` (trend_continuation_one_hr=trend_change_1*sign(ten_min_watt_trend),
           trend_change_one_hr=trend_change_1*sign(ten_min_watt_trend))]
  dt[trend_change_1>0, trend_change_one_hr:=0]
  dt[trend_change_1<0, trend_continuation_one_hr:=0]
  
  trend_change_3 <- dt[,three_hr_watt_trend*one_hr_watt_trend]
  dt[,`:=` (trend_continuation_three_hr=trend_change_3*sign(ten_min_watt_trend),
            trend_change_three_hr=trend_change_3*sign(ten_min_watt_trend))]
  dt[trend_change_3>0, trend_change_three_hr:=0]
  dt[trend_change_3<0, trend_continuation_three_hr:=0]
  return(dt)
}


create_outcome_var <- function(dt) {
  dt[, `:=` (six_hour_watt_avg=(shift(three_hr_watt_avg, type = "lead", n=1)+ 
                                      shift(three_hr_watt_avg, type = "lead", n=2))/2,
                   total_hours=shift(three_hr_count, type="lead", n=1)+
                               shift(three_hr_count, type="lead", n=2))]

  ## Only use if 6 hours are present in outcome var
  dt <- dt[total_hours==6]
  
  ## Convert outcome to Kilowatt hours
  dt[, kilowatt_hours := (six_hour_watt_avg/1000)*6]
  return(dt)
}


