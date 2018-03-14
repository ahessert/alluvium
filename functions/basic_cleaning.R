########################
### Feature Cleaning ###
########################

## Convert to unix timestamp and examine gap
convert_to_unix_ts <- function(dt) {
  dt[, unix_ts := as.numeric(as.POSIXct(ts, tz="UTC"))]
  return(dt)
}

fix_nulls <- function(dt) {
  dt[is.na(hourlyPrecip), hourlyPrecip:=0] 
  dt[, wind_dir_null_dummy:=0]
  dt[is.na(windDirection), `:=` (wind_dir_null_dummy=1, windDirection=sample(360, 1))]
  dt[is.na(rpm), rpm:=0]
  return(dt)
}


create_sin_cos_features <- function(dt) {
  dt[, `:=` (sin_hour=sin(pi*hour(ts)/12),
             cos_hour=cos(pi*hour(ts)/12),
             sin_wind=sin(pi*windDirection/180),
             cos_wind=cos(pi*windDirection/180))]
  return(dt)
}

create_state_dummies <- function(dt) {
  dt[, `:=` (state_power_ramp_up=1*(state=="Power ramp-up"),
             state_running=1*(state=="Running"),
             state_soft_start=1*(state=="Soft Start"),
             state_enabled=1*(state=="Enabled")
             )]
  return(dt)
}
