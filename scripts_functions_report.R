# # periodo:            
#      vector of dates specifying the time period for which the data should be fetched.
# # autos_coyhaique:    
#      vector of vehicle IDs for the taxis in Coyhaique.
# # querystr_ev500_2:     
#      string containing the InfluxDB query for fetching data for the taxis in Coyhaique.
# # con:                
#      connection to the InfluxDB database.
# # fetch_telemetry:   
#      function that fetches telemetry data from InfluxDB.
# # detect_peaks:      
#      function that detects peaks in a time series.
# # cast_ts:            
#      function that converts a time series to a data frame with interpolated values.
# # join_cast_ts:       
#      function that joins two cast time series data frames.
# # extract_continuous: 
#      function that extracts continuous segments from a time series.
# # extrae_carga_descarga: 
#      function that extracts charge-discharge segments from a time series.
# # odo: 
#      data frame containing the odometer data for the taxis in Coyhaique.
# # ene: 
#      data frame containing the energy data for the taxis in Coyhaique.
# # soc:
#      data frame containing the state of charge data for the taxis in Coyhaique.
# # vel_ecu: 
#      data frame containing the speed data from the ECU for the taxis in Coyhaique.
# # gps: 
#      data frame containing the GPS data for the taxis in Coyhaique.
# # tbase:
#      data frame containing a base time series with one minute resolution for the time period specified in periodo.
# # odo_ts: 
#      cast time series data frame containing the odometer data for the taxis in Coyhaique.
# # ene_ts: 
#      cast time series data frame containing the energy data for the taxis in Coyhaique.
# # soc_ts: 
#      cast time series data frame containing the state of charge data for the taxis in Coyhaique.
# # vel_ecu_ts: 
#      cast time series data frame containing the speed data from the ECU for the taxis in Coyhaique.
# # gps_ts: 
#      cast time series data frame containing the GPS data for the taxis in Coyhaique.
# # soc_ene: 
#      joined data frame containing the odometer, energy, and state of charge data for the taxis in Coyhaique.
# # odo_ene: 
#      joined data frame containing the odometer and energy data for the taxis in Coyhaique.
# # odo_alt: 
#      joined data frame containing the odometer and GPS data for the taxis in Coyhaique.
# # dt_segments_ene: 
#      data frame containing the charge-discharge segments for the taxis in Coyhaique.
# # betw:
#      data frame containing the start and end times of the charge-discharge segments for the taxis in Coyhaique.
# # ggtopl:
#      ggplot object containing a plot of the charge-discharge segments for the taxis in Coyhaique.
# # odo_day:
#      data frame containing the daily odometer readings for the taxis in Coyhaique.
# # odo_hour: 
#      data frame containing the hourly odometer readings for the taxis in Coyhaique.
# # odo_hour2: 
#      data frame containing the hourly odometer readings for the taxis in Coyhaique with the odometer readings aggregated by hour of the day.
# # soctr: 
#      data frame containing the smoothed odometer readings for the taxis in Coyhaique.
# # stest: 
#      data frame containing a subset of the smoothed odometer readings for the taxis in Coyhaique.
# # soct: 
#      data frame containing the smoothed odometer and state of charge readings for the taxis in Coyhaique.
# # odo_ene_ts:
#      time series data frame containing the odometer and energy readings for the taxis in Coyhaique.


########################################################################

# BASE TIME ----
tbase <- create_timebase(periodo=periodo,time_unit = time_res ,time_zone = "America/Santiago" ) # time_res defined in config_report_coyhaique.R


####  TELEMETRY VARIABLES FROM csv FILES *** IN LOCAL DIRECTORY *** ####
files <- c(
  "vel_ecu.csv",
  "odo.csv",
  "gps.csv",
  "ene.csv",
  "soc.csv") 

#### READ FILES TO VARIABLES ON A LIST ####
list_of_dfs <- lapply(files,\(x) read_and_convert_time(x,time_unit = time_res))


#### Assign each data.frame to a variable in the global environment ####

for (i in seq_along(list_of_dfs)) {
  # Extract the base name of the file (without extension)
  base_name <- tools::file_path_sans_ext(basename(files[i]))
  # Assign the data.frame to a variable with the base name
  assign(base_name, list_of_dfs[[i]])
}

rm(list_of_dfs)


# Odometro base ----
#### CONTINUAR !! ####

# Distancia recorrida base, en km ----
odo_ts <- odo_ts[odo < max_odo] # max_odo defined in config_report file
sbase <- seq(min(odo_ts$odo), max(odo_ts$odo) * 1.1, by = 0.5) |>
  as.data.table() |>
  setnames(1, "odo")
odo_s <- copy(odo_ts)[, odo := round(2 * odo) / 2]

### Energy ----
ene <- ene[ene > 0][, .(ene = mean(ene)), by = .(time, id)] |> setkey(time, id)
ene <- ene[tbase, on = .(time), nomatch = NA]


### SOC ----
soc <- soc[soc > 0][, .(soc = mean(soc)), by = .(time, id)] |> setkey(time, id)
soc <- soc[tbase, on = .(time), nomatch = NA]


### Vel_ecu ----
vel_ecu |> setcolorder(c("time","id","vel_ecu"))
vel_ecu <- vel_ecu[vel_ecu >= 0][vel_ecu < 160][, .(vel_ecu = mean(vel_ecu)), by = .(time, id)] |> setkey(time, id)
vel_ecu <- vel_ecu[tbase, on = .(time), nomatch = NA]


### Odometer ----
odo <- odo[odo > 0][odo < max(odo)][, .(odo = min(odo)), by = .(time, id)] |> setkey(time, id)
odo <- odo[tbase, on = .(time), nomatch = NA]


### GPS ----
gps <- gps[, lapply(.SD, mean), .SDcols = c("a", "lat", "lon", "vel"), by = .(time, id)] |> setkey(time, id)
gps <- gps[tbase, on = .(time), nomatch = NA]


# _______________________________________________________
### JOINs ----

soc_ene <- merge(soc_ts, ene_ts, all = TRUE, by = c("time", "id", "ind"))
soc_ene_samp <- soc_ene[sample(.N, 50000)]
odo_ene <- merge(odo_ts, ene_ts, all = TRUE, by = c("time", "id", "ind"))
odo_alt <- merge(odo_ts, gps_ts, all = TRUE, by = c("time", "id", "ind")) |> na.omit()
odo_alt_samp <- odo_alt[odo>500 & odo<30000][a<500 & a>200][seq(1,.N,10)]

## Impute NAs -----

cast_ene     <- cast_ts(ene, "ene", gap_min = 10)
cast_soc     <- cast_ts(soc, "soc", gap_min = 10)
cast_odo     <- cast_ts(odo, "odo", gap_min = 10)
cast_vel_ecu <- cast_ts(vel_ecu, "vel_ecu", gap_min = 10)

ene_odo      <- join_cast_ts(cast_ene, cast_odo, telem = c("ene", "odo"))
soc_odo      <- join_cast_ts(cast_ene, cast_odo, telem = c("soc", "odo"))
alt_odo      <- join_cast_ts(cast_ene, cast_gps, telem = c("gps", "odo"))

# _______________________________________________________
# Extraer segmentos Energía ----
# Para segmentar periodos de carga-descarga es mejor usar la señal SOC
# señal odo para segmentar periodos detenido y movimiento

source("config_report_coyhaique.R")

segments_soc <- list()
for (vari in autos_coyhaique |> names()) {
  print(vari)
  f1 <- 
    copy(cast_soc) |>
    extract_continuous(
      vari = vari,
      range_vari = range_vari, # this would be 100 for SOC, 52 for energy, etc.
    )
     f2  <- f1 |>  
      extrae_carga_descarga(
        vari = vari,
        range_vari = range_vari,
        max_x_no_change = max_x_no_change,
        min_y_no_change = min_y_no_change,
        FUN = detect_peaks_score,
        npoints = npoints,
        w = w,
        w_detect = w_detect,
        w_score = w_score,
        score_thres = score_thres
      )
     segments_soc[[vari]] <- f2
  }

dt_segments_soc <- rbindlist(segments_soc, idcol = "id") 
dt_segments_ene <- rbindlist(segments_ene, idcol = "id") 

segments_ene <- list()
for (vari in autos_coyhaique |> names()) {
  f1 <- 
    copy(cast_ene_ts) |>
    extract_continuous(
      vari = vari,
      range_vari = range_vari
    )
     segments_ene[[vari]] <-
      extrae_carga_descarga(f1,
        vari = vari,
        range_vari = range_vari,
        max_x_no_change = max_x_no_change,
        min_y_no_change = min_y_no_change,
        FUN = detect_peaks_score,
        npoints = npoints,
        w = w,
        w_detect = w_detect,
        w_score = w_score,
        score_thres = score_thres
      )
  }



# _____________________________________________________________
# PLot all segments *series ----

betw <-
  dt_segments_ene[delta == "descarga"][id == "coy2", .(start, end)][sample(.N, 30)]

betw <-
  seg_coy2[ind_start %in% c(110166, 110308)][, .(start, end)]

toplot <- cast_ene_ts[, .(time, coy2)][, pin := time %inrange% betw]

ggtopl <- toplot[pin == TRUE] |>
  ggplot() +
  aes(x = time, y = coy2, colour = pin) +
  geom_point(size = 0.5) +
  theme_minimal()
# plotear SOC vs energia
# plotear SOC completo con SOC en energ'ia segmentos

# _____________________________________________________________
# Odómetro por dia ----
odo_day <- odo[time %within% periodo][
  ,
  `:=`(day = date(time), time = NULL)
][,
  rec_diario := max(odo) - min(odo),
  by = c("id", "day")
] |>
  unique(by = c("id", "day"))
# rowidv(cols="id",prefix = "del.")
odo_day[, finde :=
  fifelse(
    lubridate::wday(day) < 5,
    "semana",
    "finde"
  )]

odo_hour <- odo[time %within% periodo][
  ,
  `:=`(
    day = date(time),
    hora = hour(time),
    time = NULL
  )
][lubridate::wday(day) < 5][,
  rec_horario := max(odo) - min(odo),
  by = c("id", "day", "hora")
][,
  rec_hora := sum(rec_horario),
  by = c("id", "hora")
][, c("odo", "day", "rec_horario") := NULL] |>
  unique(by = c("id", "hora"))

odo_hour2 <- odo[time %within% periodo][
  ,
  `:=`(
    day = date(time),
    hora = as.factor(hour(time)),
    time = NULL
  )
][lubridate::wday(day) < 5][,
  rec_horario := max(odo) - min(odo),
  by = c("id", "day", "hora")
][, c("odo") := NULL] |>
  unique()


# _____________________________________________________________
# Tests ----
soctr <- soc[tbase, roll = -1, on = "time"]
soctr[, socm := mean(soc), by = .(id, time)]
soctr <- soctr |> unique()
soctr <- tbase[soc, on = "time", nomatch = NULL, .(i.soc)]
stest <- soc[time > "2022-06-07 10:16:00" & time < "2022-06-10 15:16:00"][sample(.N, 20)]
soct <- stest[tbtest, roll = -1, on = .(time = time_ind)]
soct[, socm := mean(soc), by = .(id, time)]
soct <- soct |> unique()
soct <- tbtest[stest, on = .(time), nomatch = NULL]
soct[time == "2022-07-08 13:56:00"]
soct[ttime == "2022-07-08 15:43:00"]


# _____________________________________________________________
### Análisis carga descarga ----

# odo_ene_ts <-
