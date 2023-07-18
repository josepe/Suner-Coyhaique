# query_influxDB_columnar() is a function that queries the autos database in an InfluxDB connection and returns the data as a data frame. The query is specified by the select and where arguments, and the data is limited by the limit argument. The vehiculo argument specifies the measurement to be queried. The con argument specifies the InfluxDB connection. The returned data frame excludes columns statement_id, series_names, series_tags, and series_partial.
# fetch_telemetry() is a function that fetches telemetry data from an InfluxDB database. It takes in a connection (con), a list of vehicles (vehiculos_list), the variable of telemetry to fetch (var_telemetria), a query string (querystr), and a time zone (time_zone). It creates a query out of the query string and variable of telemetry, then iterates through the list of vehicles to query the InfluxDB database for the corresponding data. It then binds the results from each query together and sets the "time" column to the specified time zone. Finally, it returns the combined dataset.

# detect_peaks() is a function that takes a series of data points as input and uses the loess and frollapply functions to smooth the data and detect peaks and valleys. It then creates a data frame with the results and returns two elements: "smooth" which contains the smoothed data, and "peaks" which contains the detected peaks and valleys.
# detect_peaks_score() is a function that uses a score function to detect peaks in a series of data points. The score function is a custom function that is passed as an argument to the function. The function then returns a data frame with the detected peaks and valleys.
# cast_ts() is a function that converts a time series to a data frame with interpolated values. The function takes in a time series data frame and the variable of telemetry to cast (telem). It then creates a new data frame with the time and telemetry variable as the first two columns, and the remaining columns are interpolated values of the telemetry variable.
# join_cast_ts() is a function that joins two cast time series data frames. The function takes in two cast time series data frames and the variable of telemetry to join (telem). It then creates a new data frame that contains the joined data from the two input data frames.
# extract_continuous() is a function that extracts continuous segments from a time series. The function takes in a time series data frame and a window size (w). It then iterates through the time series and identifies segments of length w that are consecutive and non-overlapping. The function then returns a data frame that contains the start and end times of each segment.
# extrae_carga_descarga() is a function that extracts charge and discharge segments from a time series. The function takes in a time series data frame and a window size (w). It then identifies segments of the time series where the energy is increasing (charge) and segments where the energy is decreasing (discharge). The function then returns a data frame that contains the start and end times of each segment, as well as the type of segment (charge or discharge).


### QUERY INFLLUXDB ###############################
# This function queries the 'autos' database in an InfluxDB connection
# and returns the data as a data frame. The query is specified by the
# 'select' and 'where' arguments, and the data is limited by the
# 'limit' argument. The 'vehiculo' argument specifies the measurement
# to be queried. The 'con' argument specifies the InfluxDB connection.
# The returned data frame excludes columns 'statement_id',
# 'series_names', 'series_tags', and 'series_partial'.

query_influxDB_columnar <-
  function(vehiculo,
    query = list(select = "*", where = "NULL"),
    limit = NULL,
    con = con) {
    require(data.table)
    require(lubridate)
    influx_select(
      con = con,
      db = "autos",
      field_keys = query$select,
      where = query$where,
      #   include timesrange if necesary
      measurement = vehiculo,
      limit = limit,
      return_xts = FALSE,
      simplifyList = FALSE
    )  |>
      as.data.frame() |>
      setDT() |>
      DT(,
        c(
          "statement_id",
          "series_names",
          "series_tags",
          "series_partial"
        ) := NULL)
  }


### FETCH TELEMETRY ###############################
# This function fetches telemetry data from an InfluxDB database. It
# takes in a connection (con), a list of vehicles (vehiculos_list),
# the variable of telemetry to fetch (var_telemetria), a query string
# (querystr), and a time zone (time_zone). It creates a query out of
# the query string and variable of telemetry, then iterates through
# the list of vehicles to query the InfluxDB database for the
# corresponding data. It then binds the results from each query
# together and sets the "time" column to the specified time zone.
# Finally, it returns the combined dataset.

fetch_telemetry <- function(con,
  vehiculos_list,
  var_telemetria,
  querystr,
  time_zone) {
  query <- querystr[[var_telemetria]]
  y_list <- vehiculos_list |>
    map(\(x)
      (
        query_influxDB_columnar(
          x,
          query = query,
          limit = NULL,
          con = con
        ) |>
          setnames(2, var_telemetria)
      ))
  y_dt <- y_list |>
    rbindlist(idcol = "id") |> setkey(time)
  y_dt[, time := with_tz(y_dt$time, time_zone)]
  return(y_dt)
}





### READ_AND_CONVERT CSV FILES WITH TIME DIMENSION TO DATA TABLE  ###############################
# This function reads a CSV file into a data.table, converts the timezone of the 'time' column, 
# and optionally rounds the time points and merges the data.table with a provided base table.
# 
# Arguments:
# file: The path to the CSV file to be read.
# tbase: An optional base data.table that has a 'time' column. If provided, the function will round the time points in the input data.table to the minimum time difference in 'tbase', and then perform a left join with 'tbase' based on the 'time' column.
# timezone: The timezone to convert the 'time' column to. Default is "America/Santiago".
# 
# Returns:
# The resulting data.table after timezone conversion, and optional time rounding and merging with 'tbase'.

read_and_convert_time <- function(file, tbase = NULL, timezone = "America/Santiago") {
  
  # Read the file into a data.table
  df <- fread(file)
  
  # Convert the time column to the specified timezone
  df[, time := with_tz(time, tzone = timezone)]
  
  # If 'tbase' is provided...
  if (!is.null(tbase)) {
    # Calculate the minimum time difference in 'tbase'
    diff_unit <- get_min_time_difference(tbase)
    
    # Round the time points in the input data.table to the nearest time unit
    df[, time := round_date(time, unit = diff_unit)]
    
    # Perform a left join with 'tbase' based on the 'time' column
    df <- df[tbase, on = .(time), nomatch = NA]
  }
  
  # Return the resulting data.table
  return(df)
}

### CREATE TIMEBASE FUNCTION ###############################
### GET_MIN_TIME_DIFFERENCE   ###############################
# This function takes a data frame (or data.table) and the name of a time column as input.
# It calculates the differences between all consecutive timestamps in the specified column.
# Any differences that are zero (which may occur if there are duplicate timestamps) are removed.
# The function then finds the smallest non-zero difference and converts it to the appropriate units.
# The default unit is minutes, but this can be changed by modifying the 'units' argument in the as.numeric() function.
# The function returns this smallest difference as a string in the format "X mins".
#
# Arguments:
#   time_data: A data frame (or data.table) that contains a column of timestamps.
#   time_column: The name of the column in 'time_data' that contains the timestamps. Default is "time".
#
# Returns: 
#   A string in the format "X mins", where X is the smallest non-zero difference between consecutive timestamps.


get_min_time_difference <-
  function(time_data,
    time_column = "time",
    units = "mins") {
    # Calculate the differences between all times
    time_differences <- diff(time_data[[time_column]])
    
    # Remove differences that are zero
    time_differences <- time_differences[time_differences != 0]
    
    # Find the minimum difference
    min_difference <- min(time_differences, na.rm = TRUE)
    
    # Convert to appropriate units
    min_difference <- as.numeric(min_difference, units = units)
    
    # Return as "X mins"
    return(paste(min_difference, units))
  }


# This function creates a time base data.table with a 'time' column ranging from the start 
# to the end of a given period, with the specified time unit and timezone.
#
# Arguments:
# periodo: The period to create the time base for.
# time_unit: The unit of time for the time steps.
# timezone: The timezone to set for the 'time' column.
#
# Returns:
# A data.table with a 'time' column that ranges from the start to the end of 'periodo', with steps of 'time_unit' and in the 'timezone' timezone.

create_timebase <- function(periodo, time_unit = "1 mins", timezone = "America/Santiago") {
  # Create a sequence from the start to the end of 'periodo' with steps of 'time_unit'
  tbase <- seq(lubridate::int_start(periodo), lubridate::int_end(periodo), by = time_unit) |>
    # Convert the sequence to a data.table
    data.table::as.data.table() |>
    # Rename the first column to 'time'
    data.table::setnames(1, "time")
  
  # Add an 'ind' column that indicates the row index
  tbase[, ind := .I]
  
  # Round the 'time' points to the nearest 'time_unit'
  tbase[, time := lubridate::round_date(time, unit = time_unit)] |>
    # Set 'time' as the key column for fast subset and join operations
    data.table::setkey(time)
  
  # Change the timezone of the 'time' column to 'timezone'
  tbase[, time := lubridate::with_tz(time, tzone = timezone)]
  
  return(tbase)
}



### DETECT PEAKS ###############################
# This function takes a series of data points as input and uses the
# loess and frollapply functions to smooth the data and detect peaks
# and valleys. It then creates a data frame with the results and
# returns two elements: "smooth" which contains the smoothed data, and
# "peaks" which contains the detected peaks and valleys.

detect_peaks <-
  function(series,...)  {
    raw_series <- series[, .(j,
      vari)]
    
    span <- npoints / nrow(series)
    y_smoothed_obj <-
      loess(vari ~ j,
        data = series,
        span = span)
    y_smoothed <-
      predict(y_smoothed_obj)
    
    x_roll <-
      frollapply(raw_series$j,
        2 * w + 1,
        median,
        align = "center",
        fill = NA)
    
    y_roll_max <-
      frollapply(y_smoothed,
        2 * w + 1,
        max,
        align = "center",
        fill = NA)
    
    y_roll_min <-
      frollapply(y_smoothed,
        2 * w + 1,
        min,
        align = "center",
        fill = NA)

    out1 <- raw_series[, .(
      j,
      j.roll = x_roll,
      vari,
      vari_smooth = y_smoothed,
      vari_max = y_roll_max,
      vari_min = y_roll_min
    )]
    
    boundaries <- rbind(
      out1[1, .(j, vari, ext = NA)],
      out1[vari_smooth == vari_max, .(j, vari, ext = as.factor("MAX"))], #peaks
      out1[vari_smooth == vari_min, .(j, vari, ext = as.factor("MIN"))], #valleys
      out1[.N, .(j, vari, ext = NA)]) |>
      setkey(j)
    
    boundaries[.N,
      ext := fcase(
        boundaries[.N - 1, ext] == "MAX",
        "MIN",
        boundaries[.N - 1, ext] == "MIN",
        "MAX",
        boundaries[.N - 1, ext] |> is.na(),
        fifelse(vari > boundaries[.N - 1, vari],
          "MAX",
          "MIN")
      )]
    boundaries[1,
      ext := fcase(
        boundaries[2, ext] == "MAX",
        "MIN",
        boundaries[2, ext] == "MIN",
        "MAX",
        boundaries[2, ext]  |> is.na(),
        fifelse(vari > boundaries[2, vari],
          "MAX",
          "MIN")
      )]
    boundaries[, c("j.next", "ext_next") :=
        lapply(.SD,
          \(x) (shift(x, type = "lead"))),
      .SDcols = c("j", "ext")] |>
      na.omit()
    boundaries[, delta_e := fcase(ext == "MAX" &
        ext_next == "MIN",
      "descarga",
      ext == "MIN" & ext_next == "MAX",
      "carga")][, vari := NULL]
    
    return(list(smooth = out1,
      peaks = boundaries |> na.omit()))
    
  }


### DETECT PEAKS POR SCORE #######################

detect_peaks_score <- function(series,...) {
  
  raw_series <- series[, .(j,
    vari)]
  
  span <- npoints / nrow(series)
  # paste0("span = ", span) |> print()
  # paste0("npoints = ", npoints) |> print()
  
  y_smoothed_obj <-
    loess(vari ~ j,
      data = series,
      span = span)
  y_smoothed <-
    predict(y_smoothed_obj)
  
  out1 <- raw_series[, .(j,
    vari,
    vari_smooth = y_smoothed)]
  
out1[, .(j,vari_smooth = vari_smooth |> na_kalman(model = "auto.arima"))][,
    vari_smooth_valley := -vari_smooth]
      peaks_location <- to_peaks[,   detect_d(vari_smooth,w_detect, w_score)]
  valleys_location <- to_peaks[, detect_d(vari_smooth_valley,w_detect, w_score)]

  boundaries <- rbind(
    out1[peaks_location, .(j, vari, ext = as.factor("MAX"))],      # peaks
    out1[valleys_location, .(j, vari, ext = as.factor("MIN"))] )|>  # valleys
      setkey(j)
  boundaries[, c("j.next", "ext_next","vari_next") :=
      lapply(.SD,
        \(x) (shift(x, type = "lead"))),
      .SDcols = c("j", "ext","vari")] |>
    na.omit()

 boundaries[, let(
    delta_e = fcase(
    ext == "MAX" & ext_next == "MIN", "descarga",
    ext == "MIN" & ext_next == "MAX", "carga"
    ),
    del_vari = abs(vari-vari_next)   ########### FIX ME (1) ################
    )]
 # if(boundaries[del_vari>20]) {browse()}

### avoid "tickmark" effect (use SOC signal instead of energy to detect peaks)
  boundaries = boundaries |> na.omit()
  boundaries[, c("vari","vari_next") := NULL]
    return(list(smooth = out1,
    peaks = boundaries))
  }



### DETECT_D (TEST) ###############################
# function detect_d,takes three arguments:
# 
# x: a numeric vector representing the input signal. w_detect: a
# scalar value representing the width of the window used for detecting
# local maxima. w_score: a scalar value representing the width of the
# window used for computing a score. The function first applies a peak
# detection algorithm called detect_localmaxima to x using the window
# width w_detect. This returns a logical vector local_peaks, where
# each element is TRUE if the corresponding value in x is a local
# maximum within the window width.
#
# The function then computes a score for each element in x using a
# function called score_type2 and a window width w_score. The
# resulting score vector is stored in score.
#
# Next, the function creates a logical vector called true_peaks, where
# each element is TRUE if the corresponding element in local_peaks is
# also TRUE and the corresponding element in score is greater than a
# threshold value called score_thres. The threshold value is not
# defined in the given code snippet.
#
# Finally, the function extracts the indices of the TRUE elements in
# true_peaks using the which() function, and stores them in a variable
# called i.
#
# Overall, the function detect_d identifies local maxima in x that
# have a score greater than a threshold value, and returns their
# indices.

detect_d <- function(x, w_detect, w_score) {
    local_peaks <- detect_localmaxima(x, w = w_detect)
    score <- score_type2(x, w = w_score)
    true_peaks <- score > score_thres & local_peaks
    i <- true_peaks |> which()
  }

### CAST_TS PREPARE DATA FOR BULK SEGMENT EXTRACTION ###############################

cast_ts <-
  function(x_ts, telem, gap_min) {
    # series must be regularly sampled
    
    cx_ts <-
      x_ts |> dcast(time + ind ~ id,
        value.var = telem,
        fill = NA) |>
      DT(, "NA" := NULL) |>
      setDT()
    
    dt_gap <-
      cx_ts[1:2, time] |>  #  Extracts the time column from the first two rows of cx_ts
      int_diff() |>  # Calculates the difference between each element of the time vector (i.e. the time gap between row 1 and row 2)
      int_length()   # Converts the time interval difference into a scalar number representing the length of the interval in seconds.
    
    n_points <-
      # for example, if gap_min=10 and sampling period = 1 min then n_points = 10
      gap_min / (dt_gap / 60) |> # dt_gap/60 converts gap from seconds to minutes
      round()
    
    cx_ts[,
      c('coy', 'coy2', 'coy3', 'coy4', 'coy5', 'coy7') :=
        lapply(.SD[, ], \(x)(na_kalman(x, maxgap = n_points, model = "auto.arima"))),
      .SDcols = c('coy', 'coy2', 'coy3', 'coy4', 'coy5', 'coy7')] |> setkey(time)
    
  }

### JOIN_CAST_TS (JOIN DIFFERENT TIME SERIES) ###############################

# Function to join and cast two data frames 
join_cast_ts <- function(cast_x, cast_y, telem) {
 telem_x=telem[1]
 telem_y=telem[2]

# Melt the first data table
  melt_x <- melt.data.table(cast_x,id.vars=c("time","ind"),
    measure.vars= patterns("^coy", cols=names(cast_x)),
    variable.name = "id",
    value.name = telem_x
      ) |> setDT() |> setkey(time,ind,id)
 
# Melt the second data table
   melt_y <- melt.data.table(cast_y,id.vars=c("time","ind"),
    measure.vars= patterns("^coy", cols=names(cast_y)),
    variable.name = "id",
    value.name = telem_y
      ) |> setDT() |> setkey(time,ind,id)
  
# Merge the melted data tables
  out_ts <- merge(melt_x, melt_y, all = TRUE)

# Return the merged data table
  return(out_ts)
  
}


### IMPUTE_CONSTANT_SEGMENT_TO_NA IMPUTE NA IN SEGMENTS W/NO CHANGE ###############################

impute_constant_segment_to_NA <-
  function(serie,
    # (select por colectivo)
    vari = "vari",
    # measured variable (eg, ene, odo, vel_ecu )
    range_vari = NULL,
    # full range of measured data (eg, 0-52 kwH for energy, 1-100 % for SOC, etc)
    min_span_vari = 0.01,
    # minimum fraction of full range accepted for selection (eg 0.1 * range_vari)
    min_minutos_silencio = 25) {
    # minimun event duration (short events are discarded)
    
    setnames(serie, vari, "vari")
    serie <- serie[, .(time, ind, vari)]
    
    # locf impute NAns
    min_minutos_silencio = 10
    serie2 <- copy(serie)[,
      vari := vari |> na_ma(maxgap = 10)][,
        let(
          vari_roll_mad =
            frollapply(
              vari,
              min_minutos_silencio,
              sd,
              align = "center",
              fill = NA
            ) ,
          vari_rm = vari
        )][,
          vari_roll_mad := vari_roll_mad |> na_replace(fill = min(vari_roll_mad))][
            #vari_roll_mad <= min_span_vari *range_vari / (2.5*min_minutos_silencio),
            vari_roll_mad <= 10,
            vari_rm := NA]
    serie2 <- serie2[, .(time, ind, vari_rm)] |>
      setnames("vari_rm", vari)
    
    return(serie2)
  }


### UNINTERRUPTED SEGMENTS OF THE SERIES ###############################  

extract_continuous <-
  function(serie,
    # (select por colectivo)
    vari = "vari",
    # measured variable (eg, ene, odo, vel_ecu )
    range_vari,
    min_minutos_gap = 20,
    min_minutos_duration = 30,
    max_na = 0.2,
    min_span_vari = 0.3,
    min_minutos_silencio = 25)   {
    serie |> setnames(vari, "vari")
    serie <- serie[, .(time, ind, vari)]
    
    # Imputar NA a tramos en que no hay cambio
    serie <-
      impute_constant_segment_to_NA(
        serie = serie,
        vari = "vari",
        range_vari = range_vari,
        min_span_vari,
        min_minutos_silencio
      )
    
    to_replace <-
      serie[!is.na(vari)] # primeros puntos de cada segmento
    
    segmentos_temp <-
      copy(to_replace)[,
        let(time_lag = shift(time, type = "lag"),
          ind_lag = shift(ind, type = "lag"))][,
            gap := difftime(time, time_lag)][ind == first(ind) |
                # (se agrega el primer punto de la serie
                gap > duration(min_minutos_gap, "mins")] |> setkey(time)
    
    segmentos_ini <-
      segmentos_temp[, .(start = time, ind_start = ind)] |> setkey(start, ind_start)
    segmentos_end <-
      segmentos_temp[, .(end = shift(time_lag, type = "lead"),
        ind_end = shift(ind_lag, type = "lead"))] |>  na.omit() |>
      rbind(to_replace[, .(end = last(time), ind_end = last(ind))]) |> setkey(end, ind_end)
    
    segmentos <-
      copy(segmentos_ini)[,
        c("end", "ind_end") := segmentos_end]
    segmentos <-
      segmentos[!(ind_start == ind_end)][end - start > duration(min_minutos_duration, "mins")] |>
      setkey(start, end)
    
    
    for (k in segmentos[, .I]) {
      intervalo_k <-  c(segmentos[k, start], segmentos[k, end])
      serie_k <- serie[time %between% intervalo_k]
      # print(paste0( " ", k))
      
      segmentos[k,
        let(mad_k =
            serie_k[!is.na(vari)][,
              as.double(mad(vari))],
          na_frac =
            serie_k[,
              sum(is.na(vari)) / .N],
          range_k =
            serie_k[!is.na(vari)][,
              as.double(diff(range(vari)))])]
      
    }
    
    # segmentos <- segmentos[mad_k> as.double(min_span_vari*range_vari) &
    #     range_k > as.double(min_span_vari*range_vari)]
    #
    serie |> setnames("vari", vari)
    
    return(list(serie = serie,
      segmentos = segmentos))
  }


# _____________________________________________________________
# SPLIT CHARGE AND DISCHARGE PERIODS (WITH LOESS SMOOTHING) ----

extrae_carga_descarga <-
  function(serie_segmentos,
   vari="vari",                 # vehicle id is column name of measured variable (eg, ene, soc vel_ecu )
    range_vari,                
    max_x_no_change = 30,        
    min_y_no_change = 0.05,   ### por qu'e
    FUN = detect_peaks_score,...) {
    
    serie <- serie_segmentos$serie 
    segmentos <- serie_segmentos$segmentos
    carga_descarga <- list()
    
    for (k in segmentos[,.I]) {
      print(k) 
      intervalo_k <-  c(segmentos[k, start], segmentos[k, end])
      segmento_k <-
        serie[time %between% intervalo_k] |> 
       setnames(vari,"vari") |> 
        DT(, .(
          j = ind, 
          vari= na_kalman(vari,model="auto.arima")
          )) 
      
      
      if(segmento_k[, max(vari)- min(vari)] <= min_y_no_change){    #### FIX ME (2) ?? ####
       
        carga_descarga[[k]] <-NULL} else {
        cd <- segmento_k |> FUN(...)    ###################### PEAK DETECTION FUNCTION ########################
        carga_descarga[[k]] <- cd$peaks
        
        
        }
    }
    
    dt_carga_descarga <-
      rbindlist(carga_descarga) |> DT(, c("ext", "ext_next") := NULL) |>
      DT(,
        .(
          ind_start = j,
          ind_end = j.next,
          delta_e,
          del_vari
        )) 
    carga_descarga <-
      dt_carga_descarga[serie, on = .(ind_start == ind), nomatch = NULL]
   
     carga_descarga <-
      carga_descarga[serie, on = .(ind_end == ind), nomatch = NULL]
    
    carga_descarga <- 
      carga_descarga[, .(
      start = time,
      ind_start,
      end = i.time,
      ind_end,
      delta=delta_e,
        del_vari=del_vari)]
    
      return(carga_descarga)
  }

# ______________________________________________________
## select segment ----

select_segment <- function(serie, segmentos, id_segment) {
  selected <-
    serie[time %between% segmentos[id_segment, c(start, end)]]
  #####  IN PROGRESS #####
}


# _____________________________________________________
## generate list of ggplot plot objects ----
#  to plot several segments of charge / discharge lapses

plot_segment <- function(dt_cast, dt_segments) {
  intervalos <-  dt_segments[, .(id, start, end)]
  vari <- dt_segments[, id] |> unique()
  dt_melt <- dt_cast[, cbind(.SD), .SDcols = c("time", vari)] |>
    melt(measure.vars = vari, variable.name = "id") |>
    setDT()     # don't forge to setDT after melt
  
  gglist <- list()
  
  for (k in dt_segments[,.I]) {
    print(k)
    var <- intervalos[k, id]
    betw <- intervalos[k, .(start, end)]
    toplot <-
      dt_melt[time %between% betw][id == var]
    gglist[[k]] <-
      toplot |>
      ggplot() +
      aes(x = time, y = value) +
      geom_line(size = 0.5, colour = "#112446") +
      labs(caption = paste0("start = ", betw$start) ,
            subtitle = paste0("Coyhaique colectivo ", var) ) +
      theme_minimal() +
      theme( plot.caption = element_text(hjust = 0))
  }
  return(gglist)
  }


# _____________________________________________________
## Generate table of charging/discharging curves aligned to given SOC of kWh values ----
#  IN PROGRESS

# charge_aligned <- function(dt_cast,dt_segments,center=0.5)
#   intervalos <-  dt_segments[, .(id, start, end)]
#   vari <- dt_segments[, id] |> unique()
#   dt_melt <- dt_cast[, cbind(.SD), .SDcols = c("time", vari)] |>
#     melt(measure.vars = vari, variable.name = "id") |>
#     setDT()   
  
#########  IN PROGRESS #####
  
# _____________________________________________________
###### Reindex by odometer ----
# Index series by distance covered instead of time 
#####  IN PROGRESS #####

# invert_var <- 
# function(series,vari,spanvari){ # spanvari is a vector of points to interpolate 
# }
#   
# # _____________________________________________________
#####  Loess Derivative ----
#   #library(DAMisc)
#   
#   y_smoothed_obj <-
#     loess(vari ~ j,
#       data = series,
#       span = 0.005)
#   y_smoothed <-
#     predict(y_smoothed_obj)
#   
#   
  
  
#####  IN PROGRESS #####
