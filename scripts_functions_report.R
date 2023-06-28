########################################################################
# FETCH (series de tiempo) ----

# ____________________________________________________________
### Odometro ----

odo <-
  fetch_telemetry(
    con = con,
    vehiculo = autos_coyhaique,
    querystr = querystr_ev500,
    var_telemetria="odo",
    time_zone = "America/Santiago"
  )
odo[odo < 50000]
odo <- odo[time %within% periodo]
filename_odo <- str_c("../datos_colectivos/colectivos_odo_",periodo |> as.character(),".csv")
fwrite(odo,filename_odo )


# ____________________________________________________________
### Energia ----

ene <-
  fetch_telemetry(
    con = con,
    vehiculo = autos_coyhaique,
    querystr = querystr_ev500,
    var_telemetria="ene",
    time_zone = "America/Santiago"
  )

scale1 <- 1 / 64
rem1 <- 200 #(referencia batería en cero)

ene <- ene[ene < 20000][, 
  ene := ene * scale1 - rem1][ ##OJO, formula arroja valores > 52 (id=coy en agosto) chequear versus SOC escalado a 52
    ene>0]
ene <- ene[time %within% periodo]
filename_ene <- str_c("../datos_colectivos/colectivos_ene_",periodo |> as.character(),".csv")
fwrite(ene,filename_ene)

# ____________________________________________________________
### SOC ----

soc <-
  fetch_telemetry(
    con = con,
    vehiculo = autos_coyhaique,
    querystr = querystr_ev500,
    var_telemetria="soc",
    time_zone = "America/Santiago"
  )
soc <- soc[time %within% periodo]
soc <- soc[soc>0]
filename_soc <- str_c("../datos_colectivos/colectivos_soc_",periodo |> as.character(),".csv")
fwrite(soc,filename_soc)

# ____________________________________________________________
### VEL_ECU ----

vel_ecu <-
  fetch_telemetry(
    con = con,
    vehiculo = autos_coyhaique,
    querystr = querystr_ev500,
    var_telemetria="vel_ecu",
    time_zone = "America/Santiago"
  )
vel_ecu <- vel_ecu[time %within% periodo]
filename_vel_ecu <- str_c("../datos_colectivos/colectivos_vel_ecu_",periodo |> as.character(),".csv")
fwrite(vel_ecu,filename_vel_ecu)

# ____________________________________________________________
### GPS ----

gps <-
  fetch_telemetry(
    con = con,
    vehiculo = autos_coyhaique,
    querystr = querystr_ev500,
    var_telemetria="gps",
    time_zone = "America/Santiago"
  )

gps |> setnames(2, "time")
gps |> setnames(3, "lat")
gps <- gps[time %within% periodo]
filename_gps <- str_c("../datos_colectivos/colectivos_gps_",periodo |> as.character(),".csv")
fwrite(gps,filename_gps)

# ____________________________________________________________
### F Regenerativo ----

regenera <-
  fetch_telemetry(
    con = con,
    vehiculo = autos_coyhaique,
    querystr = querystr_ev500,
    var_telemetria="regenera",
    time_zone = "America/Santiago"
  )
filename_regenera <- str_c("../datos_colectivos/colectivos_regenera_",periodo |> as.character(),".csv")
fwrite(regenera,filename_regenera)

# _________________________________________________________
# Series de tiempo  

# Tiempo base ----
tbase <- seq(int_start(periodo),int_end(periodo),by= "1 min") |> as.data.table() |> setnames(1,"time") 
tbase[,ind:=.I]
tbase[,time := round_date(time, unit = "1 min")] |> setkey(time)

# Odometro base ----
#### CONTINUAR !! ####

# Distancia recorrida base, en km ----
odo_ts <- odo_ts[odo<70000]
sbase <- seq(min(odo_ts$odo),max(odo_ts$odo)*1.1,by=0.5) |>
  as.data.table() |> 
  setnames(1,"odo") 
odo_s <- copy(odo_ts)[,odo:=round(2*odo)/2]

### Energy ----
ene[,time := round_date(time, unit = "1 min")]
ene <- ene[ene>0][,.(ene=mean(ene)),by=.(time,id)] |> setkey(time,id)
ene_ts <- ene[tbase,on=.(time),nomatch=NA]


### SOC ----
soc[,time := round_date(time, unit = "1 min")]
soc <- soc[soc>0][,.(soc=mean(soc)),by=.(time,id)] |> setkey(time,id)
soc_ts <- soc[tbase,on=.(time),nomatch=NA]  


### Vel_ecu ----
vel_ecu[,time := round_date(time, unit = "1 min")]
vel_ecu <- vel_ecu[vel_ecu >= 0][vel_ecu<160][,.(vel_ecu=mean(vel_ecu)),by=.(time,id)] |> setkey(time,id)
vel_ecu_ts <- vel_ecu[tbase,on=.(time),nomatch=NA]


### Odometer ----
odo[,time := round_date(time, unit = "1 min")]
odo <- odo[odo>0][odo<max(odo)][,.(odo=mean(odo)),by=.(time,id)] |> setkey(time,id)
odo_ts <- odo[tbase,on=.(time),nomatch=NA]


### GPS ----
gps[,time := round_date(time, unit = "1 min")]
gps <- gps[,lapply(.SD,mean),.SDcols=c("a","lat","lon", "vel"), by=.(time,id)] |> setkey(time,id)
gps_ts <- gps[tbase,on=.(time),nomatch=NA]


#_______________________________________________________
### JOINs ----

soc_ene <-        merge(soc_ts,ene_ts,all=TRUE,by=c("time","id","ind"))
soc_ene_samp <-   soc_ene[sample(.N,50000)]
odo_ene <-        merge(odo,ene,all=TRUE,by=c("time","id","ind"))
odo_alt <-        merge(odo,gps,all=TRUE,by=c("time","id","ind")) |> na.omit()
# odo_alt_samp <- odo_alt[odo>500 & odo<30000][a<500 & a>200][seq(1,.N,10)]

## Impute NAs -----

cast_ene_ts <-     cast_ts(ene_ts, "ene", gap_min = 10)
cast_soc_ts <-     cast_ts(soc_ts, "soc", gap_min = 10)
cast_odo_ts <-     cast_ts(odo_ts, "odo", gap_min = 10)
cast_vel_ecu_ts <- cast_ts(vel_ecu_ts, "vel_ecu", gap_min = 10)


ene_odo <- join_cast_ts(cast_ene_ts, cast_odo_ts, telem=c("ene","odo"))
soc_odo <- join_cast_ts(cast_ene_ts, cast_odo_ts, telem=c("soc","odo"))
alt_odo <- join_cast_ts(cast_ene_ts, cast_odo_ts, telem=c("soc","odo"))



# _______________________________________________________
# Extraer segmentos Energía ----  
# Para segmentar periodos de carga-descarga es mejor usar la señal SOC
# señal odo para segmentar periodos detenido y movimiento 


  source("config_report_coyhaique.R")

    
    segments_ene <- list()
    for ( vari in autos_coyhaique |> names() )  {
      
    segments_ene[[vari]] <-
      copy(cast_ene_ts) |>
      extract_continuous(
        vari = vari,
        range_vari = range_vari,
        min_span_vari = min_span_vari,
        min_minutos_gap = min_minutos_gap,
        min_minutos_duration = min_minutos_duration,
        max_na = max_na,
        min_minutos_silencio = min_minutos_silencio
      ) |>
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
  }

  dt_segments_ene <- rbindlist(segments_ene,idcol="id")
  
    
# _____________________________________________________________
  # PLot all segments *series ----
 
  betw <- dt_segments_ene[delta=="descarga"][id=="coy2",.(start,end)][sample(.N,30)]
  
  betw <- seg_coy2[ind_start %in% c(110166,110308)][,.(start,end)]

  toplot <- cast_ene_ts[,.(time, coy2)][, pin:= time %inrange% betw]
   
   ggtopl <-  toplot[pin == TRUE] |>
      ggplot() +
      aes(x = time, y = coy2, colour=pin) +
      geom_point(size = 0.5) +
      theme_minimal()
# plotear SOC vs energia
   # plotear SOC completo con SOC en energ'ia segmentos

# _____________________________________________________________
# Odómetro por dia ----
odo_day <-  odo[time %within% periodo] [,
  `:=`(day = date(time), time = NULL)][,
    rec_diario := max(odo) - min(odo), by = c("id", "day")] |>
  unique(by = c("id", "day"))
#rowidv(cols="id",prefix = "del.")
odo_day[,finde:=
    fifelse(lubridate::wday(day)<5,
      "semana",
      "finde")] 

odo_hour <- odo[time %within% periodo][, 
  `:=`(day = date(time),
    hora = hour(time),
    time = NULL)][lubridate::wday(day)<5][, 
      rec_horario := max(odo) - min(odo), by = c("id", "day","hora")][,
        rec_hora:=sum(rec_horario),by=c("id","hora")][,c("odo","day","rec_horario"):=NULL] |> 
  unique(by = c("id", "hora"))

odo_hour2 <- odo[time %within% periodo][, 
  `:=`(day = date(time),
    hora = as.factor(hour(time)),
    time = NULL)][lubridate::wday(day)<5][, 
      rec_horario := max(odo) - min(odo), by = c("id", "day","hora")][,c("odo"):=NULL] |> 
  unique()


# _____________________________________________________________
# Tests ----
soctr <- soc[tbase,roll=-1,on="time"]
soctr[,socm:=mean(soc), by=.(id,time)]
soctr <- soctr |> unique()
soctr <- tbase[soc,on="time",nomatch=NULL,.(i.soc)]
stest <- soc[time>"2022-06-07 10:16:00"& time<"2022-06-10 15:16:00"][sample(.N,20)]
soct <- stest[tbtest,roll=-1,on=.(time=time_ind)]
soct[,socm:=mean(soc), by=.(id,time)]
soct <- soct |> unique()
soct <- tbtest[stest,on=.(time),nomatch=NULL]
soct[time=="2022-07-08 13:56:00"]
soct[ttime=="2022-07-08 15:43:00"]


# _____________________________________________________________
### Análisis carga descarga ----

# odo_ene_ts <- 
