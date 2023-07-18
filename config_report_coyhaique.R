## Periodo a analizar ----

inicio <-  as_datetime("2022-03-14 21:00:00 -03",tz="America/Santiago")
fin <-     as_datetime("2023-04-29 20:00:00 -04",tz="America/Santiago")

periodo <- interval(start=inicio,end=fin)

# Temporal resolution resamplig ----
time_res = "1 min"

# Maximum odometer allowed (to exclude artifacts)
max_odo = 70000
# _______________________________________________________
# Para extraer segmentos Energía ----  


#  ,extract_continuous ----
  
    min_minutos_gap =       20
    min_minutos_duration =  30       # minimum time between events (gaps less than this duration are considered a single event)
    max_na =                 0.2     # maximum NA in segment as fraction of segment length
   
    
#    impute_constant_segment_to_NA ----  *llamada desde extract_continuous*
     range_vari =            52       # full range of measured data (eg 0-52 kwH for energy 1-100 % for SOC etc) 
     min_span_vari =          0.01     # minimum fraction of full range accepted for selection (eg 0.01 * range_vari)
     min_minutos_silencio =  25       # minimum event duration in minutes (shorter events are discarded)
    
    
#   extrae_carga_descarga ----
    
    range_vari =            52       # full range of measured data (eg 0-52 kwH for energy 1-100 % for SOC etc)
    max_x_no_change =       30       # minutes continuous stretches with no change are split NAs also considered 
    min_y_no_change =        0.05    # maximum length of segments with no variability
    FUN = detect_peaks_score
    
#   detección de peaks ----            *llamada desde extrae_carga_descarga*
    npoints =               10       # number of points to determine span of loess function for peak extraction
    w =                     50       # window width for rolling median
    w_detect =              45       # for score detection function detect_d
    w_score =               61       # for score detection function detect_d
    score_thres =            0.5     # for score detection function detect_d
    
 
################################################################################################################################
       
# Extraer segmentos SOC ----  
# Para segmentar periodos de carga-descarga es mejor usar la señal SOC

    #  extract_continuous ----
  
    min_minutos_gap =       20
    min_minutos_duration =  30       # minimum time between events (gaps less than this duration are considered a single event)
    max_na =                 0.2     # maximum NA in segment as fraction of segment length
   
    
#    impute_constant_segment_to_NA ----  *llamada desde extract_continuous*
     range_vari =            100       # full range of measured data (eg 0-52 kwH for energy 1-100 % for SOC etc) 
     min_span_vari =          0.01     # minimum fraction of full range accepted for selection (eg 0.01 * range_vari)
     min_minutos_silencio =  25       # minimum event duration in minutes (shorter events are discarded)
    
    
#   extrae_carga_descarga ----
    
    range_vari =            100       # full range of measured data (eg 0-52 kwH for energy 1-100 % for SOC etc)
    max_x_no_change =       30       # minutes continuous stretches with no change are split NAs also considered 
    min_y_no_change =        0.05    # maximum length of segments with no variability
    FUN = detect_peaks_score
    
#   detección de peaks ----            *llamada desde extrae_carga_descarga*
    npoints =               10       # number of points to determine span of loess function for peak extraction
    w =                     50       # window width for rolling median
    w_detect =              45       # for score detection function detect_d
    w_score =               61       # for score detection function detect_d
    score_thres =            0.5     # for score detection function detect_d
    
 
    