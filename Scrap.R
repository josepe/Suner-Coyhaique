# List of files
files <- c(
  "vel_ecu.csv",
  "odo.csv",
  "gps.csv",
  "ene.csv",
  "soc.csv")

tbase <- create_timebase(periodo=periodo)
list_of_dfs <- lapply(files,\(x) read_and_convert_time(x,tbase=tbase))

# Assign each data.frame to a variable in the global environment
for (i in seq_along(list_of_dfs)) {
  # Extract the base name of the file (without extension)
  base_name <- tools::file_path_sans_ext(basename(files[i]))
  # Assign the data.frame to a variable with the base name
  assign(base_name, list_of_dfs[[i]])
}

rm(list_of_dfs)




soc <- fread("soc.csv")
soc[, time := with_tz(time, tzone = "America/Santiago")]
soc_ts <- soc[tbase, on = .(time), nomatch = NA]

vel_ecu <- fread("vel_ecu.csv")
vel_ecu[, time := with_tz(time, tzone = "America/Santiago")]
vel_ecu_ts <- vel_ecu[tbase, on = .(time), nomatch = NA]



odo <- fread("odo.csv")

ene <- fread("ene.csv")
gps <- fread("gps.csv")
vel_ecu <- fread("vel_ecu.csv")
vel_ecu[, time := with_tz(time, tzone = "America/Santiago")]
vel_ecu_ts <- vel_ecu[tbase, on = .(time), nomatch = NA]


odo[1,time]
ene[1,time]
odo[.N,time]
ene[.N,time]
fwrite(odo,"odo.csv")
fwrite(ene,"ene.csv")
fwrite(soc,"soc.csv")
fwrite(gps,"gps.csv")
fwrite(vel_ecu_ts,"vel_ecuts.csv")
tbase |>  head()
odo <- fread("odo.csv")
odo |> is.data.table()




# Plots y tablas ----
ggodoy <-  odo[time %within% periodo] [,
    `:=`(day = date(time), time = NULL)][,
        rec_diario := max(odo) - min(odo), by = c("id", "day")] |>
    unique(by = c("id", "day"))
ggodoy[,finde:=
            fifelse(lubridate::wday(day)<5,
                "semana",
                "finde")] 

ggodohour <- odo[time %within% periodo][, 
  `:=`(day = date(time),
    hora = hour(time),
    time = NULL)][lubridate::wday(day)<5][, 
        rec_horario := max(odo) - min(odo), by = c("id", "day","hora")][,
          rec_hora:=mean(rec_horario),by=c("id","hora")][,c("odo","day","rec_horario"):=NULL] |> 
    unique(by = c("id", "hora"))

ggodoy[rec_diario >= 25L & rec_diario <= 410L] |> 
 ggplot() +
  aes(x = rec_diario, fill = id) +
  geom_density(adjust = 1L) +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(id))

ggodoy[rec_diario >= 25L & rec_diario <= 410L] |> 
 ggplot() +
  aes(x = rec_diario, fill = id, alpha = 0.5) +  # Set alpha for transparency
  geom_density(adjust = 1L) +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(id), ncol = 1) +  # Set the number of columns in facet_wrap()
  guides(fill = FALSE)  # Remove the fill legend

godoy_subset <- ggodoy[rec_diario >= 25L & rec_diario <= 410L]
ggplot(data = godoy_subset, aes(x = rec_diario, fill = id, alpha = 0.5)) +
  geom_density(adjust = 1L) +
  scale_fill_hue(direction = 1) +
  theme_minimal()

ggodohour2 <- odo[time %within% periodo][, 
  `:=`(day = date(time),
    hora = as.factor(hour(time)),
    time = NULL)][lubridate::wday(day)<5][, 
        rec_horario := max(odo) - min(odo), by = c("id", "day","hora")][,c("odo"):=NULL] |> 
    unique(by = c("id", "day"))

ggodohour2[rec_horario >= 0L & rec_horario <= 262L] |> 
 ggplot() +
  aes(x = hora, y = rec_horario, fill = id) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(id))

ggplot(data = ggodohour[rec_hora >= 0L & rec_hora <= 262L], aes(x = hora, y = rec_hora, fill = id, alpha = 0.5)) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

ggplot(data = ggodohour2[rec_horario >= 0L & rec_horario <= 262L], aes(x = hora, y= fill = id, alpha = 0.5)) +
  geom_count() +
  scale_fill_hue(direction = 1) +
  theme_minimal() +
  facet_wrap(vars(id), ncol = 1, strip.position = "bottom")

ggplot(data = ggodoy[rec_diario >= 25L & rec_diario <= 410L]) +
  aes(x = id, y = rec_diario, fill = finde) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  ylab("Recorrido diario, [km]") +
  ggtitle("Distribución recorrido diario por conductor, día de semana versus fin de semana") +
  theme_minimal()

summary_table <- ggodoy[rec_diario >= 25L & rec_diario <= 410L] %>%
  group_by(id) %>%
  summarize(mean_rec_diario = mean(rec_diario),
            sd_rec_diario = sd(rec_diario))

formatted_table <- summary_table %>%
  mutate(mean_rec_diario = sprintf("%.2f", mean_rec_diario),
         sd_rec_diario = sprintf("%.2f", sd_rec_diario))

# Print the formatted table
kable(formatted_table, format = "markdown",
      col.names = c("ID", "Mean", "Standard Deviation"),
      caption = "Summary Statistics: Recorrido diario por conductor")

formatted_table <- summary_table %>%
  mutate(
    id = sprintf("**%s**", id),
    mean_rec_diario = sprintf("%.2f", mean_rec_diario),
    sd_rec_diario = sprintf("%.2f", sd_rec_diario)
  )

# Print the formatted table
kable(
  formatted_table,
  align = "c",
  col.names = c("**ID**", "**Mean**", "**Standard Deviation**"),
  caption = "Summary Statistics: Recorrido diario por conductor"
) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

ggvel <- vel_ecu[time %within% periodo][, 
  ,
    time = NULL)][lubridate::wday(day)<5][, 
        rec_horario := max(odo) - min(odo), by = c("id", "day","hora")][,
          rec_hora:=mean(rec_horario),by=c("id","hora")][,c("odo","day","rec_horario"):=NULL] |> 
    unique(by = c("id", "hora"))

summary_table_vel <- vel_ecu[vel_ecu>2] %>%
  group_by(id) %>%
  summarize(vel_media = mean(vel_ecu),
            sd = sd(vel_ecu)),
            regularity_index = vel_media/sd)

summary_table_vel

formatted_table <- summary_table %>%
  mutate(
    id = sprintf("**%s**", id),
    mean_rec_diario = sprintf("%.2f", mean_rec_diario),
    sd_rec_diario = sprintf("%.2f", sd_rec_diario)
  )

# Print the formatted table
kable(
  formatted_table,
  align = "c",
  col.names = c("**ID**", "**Mean**", "**Standard Deviation**"),
  caption = "Summary Statistics: Recorrido diario por conductor"
) %>%
  kable_styling(
    full_width = FALSE,
    bootstrap_options = c("striped", "hover", "condensed")
  )

summary_table_vel <- vel_ecu[vel_ecu>2][,.(mean(vel_ecu)/sd(vel_ecu)),by=id]
summary_table_vel








# Probando froll ----

# seleccionamos datos enero 2023 
p <- fread("serie_soc_coy4_ene2023.csv")
p$time<- with_tz(p$time, "America/Santiago")
p <- p[,ind:=NULL][month(time)==1 & day(time) %in% c(8,9)][,toplot:="p"]

psoc <- soc[id=="coy4",.(time,vari=soc)][month(time)==1 & day(time) %in% c(8,9)][,toplot:="psoc"]
psocts <- soc_ts[id=="coy4",.(time,vari=soc)][month(time)==1 & day(time) %in% c(8,9)][,toplot:="psocts"]
toplot <- rbind(p,psoc,psocts) |> setkey(time)

p <- copy(x_ts)[id=="coy4"][,ind:=NULL][month(time)==1 & day(time) %in% c(8,9)]
q <- cast_soc_ts[,.(time,vari=coy4)][month(time)==1 & day(time) %in% c(8,9)]
r <- serie[,.(time,vari)][month(time)==1 & day(time) %in% c(5,9)]
s <- serie2[,.(time,vari,vari_rm,vari_roll_mad)][month(time)==1 & day(time) %in% c(5,9)]
toplot <- s


qplot(time,vari,data=r,size=I(0.1), colour=I("black") ) |> ggplotly()
qplot(time,vari,data=s[ vari_roll_mad <= .2],size=I(0.5), colour=I("red") ) |> ggplotly()


toplotp <- cx_ts[,.(time,vari=coy4)][month(time)==1 & day(time) %in% c(8,9)]
range_vari
min_span_vari
min_span_vari * range_vari / (2.5*min_minutos_silencio)
s[vari_roll_mad>0.01]$vari_roll_mad |> hist(breaks=100)


# tabla para plotear datos originales versus con NAs removidos usando kalman (dia 9 enero)
toplot <- rbind(psoc[day(time)==9, .(time,vari, pname="psoc")], p[day(time)==9, .(time,vari, pname="p")])
toplot <- rbind(soc_ts[day(time)==9, .(time,vari, pname="psoc")], p[day(time)==9, .(time,vari, pname="p")])

ggplotly(ggplot(toplot, aes(x = time, y = vari, color = pname)) +
  geom_point(aes(size = ifelse(pname == "psoc", 3, 2), alpha = .8)) +
  scale_color_manual(values = c("psoc" = "red", "p" = "black")) +
  scale_size_continuous(range = c(.1, 1)) +
  theme_minimal() 
)


q <- p[,vari := vari |> na_locf()]
pqtoplot <- rbind(p[month(time)==1][day(time)==9, .(time,vari, pname="p")], q[month(time)==1][day(time)==9, .(time,vari=vari+2, pname="q")]) |> setkey(time)

ggplotly(ggplot(pqtoplot, aes(x = time, y = vari, color = pname)) +
  geom_point(aes(size = ifelse(pname == "p", 3, 2), alpha = .8)) +
  scale_color_manual(values = c("p" = "red", "p" = "black")) +
  scale_size_continuous(range = c(.1, 1)) +
  theme_minimal() 
)

ggplotly(ggplot(psoc[month(time)==1][day(time) %in% c(8:9)], aes(x = time, y = vari)) +
  geom_point(size = .1) +
  theme_minimal() 
)





r <-  q[,
      let(
      vari_roll_mad =
        frollapply(
          vari,
          25,
          sd,
          align = "center",
          fill = NA
        ) ,
       vari_rm = vari
      ) ]

t <- copy(r)
t <- t[,
  vari_roll_mad := vari_roll_mad |> na_replace(fill=0)][
    vari_roll_mad <= min_span_vari*range_vari/15,
  #  vari_roll_mad <= .1,
      vari_rm := NA]

ggplotly(ggplot(r[month(time)==1][day(time)==9], aes(x = time, y = vari_roll_mad)) +
  geom_point(size = .1) +
  theme_minimal() 
)

ggplotly(ggplot(t[month(time)==1][day(time)==9], aes(x = time, y = vari_rm)) +
  geom_point(size = .1) +
  theme_minimal() 
)




s <- r[,
       vari_roll_mad := vari_roll_mad |> na_replace(fill=0)][
        vari_roll_mad <= min_span_vari*range_vari, 
        vari_rm := NA]
    serie2 <- serie2[, .(time, ind, vari_rm)]


betw <-
  f1$segmentos[2, .(start, end)]


toplot <- f1$serie[, .(time, coy4)][, pin := time %inrange% betw]
toplot <- f1$serie[month(time)==1][day(time)==9][, .(time, coy4)]
toplot <- serie[month(time)==1][day(time)==9][, .(time, vari)]


ggtopl <- toplot[pin == TRUE] |>
  ggplot() +
  aes(x = time, y = coy4, colour = pin) +
  geom_point(size = 0.5) +
  theme_minimal()
ggplotly(ggtopl)
    
ggplotly(ggplot(toplot, aes(x = time, y = vari)) +
  geom_point(size = .1) +
  theme_minimal() 
)



## Checking timezone in Influx server ----
# convert the strings to POSIXct objects
time1 <- as.POSIXct("2023-07-07T00:06:57.692672109Z", format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
time2 <- as.POSIXct("2023-07-06 14:10:26", format="%Y-%m-%d %H:%M:%S", tz="UTC")

# calculate the difference in hours
time_diff <- difftime(time1, time2, units="mins")

get_min_time_difference <- function(time_data, time_column = "time",units="mins") {
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

# Use the function on your data
min_time_difference <- get_min_time_difference(tbase,units = "mins")

# Print the result
print(min_time_difference)

