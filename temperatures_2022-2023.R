library(data.table)
filepath <- "/Users/quique/Documents/EV/A3E Intel/Suner/Suner-Coyhaique/TemperaturaMedia2022_23.csv"
temp_22_23 <- fread(filepath)
temp_22_23 <- temp_22_23[1:32]
# Transform the data
l_temp <- melt(temp_22_23, id.vars = "Dia", variable.name = "Month", value.name = "Temperature")
rm(temp_22_23)

l_temp$Month <- as.character(l_temp$Month)
l_temp$Dia <- sprintf("%02d", l_temp$Dia)
l_temp |> setDT()
# Spanish month abbreviations
spanish_months <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                    "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
english_months <- month.abb
for (i in 1:length(spanish_months)) {
  l_temp[, Month := gsub(spanish_months[i], english_months[i], Month)]
}


l_temp[, year := ifelse(grepl("nex$", Month), "2023", "2022")][ # If the 'Month' column ends with 'nex', set the corresponding 'year' value to '2023', otherwise set it to '2022'
    Month %in% c("Jan", "Feb"), year := "2023"][,               # If the 'Month' column is either 'Jan' or 'Feb', set the corresponding 'year' value to '2023'
    Month := sub("nex$", "", Month)][,                          # Remove the 'nex' suffix from the 'Month' column
    Date := paste(Dia, Month, year, sep = "-")][,               # Create a new 'Date' column by concatenating 'Dia', 'Month', and 'year' with '-' as the separator
    Date := as.Date(Date, format = "%d-%b-%Y")][,               # Convert the 'Date' column to a Date object
        -c(1, 2, 3), with = FALSE]                              # Remove the first three columns from the data.table

l_temp |> setcolorder(c("Date","Temperature"))
l_temp[,Temperature:=as.numeric(Temperature)][,
  Date := as.POSIXct(Date)] |>
  setnames("Date","time")
l_temp[, c("Dia", "Month", "year") := NULL]




library(lubridate)
l_temp$time <- as.Date(l_temp$time)
attr(l_temp$time, "tzone") <- "America/Santiago"
l_temp <- l_temp |> na.omit()


# > l_temp
#   time             Temperature 
#  <Date>             <num>
# 1: 2022-03-01        14.2
# 2: 2022-03-02        14.5

library(segmented)
linear_model <- lm(Temperature ~ time, data = l_temp)

segmented_model <- segmented(linear_model, psi = list(time = c(1, 10, 20)))
last_day_of_year <- ymd(paste(year(now()), "12-31", sep = "-"))

plotly_temp <-
    l_temp |> ggplot(aes(x = time, y = Temperature, colour = Temperature)) +
    geom_point(size = 2) +
    scale_color_gradient2(
        low = "blue",
        mid = "grey",
        high = "red",
        midpoint = median(l_temp$Temperature, na.rm = TRUE)
    ) +
    scale_x_date(limits = c(min(l_temp$time), last_day_of_year)) +
    labs(title = "Variación de temperatura Coyhaique durante el Proyecto Experimental",
        x = "Fecha",
        y = "Temperatura") +
    theme_minimal()
ggplotly(plotly_temp)
