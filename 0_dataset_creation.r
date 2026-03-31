# --- 0. INITIALIZATION ---
rm(list = ls())                     # Clear all objects from the environment
graphics.off()                      # Close all open plot windows
cat("\014")                         # Clear the console screen

# Load required libraries
library(data.table)
library(tidyverse)                  # Includes dplyr, ggplot2, stringr, etc.
library(leaflet)




# --- 1. DATA LOADING ---
measurements <- fread("Dati_sensori_aria_20260326.csv")
stations <- fread("Stazioni_qualità_dell’aria_20260326.csv")




# --- 2. PRE-JOIN CLEANING (Fixing Char/Comma issue) ---
# Fix measurements: Convert 'Valore' from char to numeric
measurements[, Value := as.numeric(gsub(",", ".", Valore))]

# Se il valore è minore di zero, trasformalo in NA, altrimenti tienilo
measurements[, Value := ifelse(Value < 0, NA, Value)]

# Fix stations: Convert coordinates from char to numeric
stations[, latitude := as.numeric(gsub(",", ".", lat))]
stations[, longitude := as.numeric(gsub(",", ".", lng))]

# CORREZIONE DATA: Usiamo %I per le 12 ore invece di %H
measurements[, date_time := as.POSIXct(Data, format="%d/%m/%Y %I:%M:%S %p")]


# --- 3. JOINING & CLEANING ORPHANED SENSORS ---
#Sono sensori che ci sono nel dataset misurazioni ma non ci sono nel dataset stazioni
id_orfani <- unique(measurements$IdSensore[!measurements$IdSensore %in% stations$IdSensore])
print(paste("Gli ID dei sensori orfani sono:", paste(id_orfani, collapse = ", ")))

setkey(measurements, IdSensore)
setkey(stations, IdSensore)

# Join datasets
full_dataset <- stations[measurements]

# Remove "orphaned" sensors (measurements with no station/pollutant metadata)
clean_dataset <- full_dataset[!is.na(Idstazione) & !is.na(NomeTipoSensore)]



# --- 4. HARDWARE ANALYSIS: SENSORS INSTALLED ---
# How many physical sensors exist for each pollutant?
hardware_ranking <- stations %>%
  group_by(NomeTipoSensore) %>%
  summarise(Physical_Sensors_Count = n_distinct(IdSensore)) %>%
  arrange(desc(Physical_Sensors_Count))

print("--- HARDWARE: Most Installed Sensors ---")
print(hardware_ranking)


# --- 4b. DATA VOLUME ANALYSIS: TOTAL MEASUREMENTS ---
# How many data points were actually recorded for each pollutant?
data_volume_ranking <- clean_dataset %>%
  group_by(NomeTipoSensore) %>%
  summarise(Total_Measurements = n()) %>% # n() conta tutte le righe nel sacchetto
  arrange(desc(Total_Measurements))

print("--- DATA VOLUME: Most Recorded Measurements ---")
print(data_volume_ranking)



# --- 5. SPATIAL VISUALIZATION (MAP) ---
map_stations <- clean_dataset %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  distinct(Idstazione, .keep_all = TRUE)

# Note: View this in the "Viewer" pane in RStudio
map_widget <- leaflet(map_stations) %>%
  addTiles() %>%
  addMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    popup = ~paste("<b>Station:</b>", NomeStazione, "<br><b>City:</b>", Comune),
  )
print(map_widget) # Forces the map to render



# --- 6. RESHAPING TO WIDE FORMAT (HOURLY) ---
# Pivot data so each pollutant becomes a column
wide_dataset <- dcast(clean_dataset, 
                      Idstazione + NomeStazione + Comune + date_time ~ NomeTipoSensore, 
                      value.var = "Value",
                      fun.aggregate = mean, 
                      na.rm = TRUE)



# --- 8. FINAL VISUALIZATION & FILTERING TEST ---
print(paste("New columns in Wide Dataset:", ncol(wide_dataset)))

# Example: Extracting data just for Milan
milano_data <- wide_dataset[Comune == "Milano"]
print("--- Milan Data Preview ---")
head(milano_data)


# --- 9. EXPORTING DATA ---
fwrite(wide_dataset, "Qualita_Aria_Lombardia_2026.csv", sep = ";", dec = ",")

print("Esportazione completata! Il file CSV è pronto nella tua cartella di lavoro.")
