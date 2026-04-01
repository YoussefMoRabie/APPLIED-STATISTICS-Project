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

# If the value is less than zero, transform it into NA, otherwise keep it
measurements[, Value := ifelse(Value < 0, NA, Value)]

# Fix stations: Convert coordinates from char to numeric
stations[, latitude := as.numeric(gsub(",", ".", lat))]
stations[, longitude := as.numeric(gsub(",", ".", lng))]

# DATE CORRECTION: Use %I for 12-hour format instead of %H
measurements[, date_time := as.POSIXct(Data, format = "%d/%m/%Y %I:%M:%S %p")]


# --- 3. JOINING & CLEANING ORPHANED SENSORS ---
# These sensors are present in the measurement dataset but are missing from the station dataset
id_orfani <- unique(measurements$IdSensore[!measurements$IdSensore %in% stations$IdSensore])
print(paste("The orphan sensor IDs are:", paste(id_orfani, collapse = ", ")))

setkey(measurements, IdSensore)
setkey(stations, IdSensore)

# Join datasets
full_dataset <- stations[measurements] #is the same of "right_joint"

# Remove "orphaned" sensors (measurements with no station/pollutant metadata)
clean_dataset <- full_dataset[!is.na(Idstazione) &
                                !is.na(NomeTipoSensore)]
#check if everything is ok
colSums(is.na(clean_dataset[, .(NomeStazione, Comune, latitude, longitude)]))


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
  summarise(Total_Measurements = n()) %>% # n() counts all the rows in the group
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
    lng = ~ longitude,
    lat = ~ latitude,
    popup = ~ paste("<b>Station:</b>", NomeStazione, "<br><b>City:</b>", Comune),
  )
print(map_widget) # Forces the map to render


# --- 6. RESHAPING TO WIDE FORMAT (HOURLY) --- (slow)
# Pivot data so each pollutant becomes a column
wide_dataset <- dcast(
  clean_dataset,
  Idstazione + NomeStazione + Comune + date_time ~ NomeTipoSensore,
  value.var = "Value",
  fun.aggregate = mean,
  na.rm = TRUE
)


# --- 8. FINAL VISUALIZATION & FILTERING TEST ---
print(paste("New columns in Wide Dataset:", ncol(wide_dataset)))

# Example: Extracting data just for Milan
milano_data <- wide_dataset[Comune == "Milano"]
print("--- Milan Data Preview ---")
head(milano_data)


# --- 8.5 TRANSLATION INTO ENGLISH ---

translations <- c(
  # Metadati
  "Idstazione"               = "Station_ID",
  "NomeStazione"             = "Station_Name",
  "Comune"                   = "Municipality",
  "date_time"                = "Timestamp",
  
  # Inquinanti
  "Ammoniaca"                = "Ammonia",
  "Arsenico"                 = "Arsenic",
  "Benzene"                  = "Benzene",
  "Benzo(a)pirene"           = "Benzo(a)pyrene",
  "Biossido di Azoto"        = "Nitrogen_Dioxide",
  "Biossido di Zolfo"        = "Sulfur_Dioxide",
  "BlackCarbon"              = "Black_Carbon",
  "Cadmio"                   = "Cadmium",
  "Monossido di Azoto"       = "Nitric_Oxide",
  "Monossido di Carbonio"    = "Carbon_Monoxide",
  "Nikel"                    = "Nickel",
  "Ossidi di Azoto"          = "Nitrogen_Oxides",
  "Ozono"                    = "Ozone",
  "PM10 (SM2005)"            = "PM10",
  "Particelle sospese PM2.5" = "PM2.5",
  "Piombo"                   = "Lead"
)

# Applying bulk renaming
setnames(
  wide_dataset,
  old = names(translations),
  new = unname(translations),
  skip_absent = TRUE
)


#Final check
print("--- New English Column Names ---")
print(names(wide_dataset))


# --- 9. EXPORTING DATA ---
fwrite(wide_dataset,
       "Qualita_Aria_Lombardia_2026.csv",
       sep = ";",
       dec = ",")
