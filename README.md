# Lombardy Air Quality Analysis 2026

## 🚀 Setup Instructions
To run the script, place the following datasets in the project's root folder. 
You can download them in CSV format from the official ARPA Lombardia portal:

1. **Air Quality Sensors Data (Measurements):**
   [Link to Data](https://www.dati.lombardia.it/Ambiente/Dati-sensori-aria/nicp-bhqi/about_data)
2. **Air Quality Stations (Metadata):**
   [Link to Stations](https://www.dati.lombardia.it/Ambiente/Stazioni-qualit-dell-aria/ib47-atvt/about_data)

> **Note:** Ensure the downloaded filenames match the strings in the `fread()` functions within the R script (e.g., `Dati_sensori_aria_20260326.csv`). If the dates in the filenames change, update the script accordingly.

---

## 🛠️ Required R Packages
To install the necessary libraries, run the following command in R:

```r
install.packages(c("data.table", "tidyverse", "leaflet"))
``` 