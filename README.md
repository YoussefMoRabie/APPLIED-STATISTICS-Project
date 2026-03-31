\# Lombardy Air Quality Analysis 2026



\## 🚀 Setup Instructions

To run the script, place the following datasets in the project's root folder. 

You can download them in CSV format from the official ARPA Lombardia portal:



1\. Air Quality Sensors Data (Measurements):

&#x20;  https://www.dati.lombardia.it/Ambiente/Dati-sensori-aria/nicp-bhqi/about\_data



2\. Air Quality Stations (Metadata):

&#x20;  https://www.dati.lombardia.it/Ambiente/Stazioni-qualit-dell-aria/ib47-atvt/about\_data



> \*\*Note:\*\* Ensure the downloaded filenames match the strings in the `fread()` functions within the R script (e.g., `Dati\_sensori\_aria\_20260326.csv`). If the dates in the filenames change, update the script accordingly.



\## 🛠️ Required R Packages



install.packages(c("data.table", "tidyverse", "leaflet"))

