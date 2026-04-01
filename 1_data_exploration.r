# --- 0. INITIALIZATION ---
rm(list = ls())
graphics.off()
cat("\014")

library(dplyr)
library(ggplot2)

# --- 1. DATA LOADING ---
df <- read.csv2("Qualita_Aria_Lombardia_2026.csv")

head(df)
nrow(df)

# --- 2. DATA CLEANING ---

# Count rows that don't start with "20" (invalid ISO dates)
invalid_rows <- sum(!grepl("^20", df$Timestamp))
cat("Invalid rows found:", invalid_rows, "\n")

# Keep only valid ISO timestamps
df <- df[grepl("^20", df$Timestamp), ]

# Verify total rows after cleaning
nrow(df)


# Count NA in categorical columns
cat_na <- df %>%
  summarise(across(c(Station_ID, Station_Name, Municipality), ~sum(is.na(.x) | .x == "")))

print("--- NA in Categorical Variables ---")
print(cat_na)



# --- 3. DATA AVAILABILITY ANALYSIS ---

# 1. General: Check if any pollutant (Ammonia to Lead) is present per day
df$Date <- as.Date(df$Timestamp)
pollutants <- df %>% select(Ammonia:Lead)
df$any_val <- rowSums(!is.na(pollutants)) > 0

df_all <- df %>%
  group_by(Station_Name, Date) %>%
  summarise(has_data = as.integer(any(any_val)), .groups = "drop")

ggplot(df_all, aes(Date, Station_Name, fill = factor(has_data))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "red", "1" = "steelblue"), 
                    labels = c("Empty", "Present"), name = "Status") +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = "General Data Availability", x = "Date", y = "Station")

# 2. Specific: Check availability for a target pollutant
target_var <- "PM10" 

df_spec <- df %>%
  group_by(Station_Name, Date) %>%
  summarise(has_data = as.integer(any(!is.na(.data[[target_var]]))), .groups = "drop")

ggplot(df_spec, aes(Date, Station_Name, fill = factor(has_data))) +
  geom_tile() +
  scale_fill_manual(values = c("0" = "red", "1" = "steelblue"), 
                    labels = c("Missing", "Present"), name = "Status") +
  theme_minimal() + 
  theme(axis.text.y = element_text(size = 6)) +
  labs(title = paste("Availability:", target_var), x = "Date", y = "Station")


# --- 4. SENSORS & DATA VOLUME ---

idx_start <- match("Ammonia", names(df))
idx_end <- match("Lead", names(df))
cols <- names(df)[idx_start:idx_end]

# 1. Hardware counts (Step-by-step using a loop)
hw_counts <- numeric(length(cols)) # Create empty container
names(hw_counts) <- cols           # Assign pollutant names

for (p in cols) {
  # Get rows where the current pollutant is not NA
  valid_rows <- !is.na(df[[p]])
  
  # Extract station names for those specific rows
  active_stations <- df$Station_Name[valid_rows]
  
  # Count unique stations and save to the container
  hw_counts[p] <- length(unique(active_stations))
}

# 2. Data volume counts
vol_counts <- colSums(!is.na(df[cols]))

# --- VISUALIZATION ---

par(mfrow = c(1, 2), mar = c(5, 8, 4, 2))

barplot(sort(hw_counts), horiz = TRUE, las = 1, 
        col = "steelblue", main = "Active Stations", xlab = "Count")

barplot(sort(vol_counts), horiz = TRUE, las = 1, 
        col = "darkorange", main = "Total Records", xlab = "Count")

par(mfrow = c(1, 1))




# IDEAS FOR TO-DO LIST:
# - Hourly aggregation!
# - POLLUTANT DISTRIBUTIONS (daily, monthly, and yearly)
# - TEMPORAL TRENDS
# - TOP 10 MOST POLLUTED MUNICIPALITIES
# - ?