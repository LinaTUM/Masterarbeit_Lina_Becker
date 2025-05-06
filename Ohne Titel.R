


## Load necessary libraries

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)


#load dataset
P102 <- read.csv("102EcoSleep_glucose_26-3-2025.csv", header=TRUE, sep=",", quote="", skip = 1)
P104 <- read.csv("104EcoSleep_glucose_26-3-2025.csv", header=TRUE, sep=",", quote="", skip = 1)
P107 <- read.csv("107EcoSleep_glucose_26-3-2025.csv", header=TRUE, sep=",", quote="", skip = 1)
P108 <- read.csv("108EcoSleep_glucose_23-1-2025.csv", header=TRUE, sep=",", quote="", skip = 1)
P109 <- read.csv("109EcoSleep_glucose_26-3-2025.csv", header=TRUE, sep=",", quote="", skip = 1)
P112 <- read.csv("112EcoSleep_glucose_13-3-2025.csv", header=TRUE, sep=",", quote="", skip = 1)

## Data Reading

# Define the directory containing the files
data_directory <- "/Users/linabecker/Documents/M.Sc. Health Science/Masterarbeit/Masterarbeit"

# List all CSV files in the directory
file_list <- list.files(path = data_directory, pattern = "*.csv", full.names = TRUE)

# Initialize an empty list to store individual data frames
data_list <- list()

# Loop over each file to read, process, and add to data_list
for (file in file_list) {
  # Extract the ID from the filename
  id <- sub("_.*", "", basename(file))
  
  # Read the file, skipping the first row and using the second row as the header
  df <- read.csv(file, skip = 1, header = TRUE)
  
  # Add the ID column
  df$ID <- id
  
  # Append the processed data frame to the list
  data_list[[length(data_list) + 1]] <- df
}

# Combine all individual data frames into one
combined_data <- bind_rows(data_list)

# View the combined data 
head(combined_data)

# Get a summary of unique IDs
id_summary <- combined_data %>%
  distinct(ID) %>%
  count()

# Display the summary
id_summary

# Convert ID to a factor
combined_data <- combined_data %>%
  mutate(ID = as.factor(ID))

# Rename the columns
combined_data <- combined_data %>%
  rename(
    DeviceTimestamp = Device.Timestamp,
    `Glucose levels (mg/dl)` = Historic.Glucose.mg.dL,
    `Scan glucose levels (mg/dl)` = Scan.Glucose.mg.dL
  )



# View the updated column names (optional)
colnames(combined_data)

# Convert the Timestamp column to POSIXct format
combined_data$Timestamp <- as.POSIXct(combined_data$DeviceTimestamp, format = "%d-%m-%Y %H:%M")

# Verify the structure to confirm the change
str(combined_data$Timestamp)

# Reorder columns, placing Timestamp as the fourth column
combined_data <- combined_data %>%
  select(1:3, Timestamp, everything())%>%
  arrange(Timestamp)

#Check all values of Record Types

summary(combined_data$Record.Type)
table(combined_data$Record.Type)

# Process the data as specified
combined_data <- combined_data %>%
  # Remove rows where Record.Type is 5 or 6
  filter(!(Record.Type %in% c(5, 6))) %>%
  # Create a new column 'Glucose level'
  mutate(
    "Glucose (mg/dl)" = if_else(
      !is.na(`Glucose levels (mg/dl)`), 
      `Glucose levels (mg/dl)`, 
      `Scan glucose levels (mg/dl)`
    )
  )

# View the first few rows to verify
head(combined_data)

# Filter out rows where both glucose values are missing
glucose_data <- combined_data %>%
  filter(!is.na(`Glucose levels (mg/dl)`) | !is.na(`Scan glucose levels (mg/dl)`))

# Plot all participants in one faceted plot
ggplot(glucose_data, aes(x = Timestamp)) +
  geom_line(aes(y = `Glucose levels (mg/dl)`), color = "blue", size = 0.5, alpha = 0.7) +
  geom_point(aes(y = `Scan glucose levels (mg/dl)`), color = "red", size = 0.1, alpha = 0.9) +
  facet_wrap(~ ID, scales = "free_x") +  # One plot per ID, flexible x-axis
  labs(
    title = "Glucose Measurements Over Time by Participant",
    subtitle = "Blue = Historic Glucose (5 min), Red = Scan Glucose (manual scan)",
    x = "Time",
    y = "Glucose Level (mg/dL)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    strip.text = element_text(face = "bold", size = 8),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

## Data Cleaning (bis hierhin passt alles)

# Filter out rows with NA or infinite values in Timestamp and Glucose levels columns
cleaned_data <- combined_data %>%
  filter(!is.na(Timestamp), !is.na(`Glucose levels (mg/dl)`), is.finite(`Glucose levels (mg/dl)`))

head(cleaned_data)


cleaned_data <- cleaned_data %>%
  select(-Rapid.Acting.Insulin..units., -Ketone.mmol.L, -Non.numeric.Rapid.Acting.Insulin, -Non.numeric.Food, -Carbohydrates..grams.,-Carbohydrates..servings., -Non.numeric.Long.Acting.Insulin, -Long.Acting.Insulin.Value..units., -Notes, -Strip.Glucose.mg.dL, -Meal.Insulin..units., -Correction.Insulin..units., -User.Change.Insulin..units.)

# Add a column to indicate missing or non-finite values in Glucose (1 = Missing/Non-finite, 0 = Present)
cleaned_data <- cleaned_data %>%
  mutate(
    Missing = ifelse(is.na(`Glucose (mg/dl)`) | !is.finite(`Glucose (mg/dl)`), 1, 0)
  )

# Check how many columns have the value 1 in Missings
sum(cleaned_data$Missing == 1)

# Step 1: Determine the start date for each ID (earliest timestamp)
id_start_dates <- cleaned_data %>%
  group_by(ID) %>%
  summarise(Start_Date = min(Timestamp, na.rm = TRUE) , .groups = "drop")

# Step 2: Join the start date back to the main data and filter to include only records from the start date onward
cleaned_data_filtered <- cleaned_data %>%
  left_join(id_start_dates, by = "ID") %>%
  filter(Timestamp >= Start_Date | is.na(Timestamp))  # Include rows where Timestamp is NA as well

#Table with overview

# Schritt 1: Füge Monat und Jahr-Spalte hinzu
glucose_monthly_summary <- cleaned_data %>%
  mutate(
    Month = floor_date(Timestamp, "month")
  ) %>%
  group_by(ID, Month) %>%
  summarise(
    Measured_Values = n(),  # Tatsächlich gemessene Werte
    Days_Observed = n_distinct(as.Date(Timestamp)),  # Anzahl untersuchter Tage im Monat
    Expected_Values = Days_Observed * 288,  # Erwartet: 288 Werte pro Tag
    Missing_Values = Expected_Values - Measured_Values,
    Missing_Percentage = round((Missing_Values / Expected_Values) * 100, 2),
    .groups = "drop"
  )

# Ausgabe anzeigen
glucose_monthly_summary

cleaned_data %>%
  group_by(ID, Timestamp) %>%
  filter(n() > 1)


dup_timestamps <- cleaned_data_filtered %>%
  group_by(ID, Timestamp) %>%
  filter(n() > 1) %>%
  arrange(ID, Timestamp)

### Auseinanderliegende Zeitstempel erkennen: weniger als 4min und mehr als 6min

# Step 1: Generell Zeitabstände berechnen
time_gaps <- cleaned_data_filtered %>%
  arrange(ID, Timestamp) %>%
  group_by(ID) %>%
  mutate(
    Time_Diff_Minutes = as.numeric(difftime(Timestamp, lag(Timestamp), units = "mins"))
  ) %>%
  ungroup()

time_gaps

# Step 2: Messungen mit mehr als 6 Minuten Abstand
gaps_over_6min <- time_gaps %>%
  filter(!is.na(Time_Diff_Minutes) & Time_Diff_Minutes > 6)

gaps_over_6min

summary_over_6min <- gaps_over_6min %>%
  count (ID, name = "Gaps_More_Than_6min")

# Step 3: Messungen mit weniger als 4min Abstand

gaps_under_4min <- time_gaps %>%
  filter(!is.na(Time_Diff_Minutes) & Time_Diff_Minutes < 4)

gaps_under_4min

summary_under_4min <- gaps_under_4min %>%
  count(ID, name = "Gaps_Less_Than_4min")


gaps_1_to_3_min <- time_gaps %>%
  filter(!is.na(Time_Diff_Minutes) & Time_Diff_Minutes >= 1 & Time_Diff_Minutes <= 3)

summary_1_to_3_min <- gaps_1_to_3_min %>%
  count(ID, name = "Gaps_Between_1_and_3min")

# Step 4: Create a Plot


# Categorize the time gaps
time_gaps_plot <- time_gaps %>%
  mutate(
    Gap_Category = case_when(
      is.na(Time_Diff_Minutes) ~ NA_character_,
      Time_Diff_Minutes < 4 ~ "Under 4 min",
      Time_Diff_Minutes > 6 ~ "Over 6 min",
      TRUE ~ "Normal (4-6 min)"
    )
  )


ggplot(time_gaps_plot, aes(x = Timestamp)) +
  # Background: Normal gaps
  geom_point(data = subset(time_gaps_plot, Gap_Category == "Normal (4-6 min)"),
             aes(y = Time_Diff_Minutes),
             color = "gray70", alpha = 0.4, size = 1) +
  
  # Highlight gaps >6min and <4min
  geom_point(data = subset(time_gaps_plot, Gap_Category == "Over 6 min"),
             aes(y = Time_Diff_Minutes),
             color = "red3", alpha = 0.8, size = 1.5) +
  geom_point(data = subset(time_gaps_plot, Gap_Category == "Under 4 min"),
             aes(y = Time_Diff_Minutes),
             color = "green3", alpha = 0.8, size = 1.5) +
  
  # Labels & Layout
  labs(
    title = "Time Gaps Between Glucose Measurements per ID",
    subtitle = "Red = >6min, Green = <4min, Gray = 4–6min",
    x = "Time",
    y = "Gap to Previous Measurement (min)"
  ) +
  facet_wrap(~ ID, scales = "free_x") +  # Separate plot per ID
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
















# Lege Zeitdauer fest (z.B. 1 Jahr = 365 Tage * 24 Stunden * 60 Minuten / 5 Minuten = 105120 Intervalle)
time_intervalls <- id_start_dates %>%
  rowwise() %>%
  mutate(
    Timestamp = list(seq(
      from = Start_Date,
      to = Start_Date + lubridate::days(365),  # 1 Jahr ab Startdatum
      by = "1 min"
    ))
  ) %>%
  unnest(Timestamp) %>%
  ungroup()

# Join mit Originaldaten
cleaned_data_continuous <- time_intervalls %>%
  left_join(cleaned_data_filtered, by = c("ID", "Timestamp", "Start_Date"))

cleaned_data_continuous <- cleaned_data_continuous %>%
  mutate(
    Source = if_else(is.na(`Glucose levels (mg/dl)`), "synthetic", "historic")
  )

#double checking the unique rows that were added
# Find rows that are unique to combined_data_continuous
unique_to_df_continuous <- cleaned_data_continuous %>%
  anti_join(cleaned_data_filtered, by = c("ID", "Timestamp", "Glucose (mg/dl)")) %>%
  arrange(ID)

# View the unique rows
unique_to_df_continuous

## von 1min intervalle jetzt alles auf 5min durchschnitt bekommen

# Step 1: Zeitstempel auf 5-Minuten runden und Monat extrahieren
cleaned_data_binned <- cleaned_data_continuous %>%
  mutate(
    Timestamp_5min = floor_date(Timestamp, "5 minutes"),
    Month = format(floor_date(Timestamp, "month"))
  )

# Step 2: Gruppieren & mitteln über 5-Minuten-Bins

cleaned_data_resampled <- cleaned_data_binned %>%
  group_by(ID, Timestamp_5min) %>%
  summarise(
    Avg_Glucose = mean(`Glucose (mg/dl)`, na.rm = TRUE),
    Missing_Count = if_else(is.na(Avg_Glucose), 1, 0),
    .groups = "drop"
  )

library(dplyr)
library(lubridate)

# Schritt 1: Füge Monat und Jahr-Spalte hinzu
glucose_monthly_summary <- cleaned_data %>%
  mutate(
    Month = floor_date(Timestamp, "month")
  ) %>%
  group_by(ID, Month) %>%
  summarise(
    Measured_Values = n(),  # Tatsächlich gemessene Werte
    Days_Observed = n_distinct(as.Date(Timestamp)),  # Anzahl untersuchter Tage im Monat
    Expected_Values = Days_Observed * 288,  # Erwartet: 288 Werte pro Tag
    Missing_Values = Expected_Values - Measured_Values,
    Missing_Percentage = round((Missing_Values / Expected_Values) * 100, 2),
    .groups = "drop"
  )

# Ausgabe anzeigen
glucose_monthly_summary





