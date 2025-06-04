


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

## Duplikate erkennen
# Step 1: alle Duplikate
dup_timestamps <- cleaned_data_filtered %>%
  group_by(ID, Timestamp) %>%
  filter(n() > 1) %>%
  arrange(ID, Timestamp)

# Step 2: identische/exakte Duplikate

exact_duplicates <- dup_timestamps %>%
  group_by(ID, Timestamp, `Glucose (mg/dl)`, Serial.Number) %>%
  filter(n() > 1) %>%
  ungroup()

write_xlsx(exact_duplicates, "exact_duplicates.xlsx")

# Step 3: Duplikate mit gleicher Serial.Number aber unterschiedl Glucose Werten


conflicting_duplicates <- dup_timestamps %>%
  group_by(ID, Timestamp, Serial.Number) %>%
  filter(n_distinct(`Glucose (mg/dl)`) > 1) %>%
  ungroup()

write_xlsx(conflicting_duplicates, "conflicting_duplicates.xlsx")

conflict_summary <- conflicting_duplicates %>%
  count(ID, name = "Conflicting_Measurements")

# diese Duplikate plotten

# 1. laufende Nummerierung erstellen
conflicting_duplicates <- conflicting_duplicates %>%
  group_by(ID, Timestamp, Serial.Number) %>%
  filter(n_distinct(`Glucose (mg/dl)`) > 1) %>%
  mutate(Glucose_Row = row_number()) %>%
  ungroup()

# 2. In Wide-Format bringen (zwei Glucose-Werte nebeneinander)
conflicts_wide <- conflicting_duplicates %>%
  select(ID, Timestamp, Serial.Number, `Glucose (mg/dl)`, Glucose_Row) %>%
  pivot_wider(
    names_from = Glucose_Row,
    values_from = `Glucose (mg/dl)`,
    names_prefix = "Glucose_"
  ) %>%
  filter(!is.na(Glucose_1) & !is.na(Glucose_2))

# 3. Scatterplot: Glucose_1 vs Glucose_2


ggplot(conflicts_wide, aes(x = Glucose_1, y = Glucose_2, color = ID)) +
  geom_point(alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Comparison of glucose values in conflicting duplicates \n(same timestamp, same serialnumber)",
    x = "Glucose Value 1",
    y = "Glucose Value 2",
    color = "ID"
  ) +
  theme_minimal()

#Differenzen zwischen Duplikaten erkennen

conflicts_wide <- conflicts_wide %>%
  mutate(duplicate_difference = Glucose_2 - Glucose_1)

ggplot(conflicts_wide, aes(duplicate_difference, color = ID))+
  geom_point(alpha = 0.7)
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(title = "Differences between GLucose Values", color = "ID")+
    theme_minimal()
  
  ggplot(conflicts_wide, aes(x = duplicate_difference, color = ID)) +
    geom_point(position = position_jitter(height = 0.1), alpha = 0.7) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = "Differenzen zwischen Glukosewerten (bei gleichen Timestamps & Serial.Number)",
      x = "Glukose-Differenz (Wert 2 - Wert 1)",
      y = NULL,
      color = "ID"
    ) +
    theme_minimal()

  ggplot(conflicts_wide, aes(x = duplicate_difference, fill = ID)) +
    geom_histogram(binwidth = 1, alpha = 0.6, position = "identity") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    labs(
      title = "Differences between Glucose Values",
      x = "Difference: Glucose (mg/dl)",
      y = "Number",
      fill = "ID"
    ) +
    theme_minimal()
  

# Step 4: Duplikate mit unterschiedl. Serial.Number und gleichem Glucose Wert

duplicates_diff_serial <- dup_timestamps %>%
  group_by(ID, Timestamp, `Glucose (mg/dl)`) %>%
  filter(n_distinct(Serial.Number) > 1) %>%
  ungroup()

write_xlsx(duplicates_diff_serial, "duplicates_diff_serial.xlsx")

summary_diff_serial <- duplicates_diff_serial %>%
  count(ID, name = "Same_Glucose_Diff_Serial_Count")



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

summary_over_6min_detail <- gaps_over_6min %>%
  group_by(ID, Time_Diff_Minutes) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  arrange(ID, Time_Diff_Minutes)

# Step 3: Messungen mit weniger als 4min Abstand




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
      Time_Diff_Minutes >= 1 & Time_Diff_Minutes <= 3 ~ "Between 1 and 3",
      Time_Diff_Minutes > 6 ~ "Over 6 min",
      Time_Diff_Minutes == 0 ~ "Duplicate",
      TRUE ~ "Normal (4-6 min)"
    )
  )


ggplot(time_gaps_plot, aes(x = Timestamp)) +
  # Normaler Bereich (4–6 Minuten) – als Hintergrund
  geom_point(
    data = subset(time_gaps_plot, Gap_Category == "Normal (4-6 min)"),
    aes(y = Time_Diff_Minutes),
    color = "gray70", alpha = 0.8, size = 1.5
  ) +
  
  # Gaps > 6 Minuten – rot
  geom_point(
    data = subset(time_gaps_plot, Gap_Category == "Over 6 min"),
    aes(y = Time_Diff_Minutes),
    color = "red3", alpha = 0.8, size = 1.5
  ) +
  
  # Gaps zwischen 1–3 Minuten – grün
  geom_point(
    data = subset(time_gaps_plot, Gap_Category == "Between 1 and 3"),
    aes(y = Time_Diff_Minutes),
    color = "green3", alpha = 0.8, size = 1.5
  ) +
  
  labs(
    title = "Zeitabstände zwischen Glukosemessungen pro ID",
    subtitle = "Rot = >6min, Grün = 1–3min, Grau = 4–6min",
    x = "Zeitpunkt",
    y = "Zeitabstand zur vorherigen Messung (Minuten)"
  ) +
  facet_wrap(~ ID, scales = "free_x") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

# checken wieviele serial numbers pro id

serial_number_summary <- cleaned_data_filtered %>%
  group_by(ID, Serial.Number) %>%
  summarise(
    First_Timestamp = min(Timestamp, na.rm = TRUE),
    Last_Timestamp = max(Timestamp, na.rm = TRUE),
    Count = n(),
    .groups = "drop"
  ) %>%
  arrange(ID, First_Timestamp)

# Anzeige
serial_number_summary


write_xlsx(serial_number_summary, "serial_number_summary.xlsx")



## Datensatz erstellen ohne Duplikate

#Step 1: deduplicate: exact duplicates
cleaned_data_deduplicated <- cleaned_data_filtered %>%
  distinct(ID, Timestamp, Serial.Number, `Glucose (mg/dl)`, .keep_all = TRUE)

#Step 2: deduplicate: duplicates with same timestamp and glucose value but not same serial number

cleaned_data_deduplicated <- cleaned_data_deduplicated %>%
  distinct(ID, Timestamp, `Glucose (mg/dl)`, .keep_all = TRUE)

#Step 3: deduplicate: duplicates with same timestamp but different glucose value

cleaned_data_deduplicated <- anti_join(
  cleaned_data_deduplicated,
  conflicting_duplicates,
  by = c("ID", "Timestamp", "Serial.Number", "Glucose (mg/dl)")
)

 
 # check how many rows were deleted
nrow(cleaned_data_filtered) - nrow(cleaned_data_deduplicated)



## Day/Night analysis

cleaned_data_deduplicated <- cleaned_data_deduplicated %>%
  mutate(
    Hour = hour(Timestamp),
    Time_of_Day = case_when(
      Hour >= 6 & Hour < 22 ~ "Day",
      TRUE ~ "Night"
    )
  )

ggplot(cleaned_data_deduplicated, aes(x = Timestamp, y = `Glucose (mg/dl)`, color = Time_of_Day)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ ID, scales = "free_x") +
  theme_minimal()

ggplot(cleaned_data_deduplicated, aes(x = Time_of_Day, y = `Glucose (mg/dl)`, fill = Time_of_Day)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ ID) +
  theme_minimal() +
  labs(title = "Glukosevalue: Day vs. Night", x = "", y = "Glucose (mg/dl)")

library(ggplot2)

ggplot(cleaned_data_deduplicated, aes(x = Timestamp, y = `Glucose (mg/dl)`, color = Time_of_Day)) +geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ ID, scales = "free_x") +
  scale_color_manual(values = c("Day" = "orange3", "Night" = "midnightblue")) +
  labs(
    title = "Glucose trend over time: Day vs. Night",
    x = "Time",
    y = "Glucose (mg/dl)",
    color = "Time of Day"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## time in range
#hyper and hypo glucose day and night check
cleaned_data_deduplicated %>%
  mutate(
    Glucose_Category = case_when(
      `Glucose (mg/dl)` < 70 ~ "Hypo",
      `Glucose (mg/dl)` > 180 ~ "Hyper",
      TRUE ~ "Normal"
    )
  ) %>%
  group_by(ID, Time_of_Day, Glucose_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(ID, Time_of_Day) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))



cleaned_data_deduplicated <- cleaned_data_deduplicated %>%
  mutate(
    Glucose_Category = case_when(
      `Glucose (mg/dl)` < 54 ~ "Very Low",
      `Glucose (mg/dl)` >= 54 & `Glucose (mg/dl)` < 70 ~ "Low",
      `Glucose (mg/dl)` >= 70 & `Glucose (mg/dl)` <= 180 ~ "Normal",
      `Glucose (mg/dl)` > 180 & `Glucose (mg/dl)` <= 250 ~ "High",
      `Glucose (mg/dl)` > 250 ~ "Very High"
    )
  )
glucose_summary <- cleaned_data_deduplicated %>%
  mutate(
    Glucose_Category = case_when(
      `Glucose (mg/dl)` < 54 ~ "Very Low",
      `Glucose (mg/dl)` >= 54 & `Glucose (mg/dl)` < 70 ~ "Low",
      `Glucose (mg/dl)` >= 70 & `Glucose (mg/dl)` <= 180 ~ "Normal",
      `Glucose (mg/dl)` > 180 & `Glucose (mg/dl)` <= 250 ~ "High",
      `Glucose (mg/dl)` > 250 ~ "Very High"
    )
  ) %>%
  group_by(ID, Time_of_Day, Glucose_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(ID, Time_of_Day) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

ggplot(time_in_range_summary, aes(x = Time_of_Day, y = Percentage, fill = Glucose_Category)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_manual(values = category_colors) +
  facet_wrap(~ ID, ncol = 2) +
  labs(
    title = "Glucose Ranges by Time of Day",
    x = "Time of Day",
    y = "Percentage",
    fill = "Glucose Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )
geom_text(aes(label = paste0(Percentage, "%")),
          position = position_stack(vjust = 1.05), size = 3)


ggplot(cleaned_data_deduplicated, aes(x = Time_of_Day, y = `Glucose (mg/dl)`, fill = Glucose_Category)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +  # Or geom_violin(), or geom_jitter() for raw values
  scale_fill_manual(values = category_colors) +
  coord_cartesian(ylim = c(0, 250)) +
  facet_wrap(~ ID, ncol = 2) +
  labs(
    title = "Glucose Distributions by Time of Day",
    x = "Time of Day",
    y = "Glucose (mg/dl)",
    fill = "Glucose Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )

ggplot(cleaned_data_deduplicated, aes(x = Time_of_Day, y = `Glucose (mg/dl)`, fill = Glucose_Category)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, color = "black", size = 0.2) +
  scale_fill_manual(values = category_colors) +
  coord_cartesian(ylim = c(0, 250)) +
  facet_wrap(~ ID, ncol = 2) +
  labs(
    title = "Glucose Distributions by Time of Day",
    x = "Time of Day",
    y = "Glucose (mg/dl)",
    fill = "Glucose Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold")
  )




time_in_range_summary <- time_in_range_summary %>%
  mutate(Glucose_Category = factor(
    Glucose_Category,
    levels = c("Very Low", "Low", "Normal", "High", "Very High")
  ))


ggplot(time_in_range_summary, aes(x = Glucose_Category, y = Percentage, color = Glucose_Category)) +
  geom_segment(aes(xend = Glucose_Category, y = 0, yend = Percentage), size = 0.8) +
  geom_point(size = 3) +
  geom_text(
    aes(label = paste0(Percentage, "%")),
    hjust = -0.4,
    size = 3,
    color = "black"
  ) +
  scale_color_manual(values = category_colors) +
  facet_grid(ID ~ Time_of_Day) +
  coord_flip(ylim = c(0, 130)) +  # <- Add more space beyond 100%
  labs(
    title = "Glucose Category by ID and Time of Day",
    x = "Category",
    y = "Percentage (%)"
  ) +
  theme_minimal()

### Donut Chart
# Filter for one participant and time of day (adjust as needed)
donut_data <- time_in_range_summary %>%
  filter(ID == "102Ecosleep", Time_of_Day == "Night")

# Plot donut chart
ggplot(donut_data, aes(x = 2, y = Percentage, fill = Glucose_Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  scale_fill_manual(values = category_colors) +
  geom_text(aes(label = paste0(Percentage, "%")),
            position = position_stack(vjust = 0.5), size = 3, color = "black") +
  theme_void() +
  labs(title = "Glucose Distribution (Night) – 102Ecosleep",
       fill = "Glucose Category") +
  theme(legend.position = "right")

library(dplyr)

# Prepare data
spike_analysis <- cleaned_data_deduplicated %>%
  arrange(ID, Timestamp) %>%
  mutate(
    Glucose_Category = case_when(
      `Glucose (mg/dl)` < 54 ~ "Very Low",
      `Glucose (mg/dl)` >= 54 & `Glucose (mg/dl)` < 70 ~ "Low",
      `Glucose (mg/dl)` >= 70 & `Glucose (mg/dl)` <= 180 ~ "Normal",
      `Glucose (mg/dl)` > 180 & `Glucose (mg/dl)` <= 250 ~ "High",
      `Glucose (mg/dl)` > 250 ~ "Very High"
    ),
    Prev_Category = lag(Glucose_Category),
    Spike = Glucose_Category != Prev_Category
  ) %>%
  filter(Spike) %>%
  mutate(Spike_Direction = paste0(Prev_Category, " → ", Glucose_Category)) %>%
  group_by(ID, Spike_Direction) %>%
  summarise(Spike_Count = n(), .groups = "drop")

# View result
spike_analysis

library(ggplot2)

ggplot(spike_analysis, aes(x = Spike_Direction, y = Spike_Count, fill = Spike_Direction)) +
  geom_col() +
  facet_wrap(~ ID) +
  coord_flip() +
  labs(
    title = "Transitions Between Glucose Categories (Spikes)",
    x = "Transition",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


library(dplyr)
library(lubridate)

# Add glucose category and arrange
spike_durations <- cleaned_data_deduplicated %>%
  arrange(ID, Timestamp) %>%
  mutate(
    Glucose_Category = case_when(
      `Glucose (mg/dl)` < 54 ~ "Very Low",
      `Glucose (mg/dl)` >= 54 & `Glucose (mg/dl)` < 70 ~ "Low",
      `Glucose (mg/dl)` >= 70 & `Glucose (mg/dl)` <= 180 ~ "Normal",
      `Glucose (mg/dl)` > 180 & `Glucose (mg/dl)` <= 250 ~ "High",
      `Glucose (mg/dl)` > 250 ~ "Very High"
    ),
    Category_Change = Glucose_Category != lag(Glucose_Category) | ID != lag(ID)
  ) %>%
  # Group consecutive identical categories per ID
  mutate(Spike_Group = cumsum(replace_na(Category_Change, TRUE))) %>%
  group_by(ID, Spike_Group) %>%
  summarise(
    Start = min(Timestamp),
    End = max(Timestamp),
    Duration_min = as.numeric(difftime(max(Timestamp), min(Timestamp), units = "mins")),
    Category = first(Glucose_Category),
    .groups = "drop"
  ) %>%
  # Filter for spikes only (exclude normal periods)
  filter(Category != "Normal")

# View spike durations
head(spike_durations)


library(ggplot2)

ggplot(spike_durations, aes(x = Start, xend = End, y = ID, yend = ID, color = Category)) +
  geom_segment(size = 8) +
  scale_color_manual(values = category_colors) +  # Optional: your color scheme
  labs(
    title = "Glucose Spikes Over Time",
    x = "Time",
    y = "Participant ID",
    color = "Glucose Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(face = "bold")
  )

# Join back to full data to find values just before/after
spike_outliers <- spike_durations %>%
  rowwise() %>%
  mutate(
    Pre_Value = cleaned_data_deduplicated %>%
      filter(ID == ID, Timestamp < Start) %>%
      arrange(desc(Timestamp)) %>%
      slice(1) %>%
      pull(`Glucose (mg/dl)`),
    
    Post_Value = cleaned_data_deduplicated %>%
      filter(ID == ID, Timestamp > End) %>%
      arrange(Timestamp) %>%
      slice(1) %>%
      pull(`Glucose (mg/dl)`),
    
    # Find min and max during the spike
    Spike_Min = cleaned_data_deduplicated %>%
      filter(ID == ID, Timestamp >= Start, Timestamp <= End) %>%
      summarise(min_val = min(`Glucose (mg/dl)`, na.rm = TRUE)) %>%
      pull(min_val),
    
    Spike_Max = cleaned_data_deduplicated %>%
      filter(ID == ID, Timestamp >= Start, Timestamp <= End) %>%
      summarise(max_val = max(`Glucose (mg/dl)`, na.rm = TRUE)) %>%
      pull(max_val),
    
    Pre_Diff = abs(Pre_Value - Spike_Min),
    Post_Diff = abs(Post_Value - Spike_Min),
    Outlier = Pre_Diff > 10 | Post_Diff > 10
  ) %>%
  ungroup()











# Add a Day/Night classification based on hour
day_night_data <- cleaned_data_deduplicated %>%
  mutate(
    Period = case_when(
      hour(Timestamp) >= 6 & hour(Timestamp) < 22 ~ "Day",
      TRUE ~ "Night"
    )
  ) %>%
  group_by(ID, Period) %>%
  summarise(
    Average_Glucose = mean(`Glucose levels (mg/dl)`, na.rm = TRUE),
    .groups = 'drop'
  )
# Plot the day vs. night glucose levels for each ID
ggplot(day_night_data, aes(x = Period, y = Average_Glucose, fill = Period)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ ID, ncol = 2) +
  labs(
    title = "Average Glucose Levels by Day and Night",
    x = "Period",
    y = "Average Glucose Level (mg/dL)"
  ) +
  scale_fill_manual(values = c("Day" = "#56B4E9", "Night" = "#0072B2")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold")
  )

#### more categories across the day ####
# Define the time of day periods based on hour and reorder the levels
time_of_day_data <- cleaned_data_deduplicated %>%
  mutate(
    Time_of_Day = case_when(
      hour(Timestamp) >= 22 | hour(Timestamp) < 6 ~ "Night",
      hour(Timestamp) >= 6 & hour(Timestamp) < 12 ~ "Morning",
      hour(Timestamp) >= 12 & hour(Timestamp) < 15 ~ "Lunch",
      hour(Timestamp) >= 15 & hour(Timestamp) < 18 ~ "Afternoon",
      hour(Timestamp) >= 18 & hour(Timestamp) < 22 ~ "Evening"
    ),
    Time_of_Day = factor(Time_of_Day, levels = c("Morning", "Lunch", "Afternoon", "Evening", "Night"))
  ) %>%
  group_by(ID, Time_of_Day) %>%
  summarise(
    Average_Glucose = mean(`Glucose levels (mg/dl)`, na.rm = TRUE),
    .groups = 'drop'
  )

install.packages("colorspace")  # nur einmal nötig
library(colorspace)

# Define a Batlow color palette for the five time-of-day periods
batlow_colors <- sequential_hcl(5, palette = "Batlow")

# Map the colors to each time period
time_of_day_palette <- c("Morning" = batlow_colors[5], 
                         "Lunch" = batlow_colors[2], 
                         "Afternoon" = batlow_colors[3], 
                         "Evening" = batlow_colors[4], 
                         "Night" = batlow_colors[1])

violin_plot <- ggplot(cleaned_data_deduplicated %>%
                        mutate(
                          Time_of_Day = case_when(
                            hour(Timestamp) >= 22 | hour(Timestamp) < 6 ~ "Night",
                            hour(Timestamp) >= 6 & hour(Timestamp) < 10 ~ "Morning",
                            hour(Timestamp) >= 10 & hour(Timestamp) < 15 ~ "Lunch",
                            hour(Timestamp) >= 15 & hour(Timestamp) < 18 ~ "Afternoon",
                            hour(Timestamp) >= 18 & hour(Timestamp) < 22 ~ "Evening"
                          ),
                          Time_of_Day = factor(Time_of_Day, levels = c("Morning", "Lunch", "Afternoon", "Evening", "Night"))
                        ), aes(x = Time_of_Day, y = `Glucose levels (mg/dl)`, fill = Time_of_Day)) +
  geom_violin() +
  stat_summary(fun = mean, geom = "point", color = "red", size = 0.7) +
  facet_wrap(~ ID, ncol = 2) +
  labs(
    title = "Glucose Levels by Time of Day and ID",
    x = "Time of Day",
    y = "Glucose Level (mg/dL)"
  ) +
  scale_fill_manual(values = time_of_day_palette) +
  theme_minimal() +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = NA)  # Adds gray background to ID titles
  ) +
  geom_hline(yintercept = 80, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 140, color = "black", linetype = "dashed")

violin_plot



### Night pattern

library(dplyr)
library(ggplot2)
library(lubridate)

# Filter for night data
night_data <- cleaned_data_deduplicated %>%
  mutate(Hour = hour(Timestamp)) %>%
  filter(Hour >= 22 | Hour < 7) %>%  # Nighttime: 22 to 6
  
  # Create reordered factor for plotting (22 to 6)
  mutate(Hour_Factor = factor(Hour, levels = c(22, 23, 0, 1, 2, 3, 4, 5, 6)))

# Calculate average glucose per hour per ID
night_avg <- night_data %>%
  group_by(ID, Hour_Factor) %>%
  summarise(
    Avg_Glucose = mean(`Glucose (mg/dl)`, na.rm = TRUE),
    .groups = "drop"
  )

# Plot
ggplot(night_avg, aes(x = Hour_Factor, y = Avg_Glucose, group = ID, color = ID)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Nighttime Glucose Trends per ID",
    subtitle = "Hours from 22:00 to 06:00",
    x = "Hour of Night",
    y = "Average Glucose (mg/dl)",
    color = "ID"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )
#boxplot
ggplot(night_data, aes(x = Hour_Factor, y = `Glucose (mg/dl)`, fill = Hour_Factor)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  facet_wrap(~ ID, ncol = 2) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title = "Nighttime Glucose Distribution per Hour and ID",
    subtitle = "Hours from 22:00 to 06:00",
    x = "Hour of Night",
    y = "Glucose (mg/dl)",
    fill = "Hour"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )


ggplot(hourly_average, aes(x = Hour, y = Avg_Glucose, group = ID, color = ID)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Average Hourly Glucose Levels",
    x = "Hour of Day",
    y = "Avg Glucose (mg/dl)"
  ) +
  theme_minimal()



library(dplyr)
library(lubridate)

# Step 1: Add time-of-day and day fields
double_data <- cleaned_data_deduplicated %>%
  mutate(
    Date = as.Date(Timestamp),
    Hour = hour(Timestamp),
    Minute = minute(Timestamp),
    Time_of_Day = Hour + Minute / 60,
    Day_Index = as.integer(Date - min(Date))
  )

# Step 2: Duplicate each row, once shifted by 0h and once by 24h
double_data_long <- bind_rows(
  double_data %>% mutate(Time_Shift = 0, Double_Hour = Time_of_Day),
  double_data %>% mutate(Time_Shift = 24, Double_Hour = Time_of_Day + 24)
)


library(ggplot2)

ggplot(double_data_long %>% filter(ID == "102Ecosleep"), 
       aes(x = Double_Hour, y = `Glucose (mg/dl)`, group = Day_Index)) +
  geom_line(alpha = 0.4, color = "blue") +
  scale_x_continuous(breaks = seq(0, 48, by = 4)) +
  labs(
    title = "Double Plot of Glucose Rhythms – 102Ecosleep",
    x = "Hour (0–48)",
    y = "Glucose (mg/dl)"
  ) +
  theme_minimal()



library(dplyr)
library(lubridate)

# Extract time-of-day in hours
double_data <- cleaned_data_deduplicated %>%
  mutate(
    Date = as.Date(Timestamp),
    Hour = hour(Timestamp),
    Minute = minute(Timestamp),
    Time_of_Day = Hour + Minute / 60
  )

# Duplicate data: original time + shifted by 24h
double_data_long <- bind_rows(
  double_data %>% mutate(Double_Hour = Time_of_Day),
  double_data %>% mutate(Double_Hour = Time_of_Day + 24)
)

# Round down to full hour
double_avg <- double_data_long %>%
  mutate(Hour_Rounded = floor(Double_Hour)) %>%
  group_by(ID, Hour_Rounded) %>%
  summarise(Avg_Glucose = mean(`Glucose (mg/dl)`, na.rm = TRUE), .groups = "drop")

double_avg <- double_avg %>%
  mutate(
    Hour_Factor = factor(sprintf("%02d:00", Hour_Rounded), levels = sprintf("%02d:00", 0:47))
  )
library(ggplot2)

ggplot(double_avg, aes(x = Hour_Factor, y = Avg_Glucose, group = ID, color = ID)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Double Plot: 48-Hour Glucose Trends per ID",
    subtitle = "Shows repeating daily glucose rhythms",
    x = "Hour of Day (0–48h)",
    y = "Average Glucose (mg/dl)",
    color = "ID"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    strip.text = element_text(face = "bold")
  )







# Schritt 1: Monat berechnen
data_with_month_deduplicated <- cleaned_data_deduplicated %>%
  mutate(
    Month = floor_date(Timestamp, "month")
  )

# Schritt 2: Erster Monat pro ID
first_months_deduplicated <- data_with_month_deduplicated %>%
  group_by(ID) %>%
  summarise(First_Month = min(Month), .groups = "drop")

# Schritt 3: Vollständigen Monatsraster erzeugen (13 Monate ab Start)
full_months_per_id_deduplicated <- first_months_deduplicated %>%
  rowwise() %>%
  mutate(
    Month = list(seq(First_Month, by = "1 month", length.out = 13))
  ) %>%
  unnest(Month) %>%
  ungroup()

# Schritt 4: Tatsächlich gemessene Werte zählen pro ID und Monat
monthly_counts_deduplicated <- data_with_month_deduplicated %>%
  group_by(ID, Month) %>%
  summarise(
    Measured_Values = n(),
    Days_Observed = n_distinct(as.Date(Timestamp)),
    .groups = "drop"
  )

# Schritt 5: Join mit vollständigem Raster + fehlende Werte auffüllen
monthly_summary_Deduplicated <- full_months_per_id_deduplicated %>%
  left_join(monthly_counts_deduplicated, by = c("ID", "Month")) %>%
  mutate(
    Measured_Values = replace_na(Measured_Values, 0),
    Days_Observed = replace_na(Days_Observed, 0),
    
    Expected_Values = days_in_month(Month) * 24 * 12,  # 12 Messungen pro Stunde bei 5-minütiger Frequenz
    
    Missing_Values = Expected_Values - Measured_Values,
    Missing_Percentage = ifelse(Expected_Values == 0, NA, round((Missing_Values / Expected_Values) * 100, 2))
  )

# Replace these with your desired ID and start date
target_id_102 <- "102Ecosleep"               # e.g., "P001"
start_date_102 <- as.Date("2024-03-11")  # e.g., "2022-03-15"

# Filter for this ID and 1-year window
filtered_data_102 <- cleaned_data_deduplicated %>%
  filter(
    ID == target_id_102,
    Timestamp >= start_date_102,
    Timestamp < start_date_102 + years(1)
  )

# Count number of measurements
measurement_count_102 <- nrow(filtered_data_102)

# Optional: Show summary
cat("Participant:", target_id_102, "\n",
    "Start Date:", start_date_102, "\n",
    "End Date:", start_date_102 + years(1) - 1, "\n",
    "Measurements in first year:", measurement_count_102, "\n")

# Replace these with your desired ID and start date
target_id_104 <- "104Ecosleep"               # e.g., "P001"
start_date_104 <- as.Date("2024-03-18")  # e.g., "2022-03-15"

# Filter for this ID and 1-year window
filtered_data_104 <- cleaned_data_deduplicated %>%
  filter(
    ID == target_id_104,
    Timestamp >= start_date_104,
    Timestamp < start_date_104 + years(1)
  )

# Count number of measurements
measurement_count_104 <- nrow(filtered_data_104)

# Optional: Show summary
cat("Participant:", target_id_104, "\n",
    "Start Date:", start_date_104, "\n",
    "End Date:", start_date_104 + years(1) - 1, "\n",
    "Measurements in first year:", measurement_count_104, "\n")


# Replace these with your desired ID and start date
target_id_107 <- "107EcoSleep"               # e.g., "P001"
start_date_107 <- as.Date("2024-03-25")  # e.g., "2022-03-15"

# Filter for this ID and 1-year window
filtered_data_107 <- cleaned_data_deduplicated %>%
  filter(
    ID == target_id_107,
    Timestamp >= start_date_107,
    Timestamp < start_date_107 + years(1)
  )

# Count number of measurements
measurement_count_107 <- nrow(filtered_data_107)


# Replace these with your desired ID and start date
target_id_108 <- "108EcoSleep"               # e.g., "P001"
start_date_108 <- as.Date("2024-04-22")  # e.g., "2022-03-15"

# Filter for this ID and 1-year window
filtered_data_108 <- cleaned_data_deduplicated %>%
  filter(
    ID == target_id_108,
    Timestamp >= start_date_108,
    Timestamp < start_date_108 + years(1)
  )

# Count number of measurements
measurement_count_108 <- nrow(filtered_data_108)


# Replace these with your desired ID and start date
target_id_109 <- "109EcoSleep"               # e.g., "P001"
start_date_109 <- as.Date("2024-04-22")  # e.g., "2022-03-15"

# Filter for this ID and 1-year window
filtered_data_109 <- cleaned_data_deduplicated %>%
  filter(
    ID == target_id_109,
    Timestamp >= start_date_109,
    Timestamp < start_date_109 + years(1)
  )

# Count number of measurements
measurement_count_109 <- nrow(filtered_data_109)

# Replace these with your desired ID and start date
target_id_112 <- "112EcoSleep"               # e.g., "P001"
start_date_112 <- as.Date("2024-08-11")  # e.g., "2022-03-15"

# Filter for this ID and 1-year window
filtered_data_112 <- cleaned_data_deduplicated %>%
  filter(
    ID == target_id_112,
    Timestamp >= start_date_112,
    Timestamp < start_date_112 + years(1)
  )

# Count number of measurements
measurement_count_112 <- nrow(filtered_data_112)



# Replace these with your desired ID and start date
target_id_112 <- "112EcoSleep"               # e.g., "P001"
start_date_112 <- as.Date("2024-03-25")  # e.g., "2022-03-15"

# Filter for this ID and 1-year window
filtered_data_112 <- cleaned_data_deduplicated %>%
  filter(
    ID == target_id_112,
    Timestamp >= start_date_112,
    Timestamp < start_date_112 + years(1)
  )

# Count number of measurements
measurement_count_112 <- nrow(filtered_data_112)






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







