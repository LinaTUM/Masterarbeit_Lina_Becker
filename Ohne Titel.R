


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





library(ggrepel)

# Donut-Daten vorbereiten
donut_data <- time_in_range_summary %>%
  filter(ID == "102Ecosleep", Time_of_Day == "Night") %>%
  arrange(desc(Glucose_Category)) %>%
  mutate(
    ypos = cumsum(Percentage) - 0.5 * Percentage  # Mitte jedes Segments
  )

# Donut-Plot mit externen Labels
ggplot(donut_data, aes(x = 2, y = Percentage, fill = Glucose_Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.8) +  # größerer Abstand für äußere Labels
  scale_fill_manual(values = category_colors) +
  geom_text_repel(
    aes(
      y = ypos,
      label = paste0(Percentage, "%")
    ),
    x = 2.5,
    direction = "y",
    nudge_x = 0.5,
    segment.size = 0.4,
    size = 3,
    show.legend = FALSE
  ) +
  theme_void() +
  labs(
    title = "Glucose Distribution (Night) – 102Ecosleep",
    fill = "Glucose Category"
  ) +
  theme(legend.position = "right")


##################

# 1. Donut-Daten vorbereiten
donut_data <- time_in_range_summary %>%
  arrange(ID, Time_of_Day, desc(Glucose_Category)) %>%
  group_by(ID, Time_of_Day) %>%
  mutate(
    ypos = cumsum(Percentage) - 0.5 * Percentage
  ) %>%
  ungroup()

# 2. Donut-Plot für jede ID & Zeitphase
ggplot(donut_data, aes(x = 2, y = Percentage, fill = Glucose_Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.8) +
  scale_fill_manual(values = category_colors) +
  geom_text_repel(
    aes(
      y = ypos,
      label = paste0(Percentage, "%")
    ),
    x = 2.5,
    direction = "y",
    nudge_x = 0.5,
    segment.size = 0.4,
    size = 3,
    show.legend = FALSE
  ) +
  facet_wrap(~ ID + Time_of_Day) +  # <- Jeder Plot pro ID und Tageszeit
  theme_void() +
  labs(
    title = "Glucose Distribution by ID and Time of Day",
    fill = "Glucose Category"
  ) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 10)
  )


ggplot(donut_data, aes(x = 2, y = Percentage, fill = Glucose_Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +
  scale_fill_manual(values = category_colors) +
  geom_text_repel(
    aes(
      y = ypos,
      label = paste0(Percentage, "%")
    ),
    x = 2.5,
    direction = "y",
    nudge_x = 0.5,
    segment.size = 0.4,
    size = 3,
    show.legend = FALSE
  ) +
  facet_grid(ID ~ Time_of_Day) +  # <-- Structured layout: ID in rows, day/night in columns
  theme_void() +
  labs(
    title = "Glucose Distribution by ID and Time of Day",
    fill = "Glucose Category"
  ) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 10)
  )

# Then save the plot
ggsave("glucose_donut_plot.png", width = 14, height = 4 * n_distinct(donut_data$ID))



# Filter for daytime only
donut_data_day <- donut_data %>%
  filter(Time_of_Day == "Day")

# Create the facet donut plot for Day only
ggplot(donut_data_day, aes(x = 2, y = Percentage, fill = Glucose_Category)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(1, 2.5) +
  scale_fill_manual(values = category_colors) +
  geom_text_repel(
    aes(
      y = ypos,
      label = paste0(Percentage, "%")
    ),
    x = 2.5,
    direction = "y",
    nudge_x = 0.5,
    segment.size = 0.4,
    size = 3,
    show.legend = FALSE
  ) +
  facet_wrap(~ ID, ncol = 3) +  # Only ID facets now
  theme_void() +
  labs(
    title = "Glucose Distribution (Day Only)",
    fill = "Glucose Category"
  ) +
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 10)
  )


############ spike analyis
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
    Prev_ID = lag(ID),
    Prev_Time_of_Day = lag(Time_of_Day),
    Spike = Glucose_Category != Prev_Category & ID == Prev_ID & Time_of_Day == Prev_Time_of_Day
  ) %>%
  filter(Spike) %>%
  mutate(
    Spike_Direction = paste0(Prev_Category, " → ", Glucose_Category)
  ) %>%
  group_by(ID, Time_of_Day, Spike_Direction) %>%
  summarise(Spike_Count = n(), .groups = "drop")



spike_percent <- spike_analysis %>%
  group_by(ID, Time_of_Day) %>%
  mutate(
    Total_Spikes = sum(Spike_Count),
    Percentage = round(100 * Spike_Count / Total_Spikes, 1)
  ) %>%
  ungroup()

ggplot(spike_percent, aes(x = Spike_Direction, y = Percentage, fill = Spike_Direction)) +
  geom_col() +
  coord_flip() +
  facet_grid(ID ~ Time_of_Day) +  # Rows = IDs, Columns = Day/Night
  labs(
    title = "Transitions Between Glucose Categories (Spikes)",
    x = "Transition",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 10)
  )







###############
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



# Keep the original 0–47 factor for correct time ordering
double_avg <- double_avg %>%
  mutate(
    Display_Hour = Hour_Rounded %% 24,  # 0–23 repeated
    Day = ifelse(Hour_Rounded < 24, "Day 1", "Day 2"),
    Hour_Label = paste0(Day, " – ", sprintf("%02d:00", Display_Hour)),
    Hour_Factor = factor(Hour_Label, levels = c(
      paste0("Day 1 – ", sprintf("%02d:00", 0:23)),
      paste0("Day 2 – ", sprintf("%02d:00", 0:23))
    ))
  )

ggplot(double_avg, aes(x = Hour_Factor, y = Avg_Glucose, group = ID, color = ID)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Double Plot: 48-Hour Glucose Trends per ID",
    subtitle = "Same 24h cycle shown twice to visualize rhythm",
    x = "Time of Day (Repeated)",
    y = "Average Glucose (mg/dl)",
    color = "ID"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    strip.text = element_text(face = "bold")
  )






# Start mit Time of Day und ID-Label
double_data <- cleaned_data_deduplicated %>%
  mutate(
    Date = as.Date(Timestamp),
    Hour = hour(Timestamp),
    Minute = minute(Timestamp),
    Time_of_Day = Hour + Minute / 60,
    ID_Label = case_when(
      ID == "102Ecosleep" ~ "102",
      ID == "104Ecosleep" ~ "104",
      ID == "107EcoSleep" ~ "107",
      ID == "108EcoSleep" ~ "108",
      ID == "109EcoSleep" ~ "109",
      ID == "112EcoSleep" ~ "112",
      TRUE ~ as.character(ID)
    )
  )

# Double Plot durch Duplizieren der Zeitreihe
double_data_long <- bind_rows(
  double_data %>% mutate(Double_Hour = Time_of_Day),
  double_data %>% mutate(Double_Hour = Time_of_Day + 24)
)

# Stunden runden und mitteln
double_avg_2 <- double_data_long %>%
  mutate(Hour_Rounded = floor(Double_Hour)) %>%
  group_by(ID_Label, Hour_Rounded) %>%
  summarise(Avg_Glucose = mean(`Glucose (mg/dl)`, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Display_Hour = Hour_Rounded %% 24,
    Day = ifelse(Hour_Rounded < 24, "Day 1", "Day 2"),
    Hour_Label = paste0(Day, " – ", sprintf("%02d:00", Display_Hour)),
    Hour_Factor = factor(Hour_Label, levels = c(
      paste0("Day 1 – ", sprintf("%02d:00", 0:23)),
      paste0("Day 2 – ", sprintf("%02d:00", 0:23))
    ))
  )

# Plot
ggplot(double_avg_2, aes(x = Hour_Factor, y = Avg_Glucose, group = ID_Label, color = ID_Label)) +
  geom_line(size = 1) +
  geom_point(size = 1.5) +
  labs(
    title = "Double Plot: 48-Hour Glucose Trends per ID",
    subtitle = "Same 24h cycle shown twice to visualize rhythm",
    x = "Hour of Day (Repeated)",
    y = "Average Glucose (mg/dl)",
    color = "ID"
  ) +
  scale_y_continuous(limits = c(85, 120)) +
  scale_x_discrete(
    breaks = levels(double_avg_2$Hour_Factor)[seq(1, 48, by = 4)]
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
    strip.text = element_text(face = "bold")
  )



##########
unique(cleaned_data_deduplicated$ID)

stringi::stri_escape_unicode(cleaned_data_deduplicated$ID)




class(cleaned_data_deduplicated$ID)

############# spike analysis
spike_analysis2 <- cleaned_data_deduplicated %>%
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
    Prev_ID = lag(ID),
    Prev_Time_of_Day = lag(Time_of_Day),
    Spike = Glucose_Category != Prev_Category & ID == Prev_ID & Time_of_Day == Prev_Time_of_Day
  ) %>%
  filter(Spike) %>%
  mutate(
    From = Prev_Category,
    To = Glucose_Category
  ) %>%
  mutate(
    Spike_Direction = paste0(Prev_Category, " → ", Glucose_Category)
  ) %>%
  group_by(ID, Time_of_Day, Spike_Direction, From, To) %>%
  summarise(Spike_Count = n(), .groups = "drop")



category_levels <- c("Very Low", "Low", "Normal", "High", "Very High")

spike_percent <- spike_percent %>%
  mutate(
    From = factor(From, levels = category_levels),
    To = factor(To, levels = category_levels)
  )


ggplot(spike_percent, aes(x = From, y = To, fill = Percentage)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Percentage), size = 3) +
  facet_grid(ID ~ Time_of_Day) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(
    title = "Transition Heatmap Between Glucose Categories",
    x = "Previous Category",
    y = "Current Category",
    fill = "Percent (%)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )




##########cosine model
library(dplyr)
library(ggplot2)

# Filter data for ID 102
fit_data_102 <- double_avg %>% filter(ID_Label == "102")

# Fit cosine model (period 24h)
cos_fit_102 <- nls(
  Avg_Glucose ~ A * cos(2 * pi / 24 * Hour_Rounded + phi) + C,
  data = fit_data,
  start = list(A = 10, phi = 0, C = 100)  # rough starting guesses
)

summary(cos_fit_102)

# Add predicted values
fit_data_102 <- fit_data_102 %>%
  mutate(Fitted = predict(cos_fit_102))

# Plot actual vs fitted
ggplot(fit_data_102, aes(x = Hour_Rounded)) +
  geom_point(aes(y = Avg_Glucose), color = "blue") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(
    title = "Cosine Fit for ID 102",
    x = "Hour of Day (0–47)",
    y = "Average Glucose (mg/dl)"
  ) +
  theme_minimal()


fit_data_104 <- double_avg %>% filter(ID_Label == "104")
cos_fit_104 <- nls(
  Avg_Glucose ~ A * cos(2 * pi / 24 * Hour_Rounded + phi) + C,
  data = fit_data_104,
  start = list(A = 10, phi = 0, C = 100)
)
fit_data_104 <- fit_data_104 %>%
  mutate(Fitted = predict(cos_fit_104))
ggplot(fit_data_104, aes(x = Hour_Rounded)) +
  geom_point(aes(y = Avg_Glucose), color = "blue") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(
    title = "Cosine Fit for ID 104",
    x = "Hour of Day (0–47)",
    y = "Average Glucose (mg/dl)"
  ) +
  theme_minimal()


fit_data_104 <- double_avg %>% filter(ID_Label == "104")
cos_fit_104 <- nls(
  Avg_Glucose ~ A * cos(2 * pi / 24 * Hour_Rounded + phi) + C,
  data = fit_data_104,
  start = list(A = 10, phi = 0, C = 100)
)
fit_data_104 <- fit_data_104 %>%
  mutate(Fitted = predict(cos_fit_104))
ggplot(fit_data_104, aes(x = Hour_Rounded)) +
  geom_point(aes(y = Avg_Glucose), color = "blue") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(
    title = "Cosine Fit for ID 104",
    x = "Hour of Day (0–47)",
    y = "Average Glucose (mg/dl)"
  ) +
  theme_minimal()


fit_data_107 <- double_avg %>% filter(ID_Label == "107")

cos_fit_107 <- nls(
  Avg_Glucose ~ A * cos(2 * pi / 24 * Hour_Rounded + phi) + C,
  data = fit_data_107,
  start = list(A = 10, phi = 0, C = 100)
)

fit_data_107 <- fit_data_107 %>%
  mutate(Fitted = predict(cos_fit_107))

ggplot(fit_data_107, aes(x = Hour_Rounded)) +
  geom_point(aes(y = Avg_Glucose), color = "blue") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(
    title = "Cosine Fit for ID 107",
    x = "Hour of Day (0–47)",
    y = "Average Glucose (mg/dl)"
  ) +
  theme_minimal()


fit_data_108 <- double_avg %>% filter(ID_Label == "108")

cos_fit_108 <- nls(
  Avg_Glucose ~ A * cos(2 * pi / 24 * Hour_Rounded + phi) + C,
  data = fit_data_108,
  start = list(A = 10, phi = 0, C = 100)
)

fit_data_108 <- fit_data_108 %>%
  mutate(Fitted = predict(cos_fit_108))

ggplot(fit_data_108, aes(x = Hour_Rounded)) +
  geom_point(aes(y = Avg_Glucose), color = "blue") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(
    title = "Cosine Fit for ID 108",
    x = "Hour of Day (0–47)",
    y = "Average Glucose (mg/dl)"
  ) +
  theme_minimal()

fit_data_109 <- double_avg %>% filter(ID_Label == "109")

cos_fit_109 <- nls(
  Avg_Glucose ~ A * cos(2 * pi / 24 * Hour_Rounded + phi) + C,
  data = fit_data_109,
  start = list(A = 10, phi = 0, C = 100)
)

fit_data_109 <- fit_data_109 %>%
  mutate(Fitted = predict(cos_fit_109))

ggplot(fit_data_109, aes(x = Hour_Rounded)) +
  geom_point(aes(y = Avg_Glucose), color = "blue") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(
    title = "Cosine Fit for ID 109",
    x = "Hour of Day (0–47)",
    y = "Average Glucose (mg/dl)"
  ) +
  theme_minimal()


fit_data_112 <- double_avg %>% filter(ID_Label == "112")

cos_fit_112 <- nls(
  Avg_Glucose ~ A * cos(2 * pi / 24 * Hour_Rounded + phi) + C,
  data = fit_data_112,
  start = list(A = 10, phi = 0, C = 100)
)

fit_data_112 <- fit_data_112 %>%
  mutate(Fitted = predict(cos_fit_112))

ggplot(fit_data_112, aes(x = Hour_Rounded)) +
  geom_point(aes(y = Avg_Glucose), color = "blue") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(
    title = "Cosine Fit for ID 112",
    x = "Hour of Day (0–47)",
    y = "Average Glucose (mg/dl)"
  ) +
  theme_minimal()



# Maximum bestimmen
max_row <- fit_data_102 %>% filter(Fitted == max(Fitted))
cat("Maximum:\n")
print(max_row %>% select(Hour_Rounded, Fitted))

# Minimum bestimmen
min_row <- fit_data_102 %>% filter(Fitted == min(Fitted))
cat("\nMinimum:\n")
print(min_row %>% select(Hour_Rounded, Fitted))

#######heatmap

# Step 1: Summarize across all IDs and times of day
heatmap_data <- spike_percent %>%
  group_by(From, To) %>%
  summarise(Percentage = sum(Percentage, na.rm = TRUE), .groups = "drop")

# Step 2: Ensure correct order of categories
category_levels <- c("Very Low", "Low", "Normal", "High", "Very High")
heatmap_data <- heatmap_data %>%
  mutate(
    From = factor(From, levels = category_levels),
    To = factor(To, levels = category_levels)
  )

# Step 3: Reshape to matrix
heatmap_matrix <- tidyr::pivot_wider(
  heatmap_data,
  names_from = To,
  values_from = Percentage,
  values_fill = 0
) %>%
  column_to_rownames("From") %>%
  as.matrix()

# Step 4: Create the heatmap (base R)
heatmap(
  heatmap_matrix,
  Rowv = NA, Colv = NA,
  col = colorRampPalette(c("white", "steelblue"))(100),
  scale = "none",
  main = "Transition Heatmap: All IDs Combined",
  xlab = "To Category",
  ylab = "From Category",
  margins = c(8, 8)
)

# Step 1: Summarize across all IDs and times of day
heatmap_data <- spike_percent %>%
  group_by(From, To) %>%
  summarise(Percentage = sum(Percentage, na.rm = TRUE), .groups = "drop")

# Step 2: Ensure correct factor order
category_levels <- c("Very Low", "Low", "Normal", "High", "Very High")

heatmap_data <- heatmap_data %>%
  mutate(
    From = factor(From, levels = category_levels),
    To = factor(To, levels = category_levels)
  )

# Step 3: Create an empty matrix with correct row and column order
heatmap_matrix <- matrix(
  0, 
  nrow = length(category_levels), 
  ncol = length(category_levels),
  dimnames = list(From = category_levels, To = category_levels)
)

# Step 4: Fill in the matrix with your summarized percentages
for (i in seq_len(nrow(heatmap_data))) {
  row <- heatmap_data$From[i]
  col <- heatmap_data$To[i]
  val <- heatmap_data$Percentage[i]
  heatmap_matrix[row, col] <- val
}

# Step 5: Plot heatmap with correct axis order
heatmap(
  heatmap_matrix,
  Rowv = NA, Colv = NA,
  col = colorRampPalette(c("white", "steelblue"))(100),
  scale = "none",
  main = "Transition Heatmap: All IDs Combined",
  xlab = "To Category",
  ylab = "From Category",
  margins = c(8, 8)
)



# Install if needed
install.packages("pheatmap")

library(pheatmap)

# Ensure correct factor order
category_levels <- c("Very Low", "Low", "Normal", "High", "Very High")

# Prepare data summarized across all IDs
heatmap_data <- spike_percent %>%
  group_by(From, To) %>%
  summarise(Percentage = sum(Percentage, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    From = factor(From, levels = category_levels),
    To = factor(To, levels = category_levels)
  )

# Create matrix
heatmap_matrix <- matrix(
  0, 
  nrow = length(category_levels), 
  ncol = length(category_levels),
  dimnames = list(From = category_levels, To = category_levels)
)

for (i in seq_len(nrow(heatmap_data))) {
  row <- heatmap_data$From[i]
  col <- heatmap_data$To[i]
  val <- heatmap_data$Percentage[i]
  heatmap_matrix[row, col] <- val
}

# Plot with percentages inside squares
pheatmap(
  heatmap_matrix,
  display_numbers = TRUE,              # Shows the numbers inside squares
  number_format = "%.1f",              # Format for percentages
  color = colorRampPalette(c("white", "steelblue"))(100),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Transition Heatmap (All IDs Combined)",
  angle_col = 45
)





# Summarise raw counts, not percentages
overall_data <- spike_analysis %>%
  separate(Spike_Direction, into = c("From", "To"), sep = " → ") %>%
  group_by(From, To) %>%
  summarise(Total_Spike_Count = sum(Spike_Count), .groups = "drop")

# Total transitions for percentage calculation
total_spikes_all <- sum(overall_data$Total_Spike_Count)

# Compute true overall percentage
overall_data <- overall_data %>%
  mutate(
    Percentage = round(100 * Total_Spike_Count / total_spikes_all, 1),
    From = factor(From, levels = category_levels),
    To = factor(To, levels = category_levels)
  )

# Create matrix for pheatmap
heatmap_matrix <- matrix(
  0, 
  nrow = length(category_levels), 
  ncol = length(category_levels),
  dimnames = list(From = category_levels, To = category_levels)
)

for (i in seq_len(nrow(overall_data))) {
  row <- overall_data$From[i]
  col <- overall_data$To[i]
  val <- overall_data$Percentage[i]
  heatmap_matrix[row, col] <- val
}

# Plot with correct percentages
pheatmap(
  heatmap_matrix,
  display_numbers = TRUE,
  number_format = "%.1f",  
  color = colorRampPalette(c("white", "steelblue"))(100),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Transition Heatmap (Overall Study Percentage)",
  angle_col = 45
)





library(pheatmap)

# Summarise raw counts, not percentages
overall_data <- spike_analysis %>%
  separate(Spike_Direction, into = c("From", "To"), sep = " → ") %>%
  group_by(From, To) %>%
  summarise(Total_Spike_Count = sum(Spike_Count), .groups = "drop")

# Total transitions for percentage calculation
total_spikes_all <- sum(overall_data$Total_Spike_Count)

# Compute true overall percentage
overall_data <- overall_data %>%
  mutate(
    Percentage = round(100 * Total_Spike_Count / total_spikes_all, 1),
    From = factor(From, levels = category_levels),
    To = factor(To, levels = category_levels)
  )



# Create matrix for pheatmap with reversed row order
heatmap_matrix <- matrix(
  0, 
  nrow = length(category_levels), 
  ncol = length(category_levels),
  dimnames = list(From = rev(category_levels), To = category_levels)
)

# Populate matrix
for (i in seq_len(nrow(overall_data))) {
  row <- overall_data$From[i]
  col <- overall_data$To[i]
  val <- overall_data$Percentage[i]
  heatmap_matrix[as.character(row), as.character(col)] <- val
}


# Plot with correct percentages: using pheatmap() function to be able to integrate percentages
pheatmap(
  heatmap_matrix,
  display_numbers = TRUE,
  number_format = "%.1f",  
  color = colorRampPalette(c("white", "steelblue"))(100),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Transition Heatmap (Overall Study Percentage)",
  angle_col = 45
)

library(grid)

# Heatmap ohne automatische Zeilennamen
pheatmap(
  heatmap_matrix,
  display_numbers = TRUE,
  number_format = "%.1f",
  color = colorRampPalette(c("white", "steelblue"))(100),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Transition Heatmap (Overall Study Percentage)",
  angle_col = 45,
  show_rownames = FALSE  # Zeilennamen ausblenden
)

pheatmap(
  heatmap_matrix,
  display_numbers = TRUE,
  number_format = "%.1f",
  color = colorRampPalette(c("white", "steelblue"))(100),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Transition Heatmap (Overall Study Percentage)",
  angle_col = 45,
  show_rownames = FALSE,      # Zeilennamen ausgeblendet
  fontsize_row = 8,           # beeinflusst Höhe der Zellen/Heatmap
  fontsize_col = 10
)


# Manuelle Beschriftung links hinzufügen
grid.text(
  label = rev(category_levels),  # Beachte: Reihenfolge ggf. anpassen
  x = unit(0.02, "npc"),         # Abstand von links; ggf. justieren
  y = unit(seq(0.9, 0.1, length.out = length(category_levels)), "npc"),
  just = "right"
)

############## day and night
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


######## cosinor
install.packages("cosinor")
library(tidyverse)
library(lubridate)
library(cosinor)

# Daten für ID 102 vorbereiten
data_102 <- double_data %>%
  filter(ID_Label == "102") %>%
  mutate(
    Week = isoweek(Timestamp),
    Year = year(Timestamp)
  )

# Cosinor-Fit pro Woche, nur wenn genug Daten vorhanden sind
fit_results <- data_102 %>%
  group_by(Year, Week) %>%
  group_split() %>%
  map_df(~ {
    df <- .x
    if (nrow(df) >= 30) {  # Mindestanzahl an Messungen für Stabilität
      fit <- cosinor.lm(`Glucose (mg/dl)` ~ time(Time_of_Day), period = 24, data = df)
      tibble(
        Year = unique(df$Year),
        Week = unique(df$Week),
        Acrophase_radians = coef(fit)["acrophase"],
        Amplitude = coef(fit)["amplitude"],
        MESOR = coef(fit)["mesor"],
        p_value = summary(fit)$parameters["amplitude", "Pr(>|t|)"]
      )
    } else {
      NULL
    }
  })

# Acrophase von Radiant in Stunden umrechnen
fit_results <- fit_results %>%
  mutate(Acrophase_hours = (Acrophase_radians * 24) / (2 * pi))

# Ergebnisse ansehen
print(fit_results)


week1 <- data_102 %>% filter(Year == 2024, Week == 12) %>% drop_na(`Glucose (mg/dl)`, Time_of_Day)

ggplot(week1, aes(x = Time_of_Day, y = `Glucose (mg/dl)`)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ cos(2 * pi / 24 * x) + sin(2 * pi / 24 * x)) +
  theme_minimal()

head(data_102)


# Example: Filter one week's data for ID 102
week_data <- double_data %>%
  filter(ID_Label == "102") %>%
  mutate(
    Date = as.Date(Timestamp),
    Week = isoweek(Date),
    Year = year(Date)
  ) %>%
  filter(Year == 2024, Week == 12) %>%
  drop_na(`Glucose (mg/dl)`, Time_of_Day)

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(knitr)

# Add sine and cosine predictors
week_data <- week_data %>%
  mutate(
    Cosine = cos(2 * pi / 24 * Time_of_Day),
    Sine = sin(2 * pi / 24 * Time_of_Day)
  )

# Basic linear regression with sine and cosine terms
fit <- lm(`Glucose (mg/dl)` ~ Cosine + Sine, data = week_data)
summary(fit)

coefs <- coef(fit)

MESOR <- coefs[1]
beta_cos <- coefs["Cosine"]
beta_sin <- coefs["Sine"]

# Amplitude
Amplitude <- sqrt(beta_cos^2 + beta_sin^2)

# Phase in radians
Phase_rad <- atan2(-beta_sin, beta_cos)

# Optional: Convert Phase to hours (0–24)
Phase_hours <- (Phase_rad / (2 * pi)) * 24
if (Phase_hours < 0) {
  Phase_hours <- Phase_hours + 24
}

# Show results
cat("MESOR:", MESOR, "\n")
cat("Amplitude:", Amplitude, "\n")
cat("Phase (hours):", Phase_hours, "\n")

# Generate time grid for smooth curve
time_grid <- data.frame(Time_of_Day = seq(0, 24, length.out = 200))

# Calculate fitted values
time_grid$Fitted <- MESOR + Amplitude * cos(2 * pi / 24 * time_grid$Time_of_Day + Phase_rad)

ggplot(week_data, aes(x = Time_of_Day)) +
  geom_point(aes(y = `Glucose (mg/dl)`), color = "blue", alpha = 0.5) +
  geom_line(data = time_grid, aes(x = Time_of_Day, y = Fitted), color = "red", size = 1.2) +
  labs(
    title = "Manual Cosinor Fit - Glucose Rhythm (ID 102, Week 12)",
    x = "Time of Day (hours)",
    y = "Glucose (mg/dl)"
  ) +
  theme_minimal()

# Pick Week 12 in 2024
week_data <- data_102 %>%
  filter(Year == 2024, Week == 12) %>%
  drop_na(`Glucose (mg/dl)`, Time_of_Day)

# Classic Cosinor Fit: Period = 24 hours
cos_fit <- cosinor.lm(`Glucose (mg/dl)` ~ time(Time_of_Day), 
                      period = 24, 
                      data = week_data)

# Results
summary(cos_fit)

library(ggplot2)

# Create Prediction Data
newdata <- data.frame(Time_of_Day = seq(0, 24, length.out = 100))
newdata$Fitted <- predict(cos_fit, newdata)

# Plot
ggplot(week_data, aes(x = Time_of_Day, y = `Glucose (mg/dl)`)) +
  geom_point(alpha = 0.5) +
  geom_line(data = newdata, aes(x = Time_of_Day, y = Fitted), color = "red", size = 1.2) +
  labs(title = "Cosinor Fit - Week 12, ID 102",
       x = "Time of Day (hours)",
       y = "Glucose (mg/dl)") +
  theme_minimal()





library(cosinor)

cos_fit <- cosinor.lm(`Glucose (mg/dl)` ~ time(Time_of_Day), 
                      period = 24, 
                      data = week_data)
summary(cos_fit)

newdata <- data.frame(Time_of_Day = seq(0, 24, length.out = 100))
newdata$Fitted <- predict(cos_fit, newdata = newdata)

library(ggplot2)

ggplot(week_data, aes(x = Time_of_Day, y = `Glucose (mg/dl)`)) +
  geom_point(alpha = 0.5) +
  geom_line(data = newdata, aes(x = Time_of_Day, y = Fitted), color = "red", size = 1.2) +
  labs(title = "Cosinor Fit - Week 12, ID 102",
       x = "Time of Day (hours)",
       y = "Glucose (mg/dl)") +
  theme_minimal()


# Filter your week_data
week_data <- data_102 %>%
  filter(Year == 2024, Week == 12) %>%
  drop_na(`Glucose (mg/dl)`, Time_of_Day)

# Fit model
cos_fit <- cosinor.lm(`Glucose (mg/dl)` ~ time(Time_of_Day), 
                      period = 24, 
                      data = week_data)
summary(cos_fit)



# Generate sequence for smooth curve over 24 hours
newdata <- data.frame(Time_of_Day = seq(0, 24, length.out = 200))

# Predict fitted values
newdata$Fitted <- predict(cos_fit, newdata = newdata)
head(newdata)
ggplot(week_data, aes(x = Time_of_Day, y = `Glucose (mg/dl)`)) +
  geom_point(alpha = 0.5) +
  geom_line(data = newdata, aes(x = Time_of_Day, y = Fitted), color = "red", size = 1.2) +
  labs(title = "Cosinor Fit - Week 12, ID 102",
       x = "Time of Day (hours)",
       y = "Glucose (mg/dl)") +
  theme_minimal()


is.numeric(week_data$Time_of_Day)

ggplot_cosinor.lm(cos_fit)



data("vitamind")
 head(vitamind)

 fit <- cosinor.lm(Y ~ time(time) + X + amp.acro(X), data = vitamind, period = 12) 
summary(fit) 

test_cosinor(fit, "X", param = "amp")
summary(vitamind$Y)
summary(predict(fit))
library(ggplot2)
ggplot_cosinor.lm(fit, x_str = "X")


library(lubridate)
library(dplyr)
library(cosinor) 

# create dataset for cosinor model for 102Ecosleep
glucose_fit_102_data <- cleaned_data_deduplicated %>%
  filter(ID == "102Ecosleep") %>%    # filter for 102
  mutate(
    datetime = dmy_hm(DeviceTimestamp),  #  if format TT-MM-JJJJ HH:MM
    dec.time = hour(datetime) + minute(datetime) / 60  # time as decimal hour during the day
  )

# cosinor model
glucose_fit_102 <- cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = glucose_fit_102_data, period = 24) 
summary(glucose_fit_102) 

# raw values
summary(glucose_fit_102_data$`Glucose (mg/dl)`)

# adjusted values
summary(predict(glucose_fit_102))

# plot the fit
ggplot_cosinor.lm(glucose_fit_102)

fit_data_102 <- double_avg %>% filter(ID_Label == "102")

# Fit cosine model (period 24h)
cos_fit_102 <- nls(
  Avg_Glucose ~ A * cos(2 * pi / 24 * Hour_Rounded + phi) + C,
  data = fit_data_102,
  start = list(A = 10, phi = 0, C = 100)  # rough starting guesses
)

summary(cos_fit_102)

# Add predicted values
fit_data_102 <- fit_data_102 %>%
  mutate(Fitted = predict(cos_fit_102))

# Plot actual vs fitted
ggplot(fit_data_102, aes(x = Hour_Rounded)) +
  geom_point(aes(y = Avg_Glucose), color = "blue") +
  geom_line(aes(y = Fitted), color = "red") +
  labs(
    title = "Cosine Fit for ID 102",
    x = "Hour of Day (0–47)",
    y = "Average Glucose (mg/dl)"
  ) +
  theme_minimal()


# Vorbereitung prüfen
summary(glucose_fit_102_data$`Glucose (mg/dl)`)
summary(glucose_fit_102_data$dec.time)

# Manuelles Cosinus-Modell wie bei double_avg, aber jetzt mit Originaldaten
cos_fit_102_orig <- nls(
  `Glucose (mg/dl)` ~ A * cos(2 * pi / 24 * dec.time + phi) + C,
  data = glucose_fit_102_data,
  start = list(A = 10, phi = 0, C = 100)  # Passe Startwerte ggf. basierend auf Daten an
)

# Ergebnis anzeigen
summary(cos_fit_102_orig)

library(ggplot2)

ggplot(glucose_fit_102_data, aes(x = dec.time, y = `Glucose (mg/dl)`)) +
  geom_point(alpha = 0.3) +
  geom_line(aes(y = Fitted), color = "blue", size = 1) +
  labs(title = "Cosinus-Fit für Teilnehmer 102 mit Originaldaten",
       x = "Zeit (Dezimalstunden)",
       y = "Glukose (mg/dl)") +
  theme_minimal()

glucose_fit_102_data <- glucose_fit_102_data %>%
  mutate(Fitted = predict(cos_fit_102_orig))



ggplot(glucose_fit_102_data, aes(x = dec.time, y = `Glucose (mg/dl)`)) +
  geom_point(alpha = 0.3, color = "grey") +  # Original glucose values
  geom_line(aes(y = Fitted), color = "blue", size = 1) +  # Cosinus fit
  labs(title = "Manual Cosinus-Fit for 102 with original data",
       x = "time (decimal hours)",
       y = "Glucose (mg/dl)") +
  theme_minimal()

cos_fit_104_orig <- nls(
  `Glucose (mg/dl)` ~ A * cos(2 * pi / 24 * dec.time + phi) + C,
  data = glucose_fit_104_data,
  start = list(A = 10, phi = 0, C = 100)  # start values
)

# Ergebnis anzeigen
summary(cos_fit_104_orig)

# prepare dataset for plotting
glucose_fit_104_data <- glucose_fit_104_data %>%
  mutate(Fitted = predict(cos_fit_104_orig))


ggplot(glucose_fit_104_data, aes(x = dec.time, y = `Glucose (mg/dl)`)) +
  geom_point(alpha = 0.3, color = "grey") +  # Original glucose values
  geom_line(aes(y = Fitted), color = "blue", size = 1) +  # Cosinus fit
  labs(title = "Manual Cosinus-Fit for 104 with original data",
       x = "time (decimal hours)",
       y = "Glucose (mg/dl)") +
  theme_minimal()



library(cosinor)  # ensure cosinor.lm is loaded

# Define storage for results
moving_window_102 <- data.frame(Window_Start = as.POSIXct(character()),
                      Amp = numeric(),
                      Acrophase_Hours = numeric(),
                      Intercept = numeric(),
                      stringsAsFactors = FALSE)

# Define time boundaries
start_time_mv_102 <- min(glucose_fit_102_data$datetime)
end_time_mv_102 <- max(glucose_fit_102_data$datetime)

# Loop: slide by 1 day, window is 3 days
window_length <- days(3)
step_size <- days(1)

current_start <- start_time_mv_102

while (current_start + window_length <= end_time_mv_102) {
  
  current_end <- current_start + window_length
  
  # Filter window
  window_data_102 <- glucose_fit_102_data %>%
    filter(datetime >= current_start & datetime < current_end)
  
  # Skip if too few data points
  if (nrow(window_data_102) < 100) {  
    current_start <- current_start + step_size
    next
  }
  
  # Fit Cosinor model
  fit <- tryCatch({
    cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = window_data_102, period = 24)
  }, error = function(e) NA)
  
  if (!is.na(fit)) {
    
    # Extract transformed coefficients
    coefs <- coef(fit, transformed = TRUE)
    
    amp <- coefs["amp", "estimate"]
    acr <- coefs["acr", "estimate"]
    intercept <- coefs["(Intercept)", "estimate"]
    
    # Convert acrophase to hours
    acrophase_hours <- (acr * 24) / (2 * pi)
    
    moving_window_102 <- rbind(moving_window_102, data.frame(
      Window_Start = current_start,
      Amp = amp,
      Acrophase_Hours = (acrophase_hours + 24) %% 24,
      Intercept = intercept
    ))
  }
  
  # Advance window
  current_start <- current_start + step_size
}

library(cosinor)
library(dplyr)
library(lubridate)

# Define storage for results
moving_window_102 <- data.frame(Window_Start = as.POSIXct(character()),
                                Amp = numeric(),
                                Acrophase_Hours = numeric(),
                                Intercept = numeric(),
                                stringsAsFactors = FALSE)

# Time boundaries
start_time_mv_102 <- min(glucose_fit_102_data$datetime)
end_time_mv_102 <- max(glucose_fit_102_data$datetime)

# Window parameters
window_length <- days(3)
step_size <- days(1)

current_start <- start_time_mv_102

while (current_start + window_length <= end_time_mv_102) {
  
  current_end <- current_start + window_length
  
  # Filter window
  window_data_102 <- glucose_fit_102_data %>%
    filter(datetime >= current_start & datetime < current_end)
  
  # Skip small windows
  if (nrow(window_data_102) < 100) {
    current_start <- current_start + step_size
    next
  }
  
  # Try to fit cosinor model
  fit <- tryCatch({
    cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = window_data_102, period = 24)
  }, error = function(e) NULL)
  
  if (inherits(fit, "cosinor.lm")) {
    
    coefs <- coef(fit, transformed = TRUE)
    
    amp <- coefs["amp", "estimate"]
    acr <- coefs["acr", "estimate"]
    intercept <- coefs["(Intercept)", "estimate"]
    
    # Convert acrophase to hours
    acrophase_hours <- (acr * 24) / (2 * pi)
    
    moving_window_102 <- rbind(moving_window_102, data.frame(
      Window_Start = current_start,
      Amp = amp,
      Acrophase_Hours = (acrophase_hours + 24) %% 24,
      Intercept = intercept
    ))
  }
  

  
  library(cosinor)
  library(dplyr)
  library(lubridate)
  
  # Storage for results
  moving_window_102 <- data.frame(
    Window_Start = as.POSIXct(character()),
    Amp = numeric(),
    Acrophase_Hours = numeric(),
    Intercept = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Time boundaries
  start_time_mv_102 <- min(glucose_fit_102_data$datetime)
  end_time_mv_102 <- max(glucose_fit_102_data$datetime)
  
  # Window setup
  window_length <- days(3)
  step_size <- days(1)
  current_start <- start_time_mv_102
  
  while (current_start + window_length <= end_time_mv_102) {
    
    current_end <- current_start + window_length
    
    # Filter window
    window_data_102 <- glucose_fit_102_data %>%
      filter(datetime >= current_start & datetime < current_end)
    
    # Skip small windows
    if (nrow(window_data_102) < 100) {
      current_start <- current_start + step_size
      next
    }
    
    # Try fitting model
    fit <- tryCatch({
      cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = window_data_102, period = 24)
    }, error = function(e) NA)
    
    # Proceed only if fit succeeded
    if (inherits(fit, "cosinor.lm")) {
      
      coefs <- coef(fit, transformed = TRUE)
      
      # Safe extraction
      if (is.matrix(coefs)) {
        amp <- coefs["amp", "estimate"]
        acr <- coefs["acr", "estimate"]
        intercept <- coefs["(Intercept)", "estimate"]
      } else if (is.vector(coefs)) {
        amp <- coefs["amp"]
        acr <- coefs["acr"]
        intercept <- coefs["(Intercept)"]
      } else {
        current_start <- current_start + step_size
        next
      }
      
      # Convert acrophase to hours
      acrophase_hours <- (acr * 24) / (2 * pi)
      
      # Store results
      moving_window_102 <- rbind(moving_window_102, data.frame(
        Window_Start = current_start,
        Amp = amp,
        Acrophase_Hours = (acrophase_hours + 24) %% 24,
        Intercept = intercept
      ))
    }
    
    # Slide window
    current_start <- current_start + step_size
  }
  
  library(ggplot2)
  
  ggplot(moving_window_102, aes(x = Window_Start, y = Acrophase_Hours)) +
    geom_line(color = "blue") +
    geom_point(color = "darkred") +
    labs(title = "Acrophase over Time (3-Day Moving Window)",
         x = "Window Start Date",
         y = "Acrophase (Hours)") +
    theme_minimal()
  
  
  
  
  
  
  # Storage for results
  moving_window_102 <- data.frame(
    Window_Start = as.POSIXct(character()),
    Amp = numeric(),
    Acrophase_Hours = numeric(),
    Intercept = numeric(),
    stringsAsFactors = FALSE
  )
  
  moving_window_102 <- data.frame(
    Window_Start = as.POSIXct(character()),
    Amp = numeric(),
    Acrophase_Raw_Hours = numeric(),
    Max_Time_Hours = numeric(),
    Min_Time_Hours = numeric(),
    Intercept = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Time boundaries
  start_time_mv_102 <- min(glucose_fit_102_data$datetime)
  end_time_mv_102 <- max(glucose_fit_102_data$datetime)
  
  # Window setup
  window_length <- days(3)
  step_size <- days(1)
  current_start_102 <- start_time_mv_102
  
  while (current_start_102 + window_length <= end_time_mv_102) {
    
    current_end_102 <- current_start_102 + window_length
    
    # Filter window
    window_data_102 <- glucose_fit_102_data %>%
      filter(datetime >= current_start_102 & datetime < current_end_102)
    
    # Skip small windows
    if (nrow(window_data_102) < 100) {
      current_start_102 <- current_start_102 + step_size
      next
    }
    
    # Try fitting model
    fit_102 <- tryCatch({
      cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = window_data_102, period = 24)
    }, error = function(e) NA)
    
    # Proceed only if fit succeeded
    if (inherits(fit_102, "cosinor.lm")) {
      
      coefs_102 <- coef(fit_102, transformed = TRUE)
      
      # Safe extraction
      if (is.matrix(coefs_102)) {
        amp <- coefs_102["amp", "estimate"]
        acr <- coefs_102["acr", "estimate"]
        intercept <- coefs_102["(Intercept)", "estimate"]
      } else if (is.vector(coefs_102)) {
        amp <- coefs_102["amp"]
        acr <- coefs_102["acr"]
        intercept <- coefs_102["(Intercept)"]
      } else {
        current_start_102 <- current_start_102 + step_size
        next
      }
      
      
      # Roh-Acrophase in Stunden
      acrophase_raw <- (acr * 24) / (2 * pi)
      
      # Zeitpunkt des Maximums: ggf. 12h verschoben bei negativer Amplitude
      max_time <- ( (acrophase_raw + ifelse(amp < 0, 12, 0)) + 24 ) %% 24
      min_time <- (max_time + 12) %% 24
      
      # Ergebnisse speichern
      moving_window_102 <- rbind(moving_window_102, data.frame(
        Window_Start = current_start_102,
        Amp = amp,
        Acrophase_Raw_Hours = (acrophase_raw + 24) %% 24,
        Max_Time_Hours = max_time,
        Min_Time_Hours = min_time,
        Intercept = intercept
      ))
    }
    # Nächstes Fenster
    current_start_102 <- current_start_102 + step_size
  }
    
  
  library(ggplot2)
  
  ggplot(moving_window_102, aes(x = Window_Start)) +
    geom_line(aes(y = Max_Time_Hours, color = "Maximum")) +
    geom_line(aes(y = Min_Time_Hours, color = "Minimum")) +
    geom_point(aes(y = Max_Time_Hours, color = "Maximum")) +
    geom_point(aes(y = Min_Time_Hours, color = "Minimum")) +
    scale_color_manual(values = c("Maximum" = "red", "Minimum" = "blue")) +
    scale_y_continuous(breaks = seq(0, 24, 2), limits = c(0, 24)) +
    labs(
      title = "Tageszeitlicher Verlauf von Maximum und Minimum (Glukose-Cosinor)",
      x = "Fenster-Startzeitpunkt",
      y = "Tageszeit (Stunden)",
      color = "Zeitpunkt"
    ) +
    theme_minimal()
  
  
  
  library(dplyr)
  library(cosinor)
  library(lubridate)
  
  # Speicher für Ergebnisse vorbereiten
  moving_window_102 <- data.frame(
    Window_Start = as.POSIXct(character()),
    Amp = numeric(),
    Acrophase_Raw_Hours = numeric(),
    Max_Time_Hours = numeric(),
    Min_Time_Hours = numeric(),
    Intercept = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Zeitgrenzen bestimmen
  start_time_mv_102 <- min(glucose_fit_102_data$datetime)
  end_time_mv_102 <- max(glucose_fit_102_data$datetime)
  
  # Fenstergrößen
  window_length <- days(3)
  step_size <- days(1)
  current_start_102 <- start_time_mv_102
  
  # Gleitfenster-Schleife
  while (current_start_102 + window_length <= end_time_mv_102) {
    
    current_end_102 <- current_start_102 + window_length
    
    # Filtere Daten im aktuellen Zeitfenster
    window_data_102 <- glucose_fit_102_data %>%
      filter(datetime >= current_start_102 & datetime < current_end_102)
    
    # Überspringe Fenster mit zu wenigen Werten
    if (nrow(window_data_102) < 100) {
      current_start_102 <- current_start_102 + step_size
      next
    }
    
    # Cosinor-Modell fitten
    fit_102 <- tryCatch({
      cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = window_data_102, period = 24)
    }, error = function(e) NA)
    
    if (inherits(fit_102, "cosinor.lm")) {
      
      coefs_102 <- coef(fit_102, transformed = TRUE)
      
      if (is.matrix(coefs_102)) {
        amp_102 <- coefs_102["amp", "estimate"]
        acr_102 <- coefs_102["acr", "estimate"]
        intercept_102 <- coefs_102["(Intercept)", "estimate"]
      } else if (is.vector(coefs_102)) {
        amp_102 <- coefs_102["amp"]
        acr_102 <- coefs_102["acr"]
        intercept_102 <- coefs_102["(Intercept)"]
      } else {
        current_start_102 <- current_start_102 + step_size
        next
      }
      
      # Roh-Acrophase in Stunden
      acrophase_raw_102 <- (acr_102 * 24) / (2 * pi)
      
      # Max/Min abhängig vom Vorzeichen der Amplitude
      if (amp_102 >= 0) {
        max_time_102 <- (acrophase_raw_102 + 24) %% 24
        min_time_102 <- (acrophase_raw_102 + 12) %% 24
      } else {
        max_time_102 <- (acrophase_raw_102 + 12 + 24) %% 24
        min_time_102 <- (acrophase_raw_102 + 24) %% 24
      }
      
      # Ergebnisse speichern
      moving_window_102 <- rbind(moving_window_102, data.frame(
        Window_Start = current_start_102,
        Amp = amp_102,
        Acrophase_Raw_Hours = (acrophase_raw_102 + 24) %% 24,
        Max_Time_Hours = max_time_102,
        Min_Time_Hours = min_time_102,
        Intercept = intercept_102
      ))
    }
    
    # Fenster weiterschieben
    current_start_102 <- current_start_102 + step_size
  }
  
  
  library(ggplot2)
  
  ggplot(moving_window_102, aes(x = Window_Start)) +
    geom_line(aes(y = Max_Time_Hours, color = "Max")) +
    geom_line(aes(y = Min_Time_Hours, color = "Min")) +
    labs(
      title = "Tageszeitlicher Verlauf von Maxima und Minima (3-Tage-Fenster)",
      x = "Fenster-Startzeitpunkt",
      y = "Tageszeit (Stunden)",
      color = "Zeitpunkt"
    ) +
    scale_color_manual(values = c("Max" = "darkred", "Min" = "steelblue")) +
    theme_minimal()
  
  
  
  library(dplyr)
  library(lubridate)
  library(cosinor)
  
  # Leeres Dataframe für Ergebnisse
  moving_window_102 <- data.frame(
    Window_Start = as.POSIXct(character()),
    Amp = numeric(),
    Acrophase_Raw_Hours = numeric(),
    Max_Time_Hours = numeric(),
    Min_Time_Hours = numeric(),
    Intercept = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Zeitgrenzen bestimmen
  start_time_mv_102 <- min(glucose_fit_102_data$datetime)
  end_time_mv_102 <- max(glucose_fit_102_data$datetime)
  
  # Fenstergröße und Schrittweite
  window_length <- days(3)
  step_size <- days(1)
  current_start_102 <- start_time_mv_102
  
  # Sliding Window Analyse
  while (current_start_102 + window_length <= end_time_mv_102) {
    
    current_end_102 <- current_start_102 + window_length
    
    # Daten im Fenster filtern
    window_data_102 <- glucose_fit_102_data %>%
      filter(datetime >= current_start_102 & datetime < current_end_102)
    
    # Zu kleine Fenster überspringen
    if (nrow(window_data_102) < 100) {
      current_start_102 <- current_start_102 + step_size
      next
    }
    
    # Modell fitten
    fit_102 <- tryCatch({
      cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = window_data_102, period = 24)
    }, error = function(e) NA)
    
    if (inherits(fit_102, "cosinor.lm")) {
      
      coefs_102 <- coef(fit_102, transformed = TRUE)
      
      # Parameter extrahieren
      if (is.matrix(coefs_102)) {
        amp <- coefs_102["amp", "estimate"]
        acr <- coefs_102["acr", "estimate"]
        intercept <- coefs_102["(Intercept)", "estimate"]
      } else if (is.vector(coefs_102)) {
        amp <- coefs_102["amp"]
        acr <- coefs_102["acr"]
        intercept <- coefs_102["(Intercept)"]
      } else {
        current_start_102 <- current_start_102 + step_size
        next
      }
      
      # Roh-Acrophase in Stunden
      acrophase_raw <- (acr * 24) / (2 * pi)
      acrophase_norm <- (acrophase_raw + 24) %% 24
      
      # Max/Min Berechnung basierend auf Amplitude
      if (amp >= 0) {
        max_time <- acrophase_norm
        min_time <- (acrophase_norm + 12) %% 24
      } else {
        min_time <- acrophase_norm
        max_time <- (acrophase_norm + 12) %% 24
      }
      
      # Sicherstellen: Max > 12, Min < 12
      if (max_time < 12) {
        temp <- max_time
        max_time <- min_time
        min_time <- temp
      }
      
      # Ergebnis speichern
      moving_window_102 <- rbind(moving_window_102, data.frame(
        Window_Start = current_start_102,
        Amp = amp,
        Acrophase_Raw_Hours = acrophase_norm,
        Max_Time_Hours = max_time,
        Min_Time_Hours = min_time,
        Intercept = intercept
      ))
    }
    
    # Nächstes Fenster
    current_start_102 <- current_start_102 + step_size
  }
  
  
  ggplot(moving_window_102, aes(x = Window_Start)) +
    geom_line(aes(y = Max_Time_Hours, color = "Maximum"), size = 1) +
    geom_line(aes(y = Min_Time_Hours, color = "Minimum"), size = 1) +
    geom_point(aes(y = Max_Time_Hours, color = "Maximum"), size = 2) +
    geom_point(aes(y = Min_Time_Hours, color = "Minimum"), size = 2) +
    scale_color_manual(
      name = "Zeitpunkt",
      values = c("Maximum" = "red", "Minimum" = "blue")
    ) +
    scale_y_continuous(
      breaks = seq(0, 24, 2),
      limits = c(0, 24),
      expand = c(0, 0)
    ) +
    labs(
      title = "Tageszeitlicher Verlauf von Maximum und Minimum (Glukose-Cosinor)",
      x = "Fenster-Startzeitpunkt",
      y = "Tageszeit (in Stunden)"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
 
#### Polar plot ####    
  library(ggplot2)
  library(dplyr)
  
  # Convert hours to radians
  moving_window_102 <- moving_window_102 %>%
    mutate(
      Min_Rad = (Min_Time_Hours / 24) * 2 * pi,
      Max_Rad = (Max_Time_Hours / 24) * 2 * pi
    )
  
  # Create a long-format dataframe for min and max points
  plot_data <- moving_window_102 %>%
    select(Window_Start, Min_Rad, Max_Rad) %>%
    tidyr::pivot_longer(cols = c(Min_Rad, Max_Rad), names_to = "Type", values_to = "Radians")
  
  # Plot with polar coordinates
  ggplot(moving_window_102, aes(x = Window_Start)) +
    # Add shaded ribbon between min and max (abweichungen = deviation)
    geom_ribbon(aes(ymin = Min_Rad, ymax = Max_Rad), fill = "grey80", alpha = 0.5) +
    
    # Add lines for min and max times
    geom_line(aes(y = Min_Rad, color = "Minimum"), size = 1) +
    geom_line(aes(y = Max_Rad, color = "Maximum"), size = 1) +
    
    # Convert y axis to polar coordinates
    scale_y_continuous(
      limits = c(0, 2 * pi),
      breaks = seq(0, 2 * pi, by = pi / 6),
      labels = paste0(seq(0, 330, by = 30), "°")
    ) +
    
    coord_polar(start = 0, direction = 1) +
    
    scale_color_manual(
      values = c("Minimum" = "blue", "Maximum" = "red"),
      name = "Time Point"
    ) +
    
    labs(
      title = "Polar Plot of Diurnal Timing (Glucose Cosinor)",
      x = "Window Start Time",
      y = "Time of Day (Radians)"
    ) +
    
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.y = element_text(size = 10),
      axis.title.y = element_text(margin = margin(r = 10))
    )
  
  
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  library(ggplot2)
  library(dplyr)
  
  library(dplyr)
  library(ggplot2)
  
  # 1. Umrechnung der Zeiten in Radianten (0 bis 2pi)
  moving_window_102 <- moving_window_102 %>%
    mutate(
      Min_Rad = (Min_Time_Hours / 24) * 2 * pi,
      Max_Rad = (Max_Time_Hours / 24) * 2 * pi
    )
  
  # 2. Durchschnittliche Min und Max (Mittelwerte)
  avg_min_rad <- mean(moving_window_102$Min_Rad)
  avg_max_rad <- mean(moving_window_102$Max_Rad)
  
  # 3. Schwankungsbereich (z.B. +/- 1 Standardabweichung)
  min_lower <- avg_min_rad - sd(moving_window_102$Min_Rad)
  min_upper <- avg_min_rad + sd(moving_window_102$Min_Rad)
  
  max_lower <- avg_max_rad - sd(moving_window_102$Max_Rad)
  max_upper <- avg_max_rad + sd(moving_window_102$Max_Rad)
  
  # 4. Daten für Schattenbänder erstellen
  ribbon_data <- data.frame(
    angle = seq(0, 2*pi, length.out = 100)
  )
  
  # Da Schatten um Mittelwerte, definieren wir die "Radius" der Bänder als z.B. 0.8 bis 1.0 für Max,
  # und 0.6 bis 0.8 für Min (nur optisch, nicht radial die Zeit, Zeit ist Winkel!)
  
  # Wir nutzen Schatten als Bänder am Rand, die Variabilität zeigen:
  ribbon_data <- ribbon_data %>%
    mutate(
      ymin_min = 0.6,
      ymax_min = 0.8,
      ymin_max = 0.8,
      ymax_max = 1.0
    )
  
  # 5. Pointer (Zeiger) mit Radius 0 bis 1 am Mittelwert-Winkel
  pointers <- data.frame(
    angle = c(avg_min_rad, avg_max_rad),
    label = c("Avg Min", "Avg Max"),
    color = c("blue", "red")
  )
  
  # 6. Plot
  ggplot() +
    # Schattenbänder (Bänder am Rand für Variabilität)
    geom_ribbon(data = ribbon_data,
                aes(x = angle, ymin = ymin_min, ymax = ymax_min),
                fill = "blue", alpha = 0.3) +
    geom_ribbon(data = ribbon_data,
                aes(x = angle, ymin = ymin_max, ymax = ymax_max),
                fill = "red", alpha = 0.3) +
    
    # Zeiger als Linien vom Zentrum zum Rand (r = 1) an Mittelwert-Winkeln
    geom_segment(data = pointers,
                 aes(x = angle, xend = angle, y = 0, yend = 1, color = label),
                 size = 1.5) +
    
    # Koordinatensystem polar (Uhrzeit von 0 bis 24h)
    coord_polar(start = -pi/2) +
    
    # x-Achse als Uhrzeit in Stunden 0 bis 24 mapped auf 0 bis 2pi
    scale_x_continuous(
      breaks = seq(0, 2*pi, length.out = 25),
      labels = 0:24,
      limits = c(0, 2*pi)
    ) +
    
    # Radius von 0 bis 1
    scale_y_continuous(limits = c(0, 1)) +
    
    # Farben für Zeiger
    scale_color_manual(values = c("Avg Min" = "blue", "Avg Max" = "red")) +
    
    labs(title = "Polarplot: Durchschnittliche Min/Max Zeiten mit Schwankungsbereich",
         x = "Uhrzeit (Stunden)",
         y = NULL,
         color = "Zeiger") +
    
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  
  
  
  
  
  # 1. Umrechnung der Zeiten in Radianten (0 bis 2pi)
  moving_window_102 <- moving_window_102 %>%
    mutate(
      Min_Rad = (Min_Time_Hours / 24) * 2 * pi,
      Max_Rad = (Max_Time_Hours / 24) * 2 * pi
    )
  
  # 2. Durchschnittliche Min und Max (Mittelwerte)
  avg_min_rad <- mean(moving_window_102$Min_Rad)
  avg_max_rad <- mean(moving_window_102$Max_Rad)
  
  # 3. Schwankungsbereich (z.B. +/- 1 Standardabweichung)
  min_lower <- avg_min_rad - sd(moving_window_102$Min_Rad)
  min_upper <- avg_min_rad + sd(moving_window_102$Min_Rad)
  
  max_lower <- avg_max_rad - sd(moving_window_102$Max_Rad)
  max_upper <- avg_max_rad + sd(moving_window_102$Max_Rad)
  
 
  
  # 5. Pointer (Zeiger) mit Radius 0 bis 1 am Mittelwert-Winkel
  pointers <- data.frame(
    angle = c(avg_min_rad, avg_max_rad),
    label = c("Avg Min", "Avg Max"),
    color = c("blue", "red")
  )
  
  # 6. Plot
  ggplot() +
   
    
    # Zeiger als Linien vom Zentrum zum Rand (r = 1) an Mittelwert-Winkeln
    geom_segment(data = pointers,
                 aes(x = angle, xend = angle, y = 0, yend = 1, color = label),
                 size = 1.5) +
    
    # Koordinatensystem polar (Uhrzeit von 0 bis 24h)
    coord_polar(start = 0) +
    
    # x-Achse als Uhrzeit in Stunden 0 bis 24 mapped auf 0 bis 2pi
    scale_x_continuous(
      breaks = seq(0, 2*pi, length.out = 25),
      labels = 0:24,
      limits = c(0, 2*pi)
    ) +
    
    # Radius von 0 bis 1
    scale_y_continuous(limits = c(0, 1)) +
    
    # Farben für Zeiger
    scale_color_manual(values = c("Avg Min" = "blue", "Avg Max" = "red")) +
    
    labs(title = "Polarplot: Durchschnittliche Min/Max Zeiten mit Schwankungsbereich",
         x = "Uhrzeit (Stunden)",
         y = NULL,
         color = "Zeiger") +
    
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  
  
  
  
  # Rechteck um Avg Max (±1 SD im Winkel, Radius von 0.95 bis 1)
  max_rect <- data.frame(
    xmin = max_lower,
    xmax = max_upper,
    ymin = 0.95,
    ymax = 1
  )
  
  ggplot() +
    # Rechteck als Unsicherheitsbereich um Max
    geom_rect(data = max_rect,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "pink", alpha = 0.3) +
    
    # Zeiger
    geom_segment(data = pointers,
                 aes(x = angle, xend = angle, y = 0, yend = 1, color = label),
                 size = 1.5) +
    
    coord_polar(start = 0) +  # 0 Uhr oben
    scale_x_continuous(
      breaks = seq(0, 2*pi, length.out = 25),
      labels = 0:24,
      limits = c(0, 2*pi)
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(values = c("Avg Min" = "blue", "Avg Max" = "pink")) +
    labs(title = "Polarplot mit Standardabweichungs-Bereich um Max-Zeit") +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
 
  
  
  
  
  # --- Vorbereitung für ID 102 ---
  moving_window_102 <- moving_window_102 %>%
    mutate(
      Min_Rad = (Min_Time_Hours / 24) * 2 * pi,
      Max_Rad = (Max_Time_Hours / 24) * 2 * pi
    )
  
  avg_min_rad_102 <- mean(moving_window_102$Min_Rad)
  avg_max_rad_102 <- mean(moving_window_102$Max_Rad)
  
  min_lower_102 <- avg_min_rad_102 - sd(moving_window_102$Min_Rad)
  min_upper_102 <- avg_min_rad_102 + sd(moving_window_102$Min_Rad)
  max_lower_102 <- avg_max_rad_102 - sd(moving_window_102$Max_Rad)
  max_upper_102 <- avg_max_rad_102 + sd(moving_window_102$Max_Rad)
  
  pointers_102 <- data.frame(
    angle = c(avg_min_rad_102, avg_max_rad_102),
    label = c("Avg Min 102", "Avg Max 102"),
    color = c("blue", "red")
  )
  
  rects_102 <- data.frame(
    xmin = c(min_lower_102, max_lower_102),
    xmax = c(min_upper_102, max_upper_102),
    ymin = 0.95,
    ymax = 1,
    label = c("Avg Min 102", "Avg Max 102")
  )
  
  # --- Vorbereitung für ID 104 ---
  moving_window_104 <- moving_window_104 %>%
    mutate(
      Min_Rad = (Min_Time_Hours / 24) * 2 * pi,
      Max_Rad = (Max_Time_Hours / 24) * 2 * pi
    )
  
  avg_min_rad_104 <- mean(moving_window_104$Min_Rad)
  avg_max_rad_104 <- mean(moving_window_104$Max_Rad)
  
  min_lower_104 <- avg_min_rad_104 - sd(moving_window_104$Min_Rad)
  min_upper_104 <- avg_min_rad_104 + sd(moving_window_104$Min_Rad)
  max_lower_104 <- avg_max_rad_104 - sd(moving_window_104$Max_Rad)
  max_upper_104 <- avg_max_rad_104 + sd(moving_window_104$Max_Rad)
  
  pointers_104 <- data.frame(
    angle = c(avg_min_rad_104, avg_max_rad_104),
    label = c("Avg Min 104", "Avg Max 104"),
    color = c("darkgreen", "orange")
  )
  
  rects_104 <- data.frame(
    xmin = c(min_lower_104, max_lower_104),
    xmax = c(min_upper_104, max_upper_104),
    ymin = 0.9,
    ymax = 0.95,
    label = c("Avg Min 104", "Avg Max 104")
  )
  
  # --- Plot kombinieren ---
  all_pointers <- bind_rows(pointers_102, pointers_104)
  all_rects <- bind_rows(rects_102, rects_104)
  
  ggplot() +
    # Rechtecke (Unsicherheitsbereiche)
    geom_rect(data = all_rects,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = label),
              alpha = 0.3, color = NA) +
    
    # Zeiger
    geom_segment(data = all_pointers,
                 aes(x = angle, xend = angle, y = 0, yend = 1, color = label),
                 size = 1.2) +
    
    # Polar-Koordinatensystem (0 Uhr oben)
    coord_polar(start = 0) +
    
    scale_x_continuous(
      breaks = seq(0, 2*pi, length.out = 25),
      labels = 0:24,
      limits = c(0, 2*pi)
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    
    scale_color_manual(values = c(
      "Avg Min 102" = "blue",
      "Avg Max 102" = "red",
      "Avg Min 104" = "darkgreen",
      "Avg Max 104" = "orange"
    )) +
    scale_fill_manual(values = c(
      "Avg Min 102" = "blue",
      "Avg Max 102" = "red",
      "Avg Min 104" = "darkgreen",
      "Avg Max 104" = "orange"
    )) +
    
    labs(
      title = "Polarplot: Acrophase + SD für ID 102 & 104",
      x = "Uhrzeit (Stunden)",
      y = NULL,
      color = "Zeiger",
      fill = "Standardabweichung"
    ) +
    
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  
  # IDs und zugehörige DataFrames
  id_list <- c(102, 104, 107, 108, 109, 112)
  
  # Liste der DataFrames in gleicher Reihenfolge (z. B. vorher erstellt)
  moving_window_list <- list(
    moving_window_102,
    moving_window_104,
    moving_window_107,
    moving_window_108,
    moving_window_109,
    moving_window_112
  )
  
  # Füge die ID zu jedem DataFrame hinzu
  moving_window_data <- Map(function(df, id) {
    df$ID <- id
    return(df)
  }, moving_window_list, id_list)
  
  # Binde alles zusammen
  moving_window_data <- bind_rows(moving_window_data)
  
  
  # Rad-Winkel berechnen
  moving_window_data <- moving_window_data %>%
    mutate(Max_Rad = (Max_Time_Hours / 24) * 2 * pi)
  
  # Datenrahmen für Pointer und Rechtecke initialisieren
  all_pointers <- data.frame()
  all_rects <- data.frame()
  
  # Schleife über alle IDs
  for (id in id_list) {
    df_id <- moving_window_data %>% filter(ID == id)
    
    avg_max <- mean(df_id$Max_Rad, na.rm = TRUE)
    sd_max <- sd(df_id$Max_Rad, na.rm = TRUE)
    
    rect_ymin <- 1 - 0.05 * (which(id_list == id))  # leicht gestaffelt pro ID
    rect_ymax <- rect_ymin + 0.05
    
    all_pointers <- bind_rows(all_pointers, data.frame(
      angle = avg_max,
      label = paste0("Avg Max ", id)
    ))
    
    all_rects <- bind_rows(all_rects, data.frame(
      xmin = avg_max - sd_max,
      xmax = avg_max + sd_max,
      ymin = rect_ymin,
      ymax = rect_ymax,
      label = paste0("Avg Max ", id)
    ))
  }
  
  
  library(stringr)
  
  all_pointers <- all_pointers %>%
    mutate(ID = str_extract(label, "\\d+"))
  
  all_rects <- all_rects %>%
    mutate(ID = str_extract(label, "\\d+"))
  
  
  # Plot
  ggplot() +
    geom_rect(data = all_rects,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ID, color = ID),
              alpha = 0.3) +
    geom_segment(data = all_pointers,
                 aes(x = angle, xend = angle, y = 0, yend = 1, color = ID),
                 linewidth = 1.2) +
    coord_polar(start = 0) +
    scale_x_continuous(
      breaks = seq(0, 2*pi, length.out = 25),
      labels = 0:24,
      limits = c(0, 2*pi)
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Acrophase Maxima + SD for all IDs",
      x = "TIme of Day",
      y = NULL,
      color = "Pointer",
      fill = "Standard Deviation"
    ) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  
  
  library(dplyr)
  library(ggplot2)
  library(stringr)
  
  
  id_order <- sort(unique(moving_window_data$ID))
  # --- Berechnungen für alle IDs & Seasons ---
  season_summary <- moving_window_data %>%
    group_by(ID, Season) %>%
    summarise(
      avg_max_rad = mean((Max_Time_Hours / 24) * 2 * pi, na.rm = TRUE),
      sd_max_rad = sd((Max_Time_Hours / 24) * 2 * pi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      angle = avg_max_rad,
      xmin = avg_max_rad - sd_max_rad,
      xmax = avg_max_rad + sd_max_rad,
      ymin = 0.95,
      ymax = 1
    )
  
  # --- Zeiger-Daten ---
  pointers_seasons <- season_summary %>%
    select(ID, Season, angle)
  
  # --- Rechteck-Daten ---
  rects_seasons <- season_summary %>%
    select(ID, Season, xmin, xmax, ymin, ymax)
  
  # --- Plot ---
  ggplot() +
    geom_rect(data = rects_seasons,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ID),
              alpha = 0.3, color = NA) +
    geom_segment(data = pointers_seasons,
                 aes(x = angle, xend = angle, y = 0, yend = 1, color = ID),
                 linewidth = 1.2) +
    coord_polar(start = 0) +
    scale_x_continuous(
      breaks = seq(0, 2 * pi, length.out = 25),
      labels = 0:24,
      limits = c(0, 2 * pi)
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Polarplot: Acrophase Maxima + SD nach Season",
      x = "Uhrzeit (Stunden)",
      y = NULL,
      color = "ID",
      fill = "ID"
    ) +
    facet_wrap(~Season) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  
  
  library(dplyr)
  library(ggplot2)
  
  # --- IDs in fester Reihenfolge für konsistente Farben definieren ---
  id_order <- sort(unique(moving_window_data$ID))
  
  # --- Zusammenfassung: Durchschnitt & SD des Maxima pro Season & ID ---
  season_summary <- moving_window_data %>%
    group_by(ID, Season) %>%
    summarise(
      avg_max_rad = mean((Max_Time_Hours / 24) * 2 * pi, na.rm = TRUE),
      sd_max_rad = sd((Max_Time_Hours / 24) * 2 * pi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      angle = avg_max_rad,
      xmin = avg_max_rad - sd_max_rad,
      xmax = avg_max_rad + sd_max_rad,
      ymin = 0.95,
      ymax = 1,
      ID = factor(ID, levels = id_order)  # ID als Faktor für konsistente Farben
    )
  
  # --- Zeiger-Daten extrahieren ---
  pointers_seasons <- season_summary %>%
    select(ID, Season, angle)
  
  # --- Rechteck-Daten extrahieren ---
  rects_seasons <- season_summary %>%
    select(ID, Season, xmin, xmax, ymin, ymax)
  
  # --- Plot erstellen ---
  ggplot() +
    geom_rect(data = rects_seasons,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = ID),
              alpha = 0.3, color = NA) +
    geom_segment(data = pointers_seasons,
                 aes(x = angle, xend = angle, y = 0, yend = 1, color = ID),
                 linewidth = 1.2) +
    coord_polar(start = 0) +
    scale_x_continuous(
      breaks = seq(0, 2 * pi, length.out = 25),
      labels = 0:24,
      limits = c(0, 2 * pi)
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Polarplot: Acrophase Maxima + SD nach Season",
      x = "Uhrzeit (Stunden)",
      y = NULL,
      color = "ID",
      fill = "ID"
    ) +
    facet_wrap(~Season) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  

  
  library(dplyr)
  library(ggplot2)
  
  # --- Feste Reihenfolge der IDs für Farben & vertikale Platzierung ---
  id_list <- sort(unique(moving_window_data$ID))
  
  # --- Zusammenfassung: Durchschnitt & SD des Maxima pro Season & ID ---
  season_summary <- moving_window_data %>%
    group_by(ID, Season) %>%
    summarise(
      avg_max_rad = mean((Max_Time_Hours / 24) * 2 * pi, na.rm = TRUE),
      sd_max_rad = sd((Max_Time_Hours / 24) * 2 * pi, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      angle = avg_max_rad,
      xmin = avg_max_rad - sd_max_rad,
      xmax = avg_max_rad + sd_max_rad,
      ID = factor(ID, levels = id_list),
      # dynamische vertikale Positionen, um Überlappung zu vermeiden
      rect_ymin = 1 - 0.05 * (as.numeric(ID) - 1),
      rect_ymax = rect_ymin + 0.05
    )
  
  # --- Zeiger-Daten extrahieren ---
  pointers_seasons <- season_summary %>%
    select(ID, Season, angle)
  
  # --- Rechteck-Daten extrahieren ---
  rects_seasons <- season_summary %>%
    select(ID, Season, xmin, xmax, rect_ymin, rect_ymax)
  
  # --- Plot erstellen ---
  ggplot() +
    geom_rect(data = rects_seasons,
              aes(xmin = xmin, xmax = xmax, ymin = rect_ymin, ymax = rect_ymax, fill = ID),
              alpha = 0.3, color = NA) +
    geom_segment(data = pointers_seasons,
                 aes(x = angle, xend = angle, y = 0, yend = 1, color = ID),
                 linewidth = 1.2) +
    coord_polar(start = 0) +
    scale_x_continuous(
      breaks = seq(0, 2 * pi, length.out = 25),
      labels = 0:24,
      limits = c(0, 2 * pi)
    ) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      title = "Polarplot: Acrophase Maxima + SD nach Season",
      x = "Uhrzeit (Stunden)",
      y = NULL,
      color = "ID",
      fill = "ID"
    ) +
    facet_wrap(~Season) +
    theme_minimal() +
    theme(
      axis.title.y = element_blank(),
      panel.grid.major = element_line(color = "grey80"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
 
  
  
  # --- Feste Reihenfolge der IDs für Farben & vertikale Platzierung ---
id_list <- sort(unique(moving_window_data$ID))

# --- Zusammenfassung: Durchschnitt & SD des Maxima pro Season & ID ---
season_summary <- moving_window_data %>%
  group_by(ID, Season) %>%
  summarise(
    avg_max_rad = mean((Max_Time_Hours / 24) * 2 * pi, na.rm = TRUE),
    sd_max_rad = sd((Max_Time_Hours / 24) * 2 * pi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    angle = avg_max_rad,
    xmin = avg_max_rad - sd_max_rad,
    xmax = avg_max_rad + sd_max_rad,
    ID = factor(ID, levels = id_list),
    # dynamische vertikale Positionen, um Überlappung zu vermeiden
    rect_ymin = 0.95 - 0.05 * (as.numeric(ID) - 1),
    rect_ymax = rect_ymin + 0.05
  )

# --- Zeiger-Daten extrahieren ---
pointers_seasons <- season_summary %>%
  select(ID, Season, angle)

# --- Rechteck-Daten extrahieren ---
rects_seasons <- season_summary %>%
  select(ID, Season, xmin, xmax, rect_ymin, rect_ymax)

# --- Plot erstellen ---
ggplot() +
  geom_rect(data = rects_seasons,
            aes(xmin = xmin, xmax = xmax, ymin = rect_ymin, ymax = rect_ymax, fill = ID),
            alpha = 0.3, color = NA) +
  geom_segment(data = pointers_seasons,
               aes(x = angle, xend = angle, y = 0, yend = 1, color = ID),
               linewidth = 1.2) +
  coord_polar(start = 0) +
  scale_x_continuous(
    breaks = seq(0, 2 * pi * 23/24, length.out = 24),
    labels = 0:23,
    limits = c(0, 2 * pi)
  ) +
  scale_y_continuous(limits = c(0, 1.05), labels = NULL) +
  labs(
    title = "Acrophase + SD per Season",
    x = "Time of Day",
    y = NULL,
    color = "ID",
    fill = "ID"
  ) +
  facet_wrap(~Season) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
  
 

# IDs mit n ergänzen
id_labels <- season_summary %>%
  group_by(ID) %>%
  summarise(n = first(n)) %>%     # n-Werte pro ID
  mutate(ID_label = paste0(ID, " (n = ", n, ")"))

# ID-Labels in den Plot-Daten ersetzen
rects_seasons <- rects_seasons %>%
  left_join(id_labels, by = "ID") %>%
  mutate(ID = ID_label)

pointers_seasons <- pointers_seasons %>%
  left_join(id_labels, by = "ID") %>%
  mutate(ID = ID_label)

# Plot mit aktualisierten Labels
ggplot() +
  geom_rect(data = rects_seasons,
            aes(xmin = xmin, xmax = xmax, ymin = rect_ymin, ymax = rect_ymax, fill = ID),
            alpha = 0.3, color = NA) +
  geom_segment(data = pointers_seasons,
               aes(x = angle, xend = angle, y = 0, yend = 1, color = ID),
               linewidth = 1.2) +
  coord_polar(start = 0) +
  scale_x_continuous(
    breaks = seq(0, 2 * pi * 23/24, length.out = 24),
    labels = 0:23,
    limits = c(0, 2 * pi)
  ) +
  scale_y_continuous(limits = c(0, 1.05), labels = NULL) +
  labs(
    title = "Acrophase + SD per Season",
    x = "Time of Day",
    y = NULL,
    color = "ID",
    fill = "ID"
  ) +
  facet_wrap(~Season) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

library(dplyr)
library(ggplot2)

# Beispiel: season_summary mit n (Anzahl pro ID und Season) wurde bereits erstellt

# Schritt 1: Erstelle eine Tabelle mit unique IDs und deren n-Werten
id_labels <- season_summary %>%
  group_by(ID) %>%
  summarise(n = first(n)) %>%
  mutate(ID_label = paste0(ID, " (n = ", n, ")"))

# Schritt 2: ID als Faktor mit neuen Labels im season_summary und den Plot-Daten setzen
season_summary <- season_summary %>%
  mutate(ID = factor(ID, levels = id_labels$ID, labels = id_labels$ID_label))

rects_seasons <- season_summary %>%
  select(ID, Season, xmin, xmax, rect_ymin, rect_ymax)

pointers_seasons <- season_summary %>%
  select(ID, Season, angle)

# Schritt 3: Plot mit den neuen Labels
ggplot() +
  geom_rect(data = rects_seasons,
            aes(xmin = xmin, xmax = xmax, ymin = rect_ymin, ymax = rect_ymax, fill = ID),
            alpha = 0.3, color = NA) +
  geom_segment(data = pointers_seasons,
               aes(x = angle, xend = angle, y = 0, yend = 1, color = ID),
               linewidth = 1.2) +
  coord_polar(start = 0) +
  scale_x_continuous(
    breaks = seq(0, 2 * pi * 23/24, length.out = 24),
    labels = 0:23,
    limits = c(0, 2 * pi)
  ) +
  scale_y_continuous(limits = c(0, 1.05), labels = NULL) +
  labs(
    title = "Acrophase + SD per Season",
    x = "Time of Day",
    y = NULL,
    color = "ID",
    fill = "ID"
  ) +
  facet_wrap(~Season) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

  
  #### Tag und Nacht ####
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  # Schritt 1: Datum extrahieren
  cleaned_summary <- cleaned_data_deduplicated %>%
    mutate(
      Date = as.Date(Timestamp)  # Tagesgenaues Datum
    ) %>%
    group_by(ID, Time_of_Day, Date) %>%
    summarise(
      mean_glucose = mean(`Glucose (mg/dl)`, na.rm = TRUE),
      sd_glucose   = sd(`Glucose (mg/dl)`, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Schritt 2: Plot erstellen
  ggplot(cleaned_summary, aes(x = Date, y = mean_glucose, color = Time_of_Day, fill = Time_of_Day)) +
    geom_smooth(size = 0.5) +
    geom_ribbon(aes(ymin = mean_glucose - sd_glucose, ymax = mean_glucose + sd_glucose), alpha = 0.2, color = NA) +
    facet_wrap(~ ID, scales = "free_x") +
    scale_color_manual(values = c("Day" = "orange3", "Night" = "midnightblue")) +
    scale_fill_manual(values = c("Day" = "orange3", "Night" = "midnightblue")) +
    labs(
      title = "Daily Mean Glucose with Standard Deviation (Day vs. Night)",
      x = "Date",
      y = "Mean Glucose (mg/dl)",
      color = "Time of Day",
      fill = "Time of Day"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  
  write.csv(cleaned_data_deduplicated, "cleaned_data_deduplicated.csv", row.names = FALSE)
#### nacht ohne durchschnitt ####
  
  ggplot(night_data, aes(x = Hour_Factor, y = `Glucose (mg/dl)`, group = ID, color = ID)) +
    geom_jitter(width = 0.2, alpha = 0.3, size = 1) +  # Streuung, um Überlappung zu reduzieren
    geom_smooth(method = "loess", se = FALSE, size = 1) +  # Glättung pro ID
    labs(
      title = "Nighttime Glucose per Hour (Raw Data)",
      subtitle = "Individual glucose values between 22:00 and 06:59",
      x = "Hour of Night",
      y = "Glucose (mg/dl)",
      color = "ID"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold")
    )
  
  ggplot(night_data, aes(x = Minute5, y = `Glucose (mg/dl)`, color = ID, group = ID)) +
    geom_line(alpha = 0.4) +
    labs(
      title = "Glucoseverlauf nachts in 5-Minuten-Schritten",
      x = "Zeit (5-Minuten-Bins)",
      y = "Glucose (mg/dl)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(night_data2, aes(x = Minute5_Factor, y = `Glucose (mg/dl)`, color = ID, group = ID)) +
    geom_line(alpha = 0.4) +
    labs(
      title = "Glucoseverlauf nachts in 5-Minuten-Schritten",
      x = "Zeit (5-Minuten-Bins)",
      y = "Glucose (mg/dl)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ggplot(night_data2, aes(x = Minute5, y = `Glucose (mg/dl)`, color = ID)) +
    geom_line() +
    scale_x_datetime(
      date_breaks = "1 month",        # Beschriftung alle 60 Minuten
      date_labels = "%b %Y"          # Format der Uhrzeit
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
 #### seasons ####
  seasons <- function(date) {
    m <- as.numeric(format(date, "%m"))
    if (m %in% c(12, 1, 2)) {
      return("Winter")
    } else if (m %in% c(3, 4, 5)) {
      return("Spring")
    } else if (m %in% c(6, 7, 8)) {
      return("Summer")
    } else {
      return("Autumn")
    }
  }
  
  
  library(dplyr)
  
  amplitude_means_102 <- moving_window_102 %>%
    group_by(Season) %>%
    summarise(
      mean_amp = mean(Amp, na.rm = TRUE),
      sd_amp = sd(Amp, na.rm = TRUE),
      n = n()
    )
  
  
  anova_result <- aov(Amp ~ Season, data = moving_window_102)
  
  # Residuen extrahieren
  residuen <- residuals(anova_result)
  
  # Shapiro-Wilk-Test
  shapiro.test(residuen)
  
  anova_result <- aov(Amp ~ Season, data = moving_window_104)
  
  # Residuen extrahieren
  residuen <- residuals(anova_result)
  
  # Shapiro-Wilk-Test
  shapiro.test(residuen)
  
  
  
  library(dplyr)
  
  # Add identifier columns and bind all together
  means_cos_fit <- bind_rows(
    amplitude_means_102 %>% mutate(ID = "102"),
    amplitude_means_104 %>% mutate(ID = "104"),
    amplitude_means_107 %>% mutate(ID = "107"),
    amplitude_means_108 %>% mutate(ID = "108"),
    amplitude_means_109 %>% mutate(ID = "109"),
    amplitude_means_112 %>% mutate(ID = "112")
  )
  
  
  install.packages("lme4")        # If not already installed
  install.packages("lmerTest")    # Optional: for p-values
  
  library(lme4)
  library(lmerTest)
  
  # Model: Mean amplitude by Season with random intercept for ID
  model_amp <- lmer(mean_amp ~ Season + (1 | ID), data = means_cos_fit)
  
  # Summary with fixed effect estimates and significance
  summary(model_amp)
  
  library(ggplot2)
  
  ggplot(means_cos_fit, aes(x = Season, y = mean_amp)) +
    geom_boxplot(fill = "lightblue", alpha = 0.6) +
    geom_jitter(width = 0.2, alpha = 0.5, color = "black") +
    theme_minimal() +
    labs(title = "Mean Amplitude by Season",
         x = "Season",
         y = "Mean Amplitude") +
    theme(plot.title = element_text(hjust = 0.5))
  
  
  
  #### stacked chart ####
  library(dplyr)
  library(ggplot2)
  library(lubridate)
  
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  
 
  
  stacked_chart <- cleaned_data_deduplicated %>%
    filter(!is.na(Glucose_Category)) %>%
    mutate(
      Time = floor_date(Timestamp, "5 minutes"),
      TimeOnly = format(Time, "%H:%M:%S")
    ) %>%
    count(ID, TimeOnly, Glucose_Category) %>%   # Häufigkeiten berechnen
    group_by(ID, TimeOnly) %>%
    mutate(percentage = n / sum(n)) %>%
    ungroup() %>%
    complete(ID, TimeOnly, Glucose_Category, fill = list(n = 0, percentage = 0))
  
  stacked_chart <- stacked_chart %>%
    mutate(
      TimeOnly = factor(TimeOnly, levels = sort(unique(TimeOnly))),
      Glucose_Category = factor(Glucose_Category, levels = c("Very Low", "Low", "Normal", "High"))
    )

  stacked_chart %>% 
    filter(ID == "102") %>%
    mutate(TimeOnly = as.POSIXct(TimeOnly, format = "%H:%M:%S")) %>%
    ggplot(aes(x = TimeOnly, y = percentage, fill = Glucose_Category)) +
    geom_area() +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

  
  stacked_chart <- cleaned_data_deduplicated %>%
    filter(!is.na(Glucose_Category)) %>%
    mutate(
      Time = floor_date(Timestamp, "5 minutes"),
      TimeOnly = format(Time, "%H:%M:%S"),
      # POSIXct with reference date
      TimeOnly_POSIX = as.POSIXct(TimeOnly, format = "%H:%M:%S", tz = "UTC")
    ) %>%
    count(ID, TimeOnly_POSIX, Glucose_Category) %>%
    group_by(ID, TimeOnly_POSIX) %>%
    mutate(percentage = n / sum(n)) %>%
    ungroup() %>%
    complete(ID, TimeOnly_POSIX, Glucose_Category, fill = list(n = 0, percentage = 0)) %>%
    mutate(
      Glucose_Category = factor(Glucose_Category, levels = c("High", "Normal", "Low", "Very Low"))
    )
  ggplot(stacked_chart %>% filter(ID == "102Ecosleep"), 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_area() +
    scale_x_datetime(date_labels = "%H:%M") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_area() +
    scale_x_datetime(date_labels = "%H:%M") +
    facet_wrap(~ ID, ncol = 2) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  
  str(stacked_chart$percentage)
  
  library(ggplot2)

  
  
  ggplot(stacked_chart, aes(x = TimeOnly, y = percentage, fill = Glucose_Category)) +
    geom_area(alpha = 0.6, size = 0.3, colour = "black") +
    facet_wrap(~ ID) +
    labs(
      title = "Glucose-Kategorien über den Nachtverlauf (Stacked Area Chart)",
      x = "Uhrzeit",
      y = "Anteil",
      fill = "Kategorie"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold")
    )
  
  library(ggplot2)
  library(dplyr)
  
  # Filter nur für ID 102 
  stacked_chart_102 <- stacked_chart %>%
    filter(ID == "102Ecosleep")
  
  
  stacked_chart_102 <- stacked_chart %>%
    filter(ID == "102Ecosleep") %>%
    complete(
      TimeOnly = unique(TimeOnly),
      Glucose_Category,
      fill = list(n = 0, percentage = 0)
    )
  
  ggplot(stacked_chart_102, aes(x = TimeOnly, y = percentage, fill = Glucose_Category)) +
    geom_area(alpha = 0.6, size = 0.3, colour = "black") +
    labs(
      title = "Stacked Area Chart für ID 102",
      x = "Uhrzeit",
      y = "Anteil",
      fill = "Glucose Kategorie"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_area() +
    scale_x_datetime(date_labels = "%H:%M") +
    scale_y_log10()+
    facet_wrap(~ ID, ncol = 2) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  
  stacked_chart <- stacked_chart %>%
    mutate(percentage = ifelse(percentage == 0, 1e-6, percentage))
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_area() +
    scale_x_datetime(date_labels = "%H:%M") +
    scale_y_log10() +
    facet_wrap(~ ID, ncol = 2) +  
    labs(y = "Proportion (log10)", x = "Time of Day") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ggplot(stacked_chart %>% filter(ID == "102"),
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_col(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(y = "Anteil", x = "Uhrzeit") +
    theme_minimal()
  
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage)) +
    geom_area(fill = "steelblue") +
    facet_grid(Glucose_Category ~ ID) +  # Facet pro Kategorie & ID
    scale_x_datetime(date_labels = "%H:%M") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = "Prozentualer Anteil", x = "Uhrzeit")
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_col(position = "fill", width = 300) +  # width in seconds
    scale_y_continuous(labels = scales::percent) +
    scale_x_datetime(date_labels = "%H:%M") +
    facet_wrap(~ ID, ncol = 2) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = "Anteil", x = "Uhrzeit")
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_area() +
    scale_x_datetime(date_labels = "%H:%M") +
    scale_y_log10(limits = c(1e-4, 1)) +  # oder mit scale_y_continuous(trans = "log10")
    facet_wrap(~ ID, ncol = 2) +  
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(y = "Log10 Anteil", x = "Uhrzeit")
  
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_area() +
    scale_x_datetime(date_labels = "%H:%M") +
    scale_y_log10(
      breaks = c(0.001, 0.01, 0.1, 1),
      limits = c(0.001, 1),
      labels = scales::label_percent(scale = 1)
    ) +
    facet_wrap(~ ID, ncol = 2) +  
    labs(y = "Proportion (log10)", x = "Time of Day") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_area() +
    scale_x_datetime(date_labels = "%H:%M") +
    scale_y_continuous()
  +
    facet_wrap(~ ID, ncol = 2) +  
    labs(y = "Proportion", x = "Time of Day") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_area(alpha = 0.6) +
    scale_x_datetime(date_labels = "%H:%M") +
    scale_y_continuous(
      limits = c(0, 0.1),
      labels = scales::percent_format(accuracy = 1)
    ) +
    facet_wrap(~ ID, ncol = 2) +  
    labs(y = "Proportion", x = "Time of Day") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggplot(stacked_chart, 
         aes(x = TimeOnly_POSIX, y = percentage, fill = Glucose_Category)) +
    geom_area() +
    scale_x_datetime(date_labels = "%H:%M") +
    coord_cartesian(ylim = c(0, 0.25)) +  # Nur bis 25% anzeigen
    facet_wrap(~ ID, ncol = 2) +  
    labs(y = "Proportion", x = "Time of Day") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  
  
  # Anzahl der NA-Werte in der Spalte "Glucose levels (mg/dl)" ermitteln
  sum_na <- sum(is.na(combined_data[["Glucose levels (mg/dl)"]]))
  
  # Ergebnis ausgeben
  if (sum_na > 0) {
    message(paste0("Es gibt ", sum_na, " NA-Werte in der Spalte 'Glucose levels (mg/dl)'"))
  } else {
    message("Keine NAs in der Spalte 'Glucose levels (mg/dl)'")
  }
  
  # Anzahl der nicht-NA-Werte in der Spalte "Scan glucose levels (mg/dl)"
  anzahl_scan_werte <- sum(!is.na(combined_data[["Scan glucose levels (mg/dl)"]]))
  
  # Ergebnis anzeigen
  message(paste0("Anzahl der vorhandenen Werte in 'Scan glucose levels (mg/dl)': ", anzahl_scan_werte))
  
#### moving window
  
  library(cosinor)
  library(lubridate)
  library(dplyr)
  
  # Setup
  window_length <- days(3)
  step_size <- days(1)
  start_time_mv_102 <- min(glucose_fit_102_data$datetime)
  end_time_mv_102 <- max(glucose_fit_102_data$datetime)
  
  # Nur für die ersten zwei Fenster
  for (i in 1:2) {
    current_start <- start_time_mv_102 + (i - 1) * step_size
    current_end <- current_start + window_length
    
    window_data <- glucose_fit_102_data %>%
      filter(datetime >= current_start & datetime < current_end)
    
    if (nrow(window_data) < 100) {
      cat("Zu wenige Daten im Fenster", i, "\n")
      next
    }
    
    # Modell fitten
    fit <- tryCatch({
      cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = window_data, period = 24)
    }, error = function(e) {
      cat("Fehler im Fit für Fenster", i, ":", e$message, "\n")
      return(NA)
    })
    
    if (inherits(fit, "cosinor.lm")) {
      cat("\n--- Fenster", i, "---\n")
      print(summary(fit))
      
      cat("\n>> Transformed Coefficients:\n")
      print(coef(fit, transformed = TRUE))
      
      # Optional: auch Originalkoeffizienten (Intercept, beta, alpha)
      cat("\n>> Raw Coefficients:\n")
      print(coef(fit, transformed = FALSE))
      
    } else {
      cat("Fit fehlgeschlagen für Fenster", i, "\n")
    }
  }
  
  
  
  colnames(night_avg_5)
  
  # create reordered factor for plotting
  night_avg_5 <- night_avg_5 %>%
    mutate(
      Minute5_Factor = factor(Minute5, levels = levels_ordered),
      ID = str_remove(ID, "Ecosleep")
    )
  
  # cretae labels for x axis
  hour_labels <- format(seq.POSIXt(
    from = as.POSIXct("2024-01-01 22:00"),
    to = as.POSIXct("2024-01-02 06:00"),
    by = "1 hour"
  ), "%H:%M")
  
  # create one plot for all
  ggplot(night_avg_5, aes(x = Minute5_Factor, y = Avg_Glucose_5, group = ID, color = ID, fill = ID)) +
    geom_smooth(size = 0.8) +
    geom_line(size = 0.5) +
    geom_ribbon(aes(ymin = Avg_Glucose_5 - Sd_Glucose_5, ymax = Avg_Glucose_5 + Sd_Glucose_5), alpha = 0.1, color = NA)+
    scale_x_discrete(breaks = hour_labels) +
    labs(
      title = "Nighttime Glucose Trends per ID",
      subtitle = "Hours from 22:00 to 06:00: 5min bins",
      x = "Hour of Night",
      y = "Average Glucose (mg/dl)",
      color = "ID"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(face = "bold")
    )   
  
  charToRaw("107EcoSleep")
  stringi::stri_escape_unicode("107EcoSleep")
  
  print("107EcoSleep")
  cat("107EcoSleep")
  
  unique(night_data$ID)

  
  
  double_data <- cleaned_data_deduplicated %>%
    mutate(
      Date = as.Date(Timestamp),
      Hour = hour(Timestamp),
      Minute = minute(Timestamp),
      Time_of_Day = Hour + Minute / 60,
      ID_Label = case_when(
        ID == "102Ecosleep" ~ "102",
        ID == "104Ecosleep" ~ "104",
        ID == "107EcoSleep" ~ "107",
        ID == "108EcoSleep" ~ "108",
        ID == "109EcoSleep" ~ "109",
        ID == "112EcoSleep" ~ "112",
        TRUE ~ as.character(ID)
      )
    )
  
  # Duplicate data: original time + shifted by 24h
  double_data_long <- bind_rows(
    double_data %>% mutate(Double_Hour = Time_of_Day),
    double_data %>% mutate(Double_Hour = Time_of_Day + 24)
  )
  
  # Round down to full hour and keep the original 0–47 factor for correct time ordering
  double_avg <- double_data_long %>%
    mutate(Hour_Rounded = floor(Double_Hour)) %>%
    group_by(ID_Label, Hour_Rounded) %>%
    summarise(Avg_Glucose = mean(`Glucose (mg/dl)`, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Display_Hour = Hour_Rounded %% 24,
      Day = ifelse(Hour_Rounded < 24, "Day 1", "Day 2"),
      Hour_Label = paste0(Day, " – ", sprintf("%02d:00", Display_Hour)),
      Hour_Factor = factor(Hour_Label, levels = c(
        paste0("Day 1 – ", sprintf("%02d:00", 0:23)),
        paste0("Day 2 – ", sprintf("%02d:00", 0:23))
      ))
    )
  
#### cosinus ####
  head(moving_window_102, 2)
  
  library(ggplot2)
  
  first_two_starts <- moving_window_102$Window_Start[1:2]
  
  for (start_time in first_two_starts) {
    end_time <- start_time + days(3)
    
    window_data <- glucose_fit_102_data %>%
      filter(datetime >= start_time & datetime < end_time)
    
    fit <- tryCatch({
      cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = window_data, period = 24)
    }, error = function(e) NULL)
    
    if (!is.null(fit) && inherits(fit, "cosinor.lm")) {
      time_seq <- seq(min(window_data$dec.time), max(window_data$dec.time), length.out = 100)
      pred_df <- data.frame(dec.time = time_seq)
      pred_df$fit <- predict(fit, newdata = pred_df)
      
      p <- ggplot(window_data, aes(x = dec.time, y = `Glucose (mg/dl)`)) +
        geom_point(color = "blue") +
        geom_line(data = pred_df, aes(x = dec.time, y = fit), color = "red", size = 1) +
        labs(title = paste("Cosinor Fit für Fenster ab", start_time),
             x = "Zeit (dec.time)", y = "Glucose (mg/dl)") +
        theme_minimal()
      
      print(p)
    } else {
      message("Fit fehlgeschlagen für Fenster ab ", start_time)
    }
  }
  
 head(moving_window_102)
 
 
 
 library(cosinor)  # falls nicht geladen
 library(dplyr)
 library(lubridate)
 
 moving_window_102 <- data.frame(
   Window_Start = as.POSIXct(character()),
   Amp = numeric(),
   Acrophase_Rad = numeric(),
   Acrophase_Raw = numeric(),
   Acrophase_Raw_Hours = numeric(),
   Max_Time_Hours = numeric(),
   Min_Time_Hours = numeric(),
   Intercept = numeric(),
   stringsAsFactors = FALSE
 )
 
 start_time_mv_102 <- min(glucose_fit_102_data$datetime)
 end_time_mv_102   <- max(glucose_fit_102_data$datetime)
 
 window_length <- days(3)
 step_size <- days(1)
 current_start_102 <- start_time_mv_102
 
 while (current_start_102 + window_length <= end_time_mv_102) {
   
   current_end_102 <- current_start_102 + window_length
   
   window_data_102 <- glucose_fit_102_data %>%
     filter(datetime >= current_start_102 & datetime < current_end_102)
   
   if (nrow(window_data_102) < 400) {
     current_start_102 <- current_start_102 + step_size
     next
   }
   
   # Fit cosinor model
   fit_102 <- tryCatch({
     cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), data = window_data_102, period = 24)
   }, error = function(e) NA)
   
   if (inherits(fit_102, "cosinor.lm")) {
     
     coefs <- coef(fit_102, transformed = FALSE)
     
     if (any(is.na(coefs[c("(Intercept)", "time(dec.time)", "sin(time(dec.time))")]))) {
       # Überspringe dieses Fenster, weil ungültige Koeffizienten vorliegen
       current_start_102 <- current_start_102 + step_size
       next
     }
     
     intercept_102 <- coefs["(Intercept)"]
     beta_102      <- coefs["time(dec.time)"]
     gamma_102     <- coefs["sin(time(dec.time))"]
     
     # Amplitude
     amp_102 <- sqrt(beta_102^2 + gamma_102^2)
     
     # Acrophase in Radiant
     acr_rad_102 <- atan2(-gamma_102, beta_102)  # Minuszeichen beachten!
     
     # Raw Acrophase in Stunden
     acrophase_raw_102 <- (acr_rad_102 * 24) / (2 * pi)
     acrophase_norm_102 <- (acrophase_raw_102 + 24) %% 24
     
     # Max = acrophase_norm, Min = +12h versetzt
     max_time_102 <- acrophase_norm_102
     min_time_102 <- (acrophase_norm_102 + 12) %% 24
     
     # Save to dataframe
     moving_window_102 <- rbind(moving_window_102, data.frame(
       Window_Start = current_start_102,
       Amp = amp_102,
       Acrophase_Rad = acr_rad_102,
       Acrophase_Raw = acrophase_raw_102,
       Acrophase_Raw_Hours = acrophase_norm_102,
       Max_Time_Hours = max_time_102,
       Min_Time_Hours = min_time_102,
       Intercept = intercept_102
     ))
   }
   
   current_start_102 <- current_start_102 + step_size
 }
 
 print(coefs)
 
 
 
 
 summary(window_data_102$dec.time)
 any(is.na(window_data_102$dec.time))
 length(unique(window_data_102$dec.time))
 
 range(window_data_102$dec.time)
 length(unique(window_data_102$dec.time))
 
 
 cos_part <- cos(2 * pi * window_data_102$dec.time / 24)
 sin_part <- sin(2 * pi * window_data_102$dec.time / 24)
 
 cor(cos_part, sin_part)
 summary(cos_part)
 summary(sin_part)
 
 fit_test <- lm(`Glucose (mg/dl)` ~ cos(2*pi*dec.time/24) + sin(2*pi*dec.time/24), data = window_data_102)
 summary(fit_test)
 
 
 library(lubridate)
 library(dplyr)
 
 # Ergebnis-DataFrame vorbereiten
 moving_window_102_l <- data.frame(
   Window_Start = as.POSIXct(character()),
   Amp = numeric(),
   Acrophase_Rad = numeric(),
   Acrophase_Raw = numeric(),
   Acrophase_Raw_Hours = numeric(),
   Max_Time_Hours = numeric(),
   Min_Time_Hours = numeric(),
   Intercept = numeric(),
   stringsAsFactors = FALSE
 )
 
 # Zeitgrenzen setzen
 start_time_mv_102 <- min(glucose_fit_102_data$datetime)
 end_time_mv_102   <- max(glucose_fit_102_data$datetime)
 
 # Fensterlänge und Schrittweite
 window_length <- days(3)
 step_size <- days(1)
 current_start_102 <- start_time_mv_102
 
 # Sliding window loop
 while (current_start_102 + window_length <= end_time_mv_102) {
   
   current_end_102 <- current_start_102 + window_length
   
   # Zeitfenster filtern
   window_data_102_l <- glucose_fit_102_data %>%
     filter(datetime >= current_start_102 & datetime < current_end_102)
   
   # Datenmenge prüfen
   if (nrow(window_data_102) < 400) {
     current_start_102 <- current_start_102 + step_size
     next
   }
   
   # Cosinor-Komponenten berechnen
   dec_time <- window_data_102_l$dec.time
   window_data_102_l$cos_part <- cos(2 * pi * dec_time / 24)
   window_data_102_l$sin_part <- sin(2 * pi * dec_time / 24)
   
   # Fit per linearer Regression
   fit_test <- tryCatch({
     lm(`Glucose (mg/dl)` ~ cos_part + sin_part, data = window_data_102_l)
   }, error = function(e) NA)
   
   # Prüfen, ob erfolgreich
   if (!inherits(fit_test, "lm")) {
     current_start_102 <- current_start_102 + step_size
     next
   }
   
   coefs <- coef(fit_test)
   
   # Falls NA in den relevanten Koeffizienten, überspringen
   if (any(is.na(coefs[c("(Intercept)", "cos_part", "sin_part")]))) {
     current_start_102 <- current_start_102 + step_size
     next
   }
   
   # Koeffizienten extrahieren
   intercept_102_l <- coefs["(Intercept)"]
   beta_102      <- coefs["cos_part"]
   gamma_102     <- coefs["sin_part"]
   
   # Amplitude berechnen
   amp_102_l <- sqrt(beta_102^2 + gamma_102^2)
   
   # Acrophase (Radiant)
   acr_rad_102_l <- atan2(-gamma_102, beta_102)  # Wichtig: Minuszeichen
   
   # Acrophase (Stunden)
   acrophase_raw_102_l <- (acr_rad_102_l * 24) / (2 * pi)
   acrophase_norm_102_l <- (acrophase_raw_102_l + 24) %% 24
   
   # Max- und Min-Zeiten
   max_time_102_l <- acrophase_norm_102
   min_time_102_l <- (acrophase_norm_102 + 12) %% 24
   
   # Ergebnis speichern
   moving_window_102_l <- rbind(moving_window_102_l, data.frame(
     Window_Start = current_start_102,
     Amp = amp_102_l,
     Acrophase_Rad = acr_rad_102_l,
     Acrophase_Raw = acrophase_raw_102_l,
     Acrophase_Raw_Hours = acrophase_norm_102_l,
     Max_Time_Hours = max_time_102_l,
     Min_Time_Hours = min_time_102_l,
     Intercept = intercept_102_l
   ))
   
   # Weiter zum nächsten Fenster
   current_start_102 <- current_start_102 + step_size
 }
 
 
head(moving_window_data) 
head(moving_window_102)

write.csv(glucose_fit_102_data, "glucose_fit_102_data.csv", row.names = FALSE)

write.csv(moving_window_comparison, "moving_window_comparison.csv", row.names = FALSE)


## synthetischen Datensatz erstellen
library(dplyr)
library(ggplot2)
library(cosinor)

# 1. Parameter festlegen
set.seed(42)
start_time <- as.POSIXct("2024-01-01 00:00:00")
end_time <- start_time + days(7)
time_seq <- seq(from = start_time, to = end_time, by = "5 min")  # 5-Minuten-Intervalle

# 2. Zeit in Dezimalstunden umrechnen
decimal_time <- as.numeric(format(time_seq, "%H")) + as.numeric(format(time_seq, "%M")) / 60

# 3. Cosinuswerte berechnen
amplitude <- 20
mesor <- 100  # Mittelwert
period <- 24
phase_shift <- 0  # Maximum bei 12h → cos(-π) = -1 → muss angepasst werden (siehe unten)

# Achtung: cos() hat Maximum bei 0h → wir wollen es bei 12h
# Daher Phase-Verschiebung um 12h → +π (d.h. minuszeichen im Argument)
glucose <- mesor + amplitude * cos(2 * pi * decimal_time / period + pi)

# 4. Datensatz bauen
synthetic_data <- data.frame(
  datetime = time_seq,
  dec.time = decimal_time,
  glucose = glucose
)

# 5. Plot zum Prüfen
ggplot(synthetic_data, aes(x = datetime, y = glucose)) +
  geom_line(color = "blue") +
  labs(title = "Synthetischer Glukosedatensatz mit 24h-Cosinus",
       x = "Zeit", y = "Glukose (mg/dl)") +
  theme_minimal()


# 1. Manuelles Cos-Modell
synthetic_data$cos_part <- cos(2 * pi * synthetic_data$dec.time / 24)
synthetic_data$sin_part <- sin(2 * pi * synthetic_data$dec.time / 24)

fit_lm <- lm(glucose ~ cos_part + sin_part, data = synthetic_data)
coefs_lm <- coef(fit_lm)

# Amplitude & Acrophase manuell berechnen
amp_l <- sqrt(coefs_lm["cos_part"]^2 + coefs_lm["sin_part"]^2)
acr_l <- atan2(-coefs_lm["sin_part"], coefs_lm["cos_part"]) * 24 / (2 * pi)  # in Stunden
intercept_l <- coefs_lm["(Intercept)"]

# 2. Cosinor-Modell
fit_cos <- cosinor.lm(glucose ~ time(dec.time), data = synthetic_data, period = 24)
coefs_cos <- coef(fit_cos, transformed = TRUE)

# Ausgabe vergleichen
cat("===== Manuelles lm-Modell =====\n")
cat("Intercept (Mesor):", intercept_l, "\n")
cat("Amplitude:", amp_l, "\n")
cat("Acrophase (Stunden):", acr_l %% 24, "\n\n")

cat("===== Cosinor.lm Modell =====\n")
print(coefs_cos)



# Pakete laden
library(dplyr)
library(ggplot2)
library(cosinor)

# ---- 1. Synthetische Daten generieren (12-Stunden-Periode) ----
# Zeitreihe im Halbstundentakt über 7 Tage
datetime <- seq(
  from = as.POSIXct("2025-01-01 00:00:00"),
  to = as.POSIXct("2025-01-07 23:30:00"),
  by = "30 min"
)

# Dezimalzeit (für cosinor)
dec.time <- as.numeric(format(datetime, "%H")) + as.numeric(format(datetime, "%M")) / 60

# Cosinus mit Periode = 12h, max bei 18 Uhr, min bei 6 Uhr
mesor <- 100
amplitude <- 20
phase_shift <- -pi * 3  # max bei 18 Uhr

glucose <- mesor + amplitude * cos(2 * pi * dec.time / 12 + phase_shift)

# In DataFrame
synthetic_12h <- data.frame(datetime, dec.time, glucose)

# Plot zur Kontrolle
ggplot(synthetic_12h, aes(x = datetime, y = glucose)) +
  geom_line(color = "steelblue") +
  labs(title = "Synthetische Glukosekurve (Periode = 12 Stunden)",
       y = "Glukose", x = "Zeit") +
  theme_minimal()

# ---- 2. Manuelles lm-Modell ----
# Cosinus- und Sinusteile
synthetic_12h <- synthetic_12h %>%
  mutate(
    cos_part = cos(2 * pi * dec.time / 12),
    sin_part = sin(2 * pi * dec.time / 12)
  )

fit_lm <- lm(glucose ~ cos_part + sin_part, data = synthetic_12h)
coefs_lm <- coef(fit_lm)

intercept_l <- coefs_lm["(Intercept)"]
beta <- coefs_lm["cos_part"]
gamma <- coefs_lm["sin_part"]

amp_l <- sqrt(beta^2 + gamma^2)
acr_l_rad <- atan2(-gamma, beta)
acr_l_hr <- (acr_l_rad * 12 / (2 * pi)) %% 12  # auf 12h-Periode skalieren

# ---- 3. cosinor.lm-Modell ----
fit_cos <- cosinor.lm(glucose ~ time(dec.time), data = synthetic_12h, period = 12)
coefs_cos <- coef(fit_cos, transformed = TRUE)

# ---- 4. Ausgabe ----
cat("===== Manuelles lm-Modell =====\n")
cat("Intercept (MESOR):", round(intercept_l, 2), "\n")
cat("Amplitude:", round(amp_l, 2), "\n")
cat("Acrophase (Stunden):", round(acr_l_hr, 2), "\n\n")

cat("===== cosinor.lm-Modell =====\n")
print(round(coefs_cos, 2))

library(dplyr)
library(cosinor)

# ----- Parameter für synthetische Daten -----
set.seed(42)
n_hours <- 7 * 24
time_hours <- seq(0, n_hours - 1)

mesor <- 100
amplitude <- 20
period <- 12  # Doppelt so schnell: 12h-Periode
phase_shift <- -pi * 1.0  # Peak bei 6 Uhr

# Cosinusfunktion erzeugen (2 Zyklen pro Tag)
glucose <- mesor + amplitude * cos(2 * pi * time_hours / period + phase_shift)

# DataFrame erstellen
data <- data.frame(
  time = time_hours,
  dec.time = time_hours %% 24,
  glucose = glucose
)

# ---- Manuelles lm()-Modell ----
data$cos_part <- cos(2 * pi * data$dec.time / period)
data$sin_part <- sin(2 * pi * data$dec.time / period)

fit_lm <- lm(glucose ~ cos_part + sin_part, data = data)
coefs <- coef(fit_lm)

intercept_l <- coefs["(Intercept)"]
beta <- coefs["cos_part"]
gamma <- coefs["sin_part"]

amp_l <- sqrt(beta^2 + gamma^2)
acr_l_rad <- atan2(-gamma, beta) %% (2 * pi)

# ---- cosinor.lm-Modell ----
fit_cos <- cosinor.lm(glucose ~ time(dec.time), data = data, period = period)
coefs_cos <- coef(fit_cos, transformed = TRUE)

# ---- Ausgabe ----
cat("===== Manuelles lm()-Modell =====\n")
cat("Intercept (MESOR):", round(intercept_l, 2), "\n")
cat("Amplitude:", round(amp_l, 2), "\n")
cat("Acrophase (Rad):", round(acr_l_rad, 4), "\n\n")

cat("===== cosinor.lm-Modell =====\n")
cat("Intercept (MESOR):", round(coefs_cos["(Intercept)"], 2), "\n")
cat("Amplitude:", round(coefs_cos["amp"], 2), "\n")
cat("Acrophase (Rad):", round(coefs_cos["acr"], 4), "\n")




# Cosinor-Komponenten berechnen
glucose_fit_102_data$cos_part <- cos(2 * pi * glucose_fit_102_data$dec.time / 24)
glucose_fit_102_data$sin_part <- sin(2 * pi * glucose_fit_102_data$dec.time / 24)

# Lineares Modell (Trigonometrische Regression)
fit_linear <- tryCatch({
  lm(`Glucose (mg/dl)` ~ cos_part + sin_part, data = glucose_fit_102_data)
}, error = function(e) NA)

# Cosinor-Modell
fit_cosinor <- tryCatch({
  cosinor.lm(`Glucose (mg/dl)` ~ time(dec.time), 
             data = glucose_fit_102_data, 
             period = 24)
}, error = function(e) NA)

# Ergebnisse extrahieren und vergleichen
if (inherits(fit_linear, "lm")) {
  coefs_lin <- coef(fit_linear)
  intercept_L <- coefs_lin["(Intercept)"]
  beta_L     <- coefs_lin["cos_part"]
  gamma_L    <- coefs_lin["sin_part"]
  amp_L      <- sqrt(beta_L^2 + gamma_L^2)
  acr_L_rad  <- atan2(-gamma_L, beta_L)
}

if (inherits(fit_cosinor, "cosinor.lm")) {
  coefs_cos <- coef(fit_cosinor, transformed = TRUE)
  
  if (is.matrix(coefs_cos)) {
    amp_C       <- coefs_cos["amp", "estimate"]
    acr_C_rad   <- coefs_cos["acr", "estimate"]
    intercept_C <- coefs_cos["(Intercept)", "estimate"]
  } else {
    amp_C       <- coefs_cos["amp"]
    acr_C_rad   <- coefs_cos["acr"]
    intercept_C <- coefs_cos["(Intercept)"]
  }
}

# Ergebnisse anzeigen
cat("Vergleich über den gesamten Datensatz:\n")
cat("Linearer Fit:\n")
cat("  Amplitude: ", amp_L, "\n")
cat("  Acrophase (rad): ", acr_L_rad, "\n")
cat("  Intercept: ", intercept_L, "\n\n")

cat("Cosinor Fit:\n")
cat("  Amplitude: ", amp_C, "\n")
cat("  Acrophase (rad): ", acr_C_rad, "\n")
cat("  Intercept: ", intercept_C, "\n")


# Neue Zeitreihe für Vorhersagen (z. B. alle 10 Minuten)
pred_times <- seq(0, 24, length.out = 240)
pred_cos  <- cos(2 * pi * pred_times / 24)
pred_sin  <- sin(2 * pi * pred_times / 24)

# Vorhersagen aus dem linearen Modell
if (inherits(fit_linear, "lm")) {
  pred_linear <- coef(fit_linear)["(Intercept)"] +
    coef(fit_linear)["cos_part"] * pred_cos +
    coef(fit_linear)["sin_part"] * pred_sin
}

# Vorhersagen aus dem Cosinor-Modell
if (inherits(fit_cosinor, "cosinor.lm")) {
  pred_df <- data.frame(dec.time = pred_times)
  pred_cosinor <- predict(fit_cosinor, newdata = pred_df)
}

# Daten für ggplot vorbereiten
plot_df <- data.frame(
  Time = pred_times,
  Linear = pred_linear,
  Cosinor = pred_cosinor
)

# Originaldaten: Mittelwert je Stunde für Vergleich (optional geglättet)
glucose_avg <- glucose_fit_102_data %>%
  mutate(hour = dec.time %% 24) %>%
  group_by(hour) %>%
  summarise(Glucose = mean(`Glucose (mg/dl)`))

# Plot
ggplot() +
  geom_point(data = glucose_avg, aes(x = hour, y = Glucose), color = "gray40", size = 2, alpha = 0.6) +
  geom_line(data = plot_df, aes(x = Time, y = Linear), color = "blue", size = 1, linetype = "dashed") +
  geom_line(data = plot_df, aes(x = Time, y = Cosinor), color = "red", size = 1) +
  labs(title = "Vergleich: Linearer Fit vs. Cosinor-Fit",
       x = "Zeit des Tages (h)",
       y = "Glukose (mg/dl)",
       caption = "Punkte = gemittelte Originaldaten | Blau = linearer Fit | Rot = cosinor.lm") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))



# Filterdaten (optional, falls du z.B. vorher filterst)
window_data_102_l <- glucose_fit_102_data


  
# Kompletten Datensatz filtern (z.B. ID 102, falls relevant)
fit_data_102_l <- glucose_fit_102_data %>% 
  filter(ID == "102")  # falls keine ID, einfach weglassen

# Berechne cosinus und sinus Anteile
dec_time <- fit_data_102_l$dec.time
fit_data_102_l$cos_part <- cos(2 * pi * dec_time / 24)
fit_data_102_l$sin_part <- sin(2 * pi * dec_time / 24)

# Lineares Modell fitten
fit_test <- lm(`Glucose (mg/dl)` ~ cos_part + sin_part, data = fit_data_102_l)

# Koeffizienten extrahieren
coefs <- coef(fit_test)
intercept <- coefs["(Intercept)"]
beta <- coefs["cos_part"]
gamma <- coefs["sin_part"]

# Amplitude berechnen
amp <- sqrt(beta^2 + gamma^2)

# Acrophase (Radiant)
acr_rad <- atan2(-gamma, beta)  # Wichtig: Minuszeichen

# Acrophase in Stunden umrechnen und normieren (0-24h)
acrophase_raw <- (acr_rad * 24) / (2 * pi)
acrophase_norm <- (acrophase_raw + 24) %% 24

# Maximal- und Minimalzeit
max_time <- acrophase_norm
min_time <- (acrophase_norm + 12) %% 24

# Ergebnisse anzeigen
cat("Ergebnisse für den gesamten Datensatz:\n")
cat("  Amplitude: ", amp, "\n")
cat("  Acrophase (rad): ", acr_rad, "\n")
cat("  Acrophase (h): ", acrophase_norm, "\n")
cat("  Max. Zeit (h): ", max_time, "\n")
cat("  Min. Zeit (h): ", min_time, "\n")
cat("  Intercept: ", intercept, "\n")


library(dplyr)
glucose_fit_data <- cleaned_data_deduplicated %>%
  mutate(
    datetime = dmy_hm(DeviceTimestamp),  #  if format TT-MM-JJJJ HH:MM
    dec.time = hour(datetime) + minute(datetime) / 60  # time as decimal hour during the day
  )
# Alle eindeutigen IDs im Datensatz
all_ids <- unique(glucose_fit_data$ID)

# Leerer DataFrame für Ergebnisse
results_all_ids <- data.frame(
  ID = character(),
  Amp = numeric(),
  Acrophase_Rad = numeric(),
  Acrophase_Hours = numeric(),
  Intercept = numeric(),
  stringsAsFactors = FALSE
)

for (current_id in all_ids) {
  
  # Daten für aktuelle ID filtern
  data_subset <- glucose_fit_102_data %>%
    filter(ID == current_id)
  
  # Falls keine Daten, nächster Durchgang
  if (nrow(data_subset) == 0) next
  
  # cos und sin Anteile berechnen
  dec_time <- data_subset$dec.time
  data_subset$cos_part <- cos(2 * pi * dec_time / 24)
  data_subset$sin_part <- sin(2 * pi * dec_time / 24)
  
  # Fit mit lm
  fit <- tryCatch({
    lm(`Glucose (mg/dl)` ~ cos_part + sin_part, data = data_subset)
  }, error = function(e) NULL)
  
  # Falls Fit nicht klappt, nächsten Durchgang
  if (is.null(fit)) next
  
  coefs <- coef(fit)
  
  # Falls NAs in Koeffizienten, nächsten Durchgang
  if (any(is.na(coefs[c("(Intercept)", "cos_part", "sin_part")]))) next
  
  intercept <- coefs["(Intercept)"]
  beta <- coefs["cos_part"]
  gamma <- coefs["sin_part"]
  
  # Amplitude
  amp <- sqrt(beta^2 + gamma^2)
  
  # Acrophase in Radiant
  acr_rad <- atan2(-gamma, beta)
  
  # Acrophase in Stunden (0-24)
  acr_hours <- (acr_rad * 24) / (2 * pi)
  acr_hours <- (acr_hours + 24) %% 24
  
  # Ergebnis anhängen
  results_all_ids <- rbind(results_all_ids, data.frame(
    ID = current_id,
    Amp = amp,
    Acrophase_Rad = acr_rad,
    Acrophase_Hours = acr_hours,
    Intercept = intercept,
    stringsAsFactors = FALSE
  ))
}

# Ergebnisse anschauen
print(results_all_ids)





library(dplyr)
library(lubridate)

# Daten vorbereiten: datetime und dec.time berechnen
glucose_fit_data <- cleaned_data_deduplicated %>%
  mutate(
    datetime = dmy_hm(DeviceTimestamp),  # Format TT-MM-JJJJ HH:MM
    dec.time = hour(datetime) + minute(datetime) / 60  # Dezimalzeit im Tag
  )

# Alle eindeutigen IDs im Datensatz
all_ids <- unique(glucose_fit_data$ID)

# Leerer DataFrame für Ergebnisse mit korrekter Initialisierung
results_all_ids <- data.frame(
  ID = character(0),
  Amp = numeric(0),
  Acrophase_Rad = numeric(0),
  Acrophase_Hours = numeric(0),
  Intercept = numeric(0),
  stringsAsFactors = FALSE
)

for (current_id in all_ids) {
  
  # Daten für aktuelle ID filtern
  data_subset <- glucose_fit_data %>% 
    filter(ID == current_id)
  
  # Falls keine Daten, nächster Durchgang
  if (nrow(data_subset) == 0) next
  
  # cos und sin Anteile berechnen
  data_subset <- data_subset %>%
    mutate(
      cos_part = cos(2 * pi * dec.time / 24),
      sin_part = sin(2 * pi * dec.time / 24)
    )
  
  # Fit mit lm (tryCatch für Fehlerbehandlung)
  fit <- tryCatch({
    lm(`Glucose (mg/dl)` ~ cos_part + sin_part, data = data_subset)
  }, error = function(e) NULL)
  
  # Falls Fit nicht klappt, nächsten Durchgang
  if (is.null(fit)) next
  
  coefs <- coef(fit)
  
  # Falls NAs in Koeffizienten, nächsten Durchgang
  if (any(is.na(coefs[c("(Intercept)", "cos_part", "sin_part")]))) next
  
  intercept <- coefs["(Intercept)"]
  beta <- coefs["cos_part"]
  gamma <- coefs["sin_part"]
  
  # Amplitude
  amp <- sqrt(beta^2 + gamma^2)
  
  # Acrophase in Radiant
  acr_rad <- atan2(-gamma, beta)  # wichtig: Minuszeichen
  
  # Acrophase in Stunden (0-24)
  acr_hours <- (acr_rad * 24) / (2 * pi)
  acr_hours <- (acr_hours + 24) %% 24
  
  # Ergebnis anhängen
  results_all_ids <- rbind(results_all_ids, data.frame(
    ID = current_id,
    Amp = amp,
    Acrophase_Rad = acr_rad,
    Acrophase_Hours = acr_hours,
    Intercept = intercept,
    stringsAsFactors = FALSE
  ))
}

# Ergebnisse anschauen
print(results_all_ids)


coefs <- coef(fit)
beta <- coefs["cos_part"]
gamma <- coefs["sin_part"]

amp <- sqrt(beta^2 + gamma^2)
acr_max <- (atan2(-gamma, beta) * 24) / (2 * pi) %% 24
acr_min <- (atan2(gamma, beta) * 24) / (2 * pi) %% 24

cat("Max bei (nach atan2(-gamma, beta)):", acr_max, "\n")
cat("Min bei (nach atan2(gamma, beta)):", acr_min, "\n")


library(ggplot2)

# Beispieldaten für 0–24h
hour_seq <- seq(0, 24, length.out = 200)
cos_pred <- amp * cos(2 * pi * hour_seq / 24 + acr_rad) + intercept

plot_df <- data.frame(Hour = hour_seq, Glucose = cos_pred)

ggplot(plot_df, aes(x = Hour, y = Glucose)) +
  geom_line(color = "blue") +
  labs(title = "Cosinor-Modell für ID 102",
       x = "Stunde des Tages",
       y = "Modellierter Glukosewert (mg/dl)") +
  theme_minimal()


# ID auswählen
selected_id <- "109"

# Werte für diese ID extrahieren
id_params <- results_all_ids %>% filter(ID == selected_id)

amp <- id_params$Amp
acr_rad <- id_params$Acrophase_Rad
intercept <- id_params$Intercept

# Plot vorbereiten
hour_seq <- seq(0, 24, length.out = 200)
cos_pred <- amp * cos(2 * pi * hour_seq / 24 + acr_rad) + intercept

plot_df <- data.frame(Hour = hour_seq, Glucose = cos_pred)

ggplot(plot_df, aes(x = Hour, y = Glucose)) +
  geom_line(color = "blue") +
  labs(title = paste("Cosinor-Modell für ID", selected_id),
       x = "Stunde des Tages",
       y = "Modellierter Glukosewert (mg/dl)") +
  theme_minimal()


# ---- 1. Synthetische Daten generieren (12-Stunden-Periode) ----
datetime <- seq(
  from = as.POSIXct("2025-01-01 00:00:00"),
  to = as.POSIXct("2025-01-07 23:30:00"),
  by = "30 min"
)

# Dezimalzeit (für cosinor)
dec.time <- as.numeric(format(datetime, "%H")) + as.numeric(format(datetime, "%M")) / 60

# Cosinus mit Periode = 12h, max bei 18 Uhr, min bei 6 Uhr
mesor <- 100
amplitude <- 20
phase_shift <- -pi  # max bei 18 Uhr

glucose <- mesor + amplitude * cos(2 * pi * dec.time / 12 + phase_shift)

# In DataFrame
synthetic_12h <- data.frame(datetime, dec.time, glucose)

# Plot zur Kontrolle
library(ggplot2)
ggplot(synthetic_12h, aes(x = datetime, y = glucose)) +
  geom_line(color = "steelblue") +
  labs(title = "Synthetische Glukosekurve (Periode = 12 Stunden)",
       y = "Glukose", x = "Zeit") +
  theme_minimal()

# ---- 2. Manuelles lm-Modell ----
library(dplyr)
synthetic_12h <- synthetic_12h %>%
  mutate(
    cos_part = cos(2 * pi * dec.time / 12),
    sin_part = sin(2 * pi * dec.time / 12)
  )

fit_lm <- lm(glucose ~ cos_part + sin_part, data = synthetic_12h)
coefs_lm <- coef(fit_lm)

intercept_l <- coefs_lm["(Intercept)"]
beta <- coefs_lm["cos_part"]
gamma <- coefs_lm["sin_part"]

amp_l <- sqrt(beta^2 + gamma^2)
acr_l_rad <- atan2(-gamma, beta)
acr_l_hr <- (acr_l_rad * 12 / (2 * pi)) %% 12  # auf 12h-Periode skalieren

# ---- 3. cosinor.lm-Modell ----
library(cosinor)
fit_cos <- cosinor.lm(glucose ~ time(dec.time), data = synthetic_12h, period = 12)
coefs_cos <- coef(fit_cos, transformed = TRUE)

mesor_cos <- coefs_cos["mesor"]
amp_cos <- coefs_cos["amplitude"]
acr_cos_hr <- coefs_cos["acrophase (hr)"]

# ---- 4a. Min/Max for manual model ----
min_l <- intercept_l - amp_l
max_l <- intercept_l + amp_l

# ---- 4b. Min/Max for cosinor.lm model ----
min_cos <- mesor_cos - amp_cos
max_cos <- mesor_cos + amp_cos

# ---- 5. Ausgabe ----
cat("===== Manuelles lm-Modell =====\n")
cat("Intercept (MESOR):", round(intercept_l, 2), "\n")
cat("Amplitude:", round(amp_l, 2), "\n")
cat("Acrophase (Stunden):", round(acr_l_hr, 2), "\n")
cat("Minimale Glukose:", round(min_l, 2), "\n")
cat("Maximale Glukose:", round(max_l, 2), "\n\n")

cat("===== cosinor.lm-Modell =====\n")
cat("MESOR:", round(mesor_cos, 2), "\n")
cat("Amplitude:", round(amp_cos, 2), "\n")
cat("Acrophase (Stunden):", round(acr_cos_hr, 2), "\n")
cat("Minimale Glukose:", round(min_cos, 2), "\n")
cat("Maximale Glukose:", round(max_cos, 2), "\n")

# ---- 6. Uhrzeiten für Minimum und Maximum ----
# Manuelles Modell
max_time_l_hr <- acr_l_hr
min_time_l_hr <- (acr_l_hr + 6) %% 12  # Periode = 12h

# Umrechnung in Uhrzeit-Format (z. B. "06:00")
to_time <- function(hour) {
  sprintf("%02d:%02d", floor(hour), round((hour %% 1) * 60))
}

# cosinor.lm Modell
max_time_cos_hr <- acr_cos_hr
min_time_cos_hr <- (acr_cos_hr + 6) %% 12

# ---- 7. Zusätzliche Ausgabe ----
cat("===== Uhrzeiten für Min/Max (manuelles Modell) =====\n")
cat("Max. bei ca.:", to_time(max_time_l_hr), "\n")
cat("Min. bei ca.:", to_time(min_time_l_hr), "\n\n")

cat("===== Uhrzeiten für Min/Max (cosinor.lm Modell) =====\n")
cat("Max. bei ca.:", to_time(max_time_cos_hr), "\n")
cat("Min. bei ca.:", to_time(min_time_cos_hr), "\n")






library(ggplot2)
library(dplyr)
library(cosinor)

# ---- 1. Synthetische Daten mit 24h-Periode ----
datetime <- seq(
  from = as.POSIXct("2025-01-01 00:00:00"),
  to = as.POSIXct("2025-01-07 23:30:00"),
  by = "30 min"
)

dec.time <- as.numeric(format(datetime, "%H")) + as.numeric(format(datetime, "%M")) / 60

# Cosinus mit Periode = 24h, Max bei 6 Uhr
mesor <- 100
amplitude <- 20
phase_shift <- -pi / 2  # Max bei 6 Uhr

glucose <- mesor + amplitude * cos(2 * pi * dec.time / 24 + phase_shift)
set.seed(123)


synthetic_24h <- data.frame(datetime, dec.time, glucose)

# ---- 2. Manuelles lm-Modell (für 24h-Periode) ----
synthetic_24h <- synthetic_24h %>%
  mutate(
    cos_part = cos(2 * pi * dec.time / 24),
    sin_part = sin(2 * pi * dec.time / 24)
  )

fit_lm <- lm(glucose ~ cos_part + sin_part, data = synthetic_24h)
coefs_lm <- coef(fit_lm)

intercept_l <- coefs_lm["(Intercept)"]
beta <- coefs_lm["cos_part"]
gamma <- coefs_lm["sin_part"]

amp_l <- sqrt(beta^2 + gamma^2)
acr_l_rad <- atan2(gamma, beta)
acr_l_hr <- (acr_l_rad * 24 / (2 * pi)) %% 24  # jetzt 24h-Periode

# ---- 3. cosinor.lm-Modell (24h-Periode) ----
fit_cos <- cosinor.lm(glucose ~ time(dec.time), data = synthetic_24h, period = 24)
coefs_cos <- coef(fit_cos, transformed = TRUE)

mesor_cos   <- coefs_cos["(Intercept)"]
amp_cos     <- coefs_cos["amp"]
acr_cos_hr  <- coefs_cos["acr"]

# ---- 4a. Min/Max für manuelles Modell ----
min_l <- intercept_l - amp_l
max_l <- intercept_l + amp_l

# ---- 4b. Min/Max für cosinor.lm Modell ----
min_cos <- mesor_cos - amp_cos
max_cos <- mesor_cos + amp_cos

# ---- 5. Uhrzeiten für Minimum & Maximum berechnen ----
max_time_l_hr <- acr_l_hr
min_time_l_hr <- (acr_l_hr + 12) %% 24  # 12h Unterschied bei 24h-Periode

max_time_cos_hr <- acr_cos_hr
min_time_cos_hr <- (acr_cos_hr + 12) %% 24

# Uhrzeit-Format
to_time <- function(hour) {
  sprintf("%02d:%02d", floor(hour), round((hour %% 1) * 60))
}

# ---- 6. Ausgabe ----
cat("===== Manuelles lm-Modell =====\n")
cat("Intercept (MESOR):", round(intercept_l, 2), "\n")
cat("Amplitude:", round(amp_l, 2), "\n")
cat("Acrophase (Stunden):", round(acr_l_hr, 2), "\n")
cat("Maximale Glukose:", round(max_l, 2), "bei ca.", to_time(max_time_l_hr), "\n")
cat("Minimale Glukose:", round(min_l, 2), "bei ca.", to_time(min_time_l_hr), "\n\n")

cat("===== cosinor.lm-Modell =====\n")
cat("MESOR:", round(mesor_cos, 2), "\n")
cat("Amplitude:", round(amp_cos, 2), "\n")
cat("Acrophase (Stunden):", round(acr_cos_hr, 2), "\n")
cat("Maximale Glukose:", round(max_cos, 2), "bei ca.", to_time(max_time_cos_hr), "\n")
cat("Minimale Glukose:", round(min_cos, 2), "bei ca.", to_time(min_time_cos_hr), "\n")

summary(fit_cos)
summary(fit_lm)





library(ggplot2)

# 1. Zeitreihe erzeugen
datetime <- seq(
  from = as.POSIXct("2025-01-01 00:00:00"),
  to = as.POSIXct("2025-01-07 23:30:00"),
  by = "30 min"
)

# 2. Dezimalzeit für 24h-Periode
dec.time <- as.numeric(format(datetime, "%H")) + as.numeric(format(datetime, "%M")) / 60

# 3. Cosinus mit 24h-Periode, Maximum um 6 Uhr
mesor <- 100
amplitude <- 20
phase_shift <- pi/2  # Maximum bei 6 Uhr

glucose <- mesor + amplitude * cos(2 * pi * dec.time / 24 + phase_shift)

# 4. In DataFrame
synthetic_24h <- data.frame(datetime, dec.time, glucose)

# 5. Plot
ggplot(synthetic_24h, aes(x = datetime, y = glucose)) +
  geom_line(color = "darkorange3") +
  labs(
    title = "Synthetische Glukosekurve (Max bei 6 Uhr, Min bei 18 Uhr)",
    y = "Glukose", x = "Zeit"
  ) +
  theme_minimal()



# Pakete laden
library(dplyr)
library(cosinor)
library(lubridate)

# 1. Zeitreihe erzeugen
datetime <- seq(
  from = as.POSIXct("2025-01-01 00:00:00"),
  to   = as.POSIXct("2025-01-07 23:30:00"),
  by   = "30 min"
)

# 2. Dezimalzeit für 24h-Periode
dec.time <- as.numeric(format(datetime, "%H")) + as.numeric(format(datetime, "%M")) / 60

# 3. Cosinus mit 24h-Periode, Maximum um 6 Uhr
mesor     <- 100
amplitude <- 20
phase_shift <- pi / 2  # → Maximum bei 6 Uhr

glucose <- mesor + amplitude * cos(2 * pi * dec.time / 24 + phase_shift)

# 4. In DataFrame
synthetic_24h <- data.frame(datetime, dec.time, glucose)

# 5. Cosinor-Komponenten
synthetic_24h <- synthetic_24h %>%
  mutate(
    cos_part = cos(2 * pi * dec.time / 24),
    sin_part = sin(2 * pi * dec.time / 24)
  )

### ----- Manueller lm-Fit -----
fit_lm <- lm(glucose ~ cos_part + sin_part, data = synthetic_24h)
coefs_lm <- coef(fit_lm)

intercept_l <- coefs_lm["(Intercept)"]
beta <- coefs_lm["cos_part"]
gamma <- coefs_lm["sin_part"]

amp_l <- sqrt(beta^2 + gamma^2)
acr_rad_l <- atan2(-gamma, beta)
acr_hr_l <- (acr_rad_l * 24 / (2 * pi)) %% 24
max_time_l <- acr_hr_l
min_time_l <- (acr_hr_l + 12) %% 24

### ----- cosinor.lm-Fit -----
fit_cos <- cosinor.lm(glucose ~ time(dec.time), data = synthetic_24h, period = 24)
coefs_cos <- coef(fit_cos, transformed = TRUE)


acr_rad_cos     <- (acr_hr_cos * 2 * pi) / 24
intercept_cos   <- coefs_cos["(Intercept)"]
amp_cos     <- coefs_cos["amp"]
acr_hr_cos  <- coefs_cos["acr"]

max_time_cos <- acr_hr_cos %% 24
min_time_cos <- (acr_hr_cos + 12) %% 24

### ----- Zusammenfassung in DataFrame -----
summary_df <- data.frame(
  Window_Start      = as.POSIXct("2025-01-01 00:00:00"),
  
  # manuelles Modell
  Intercept_lm      = intercept_l,
  Amp_lm            = amp_l,
  Acrophase_Rad_lm  = acr_rad_l,
  Acrophase_Hours_lm= acr_hr_l,
  MaxTime_Hours_lm  = max_time_l,
  MinTime_Hours_lm  = min_time_l,
  
  # cosinor.lm Modell
  Intercept_cos     = intercept_cos,
  Amp_cos           = amp_cos,
  Acrophase_Rad_cos = acr_rad_cos,
  Acrophase_Hours_cos = acr_hr_cos,
  MaxTime_Hours_cos = max_time_cos,
  MinTime_Hours_cos = min_time_cos
)

### Ausgabe
print(summary_df)



# Zuerst testen, was cosinor.lm ausgibt
fit_cos <- cosinor.lm(glucose ~ time(dec.time), data = synthetic_24h, period = 24)
coefs_cos <- coef(fit_cos, transformed = TRUE)
print(coefs_cos)  # Schauen Sie sich die "acr" Werte an

# Dann entsprechend anpassen:
intercept_cos <- coefs_cos["(Intercept)"]
amp_cos <- coefs_cos["amp"]

# OPTION 1: Wenn acr in RADIANS ausgegeben wird
if (abs(coefs_cos["acr"]) <= 2*pi) {  # Wahrscheinlich Radians
  acr_rad_cos <- coefs_cos["acr"]
  acr_hr_cos <- (acr_rad_cos * 24) / (2 * pi)
} else {  # OPTION 2: Wenn acr bereits in HOURS ausgegeben wird
  acr_hr_cos <- coefs_cos["acr"]  
  acr_rad_cos <- (acr_hr_cos * 2 * pi) / 24
}

max_time_cos <- acr_hr_cos %% 24
min_time_cos <- (acr_hr_cos + 12) %% 24


### Season Analyse

attr(cleaned_data_deduplicated$Timestamp, "tzone")
Sys.timezone()

