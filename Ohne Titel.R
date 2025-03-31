# Load necessary libraries
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




#### DATA READING ####
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

##### DATA CLEANING ####

# Filter out rows with NA or infinite values in Timestamp and Glucose levels columns
cleaned_data <- combined_data %>%
  filter(!is.na(Timestamp), !is.na(`Glucose levels (mg/dl)`), is.finite(`Glucose levels (mg/dl)`))

cleaned_data 

# Add a column to indicate missing or non-finite values (1 = Missing/Non-finite, 0 = Present)
combined_data <- combined_data %>%
  mutate(
    Missing = ifelse(is.na(`Glucose (mg/dl)`) | !is.finite(`Glucose (mg/dl)`), 1, 0)
  )

# Check how many cloumns have the value 1 in Missings
sum(combined_data$Missing == 1)

# Step 1: Determine the start date for each ID (earliest timestamp)
id_start_dates <- combined_data %>%
  group_by(ID) %>%
  summarise(Start_Date = min(Timestamp, na.rm = TRUE)) #, .groups = "drop"

# Step 2: Join the start date back to the main data and filter to include only records from the start date onward
combined_data_filtered <- combined_data %>%
  left_join(id_start_dates, by = "ID") %>%
  filter(Timestamp >= Start_Date | is.na(Timestamp))  # Include rows where Timestamp is NA as well

#### including a synthetic continuous time axis ####
# Step 1: Create a 5-minute interval sequence for each ID starting from Start_Date
expanded_data <- combined_data_filtered %>%
  # Keep only necessary columns (ID and Start_Date) and remove duplicates
  distinct(ID, Start_Date) %>%
  # Create a continuous time series of 5-minute intervals for each ID
  rowwise() %>%
  mutate(
    Timestamp = list(seq(Start_Date, max(combined_data_filtered$Timestamp[combined_data_filtered$ID == ID], na.rm = TRUE), 
                         by = "1 min"))
  ) %>%
  unnest(Timestamp) %>%
  ungroup()

# Step 2: Left join the original data with the expanded data to keep original columns and values
combined_data_continuous <- expanded_data %>%
  left_join(combined_data_filtered, by = c("ID", "Timestamp", "Start_Date")) %>%
  mutate(
    Source = if_else(is.na(`Glucose (mg/dl)`), "synthetic timestamp", "original timestamp")
  )

# View the resulting DataFrame
combined_data_continuous

#double checking the unique rows that were added
# Find rows that are unique to combined_data_continuous
unique_to_df_continuous <- combined_data_continuous %>%
  anti_join(combined_data_filtered, by = c("ID", "Timestamp", "Glucose (mg/dl)")) %>%
  arrange(ID)

# View the unique rows
unique_to_df_continuous

# Add a new column based on conditions in Missing and Source
combined_data_continuous <- combined_data_continuous %>%
  mutate(
    Missing = if_else(Source == "synthetic timestamp", 1, Missing)
  )

# Calculate the percentage of missing values per ID and month
# Step 1a: Bin the timestamps into 5-minute intervals
combined_data_binned <- combined_data_continuous %>%
  mutate(
    Timestamp_5min = floor_date(Timestamp, "5 minutes"),  # Bin into 5-minute intervals
    Month = format(floor_date(Timestamp, "month"))  # , "%B" Extract the month for grouping
  )

# Step 1b: Resample combined_data_continuous to 5-minute intervals
combined_data_resampled <- combined_data_continuous %>%
  mutate(
    Timestamp_5min = floor_date(Timestamp, "5 minutes")  # Round down to nearest 5 minutes
  ) %>%
  group_by(ID, Timestamp_5min) %>%  # Group by ID, 5-minute timestamp, and Month
  summarise(
    Avg_Glucose = mean(`Glucose (mg/dl)`, na.rm = TRUE),  # Calculate the average glucose level per bin
    Missing_Count = if_else(is.na(Avg_Glucose), 1, 0),    # Assign 1 if Avg_Glucose is NA, else 0
    .groups = "drop"
  )

#add month column


# Step 2: Calculate the missing percentage per ID and month
missing_percentage <- combined_data_resampled %>%
  mutate(
    Month = floor_date(Timestamp_5min, "month")
  ) %>%
  group_by(ID, Month) %>%  # Group by 5-minute intervals, ID, and Month
  summarise(
    Total_Entries = n(),  # Total number of entries in each 5-minute bin divided by 5 
    Missing_Count = sum((Missing_Count == 1), na.rm = TRUE),  # Count of missing values in each bin
    Missing_Percentage = (Missing_Count / Total_Entries) * 100,  # Calculate missing percentage
    Non_Missing_Percentage = 100-Missing_Percentage,
    .groups = "drop"
  )


# View the result
missing_percentage

### trouble shooting
ID112 <- combined_data_continuous %>% 
  filter(ID=="112")

ggplot(ID112, aes(x = Timestamp, y = `Glucose (mg/dl)`)) +
  geom_line() +
  labs(
    title = "Glucose Levels Over Time",
    x = "Timestamp",
    y = "Glucose levels (mg/dL)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

#new
# Reshape the data to long format
missing_long <- missing_percentage %>%
  pivot_longer(
    cols = c(Missing_Percentage, Non_Missing_Percentage),
    names_to = "Status",
    values_to = "Percentage"
  ) 

#Other month format
missing_long$Month_Name <- factor(format(missing_long$Month, "%b"), levels = month.abb)

# Plot the stacked bar chart
ggplot(missing_long, aes(x = Month_Name, y = Percentage, fill = Status)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "black") +  # Add a dashed line at 50%
  facet_wrap(~ ID) +  # Facet by ID
  labs(
    title = "Percentage of Missing and Non-Missing Glucose Values by Month and ID",
    x = "Month",
    y = "Percentage (%)"
  ) +
  scale_fill_manual(
    values = c("Missing_Percentage" = "#E57373", "Non_Missing_Percentage" = "#81C784"),  # Shades of red and green
    labels = c("Missing_Percentage" = "Missing", "Non_Missing_Percentage" = "Non-Missing")
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

setwd("/Users/linabecker/Documents/M.Sc. Health Science/Masterarbeit/Masterarbeit")
current_date <- Sys.Date()  # Get the current date
ggsave(paste0(current_date,".Missing_glucose_percentage_by_ID.png"), width = 10, height = 8, bg = "white")

##### Sort data, calculate time differences, and summarize gaps by duration ####
gap_summary <- combined_data_filtered %>%
  arrange(ID, Timestamp) %>%
  group_by(ID) %>%
  mutate(Time_Diff_mins = as.numeric(difftime(Timestamp, lag(Timestamp), units = "mins"))) %>%
  filter(Time_Diff_mins > 5) %>% # Filter for gaps greater than 5 minutes
  group_by(ID, Time_Diff_mins) %>%
  summarise(
    Gap_Count = n(),                   # Count occurrences of each gap duration
    .groups = "drop"
  ) %>%
  arrange(ID, Time_Diff_mins)

# Display the summary of gap types and their frequencies
gap_summary

# Bar plot of gap duration frequencies by ID
ggplot(gap_summary, aes(x = as.factor(Time_Diff_mins), y = as.numeric(Gap_Count))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  facet_wrap(~ ID) +
  labs(
    title = "Frequency of Gap Durations by ID",
    x = "Gap Duration (minutes)",
    y = "Frequency"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## Second missings category: the times and amount of glucose missing
# Compute the number of occasions per ID where Timestamp is present but Glucose level is NA
glucose_missing_summary <- combined_data_filtered %>%
  group_by(ID) %>%
  summarise(
    Missing_Glucose_Occasions = n(),  # Count occurrences per ID
    .groups = "drop"
  )

# Display the summary table
glucose_missing_summary


missing_data_df <- combined_data %>%
  left_join(id_start_dates, by = "ID") %>%
  filter(Timestamp >= Start_Date | is.na(Timestamp)) %>%  # Keep rows with valid start date or missing timestamp
  mutate(Missing = ifelse(is.na(`Glucose levels (mg/dl)`) | !is.finite(`Glucose levels (mg/dl)`), 1, 0)) %>%
  filter(Missing == 1) %>%  # Only rows with missing or non-finite values
  select(ID, Timestamp, `Glucose levels (mg/dl)`, Missing)

# Bar plot of missing glucose occasions per ID
ggplot(glucose_missing_summary, aes(x = ID, y = Missing_Glucose_Occasions)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of Occasions with Missing Glucose Values by ID",
    x = "ID",
    y = "Missing Glucose Occasions"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )

# Calculate the total recording period for each ID (in 5-minute intervals)
recording_period_summary <- combined_data %>%
  group_by(ID) %>%
  summarise(
    Start_Date = min(Timestamp, na.rm = TRUE),
    End_Date = max(Timestamp, na.rm = TRUE),
    Recording_Period_Minutes = as.numeric(difftime(End_Date, Start_Date, units = "mins")),
    Expected_Recordings = Recording_Period_Minutes / 5,  # Expected number of 5-minute intervals
    .groups = "drop"
  )

# Join recording period data with the missing glucose summary
glucose_missing_percentage <- glucose_missing_summary %>%
  left_join(recording_period_summary, by = "ID") %>%
  mutate(
    Missing_Glucose_Percentage = (Missing_Glucose_Occasions / Expected_Recordings) * 100
  ) %>%
  select(ID, Missing_Glucose_Percentage)  # Select relevant columns for plotting

# Bar plot of missing glucose values as a percentage of the recording period per ID
ggplot(glucose_missing_percentage, aes(x = ID, y = Missing_Glucose_Percentage)) +
  geom_bar(stat = "identity", fill = "#377EB8") +
  labs(
    title = "Percentage of missing glucose values by ID when timestamp is present",
    x = "ID",
    y = "Percentage of Missing Glucose Values (%)"
  ) +
  theme(
    strip.text = element_text(face = "bold")
  )

setwd("/Users/linabecker/Documents/M.Sc. Health Science/Masterarbeit/Masterarbeit")
ggsave(paste0(current_date,".Barplot_missing_glucose_percentage_by_ID.png"), width = 10, height = 8, bg = "white")

#Add month column
# Add a Month column to combined_data
combined_data <- combined_data %>%
  mutate(Month = floor_date(Timestamp, "month"))

# Calculate the recording summary per ID and Month
recording_period_summary <- combined_data %>%
  group_by(ID, Month) %>%
  summarise(
    Start_Date = min(Timestamp, na.rm = TRUE),
    End_Date = max(Timestamp, na.rm = TRUE),
    Recording_Period_Minutes = as.numeric(difftime(End_Date, Start_Date, units = "mins")),
    Expected_Recordings = Recording_Period_Minutes / 5,  # Expected number of 5-minute intervals
    Actual_Recordings = sum(!is.na(`Glucose levels (mg/dl)`)),  # Count non-missing glucose recordings
    Missing_Percentage = ((Expected_Recordings - Actual_Recordings) / Expected_Recordings) * 100,
    .groups = "drop"
  )

# View the summary
recording_period_summary

# Ensure Month is formatted to show month names (e.g., "March")
recording_period_summary <- recording_period_summary %>%
  mutate(Month = factor(format(Month, "%B"), levels = month.name))

# Plot missing percentage by ID, faceted by month
ggplot(recording_period_summary, aes(x = Month, y = Missing_Percentage)) +
  geom_bar(stat = "identity", fill = "#377EB8") +
  labs(
    title = "Percentage of Missing Glucose Values by ID and Month",
    x = "ID",
    y = "Percentage of Missing Glucose Values (%)"
  ) +
  facet_wrap(~ ID) +  # Facet by month with month names as labels
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




###Glucose plotting

# Load necessary for plotting
library(ggplot2)
library(colorspace)
library(scico)             # Load the package

# Define the start date
start_date <- as.POSIXct("2024-03-10 00:00:00")

# Create the faceted plot
ggplot(cleaned_data, aes(x = Timestamp, y = `Glucose levels (mg/dl)`)) +
  geom_line(color = "#619CFF") +
  facet_wrap(~ ID, ncol = 1, nrow = 6) + #scales = "free_x"
  geom_hline(yintercept = 70, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 180, color = "red", linetype = "dashed") +
  labs(
    title = "Glucose Levels Over Time by ID",
    x = "Timestamp",
    y = "Glucose levels (mg/dL)"
  ) +
  scale_x_datetime(
    date_breaks = "2 weeks",
    date_labels = "%d-%b",
    limits = c(start_date, NA)  # Fix the start date; end date will adjust automatically
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

setwd("/Users/linabecker/Documents/M.Sc. Health Science/Masterarbeit/Masterarbeit")
current_date <- Sys.Date()  # Get the current date
ggsave(paste0(current_date,".Glucose_over_time_by_ID.png"), width = 15, height = 10, bg = "white")

library(plotly)
# Extract month and hour, then calculate average glucose level per hour for each ID and month
profile_data <- cleaned_data %>%
  mutate(
    Month = floor_date(Timestamp, "month"),
    Hour = hour(Timestamp)
  ) %>%
  group_by(ID, Month, Hour) %>%
  summarize(Average_Glucose = mean(`Glucose levels (mg/dl)`, na.rm = TRUE), .groups = 'drop')

# Generate Batlow color palette based on the number of unique months
unique_months <- unique(profile_data$Month)
num_months <- length(unique_months)
batlow_colors <- sequential_hcl(num_months, palette = "Batlow")

# Create the ggplot object
p <- ggplot(profile_data, aes(x = Hour, y = Average_Glucose, color = factor(Month))) +
  geom_line(size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "red", se = FALSE, aes(linetype = "Trendline")) +
  facet_wrap(~ ID ) + #ncol = 2, nrow = 2
  labs(
    title = "24-Hour Glucose Profiles by ID with Spline Trendline",
    x = "Hour of Day",
    y = "Average Glucose Level (mg/dL)",
    color = "Month",
    linetype = ""
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +
  scale_color_manual(values = batlow_colors) +
  geom_hline(yintercept = 80, color = "black", linetype = "dashed") +
  geom_hline(yintercept = 140, color = "black", linetype = "dashed") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = NA)
  )
# Convert to an interactive plotly object
ggplotly(p) %>%
  layout(legend = list(title = list(text = "<b>Toggle Lines</b>")))

setwd("/Users/linabecker/Documents/M.Sc. Health Science/Masterarbeit/Masterarbeit")
ggsave(paste0(current_date,".24_hour_glucose_profiles_by_ID.png"), plot = p, width = 15, height = 10, bg = "white")
