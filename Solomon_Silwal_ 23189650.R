## ----------------------------------------------------------------------------------------------------------------
# Load the readr package if not already loaded
library(readr)
library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)

# Load the data set from "accidents.csv"
df <- read_csv("accidents.csv")
df


## ----------------------------------------------------------------------------------------------------------------
library(lubridate)


df_clean <- df
df_clean$`Accident Date` <- dmy(df_clean$`Accident Date`)
df_clean$`Time (24hr)` <- format(as.POSIXct(sprintf("%04d", df_clean$`Time (24hr)`),
                                            format = "%H%M", tz = "UTC"), 
                                 format = "%H:%M", usetz = FALSE)
df_clean



## ----------------------------------------------------------------------------------------------------------------
missing_values <- colSums(is.na(df_clean))
print(missing_values)



## ----------------------------------------------------------------------------------------------------------------
missing_values <- colSums(is.na(df_clean))
print("Columns with Missing Values:",)
print(missing_values[missing_values > 0])

ggplot(df_clean, aes(x = factor(`Lighting Conditions`), fill = factor(is.na(`Daylight/Dark`)))) +
  geom_bar(position = "stack") +
  labs(title = "Distribution of Missing Values in Daylight/Dark by Lighting Conditions",
       x = "Lighting Conditions",
       y = "Count") +
  scale_fill_manual(values = c("FALSE" = "gray", "TRUE" = "black"), name = "Missing Values") +
  theme_minimal()

missing_day <- df_clean %>% filter(is.na(`Daylight/Dark`))


## ----------------------------------------------------------------------------------------------------------------
first_road_class_mapping <- c("1" = "Motorway","2" = "A(M)","3" = "A","4" = "B","5" = "C","6" = "Unclassified")

df_clean <- df_clean %>%
  mutate(`1st Road Class` = map_chr(`1st Road Class`, function(x) {
    ifelse(as.character(x) %in% names(first_road_class_mapping), first_road_class_mapping[as.character(x)], as.character(x))
  }))

# Further consistency adjustments
df_clean <- df_clean %>%
  mutate(
    `1st Road Class` = case_when(
      `1st Road Class` %in% c("U") ~ "Unclassified",
      grepl("^A\\d+", `1st Road Class`) ~ "A",
      grepl("^A\\(M\\)$", `1st Road Class`) ~ "A(M)",
      grepl("^A\\d+\\(M\\)$", `1st Road Class`) ~ "A(M)",
      grepl("^B\\d+", `1st Road Class`) ~ "B",
      `1st Road Class` %in% c("Motorway", "A", "B", "C", "Unclassified", "A(M)") ~ `1st Road Class`,
      `1st Road Class` %in% c("M62") ~ "Motorway",
      TRUE ~ `1st Road Class`
    )
  )

unique(df_clean$`1st Road Class`)


## ----------------------------------------------------------------------------------------------------------------
unique(df_clean$`Road Surface`)


## ----------------------------------------------------------------------------------------------------------------
df_clean <- df_clean %>%
  mutate(
    `Road Surface` = case_when(
      `Road Surface` %in% c("Wet/Damp", "Wet") ~ "Wet / Damp",
      `Road Surface` %in% c("Frost/Ice", "Ice") ~ "Frost / Ice",
      `Road Surface` %in% c("1") ~ "Dry",
      `Road Surface` %in% c("2") ~ "Wet / Damp",
      `Road Surface` %in% c("3") ~ "Snow",
      `Road Surface` %in% c("4") ~ "Frost / Ice",
      `Road Surface` %in% c("5") ~ "Flood (surface water over 3cm deep)",
      `Road Surface` %in% c("Wet \xa8 Damp") ~ "Wet / Damp",
      TRUE ~ `Road Surface`
    )
  )

unique(df_clean$`Road Surface`)


## ----------------------------------------------------------------------------------------------------------------
unique(df_clean$`Lighting Conditions`)


## ----------------------------------------------------------------------------------------------------------------
# Transform Lighting Conditions using case_when
df_clean <- df_clean %>%
  mutate(
    `Lighting Conditions` = case_when(
      `Lighting Conditions` == "1" ~ "Daylight: street lights present",
      `Lighting Conditions` == "2" ~ "Daylight: no street lighting",
      `Lighting Conditions` == "3" ~ "Daylight: street lighting unknown",
      `Lighting Conditions` == "4" ~ "Darkness: street lights present and lit",
      `Lighting Conditions` == "5" ~ "Darkness: street lights present but unlit",
      `Lighting Conditions` == "6" ~ "Darkness: no street lighting",
      `Lighting Conditions` == "7" ~ "Darkness: street lighting unknown",
      TRUE ~ as.character(`Lighting Conditions`)
    )
  )

# Display unique values after transformation
unique(df_clean$`Lighting Conditions`)



## ----------------------------------------------------------------------------------------------------------------
df_clean


## ----------------------------------------------------------------------------------------------------------------
unique(df_clean$`Weather Conditions`)


## ----------------------------------------------------------------------------------------------------------------
# Transform Weather Conditions using case_when
df_clean <- df_clean %>%
  mutate(
    `Weather Conditions` = case_when(
      `Weather Conditions` == "1" ~ "Fine without high winds",
      `Weather Conditions` == "2" ~ "Raining without high winds",
      `Weather Conditions` == "3" ~ "Snowing without high winds",
      `Weather Conditions` == "4" ~ "Fine with high winds",
      `Weather Conditions` == "5" ~ "Raining with high winds",
      `Weather Conditions` == "6" ~ "Snowing with high winds",
      `Weather Conditions` == "7" ~ "Fog or mist ? if hazard",
      `Weather Conditions` == "8" ~ "Other",
      `Weather Conditions` == "9" ~ "Unknown",
      TRUE ~ as.character(`Weather Conditions`)
    )
  )

# Display unique values after transformation
unique(df_clean$`Weather Conditions`)


## ----------------------------------------------------------------------------------------------------------------
df_clean


## ----------------------------------------------------------------------------------------------------------------
unique(df_clean$`Casualty Class`)


## ----------------------------------------------------------------------------------------------------------------
# Transform Casualty Class using case_when
df_clean <- df_clean %>%
  mutate(
    `Casualty Class` = case_when(
      `Casualty Class` == "1" ~ "Driver or rider",
      `Casualty Class` == "2" ~ "Vehicle or pillion passenger",
      `Casualty Class` == "3" ~ "Pedestrian",
      TRUE ~ as.character(`Casualty Class`)
    )
  )

# Display unique values after transformation
unique(df_clean$`Casualty Class`)


## ----------------------------------------------------------------------------------------------------------------
df_clean


## ----------------------------------------------------------------------------------------------------------------
unique(df_clean$'Casualty Severity')



## ----------------------------------------------------------------------------------------------------------------
casualty_severity_mapping <- c("1" = "Fatal",
                               "2" = "Serious",
                               "3" = "Slight")

df_clean <- df_clean %>%
  mutate(`Casualty Severity` = map_chr(`Casualty Severity`, function(x) {
    ifelse(as.character(x) %in% names(casualty_severity_mapping), casualty_severity_mapping[as.character(x)], as.character(x))
  }))

# Display unique values after transformation
unique(df_clean$`Casualty Severity`)


## ----------------------------------------------------------------------------------------------------------------
df_clean


## ----------------------------------------------------------------------------------------------------------------
unique(df_clean$`Type of Vehicle`)


## ----------------------------------------------------------------------------------------------------------------

df_clean <- df_clean %>%
  mutate(
    `Type of Vehicle` = case_when(
      `Type of Vehicle` == "1" ~ "Pedal cycle",
      `Type of Vehicle` == "2" ~ "M/cycle 50cc and under",
      `Type of Vehicle` == "3" ~ "Motorcycle over 50cc and up to 125cc",
      `Type of Vehicle` == "4" ~ "Motorcycle over 125cc and up to 500cc",
      `Type of Vehicle` == "5" ~ "Motorcycle over 500cc",
      `Type of Vehicle` == "8" ~ "Taxi/Private hire car",
      `Type of Vehicle` == "9" ~ "Car",
      `Type of Vehicle` == "10" ~ "Minibus (8 – 16 passenger seats)",
      `Type of Vehicle` == "11" ~ "Bus or coach (17 or more passenger seats)",
      `Type of Vehicle` == "14" ~ "Other motor vehicle",
      `Type of Vehicle` == "15" ~ "Other non-motor vehicle",
      `Type of Vehicle` == "16" ~ "Ridden horse",
      `Type of Vehicle` == "17" ~ "Agricultural vehicle (includes diggers etc.)",
      `Type of Vehicle` == "18" ~ "Tram / Light rail",
      `Type of Vehicle` == "19" ~ "Goods vehicle 3.5 tonnes mgw and under",
      `Type of Vehicle` == "20" ~ "Goods vehicle over 3.5 tonnes and under 7.5 tonnes mgw",
      `Type of Vehicle` == "21" ~ "Goods vehicle 7.5 tonnes mgw and over",
      `Type of Vehicle` == "22" ~ "Mobility Scooter",
      `Type of Vehicle` == "90" ~ "Other Vehicle",
      `Type of Vehicle` == "97" ~ "Motorcycle - Unknown CC",
      TRUE ~ as.character(`Type of Vehicle`)
    )
  )

# Display unique values after transformation
unique(df_clean$`Type of Vehicle`)


## ----------------------------------------------------------------------------------------------------------------
df_clean


## ----------------------------------------------------------------------------------------------------------------
unique(df_clean$`Sex of Casualty`)


## ----------------------------------------------------------------------------------------------------------------

df_clean <- df_clean %>%
  mutate(
    `Sex of Casualty` = case_when(
      `Sex of Casualty` == "1" ~ "Male",
      `Sex of Casualty` == "2" ~ "Female",
      TRUE ~ as.character(`Sex of Casualty`)
    )
  )

# Display unique values after transformation
unique(df_clean$`Sex of Casualty`)


## ----------------------------------------------------------------------------------------------------------------
df_clean


## ----------------------------------------------------------------------------------------------------------------

df_clean <- df_clean %>% select(-c(`Local Authority`))



## ----------------------------------------------------------------------------------------------------------------
rows_with_missing_daylight_dark <- df_clean[is.na(df_clean$`Daylight/Dark`), ]

print(rows_with_missing_daylight_dark)


## ----------------------------------------------------------------------------------------------------------------
# Histogram of Age of Casualty
hist(df_clean$`Age of Casualty`, main = "Histogram of Age of Casualty", 
     xlab = "Age of Casualty", col = "lightblue", border = "black", breaks = 10)

# Calculate IQR and bounds
iqr_age <- IQR(df_clean$`Age of Casualty`, na.rm = TRUE)
quantiles <- quantile(df_clean$`Age of Casualty`, probs = c(0.25, 0.75), na.rm = TRUE)
lower_bound <- max(0, quantiles[1] - 1.5 * iqr_age)
upper_bound <- quantiles[2] + 1.5 * iqr_age

# Identify outliers
outliers <- df_clean %>% filter(`Age of Casualty` < lower_bound | `Age of Casualty` > upper_bound)

# Display outliers
outliers$`Age of Casualty`




## ----------------------------------------------------------------------------------------------------------------
# Boxplot of Age of Casualty
boxplot(df_clean$`Age of Casualty`, main = "Boxplot of Age of Casualty", 
        ylab = "Age of Casualty")

# Identify outliers
outlier_box <- boxplot(df_clean$`Age of Casualty`, plot = FALSE)$out

# Display outliers
outlier_box





## ----------------------------------------------------------------------------------------------------------------
# Histogram of Age of Casualty
hist(df_clean$`Age of Casualty`, main = "Histogram of Age of Casualty", 
     xlab = "Age of Casualty", col = "lightblue", border = "black", breaks = 10)

# Calculate mean and standard deviation
mean_age <- mean(df_clean$`Age of Casualty`, na.rm = TRUE)
sd_age <- sd(df_clean$`Age of Casualty`, na.rm = TRUE)

# Calculate three sigma bounds
lower_bound <- mean_age - 3 * sd_age
upper_bound <- mean_age + 3 * sd_age

# Identify outliers
outliers <- df_clean %>% filter(`Age of Casualty` < lower_bound | `Age of Casualty` > upper_bound)

# Display outliers
outliers$`Age of Casualty`



## ----------------------------------------------------------------------------------------------------------------
write_csv(df_clean, "clean_accident.csv")



## ----------------------------------------------------------------------------------------------------------------
data <- read_csv("clean_accident.csv")
data


## ----------------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(dplyr)

# Filter and summarize data for male and female drivers/riders
gender_weather_accidents <- data %>%
  filter(`Casualty Class` == "Driver or rider") %>%
  group_by(`Weather Conditions`, `Sex of Casualty`) %>%
  summarise(accidents = n()) %>%
  spread(`Sex of Casualty`, accidents, fill = 0) %>%
  mutate(difference = Male - Female) %>%
  arrange(desc(difference))

# Display the result
gender_weather_accidents



## ----------------------------------------------------------------------------------------------------------------
# Extract year from Accident Date
data <- data %>%
  mutate(Year = year(`Accident Date`))

# Summarize number of casualties per year
casualties_per_year <- data %>%
  group_by(Year) %>%
  summarise(total_casualties = n())

# Determine the year with the highest number of casualties
max_casualties_year <- casualties_per_year %>%
  filter(total_casualties == max(total_casualties))

# Display the result
casualties_per_year
max_casualties_year




## ----------------------------------------------------------------------------------------------------------------
# Plot: Light conditions and severity
library(ggplot2)

ggplot(data, aes(x = `Lighting Conditions`, fill = `Casualty Severity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Accident Severity by Lighting Conditions", x = "Lighting Conditions", y = "Count of Accidents") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~`Casualty Severity`, scales = "free_y") +
  theme(legend.position = "none")



## ----------------------------------------------------------------------------------------------------------------
data %>%
  group_by(`Lighting Conditions`, `Casualty Severity`) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = `Casualty Severity`, y = `Lighting Conditions`, fill = count)) +
  geom_tile(color = "white") +  # Add white borders around tiles for clarity
  labs(title = "Relationship Between Light Conditions and Severity",
       x = "Casualty Severity",
       y = "Light Conditions",
       fill = "Count of Casualties") +
  scale_fill_viridis_c() +  # Use viridis color palette for better differentiation
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14))


## ----------------------------------------------------------------------------------------------------------------
# Plot: Weather condition and number of vehicles involved
ggplot(data, aes(x = `Weather Conditions`)) +
  geom_bar(aes(fill = `Type of Vehicle`), position = "stack") +
  labs(title = "Number of Vehicles Involved by Weather Conditions", x = "Weather Conditions", y = "Count of Vehicles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=0.5), size=3)



## ----------------------------------------------------------------------------------------------------------------
data %>%
  group_by(`Weather Conditions`) %>%
  summarise(total_vehicles = sum(`Number of Vehicles`)) %>%
  ggplot(aes(x = `Weather Conditions`, y = total_vehicles, fill = `Weather Conditions`)) +
  geom_bar(stat = "identity") +
  labs(title = "Relationship Between Weather Conditions and Number of Vehicles Involved",
       x = "Weather Conditions",
       y = "Total Number of Vehicles") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


## ----------------------------------------------------------------------------------------------------------------
# Line plot of accidents over years
ggplot(casualties_per_year, aes(x = Year, y = total_casualties)) +
  geom_line() +
  labs(title = "Number of Accidents Over Years", x = "Year", y = "Total Accidents") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))




## ----------------------------------------------------------------------------------------------------------------
# Stacked bar chart of Weather Conditions and Severity
ggplot(data , aes(x = `Weather Conditions`, fill = `Casualty Severity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Severity Distribution Across Weather Conditions", x = "Weather Conditions", y = "Count of Accidents") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



## ----------------------------------------------------------------------------------------------------------------
# Grouped bar chart of Casualty Class by Gender
ggplot(data, aes(x = `Casualty Class`, fill = `Sex of Casualty`)) +
  geom_bar(position = "dodge") +
  labs(title = "Casualties by Casualty Class and Gender", x = "Casualty Class", y = "Count of Casualties") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_manual(values = c("blue", "pink"))




## ----------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = `Accident Date`)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "gray") +
  labs(title = "Distribution of Accidents over Time",
       x = "Accident Date",
       y = "Number of Accidents") +
  theme_minimal()



## ----------------------------------------------------------------------------------------------------------------
# Load ggplot2 library
library(ggplot2)

# Bar plot of accidents by road surface conditions
ggplot(data, aes(x = `Road Surface`)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Accidents by Road Surface Conditions", 
       x = "Road Surface Conditions", y = "Number of Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ----------------------------------------------------------------------------------------------------------------
# Extract year from Accident Date if not already extracted
data <- data %>%
  mutate(Year = year(`Accident Date`))

# Plot number of accidents by severity over time
ggplot(data, aes(x = Year, fill = `Casualty Severity`)) +
  geom_bar(position = "dodge") +
  labs(title = "Severity of Accidents Over Time", 
       x = "Year", y = "Number of Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ----------------------------------------------------------------------------------------------------------------

# Adjust plot size and clarity
ggplot(data, aes(x = `Type of Vehicle`, fill = `Casualty Severity`)) +
  geom_bar(position = "stack") +
  labs(title = "Casualty Severity by Type of Vehicle", 
       x = "Type of Vehicle", y = "Number of Casualties") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 14)) +
  theme_minimal() +
  theme(legend.position = "top") +
  coord_flip() +
  scale_fill_brewer(palette = "Set2")



## ----------------------------------------------------------------------------------------------------------------
# Plot: Accidents by lighting conditions over time
ggplot(data, aes(x = Year, fill = `Lighting Conditions`)) +
  geom_bar(position = "stack") +
  labs(title = "Accidents by Lighting Conditions Over Time", 
       x = "Year", y = "Number of Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ----------------------------------------------------------------------------------------------------------------
# Plot: Accidents by road surface and weather conditions
ggplot(data, aes(x = `Road Surface`, fill = `Weather Conditions`)) +
  geom_bar(position = "stack") +
  labs(title = "Accidents by Road Surface and Weather Conditions", 
       x = "Road Surface", y = "Number of Accidents") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## ----------------------------------------------------------------------------------------------------------------
# List of packages to install
packages <- c("tidyr", "dplyr", "caret", "MASS", "ggplot2", "lubridate")

# Loop through each package
for (package in packages) {
  # Check if the package is not already installed
  if (!(package %in% installed.packages())) {
    # Install the package
    install.packages(package)
  }
}




## ----------------------------------------------------------------------------------------------------------------
data <- read_csv("clean_accident.csv")
data


## ----------------------------------------------------------------------------------------------------------------
# Select relevant columns
#data <- data %>%
 # select(`Casualty Class`, `Casualty Severity`, `Type of Vehicle`, `Weather Conditions`, `Age of Casualty`)

# Drop rows with missing values in predictor variables
#data <- data %>% drop_na(`Casualty Class`, `Casualty Severity`, `Type of Vehicle`, `Weather Conditions`)

data


## ----------------------------------------------------------------------------------------------------------------
data <- data %>%
  mutate(
    `Casualty Class` = as.factor(`Casualty Class`),
    `Casualty Severity` = as.factor(`Casualty Severity`),
    `Type of Vehicle` = as.factor(`Type of Vehicle`),
    `Weather Conditions` = as.factor(`Weather Conditions`)
  )
data


## ----------------------------------------------------------------------------------------------------------------
# Install and load caret package if not already installed
if (!requireNamespace("caret", quietly = TRUE)) {
  install.packages("caret")
}
library(caret)

# Assuming missing values in 'Age of Casualty' are handled
# Example: Imputing missing values with median
median_age <- median(data$`Age of Casualty`, na.rm = TRUE)
data$`Age of Casualty`[is.na(data$`Age of Casualty`)] <- median_age

# Now proceed with data partitioning
set.seed(123)
trainingIndex <- createDataPartition(data$`Age of Casualty`, p = 0.8, list = FALSE)
trainingData <- data[trainingIndex, ]
testingData <- data[-trainingIndex, ]



## ----------------------------------------------------------------------------------------------------------------
model <- lm(`Age of Casualty` ~ `Casualty Class` + `Casualty Severity` + `Type of Vehicle` + `Weather Conditions`, data = trainingData)

# Print model summary
summary(model)



## ----------------------------------------------------------------------------------------------------------------
# Get the rows with missing `Age of Casualty`
missing_age_data <- data %>% filter(is.na(`Age of Casualty`))

# Predict missing values
predicted_age <- predict(model, newdata = missing_age_data)

# Replace the missing values with predicted values
data <- data %>%
  mutate(`Age of Casualty` = ifelse(is.na(`Age of Casualty`), predicted_age, `Age of Casualty`))



## ----------------------------------------------------------------------------------------------------------------
write_csv(data, "regression.csv")

