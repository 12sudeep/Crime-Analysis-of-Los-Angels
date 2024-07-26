# Required libraries
library(readr)
library(plotly)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(viridis)
library(treemap)
library(arules)
library(arulesViz)
library(extrafont)


# Understanding and loading the Data
crime_data <- read_csv("Crime_Data_from_2020_to_Present.csv")

problems(crime_data) # finding the part which is causing problem in data

head(crime_data[,24]) #checking the column name that has problem

crime_data <- read_csv("Crime_Data_from_2020_to_Present.csv", col_types = cols(
  `Crm Cd 4` = col_character(),
  .default = col_guess()
)) # providing appropriate data type to solve the problem

problems(crime_data) #checking if any problem occurs again or not

head(crime_data)

str(crime_data)

summary(crime_data)



# Data Wrangling
# changing columns names for better understanding
crime_data <- crime_data %>%
  rename(
    division_number = DR_NO,
    date_reported = `Date Rptd`,
    date_occurred = `DATE OCC`,
    time_occurred = `TIME OCC`,
    area = AREA,
    area_name = `AREA NAME`,
    reporting_district = `Rpt Dist No`,
    part = `Part 1-2`,
    crime_code = `Crm Cd`,
    crime_description = `Crm Cd Desc`,
    modus_operandi = Mocodes,
    victim_age = `Vict Age`,
    victim_sex = `Vict Sex`,
    victim_descent = `Vict Descent`,
    premise_code = `Premis Cd`,
    premise_description = `Premis Desc`,
    weapon_code = `Weapon Used Cd`,
    weapon_description = `Weapon Desc`,
    status = Status,
    status_description = `Status Desc`,
    crime_code_1 = `Crm Cd 1`,
    crime_code_2 = `Crm Cd 2`,
    crime_code_3 = `Crm Cd 3`,
    crime_code_4 = `Crm Cd 4`,
    location = LOCATION,
    cross_street = `Cross Street`,
    latitude = LAT,
    longitude = LON
  )

#converting data to appropriate data types
crime_data <- crime_data %>%
  mutate(
    date_occurred = as.POSIXct(date_occurred, format = "%m/%d/%Y %I:%M:%S %p"),
    time_occurred = as.POSIXct(sprintf("%s", time_occurred), format = "%H%M")
  )

#Missing Data:
colSums(is.na(crime_data))

#Data Cleaning
#removing data of 2024 beacuse it is incomplete
crime_data <- crime_data %>%
  filter(year(date_occurred) != 2024)

#imputing missing values in sex and weapon description column
unique(crime_data$victim_sex)

crime_data <- crime_data %>%
  mutate(
    victim_sex = ifelse(is.na(victim_sex) | victim_sex == "H" | victim_sex == "-", "X", victim_sex),
    weapon_description = ifelse(is.na(weapon_description), "Unknown", weapon_description)
  )


#Dimensionality Reduction
# Feature Selection based on the research questions
crime_data <- crime_data %>%
  select(
    date_occurred,
    time_occurred,
    area_name,
    crime_description,
    victim_age,
    victim_sex,
    location,
    weapon_description,
    status_description,
    latitude,
    longitude
  )




# Feature Engineering

get_season <- function(month) {
  case_when(
    month %in% c(12, 1, 2) ~ "Winter",
    month %in% c(3, 4, 5) ~ "Spring",
    month %in% c(6, 7, 8) ~ "Summer",
    month %in% c(9, 10, 11) ~ "Fall"
  )
}

# Add year, month, weekday, and season columns

# Define night hours
start_hour = 18
end_hour = 6

crime_data <- crime_data %>%
  mutate(
    year = year(date_occurred),
    month = month(date_occurred, label = TRUE, abbr = TRUE),
    weekday = wday(date_occurred, label = TRUE, abbr = TRUE),
    season = get_season(month(date_occurred)),
    hour_occurred = hour(time_occurred),
    part_of_day = ifelse((hour_occurred >= start_hour) | (hour_occurred < end_hour), "Night", "Day") 
  )

# Verify the structure of the modified data
str(crime_data)

unique(crime_data$part_of_day)

unique(crime_data$hour_occurred)

# Saving the cleaned dataset
# write_csv(crime_data, "cleaned_crimedata.csv")



# Descriptive Analysis
# showing some basic visualization
# Assuming your data frame is named 'crime_data' and it has a column 'year' representing the year of the crime
ggplot(crime_data, aes(x = year)) +
  geom_line(stat = "count", color = "#7f0c0f",size = 0.8) +
  geom_hline(yintercept = seq(200000, 240000, by = 10000), color = "grey", size = 0.4, linetype = "solid") +  
  labs(title = "Number of Crimes Over Time", x = "Years", y = "Number of Crimes") +
  theme_minimal() +
  theme(text = element_text(family = "sans", colour = "black"),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),  
        axis.text.x = element_text(angle = 0, vjust = 0.5, size = 13, colour = "black"),  
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 16, margin = margin(t = 10)),
        axis.title.y = element_text(size = 16, margin = margin(t = 10, r = 10)),
        axis.line.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  geom_text(stat = "count", 
            aes(label = ifelse(x != 2020, paste0(round((..count.. - lag(..count..)) / lag(..count..) * 100, 2), "%"), "")), 
            vjust = -1,hjust=0.8, size = 4, color = "#7f0c0f")



# Calculate the count of crimes for each area
crime_counts <- crime_data %>%
  group_by(area_name) %>%
  summarise(crime_count = n()) %>%
  arrange(desc(crime_count))  # Arrange by descending order of crime count

# Reorder the levels of area_name based on crime_counts
crime_data$area_name <- factor(crime_data$area_name, levels = crime_counts$area_name)

# Plotting the bar chart with ordered levels and similar style to the first plot
ggplot(crime_data, aes(x = area_name)) +
  geom_bar(fill = "#1380A1") +
  labs(title = "Number of Crimes by Area", x = "Area Name", y = "Number of Crimes") +
  theme_minimal() +
  theme(text = element_text(family = "sans", colour = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, size = 12, colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.x = element_text(size = 14, margin = margin(b = 10)),
        axis.title.y = element_text(size = 14, margin = margin(t = 10, r = 10)),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# Box plot of victim ages
ggplot(crime_data, aes(y = victim_age)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Distribution of Victim Ages", y = "Victim Age") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", colour = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


crime_sex_counts <- crime_data %>%
  filter(!is.na(victim_sex)) %>%
  count(victim_sex) %>%
  mutate(percentage = n / sum(n) * 100)

# Pie chart of crimes by victim sex with percentages
ggplot(crime_sex_counts, aes(x = "", y = percentage, fill = victim_sex)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Crimes by Victim Sex", fill = "Victim Sex", y = "Victim Sex") +
  theme_void() +
  scale_fill_brewer(palette = "Set2") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5,
            color="white") +
  theme(text = element_text(family = "Arial", colour = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "right") +
  scale_fill_discrete(labels = c("Male" = "Male", "Female" = "Female", "Unknown" = "Unknown"))



# Violin plot of victim age by area
ggplot(crime_data, aes(x = area_name, y = victim_age)) +
  geom_violin(fill = "lightgreen") +
  labs(title = "Distribution of Victim Ages by Area", x = "Area Name", y = "Victim Age") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", colour = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.title.y = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))


# Filter out "Unknown" values in weapon_description
crime_data_weapon <- crime_data %>%
  filter(weapon_description != "Unknown")

# Calculate the count of each weapon description and arrange in descending order
weapon_counts <- crime_data_weapon %>%
  count(weapon_description) %>%
  arrange(desc(n)) %>%
  slice(2:10)  # Select top 10 most used weapons

# Reorder the levels of weapon_description in descending order
weapon_counts$weapon_description <- factor(weapon_counts$weapon_description, levels = weapon_counts$weapon_description[order(weapon_counts$n, decreasing = TRUE)])

# Plot the bar chart
ggplot(weapon_counts, aes(x = weapon_description, y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = paste0(n, " m")), 
            vjust = 1.5, 
            hjust = 0.5,
            color = "black",
            fontface = 'bold',
            size = 3.2) +
  labs(title = "Top 10 Most Used Weapons excluding hand or fist", x = "Weapon Description", y = "Number of Crimes") +
  theme_minimal() +
  theme(text = element_text(family = "Arial", colour = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1),
        axis.text.y = element_blank(),
        axis.title.y = element_text(size = 14),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
  


# Calculate the count of each status description
status_counts <- crime_data %>%
  count(status_description)

# Pie chart for distribution of case statuses
ggplot(status_counts, aes(x = "", y = n, fill = reorder(status_description, n))) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Case Status", fill = "Status Description", y = "") +
  scale_fill_manual(values = brewer.pal(6, "Set2")) +
  theme_void() +
  theme(text = element_text(family = "Arial", colour = "black"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        legend.position = "right")




#Identifying Patterns and Trends
# showing through non-trival questions visualization

#1 Temporal and Spatial Trends in High-Crime Areas

# Filter data for the specified areas
selected_areas <- head(crime_counts$area_name, 5)

# Prepare the data
clustered_data1 <- crime_data %>%
  filter(area_name %in% selected_areas)

crime_summary <- clustered_data1 %>%
  group_by(area_name, year, crime_description) %>%
  summarise(crime_count = n()) %>%
  arrange(desc(crime_count))

top_crimes <- crime_summary %>%
  group_by(crime_description) %>%
  summarise(total_crime_count = sum(crime_count)) %>%
  top_n(2, total_crime_count) %>%
  pull(crime_description)

crime_summary_top <- crime_summary %>%
  filter(crime_description %in% top_crimes)

# Heatmap
ggplot(crime_summary_top, aes(x = year, y = reorder(area_name, crime_count), fill = crime_count)) +
  geom_tile(color = "white", size = 0.2) + 
  facet_wrap(~ crime_description, scales = "free_y", ncol = 1) + 
  scale_fill_viridis(option = "magma", direction = -1) + 
  labs(
    title = "Crime Concentration in Areas along Time",
    x = "Years",
    y = "Area",
    fill = "Crime Count"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(family = "sans", color = "black"), 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 13, margin = margin(t = 10)),
    axis.text.y = element_text(size = 11), 
    axis.title.x = element_text(size = 15, margin = margin(t = 20)),
    axis.title.y = element_text(size = 15),                           
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),  
    legend.title = element_text(size = 13),               
    legend.text = element_text(size = 11)                
  )

ggplot(crime_summary_top, aes(x = year, y = crime_count, color = area_name)) +
  geom_line(size = 1) + 
  facet_wrap(~ crime_description, scales = "free_y", ncol = 1) +  # Adjust ncol as needed
  labs(
    title = "Crime Trends in Areas along Time",
    x = "Years",
    y = "Number of Crimes",
    color = "Area"
  ) +
  theme_minimal() + 
  theme(
    text = element_text(family = "sans", color = "black"), 
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 0, vjust = 0.5, size = 11), 
    axis.text.y = element_text(size = 11), 
    axis.title.x = element_text(size = 13, margin = margin(b = 10)),
    axis.title.y = element_text(size = 13, margin = margin(t = 10)),                         
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(color = "black"),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "none" 
    ) +
      geom_text(
        data = crime_summary_top %>% filter(year == 2023),
        aes(label = area_name, x = year, y = crime_count, color = area_name),
        hjust = 1.1, vjust = -0.2, size = 3
      )




#2 Crime Disparities by Demographic and Time
# Define nighttime hours (e.g., 6 PM to 6 AM)
# Add "Part of Day" column to crime_data
# Finding Top Crimes by Area (Day and Night Separately)
# Combining Top Crimes for Day and Night


top_crimes_combined <- crime_data %>%
  group_by(area_name, crime_description, part_of_day) %>%
  summarise(crime_count = n()) %>%
  arrange(area_name, desc(crime_count)) %>%
  group_by(area_name, part_of_day) %>%
  top_n(1, crime_count) %>%
  ungroup()

# Treemap Visualization (Facets for Day & Night)
treemap(
  top_crimes_combined,
  index = c("area_name", "part_of_day", "crime_description"),
  vSize = "crime_count",
  title = "Trend of Top Crimes in Areas by Day/Night",
  palette = "Set3",
  fontsize.labels = c(8, 8, 7),  # Increase font size for all levels
  fontfamily.labels = "sans",
  fontface.labels = c(2, 1, 1),  # Bold area names (level 1)
  bg.labels = "transparent",
  align.labels = list(
    c("left", "top"),  
    c("center", "bottom"),  
    c("left", "center")
  ),
  border.col = c("white", "grey", "grey"),  
  border.lwds = c(1, 0.5, 0.5),  
  overlap.labels = 0.5,
)




# Stacked Bar Plot (Day & Night)
victim_demographics <- crime_data %>%
  mutate(age_group = case_when(
    victim_age < 18 ~ "<18",
    victim_age >= 18 & victim_age < 30 ~ "18-29",
    victim_age >= 30 & victim_age < 40 ~ "30-39",
    victim_age >= 40 & victim_age < 50 ~ "40-49",
    victim_age >= 50 & victim_age < 60 ~ "50-59",
    victim_age >= 60 ~ "60+"
  )) %>%
  group_by(age_group, victim_sex, part_of_day) %>%
  summarise(victim_count = n()) %>%
  arrange(part_of_day, age_group, desc(victim_count)) 

ggplot(victim_demographics, aes(x = age_group, y = victim_count, fill = victim_sex)) +
  geom_bar(stat = "identity", color = "black", width = 0.7, position = "stack") + # Stacked bars
  facet_wrap(~ part_of_day) +
  labs(
    title = "Victim Demographics by Age and Sex (Day & Night)",
    x = "Age Group",
    y = "Count",
    fill = "Gender"
  ) +
  scale_fill_manual(
    values = viridis_pal(option = "D")(3),
    labels = c("F" = "Female", "M" = "Male", "X" = "Unknown")
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "sans", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, margin = margin(t = 20)),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.grid.major = element_blank()
  )



#3.

# Aggregate the crime counts by month
crime_counts_month <- crime_data %>% 
  group_by(year, month) %>%
  summarize(Total_Crimes = n(), .groups = 'drop')

# Plot the monthly crime trend
ggplot(crime_counts_month, aes(x = month, y = Total_Crimes, color = factor(year), group = year)) +
  geom_line(size = 1) +  
  labs(
    title = "Crime Trends Over Time (Monthly)", 
    x = "Month", 
    y = "Number of Crimes" 
  ) +
  scale_x_discrete(labels = month.abb) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  ) +
  geom_text(
    data = crime_counts_month %>% filter(month == "Dec"), # Filter for December data
    aes(label = year, x = month, y = Total_Crimes, color = factor(year)), 
    hjust = 0.1, vjust = -0.9, size = 4 
  )

# Aggregate the crime counts by season and year
crime_counts_season <- crime_data %>%
  group_by(year, season) %>%
  summarize(Total_Crimes = n(), .groups = 'drop')

# Plot the seasonal crime trend
ggplot(crime_counts_season, aes(x = year, y = Total_Crimes, color = season, group = season)) +
  geom_line(size = 1) +
  geom_point(size = 1) + 
  labs(
    title = "Seasonal Crime Trends Over Time",
    x = "Year",
    y = "Total Crimes",
    color = "Season"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_text(size = 13),
    legend.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  ) + 
  geom_text(
    data = crime_counts_season %>% filter(year == 2021 & season %in% c("Fall", "Summer")),  
    aes(label = season, x = year, y = Total_Crimes, color = season),
    hjust = 0, vjust = -0.5, size = 4 
  ) +
  geom_text(
    data = crime_counts_season %>% filter(year == 2020 & season %in% c("Winter", "Spring")), 
    aes(label = season, x = year, y = Total_Crimes, color = season),
    hjust = 0, vjust = -0.5, size = 4 
  )


# Aggregate crime counts by year and weekday
crime_counts_weekday <- crime_data %>%
  group_by(year, weekday) %>%
  summarize(Crime_Count = n(), .groups = 'drop')

# Plot crime distribution by day of week divided by year
ggplot(crime_counts_weekday, aes(x = weekday, y = Crime_Count)) +
  geom_col(fill = "#3182bd", color = "black", size = 0.5, width = 0.7) + # Adjusted bar width
  facet_wrap(~ year, ncol = 2, scales = "free_y") +  # Use free_y for better scaling
  labs(
    title = "Crime Distribution by Day of Week",
    x = "Day of Week",
    y = "Number of Crimes"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.2, "lines") # Add spacing between facets 
  )


# Aggregate crime counts by hour and year
crime_counts_hour <- crime_data %>%
  group_by(year, hour_occurred) %>%
  summarize(Crime_Count = n(), .groups = 'drop')

# Plot crime distribution by hour divided by year
ggplot(crime_counts_hour, aes(x = hour_occurred, y = Crime_Count)) +
  geom_col(fill = "#3182bd", color = "black", size = 0.5, width = 0.7) + 
  facet_wrap(~ year, ncol = 2, scales = "free_y") +  
  labs(
    title = "Crime Distribution by Hour of Day",
    x = "Hour of Day",
    y = "Number of Crimes"
  ) +
  scale_x_continuous(breaks = seq(0, 23, by = 3)) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
    axis.title = element_text(size = 14),
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid = element_blank(),
    panel.spacing = unit(1.2, "lines") 
  ) 



#4. 

# Filter and prepare the data
crime_data_filtered <- crime_data %>%
  select(date_occurred, location, crime_description) %>%
  mutate(date = as.Date(date_occurred)) %>%
  group_by(date, location) %>%
  summarize(crime_types = paste(unique(crime_description), collapse = ","), .groups = 'drop')

# Convert the crime types into a list of transactions
transactions_list <- strsplit(crime_data_filtered$crime_types, ",")

# Create a transactions object
transactions <- as(transactions_list, "transactions")

# Apply the Apriori algorithm
rules <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.8))

print(head(transactions))


top_crimes <- sort(table(unlist(transactions_list)), decreasing = TRUE)[1:5]

print(top_crimes)

# Filter rules to include only the top 5 crimes

rules_top <- subset(rules, lhs %in% names(top_crimes) | rhs %in% names(top_crimes))

rules_df <- data.frame(
  Antecedent = labels(lhs(rules_top)),
  Consequent = labels(rhs(rules_top)),
  Support = rules_top@quality$support,
  Confidence = rules_top@quality$confidence,
  Lift = rules_top@quality$lift
)
# Plot heatmap

ggplot(rules_df, aes(x = Antecedent, y = Consequent, fill = Lift)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Association Rules Heatmap (Top 5 Crimes)",
       x = "If This Crime Occurs (Antecedent)",
       y = "Then This Crime Is Likely (Consequent)",
       fill = "Lift (Strength of Association)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(family = "Arial")
  )



#5.

# 1. Filter data for the Central area
central_area_data <- crime_data %>%
  filter(area_name == "Central")

# Filter for top weapon types (excluding "Unknown")
top_weapons <- central_area_data %>%
  filter(!is.na(weapon_description) & !(weapon_description %in% "Unknown")) %>%
  count(weapon_description) %>%
  top_n(5, n) %>% 
  pull(weapon_description)

# Filter data for top weapons and Central area
filtered_data <- central_area_data %>%
  filter(weapon_description %in% top_weapons)


# Box Plot
ggplot(filtered_data, aes(x = weapon_description, y = victim_age)) +
  geom_boxplot(fill = "lightblue", color = "black", outlier.color = "red") +
  labs(
    title = "Distribution of Victim Age by Weapon Used (Central Area)",
    x = "Weapon Used",
    y = "Victim Age"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.major.x = element_blank()  # Remove vertical grid lines
  )

6.

# Filter for the top 10 most frequent crime types
top_crimes <- crime_data %>%
  count(crime_description) %>%
  top_n(6, n) %>%
  pull(crime_description)

# Filter and group the data
crime_status_data <- crime_data %>%
  filter(crime_description %in% top_crimes) %>%
  group_by(crime_description, status_description) %>%
  summarize(count = n(), .groups = 'drop') 
# Reorder levels of status_description within each crime_description group
crime_status_data <- crime_status_data %>%
  arrange(crime_description, desc(count)) %>%
  mutate(status_description = factor(status_description, levels = unique(status_description)))


# Faceted Bar Plot
ggplot(crime_status_data, aes(x = status_description, y = count, fill = status_description)) +
  geom_bar(stat = "identity", color = "black", size = 0.3, width = 0.7) +  
  facet_wrap(~ crime_description, scales = "free_y", ncol = 2) + 
  labs(
    x = "Status Description", 
    y = "Count", 
    fill = "Status Description",
    title = "Distribution of Case Statuses Across Top 9 Crime Types"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Arial", color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 13),
    strip.text = element_text(size = 8, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.spacing = unit(1.2, "lines") # Add spacing between facets
  )




#7. Grouping data by area and calculating the mean(latitude & longitude) for each area, counts per area, and the most common crime description
district_crime_counts <- crime_data %>%
  group_by(area_name) %>%
  summarise(
    latitude = mean(latitude, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),
    counts = n(),
    common_crime = names(sort(table(crime_description), decreasing = TRUE)[1])
  ) %>%
  ungroup()

# Verify the data
print(district_crime_counts)

# Map Plotting
fig <- plot_ly(
  data = district_crime_counts,
  lat = ~latitude,
  lon = ~longitude,
  size = ~counts,
  color = ~counts,
  colors = colorRamp(c("blue", "red")),  # Using a custom color ramp
  text = ~paste("Area:", area_name, "<br>Counts:", counts, "<br>Common Crime:", common_crime, "<br>Latitude:", latitude, "<br>Longitude:", longitude),
  hoverinfo = "text",
  type = 'scattermapbox',
  mode = 'markers',
  marker = list(sizeref = 0.1, sizemode = 'area'),
  height = 750,
  width = 1200
) %>%
  layout(
    title = "Spatial Clustering of Crime Counts by District",
    mapbox = list(
      style = "open-street-map",
      zoom = 9,  # Adjust zoom level for better focus
      center = list(lat = 33.922, lon = -117.9437)  # Centering on Los Angeles
    )
  )

# Display the map
fig




