
# Load necessary libraries
library(dplyr)
library(nycflights13)
library(ggplot2)
library(zoo)

# Preview the Star Wars dataset
data("starwars")
head(starwars)

# Task 1: Select Specific Columns, Filter Rows, and Arrange Data
# Selecting specific columns (name, species, height, mass), filtering out missing species,
# and arranging by height in descending order
starwars_filtered <- starwars %>% 
  dplyr::select(name, species, height, mass) %>%
  dplyr::filter(!is.na(species) & !is.na(height) & height > 100) %>%
  dplyr::arrange(desc(height)) 

# Display the filtered data
head(starwars_filtered)

# Plotting the filtered data
ggplot(starwars_filtered, aes(x = reorder(name, -height), y = height, fill = species)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Height of Star Wars Characters", x = "Character", y = "Height (cm)") +
  theme_minimal()

# Task 2: Group Data by Species and Summarize
# Grouping by species, calculating average height and mass, and counting observations
species_summary <- starwars %>%
  group_by(species) %>%
  summarize(
    avg_height = mean(height, na.rm = TRUE),  # Compute average height
    avg_mass = mean(mass, na.rm = TRUE),  # Compute average mass
    count = n()  # Count the number of observations
  ) %>%
  arrange(desc(count))  # Sort by count

# Display the summary
head(species_summary)

# Plotting average height by species
ggplot(species_summary, aes(x = reorder(species, -avg_height), y = avg_height, fill = species)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Average Height by Species", x = "Species", y = "Average Height (cm)") +
  theme_minimal()

# Task 3: Create a New Column Using mutate()
# Adding a new column that classifies characters based on height
starwars_classified <- starwars %>%
  mutate(height_category = ifelse(height > 180, "Tall", "Short"))  # Conditional classification

# Preview the new column
head(starwars_classified)

# Plotting height category distribution
ggplot(starwars_classified, aes(x = height_category, fill = height_category)) +
  geom_bar() +
  labs(title = "Distribution of Height Categories", x = "Height Category", y = "Count") +
  theme_minimal()

# Task 4: Join Two Data Frames Using Inner and Outer Joins
# Using the NYC Flights dataset for demonstration
data("flights")
data("airlines")

# Inner join: Merging flights with airlines on the common column 'carrier'
flights_inner_join <- flights %>%
  inner_join(airlines, by = "carrier")

# Outer join: Performing a full join on flights and airlines
flights_outer_join <- flights %>%
  full_join(airlines, by = "carrier")

# Display the joined data
head(flights_inner_join)
head(flights_outer_join)

# Task 5: Compute Rolling Averages and Cumulative Sums
# Calculating a 5-period rolling average of flight arrival delay and cumulative sum


# Compute rolling averages and cumulative sums again (correcting missing data handling)
flights_rolling <- flights %>%
  arrange(year, month, day) %>%
  mutate(
    arr_delay = ifelse(is.na(arr_delay), 0, arr_delay),  # Replace NA with 0 for cumulative sum
    rolling_avg_delay = rollmean(arr_delay, 5, fill = NA),  # 5-period rolling average
    cumulative_delay = cumsum(arr_delay)  # Cumulative sum of delays
  )

# Display the transformed data
head(flights_rolling)

# Plotting Rolling Average and Cumulative Delay
ggplot(flights_rolling, aes(x = day)) +
  geom_line(aes(y = rolling_avg_delay, color = "Rolling Average Delay")) +
  geom_line(aes(y = cumulative_delay / 1000, color = "Cumulative Delay (x1000)")) +
  labs(title = "Rolling Average and Cumulative Delay of Flights",
       x = "Day of the Month", y = "Delay (minutes)") +
  scale_color_manual(values = c("Rolling Average Delay" = "blue", "Cumulative Delay (x1000)" = "red")) +
  theme_minimal()
