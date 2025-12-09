## LAB-1

Write an R script that performs the following tasks: . Prompt the user to input three values representing the lengths of the sides of a triangle. . Validate the inputs to ensure they are positive numbers.
. Check if the given sides can form a valid triangle (using the triangle inequality theorem). . Write a
function to determine whether the triangle is equilateral, isosceles, or scalene. . Compute the area of
the triangle using Heron's formula. . Handle any invalid input scenarios gracefully with appropriate
error messages. . Print the type of triangle and its area, if valid.


```{r}
is_valid_triangle <- function(a,b,c){
return((a+b>c)& (b+c>a)& (a+c>b))
}
triangle_type <- function(a,b,c){
if (a==b && b==c){
return("Equilateral")
} else if(a==b || b==c || a==c){
return("Isosceles")
} else{
return("Scalene")
}
}
triangle_area <- function(a,b,c){
s <- (a+b+c) /2
area <- sqrt(s*(s-a)*(s-b)*(s-c))
return(area)
}
validate_input <- function(x){
if(!is.numeric(x) || x<=0){
stop("Error: input must be positive")
}
return(TRUE)
}
cat("Enter the lengths of the side of the triangle:\n")
a <- 3
b <- 4
c <- 5
tryCatch({
validate_input(a)
validate_input(b)
validate_input(c)
if(!is_valid_triangle(a,b,c)){
stop("Error: The given sides do not form a valid triangle")
}
type_of_triangle <- triangle_type(a,b,c)
cat("The triangle is:", type_of_triangle, "\n")
area_of_triangle <- triangle_area(a,b,c)
cat("area is:", area_of_triangle,"\n"
)
}, error= function(e){
cat(e$message, "\n")
})

```
\newpage
\section*{\centering Program - 2}

## Program 2
The script generates a random vector, sorts it, and filters values based on conditions. It performs matrix operations like multiplication and element-wise multiplication, and modifies a list of mixed data types. Additionally, it creates a data frame, applies filters, calculates summary statistics, handles missing values, and groups data by a factor for further analysis.

### Program Code
Below is the R script designed for interactive execution in the R console:
```{r }

# Create a vector of random numbers and apply operations such as sorting and searching
set.seed(42) # For reproducibility
random_vector <- runif(20, min = 1, max = 100) # Vector of 20 random numbers between 1 and 100
cat("Original random vector:\n")
print(random_vector)

# Sort the vector
sorted_vector <- sort(random_vector)
cat("Sorted vector:\n")
print(sorted_vector)

# Search for a specific value (check if a number is present)
search_value <- 50
is_value_present <- any(random_vector == search_value)
cat("Is", search_value, "present in the vector? ", is_value_present, "\n")

# Find values in the vector greater than 60
values_greater_than_60 <- random_vector[random_vector > 60]
cat("Values greater than 60:\n")
print(values_greater_than_60)

# Convert the vector into a matrix and perform matrix multiplication
matrix_from_vector <- matrix(random_vector, nrow = 4, ncol = 5)
cat("Matrix from vector:\n")
print(matrix_from_vector)

# Perform matrix multiplication (matrix with its transpose)
matrix_transpose <- t(matrix_from_vector)
matrix_multiplication_result <- matrix_from_vector %*% matrix_transpose
cat("Matrix multiplication result:\n")
print(matrix_multiplication_result)

# Element-wise matrix multiplication (Hadamard product)
elementwise_multiplication_result <- matrix_from_vector * matrix_from_vector
cat("Element-wise matrix multiplication result:\n")
print(elementwise_multiplication_result)

# Create a list containing different types of elements and perform subsetting
my_list <- list(
  numbers = random_vector,
  characters = c("A", "B", "C", "D"),
  logical_values = c(TRUE, FALSE, TRUE),
  matrix = matrix_from_vector
)
cat("List:\n")
print(my_list)

# Subsetting the list (extracting numeric and logical parts)
cat("Subset (numeric part of the list):\n")
print(my_list$numbers)

cat("Subset (logical part of the list):\n")
print(my_list$logical_values)

# Modify elements in the list (replace the second character with "Z")
my_list$characters[2] <- "Z"
cat("Modified list of characters:\n")
print(my_list$characters)

# Apply a function to the numeric part of the list (e.g., calculate the square of the numbers)
squared_numbers <- my_list$numbers ^ 2
cat("Squared numbers:\n")
print(squared_numbers)

# Create a data frame, apply filtering based on multiple conditions, and calculate summary statistics
df <- data.frame(
  ID = 1:20,
  Age = sample(18:65, 20, replace = TRUE),
  Score = runif(20, min = 50, max = 100),
  Passed = sample(c(TRUE, FALSE), 20, replace = TRUE)
)
cat("Data frame:\n")
print(df)

# Filter the data frame (rows where Age > 30 and Score > 70)
filtered_df <- subset(df, Age > 30 & Score > 70)
cat("Filtered data frame (Age > 30 and Score > 70):\n")
print(filtered_df)

# Calculate mean, sum, and variance of numerical columns (Age and Score)
mean_age <- mean(df$Age)
sum_age <- sum(df$Age)
var_age <- var(df$Age)

mean_score <- mean(df$Score)
sum_score <- sum(df$Score)
var_score <- var(df$Score)

cat("Summary statistics for Age column:\n")
print(c(Mean = mean_age, Sum = sum_age, Variance = var_age))

cat("Summary statistics for Score column:\n")
print(c(Mean = mean_score, Sum = sum_score, Variance = var_score))

# Handling missing values in the data frame
df$Score[sample(1:20, 5)] <- NA
cat("Data frame with missing values:\n")
print(df)

# Replace NA values with the mean of the Score column
df$Score[is.na(df$Score)] <- mean(df$Score, na.rm = TRUE)
cat("Data frame after imputation of missing values:\n")
print(df)

# Grouping the data by Passed status and calculating group-wise statistics
library(dplyr)
grouped_stats <- df %>%
  group_by(Passed) %>%
  summarise(
    mean_score = mean(Score, na.rm = TRUE),
    mean_age = mean(Age)
  )
cat("Grouped statistics by Passed status:\n")
print(grouped_stats)

```

\newpage
\section*{\centering Program - 3}
## Program 3: Basic Statistical Operations on Open-Source Datasets

### Objective: This program emphasizes the application of statistical concepts on real-world datasets and visual-
ization of the data.

Write an R script to perform the following statistical operations:
• Calculate mean, median, mode, variance, and standard deviation.
• Perform skewness and kurtosis analysis.
• Conduct a hypothesis test (t-test) to compare means of two groups.
• Visualize the distributions using histograms and boxplots.
Datasets:
• Iris dataset (available in R by default)
• Palmer Penguins dataset (palmerpenguins package)

```{r}
library(dplyr)
library(ggplot2)
library(moments)
library(palmerpenguins)

data(iris)
data(penguins)

calc_mode <- function(x) {
  return(as.numeric(names(sort(table(x), decreasing = TRUE))[1]))
}

print("----- Iris Dataset Analysis -----")

iris_mean <- sapply(iris[, 1:4], mean, na.rm = TRUE)
print(paste("Mean of Iris dataset:", iris_mean))

iris_median <- sapply(iris[, 1:4], median, na.rm = TRUE)
print(paste("Median of Iris dataset:", iris_median))

iris_mode <- sapply(iris[, 1:4], calc_mode)
print(paste("Mode of Iris dataset:", iris_mode))

iris_variance <- sapply(iris[, 1:4], var, na.rm = TRUE)
print(paste("Variance of Iris dataset:", iris_variance))

iris_sd <- sapply(iris[, 1:4], sd, na.rm = TRUE)
print(paste("Standard Deviation of Iris dataset:", iris_sd))

iris_skewness <- sapply(iris[, 1:4], skewness, na.rm = TRUE)
print(paste("Skewness of Iris dataset:", iris_skewness))

iris_kurtosis <- sapply(iris[, 1:4], kurtosis, na.rm = TRUE)
print(paste("Kurtosis of Iris dataset:", iris_kurtosis))

setosa <- subset(iris, Species == "setosa")$Sepal.Length
versicolor <- subset(iris, Species == "versicolor")$Sepal.Length
t_test <- t.test(setosa, versicolor)
print(t_test)

ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(binwidth = 0.3, fill = "blue", color = "black") +
  ggtitle("Histogram of Sepal Length in Iris Dataset")

ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  ggtitle("Boxplot of Sepal Length by Species in Iris Dataset")

print("----- Palmer Penguins Dataset Analysis -----")

penguins_clean <- na.omit(penguins)

penguins_mean <- sapply(penguins_clean[, 3:6], mean, na.rm = TRUE)
print(paste("Mean of Palmer Penguins dataset:", penguins_mean))

penguins_median <- sapply(penguins_clean[, 3:6], median, na.rm = TRUE)
print(paste("Median of Palmer Penguins dataset:", penguins_median))

penguins_mode <- sapply(penguins_clean[, 3:6], calc_mode)
print(paste("Mode of Palmer Penguins dataset:", penguins_mode))

penguins_variance <- sapply(penguins_clean[, 3:6], var, na.rm = TRUE)
print(paste("Variance of Palmer Penguins dataset:", penguins_variance))

penguins_sd <- sapply(penguins_clean[, 3:6], sd, na.rm = TRUE)
print(paste("Standard Deviation of Palmer Penguins dataset:", penguins_sd))

penguins_skewness <- sapply(penguins_clean[, 3:6], skewness, na.rm = TRUE)
print(paste("Skewness of Palmer Penguins dataset:", penguins_skewness))

penguins_kurtosis <- sapply(penguins_clean[, 3:6], kurtosis, na.rm = TRUE)
print(paste("Kurtosis of Palmer Penguins dataset:", penguins_kurtosis))

adelie <- subset(penguins_clean, species == "Adelie")$flipper_length_mm
gentoo <- subset(penguins_clean, species == "Gentoo")$flipper_length_mm
t_test_penguins <- t.test(adelie, gentoo)
print(t_test_penguins)

ggplot(penguins_clean, aes(x = flipper_length_mm)) +
  geom_histogram(binwidth = 3, fill = "green", color = "black") +
  ggtitle("Histogram of Flipper Length in Palmer Penguins Dataset")

ggplot(penguins_clean, aes(x = species, y = flipper_length_mm, fill = species)) +
  geom_boxplot() +
  ggtitle("Boxplot of Flipper Length by Species in Palmer Penguins Dataset")


```
\newpage
\section*{\centering Program - 4}
## Program 4: Data Import, Cleaning, and Export with Advanced Data Wrangling

### Objective: This program introduces students to real-world data cleaning processes and emphasizes the importance of data wrangling techniques in R.

Design an R program that:
• Imports a dataset from a CSV file.
• Handles missing data by filling NA values with median or mode.
• Removes outliers based on z-scores.
• Summarizes the dataset before and after cleaning (mean, median, standard deviation, and correlation
matrix).
• Exports the cleaned dataset to a new CSV file.
Datasets:
• Titanic dataset (available in titanic package)
• Adult income dataset (available on UCI Machine Learning Repository)



```{r}
# Program 4: Data Import, Cleaning, and Export with Advanced Data Wrangling
# Wrapped output-friendly version for RMarkdown (PDF safe)

library(tidyverse)    # For data manipulation
library(titanic)      # Titanic dataset
library(dplyr)        # Data manipulation
library(caret)        # For z-score based outlier detection
library(ggcorrplot)   # For correlation plot

# ------------------- TITANIC DATASET -------------------

data <- titanic::titanic_train

# Handle missing values
data$Age[is.na(data$Age)] <- median(data$Age, na.rm = TRUE)
mode_embarked <- names(sort(table(data$Embarked), decreasing = TRUE))[1]
data$Embarked[is.na(data$Embarked)] <- mode_embarked

# Detect numeric columns and remove outliers using z-scores
numeric_columns <- sapply(data, is.numeric)
z_scores <- as.data.frame(scale(data[, numeric_columns]))
outlier_condition <- apply(z_scores, 1, function(row) any(abs(row) > 3))
data_clean <- data[!outlier_condition, ]

# Summaries before and after cleaning
summary_before <- summary(titanic::titanic_train)
summary_after <- summary(data_clean)

# Correlation matrix
correlation_matrix <- cor(data_clean[, numeric_columns], use = "complete.obs")

# Export cleaned data
write.csv(data_clean, "cleaned_titanic_data.csv", row.names = FALSE)

cat("\n--- Titanic Dataset Summary Before Cleaning ---\n")
print(format(summary_before, justify = "left"))

cat("\n--- Titanic Dataset Summary After Cleaning ---\n")
print(format(summary_after, justify = "left"))

cat("\n--- Correlation Matrix (Titanic Dataset) ---\n")
print(round(correlation_matrix, 3))

ggcorrplot(
  correlation_matrix,
  method = "circle",
  lab = TRUE,
  title = "Correlation Matrix of Titanic Dataset"
)

# ------------------- ADULT INCOME DATASET -------------------

library(tidyverse)
library(dplyr)
library(caret)
library(ggcorrplot)

# Import data
data <- read.csv("C:\\Users\\tchir\\Downloads\\adult.data", header = FALSE)

colnames(data) <- c(
  'age', 'workclass', 'fnlwgt', 'education', 'education_num',
  'marital_status', 'occupation', 'relationship', 'race', 'sex',
  'capital_gain', 'capital_loss', 'hours_per_week', 'native_country', 'income'
)

# Replace '?' with NA
data[data == '?'] <- NA

# Mode replacement function
replace_mode <- function(x) {
  mode_value <- names(sort(table(x), decreasing = TRUE))[1]
  x[is.na(x)] <- mode_value
  return(x)
}

# Impute missing values
data <- data %>%
  mutate_if(is.character, replace_mode) %>%
  mutate_if(is.numeric, ~ ifelse(is.na(.), median(., na.rm = TRUE), .))

# Remove outliers using z-score filtering
numeric_columns <- sapply(data, is.numeric)
data_clean <- data %>%
  filter(!apply(as.data.frame(scale(data[, numeric_columns])), 1, function(row) any(abs(row) > 3)))

# Summaries before and after cleaning
summary_before <- summary(read.csv("C:\\Users\\tchir\\Downloads\\adult.data", header = FALSE))
summary_after <- summary(data_clean)

# Correlation matrix
correlation_matrix <- cor(data_clean[, numeric_columns], use = "complete.obs")

# Export cleaned dataset
write.csv(data_clean, "cleaned_adult_income_data.csv", row.names = FALSE)

cat("\n--- Adult Income Dataset Summary Before Cleaning ---\n")
print(format(summary_before, justify = "left"))

cat("\n--- Adult Income Dataset Summary After Cleaning ---\n")
print(format(summary_after, justify = "left"))

cat("\n--- Correlation Matrix (Adult Income Dataset) ---\n")
print(round(correlation_matrix, 3))

ggcorrplot(
  correlation_matrix,
  method = "circle",
  lab = TRUE,
  title = "Correlation Matrix of Adult Income Dataset"
)

```
\newpage
\section*{\centering Program - 5}
## Program 5: Advanced Data Manipulation with dplyr and Complex Grouping

### Objective: The goal of this program is to test advanced data manipulation techniques using the dplyr package. Using the dplyr package, perform the following:
. Select specific columns, filter rows based on multiple conditions, and arrange data.
. Group data by one or more variables and summarize it.
. Create a new column using mutate() to compute conditional values.
. Join two data frames using inner and outer joins.
. Compute rolling averages and cumulative sums.
Datasets:
. Star Wars dataset (available in dplyr package)
. NYC Flights dataset (nycflights13 package)



```{r}
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


```
\newpage
\section*{\centering Program - 6}
## Program 6: Data Visualization with ggplot2 and Customizations

### Objective: This program evaluates students' ability to create and customize complex data visualizations using the ggplot2 package.
Create the following visualizations using ggplot2:
. Scatter plot with regression lines and confidence intervals.
. Multi-panel plot using faceting.
. Heatmap of correlation matrix.
. Customize plot aesthetics, including colors, fonts, and themes.
. Annotate plots with text and save the visualizations as image files.
Datasets:
. MPG dataset (available in ggplot2 package)
. Diamonds dataset (available in ggplot2 package)

```{r}
# Program 6: Data Visualization with ggplot2 and Customizations
# Wrapped output version for RMarkdown PDF rendering

library(ggplot2)
library(reshape2)
library(dplyr)

# -------------------- MPG Dataset --------------------
data("mpg")

# Scatter plot with regression line
cat("\n--- Scatter Plot: Engine Displacement vs Highway MPG ---\n")
p1 <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, alpha = 0.7) + 
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "black", size = 1) + 
  labs(
    title = "Scatter Plot of Engine Displacement vs Highway MPG with Regression Line",
    x = "Engine Displacement (L)",
    y = "Highway Miles per Gallon",
    color = "Vehicle Class"
  ) +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    legend.position = "bottom"
  )
print(p1)

# Faceted plot by class
cat("\n--- Faceted Scatter Plot by Vehicle Class ---\n")
p2 <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "darkgreen", size = 2) +
  facet_wrap(~ class, ncol = 3) + 
  labs(
    title = "Faceted Scatter Plot by Vehicle Class",
    x = "Engine Displacement (L)",
    y = "Highway Miles per Gallon"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "italic"), 
    plot.title = element_text(hjust = 0.5, size = 16)
  )
print(p2)

# -------------------- DIAMONDS DATASET --------------------
data("diamonds")

# Compute correlation matrix
cor_matrix <- cor(diamonds[, sapply(diamonds, is.numeric)], use = "complete.obs")

cat("\n--- Correlation Matrix for Diamonds Dataset ---\n")
print(format(round(cor_matrix, 3), justify = "left"))

# Melt for ggplot heatmap
cor_data <- melt(cor_matrix)

# Heatmap visualization
p3 <- ggplot(cor_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") + 
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab", name = "Correlation"
  ) +
  labs(
    title = "Heatmap of Correlation Matrix for Diamonds Dataset",
    x = "Variables", y = "Variables"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16)
  )
print(p3)

# -------------------- Custom Aesthetic Enhancements --------------------
cat("\n--- Customized Scatter Plot with Aesthetic Enhancements ---\n")
p4 <- ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point(size = 3, shape = 21, fill = "lightblue", alpha = 0.8) +
  theme_light() + 
  scale_color_brewer(palette = "Set2") + 
  labs(
    title = "Customized Scatter Plot with Aesthetic Enhancements",
    x = "Engine Displacement (L)",
    y = "Highway Miles per Gallon",
    color = "Class"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.background = element_rect(fill = "gray90")
  )
print(p4)

# -------------------- Annotated Plot --------------------
cat("\n--- Annotated Scatter Plot with Highlighted Zone ---\n")
annotated_plot <- ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "purple", size = 3) +
  annotate("text", x = 4, y = 40, label = "High Efficiency Zone",
           color = "red", size = 5, fontface = "bold", angle = 15) +
  annotate("rect", xmin = 2, xmax = 4, ymin = 30, ymax = 45,
           alpha = 0.2, fill = "yellow", color = "orange") + 
  labs(
    title = "Annotated Scatter Plot with Highlighted Zone",
    x = "Engine Displacement (L)",
    y = "Highway Miles per Gallon"
  ) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
print(annotated_plot)

# Save annotated plot to file
ggsave("annotated_scatter_plot_expanded.png", annotated_plot, width = 10, height = 8, dpi = 300)
cat("\nPlot saved as 'annotated_scatter_plot_expanded.png'\n")

```


\newpage
\section*{\centering Program - 7}
## Program 7: Linear and Multiple Regression Analysis with Interaction Terms

### Objective:  This program focuses on regression modeling, interaction effects, and model diagnostics.
Write an R script to:
• Fit a simple linear regression model and interpret the results.
• Extend to multiple linear regression with interaction terms.
• Evaluate model performance using adjusted R2
, AIC, and BIC criteria.
• Perform model diagnostics with residual plots and Q-Q plots.
• Use cross-validation to assess model accuracy.
Datasets:
• Boston Housing dataset (MASS package)
• Auto MPG dataset (UCI Machine Learning Repository)

```{r}
# Load Required Libraries
library(MASS)
library(ggplot2)
library(caret)
library(car)
library(pROC)
library(dplyr)
library(corrplot)

# Load the Boston Housing dataset
data("Boston")
head(Boston)

# Preprocessing
sum(is.na(Boston))
summary(Boston)
boxplot(Boston$medv, main = "Boxplot of Median Value of Homes (medv)")
Boston <- Boston %>% filter(medv < 50)

# Feature Selection
cor_matrix <- cor(Boston)
corrplot(cor_matrix, method = "circle")

# Simple Linear Regression
simple_model <- lm(medv ~ lstat, data = Boston)
summary(simple_model)

# Multiple Linear Regression with Interaction Terms
multiple_model <- lm(medv ~ lstat * rm, data = Boston)
summary(multiple_model)

# Model Performance Evaluation
adjusted_R2 <- summary(multiple_model)$adj.r.squared
AIC_value <- AIC(multiple_model)
BIC_value <- BIC(multiple_model)

cat("Adjusted R^2:", adjusted_R2, "\n")
cat("AIC:", AIC_value, "\n")
cat("BIC:", BIC_value, "\n")

# Model Diagnostics
plot(multiple_model, which = 1, main = "Residuals vs Fitted Plot")
plot(multiple_model, which = 2, main = "Normal Q-Q Plot")

# Cross-Validation
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(medv ~ lstat * rm, data = Boston,
                  method = "lm",
                  trControl = train_control)
print(cv_model)

# ROC Curve Analysis
Boston$medv_class <- ifelse(Boston$medv >= 25, 1, 0)
logistic_model <- glm(medv_class ~ lstat * rm, data = Boston, family = "binomial")
summary(logistic_model)

pred_probs <- predict(logistic_model, type = "response")
roc_curve <- roc(Boston$medv_class, pred_probs)

plot(roc_curve, main = "ROC Curve for Logistic Regression Model", col = "blue")
abline(a = 0, b = 1, lty = 2, col = "red")
cat("AUC:", auc(roc_curve), "\n")

```

\newpage
\section*{\centering Program - 8}
## Program 8: K-Means Clustering and PCA for Dimensionality Reduction

### Objective:  This program tests the student’s knowledge of clustering techniques and dimensionality reduction through PCA.
Perform the following:
• Normalize the dataset and apply Principal Component Analysis (PCA).
• Use k-means clustering and determine the optimal number of clusters using the Elbow method and Silhouette
score.
• Visualize the clusters in the reduced PCA dimensions.
• Interpret the results and discuss potential applications.
Datasets:
• Wine dataset (rattle package)
• Breast Cancer Wisconsin dataset (UCI Machine Learning Repository)


```{r}
# Load required libraries
#install.packages("rattle")
library(rattle)       # For Wine dataset
library(ggplot2)      # For visualization
library(cluster)      # For silhouette scores
#install.packages("factoextra")
library(factoextra)   # For PCA and clustering visualization

# Normalize function
normalize <- function(data) {
  return((data - min(data)) / (max(data) - min(data)))
}

# Step 1: Load Wine dataset and normalize
wine <- wine
wine_data <- wine[, -1]  # Remove the class label
wine_norm <- as.data.frame(lapply(wine_data, normalize))

# Step 2: Apply PCA
wine_pca <- prcomp(wine_norm, scale. = TRUE)
summary(wine_pca)

# Reduce to top 2 principal components
wine_pca_data <- as.data.frame(wine_pca$x[, 1:2])

# Step 3: Determine the optimal number of clusters (Elbow method)
elbow_wine <- fviz_nbclust(wine_pca_data, kmeans, method = "wss")
print(elbow_wine)

# Step 4: Silhouette analysis
silhouette_wine <- fviz_nbclust(wine_pca_data, kmeans, method = "silhouette")
print(silhouette_wine)

# Step 5: Apply K-means clustering
set.seed(123)
wine_kmeans <- kmeans(wine_pca_data, centers = 3, nstart = 25)

# Add cluster assignments to PCA data
wine_pca_data$cluster <- as.factor(wine_kmeans$cluster)

# Step 6: Visualize clusters
p1 <- ggplot(wine_pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering on Wine Dataset")
print(p1)

# Step 7: Interpret results
cat("Wine Dataset Clustering Results:\n")
cat("Cluster Sizes:", wine_kmeans$size, "\n")

# Step 8: Repeat for Breast Cancer Wisconsin dataset
# Load dataset from UCI repository
bc_data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data", header = FALSE)
bc_features <- bc_data[, -c(1, 2)]  # Exclude ID and class columns
bc_norm <- as.data.frame(lapply(bc_features, normalize))

# Apply PCA
bc_pca <- prcomp(bc_norm, scale. = TRUE)
summary(bc_pca)

# Reduce to top 2 principal components
bc_pca_data <- as.data.frame(bc_pca$x[, 1:2])

# Optimal number of clusters
elbow_bc <- fviz_nbclust(bc_pca_data, kmeans, method = "wss")
print(elbow_bc)

silhouette_bc <- fviz_nbclust(bc_pca_data, kmeans, method = "silhouette")
print(silhouette_bc)

# Apply K-means clustering
set.seed(123)
bc_kmeans <- kmeans(bc_pca_data, centers = 2, nstart = 25)

# Add cluster assignments to PCA data
bc_pca_data$cluster <- as.factor(bc_kmeans$cluster)

# Visualize clusters
p2 <- ggplot(bc_pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering on Breast Cancer Dataset")
print(p2)

# Interpret results
cat("Breast Cancer Dataset Clustering Results:\n")
cat("Cluster Sizes:", bc_kmeans$size, "\n")
```






\newpage
\section*{\centering Program - 9}
## Program 9: Time Series Analysis using ARIMA and Seasonal Decomposition

### Objective:  This program evaluates students’ skills in time series analysis, model fitting, and forecasting.
Write an R script to:
• Perform exploratory data analysis on time series data.
• Decompose the time series into trend, seasonal, and residual components.
• Fit an ARIMA model and evaluate the forecast accuracy.
• Compare ARIMA with seasonal ARIMA (SARIMA).
• Plot the original and forecasted values.
Datasets:
• Monthly Milk Production dataset (TSA package)

```{r}
# Load required libraries for time series analysis and modeling
#install.packages("forecast")
#install.packages("TSA")
library(forecast)
library(ggplot2)
library(TSA)
library(tseries)

# Function to perform Exploratory Data Analysis (EDA) on the time series data
perform_eda <- function(ts_data, dataset_name) {
  cat("Exploratory Data Analysis for", dataset_name, "\n")
  print(summary(ts_data)) # Print summary of the dataset
  plot(ts_data, main = paste(dataset_name, "Time Series"), ylab = "Values", xlab = "Time") # Plot the time series data
  cat("ACF and PACF plots:\n")
  acf(ts_data, main = paste("ACF of", dataset_name)) # Autocorrelation plot
  pacf(ts_data, main = paste("PACF of", dataset_name)) # Partial autocorrelation plot
}

# Function to decompose the time series into trend, seasonal, and residual components
decompose_ts <- function(ts_data, dataset_name) {
  cat("Decomposing the time series for", dataset_name, "\n")
  decomposition <- decompose(ts_data) # Decompose the time series
  plot(decomposition) # Plot the decomposition
  return(decomposition) # Return the decomposition result
}

# Function to fit an ARIMA model to the time series data
fit_arima <- function(ts_data, dataset_name) {
  cat("Fitting ARIMA model for", dataset_name, "\n")
  adf_test <- adf.test(ts_data, alternative = "stationary") # ADF test for stationarity
  cat("ADF Test p-value:", adf_test$p.value, "\n")
  # If p-value > 0.05, data is non-stationary, so we difference the data
  if (adf_test$p.value > 0.05) {
    ts_data <- diff(ts_data) # Difference the data to make it stationary
    plot(ts_data, main = paste(dataset_name, "Differenced Time Series")) # Plot the differenced data
  }
  auto_model <- auto.arima(ts_data, seasonal = FALSE) # Fit ARIMA model (non-seasonal)
  print(summary(auto_model)) # Print ARIMA model summary
  forecast_result <- forecast(auto_model, h = 12) # Forecast next 12 periods
  plot(forecast_result, main = paste(dataset_name, "ARIMA Forecast")) # Plot ARIMA forecast
  return(auto_model) # Return the fitted ARIMA model
}

# Function to fit a Seasonal ARIMA (SARIMA) model to the time series data
fit_sarima <- function(ts_data, dataset_name) {
  cat("Fitting SARIMA model for", dataset_name, "\n")
  auto_sarima <- auto.arima(ts_data, seasonal = TRUE) # Fit SARIMA model (seasonal)
  print(summary(auto_sarima)) # Print SARIMA model summary
  sarima_forecast <- forecast(auto_sarima, h = 12) # Forecast next 12 periods
  plot(sarima_forecast, main = paste(dataset_name, "SARIMA Forecast")) # Plot SARIMA forecast
  return(auto_sarima) # Return the fitted SARIMA model
}

# Function to compare ARIMA and SARIMA models by evaluating forecast accuracy
compare_models <- function(arima_model, sarima_model, ts_data) {
  cat("Comparing ARIMA and SARIMA models:\n")
  h <- min(12, length(ts_data)) # Forecast horizon of 12 or adjusted based on dataset length
  arima_forecast <- forecast(arima_model, h = h) # ARIMA forecast
  sarima_forecast <- forecast(sarima_model, h = h) # SARIMA forecast
  actual_values <- ts_data[(length(ts_data) - h + 1):length(ts_data)] # Actual values for comparison
  # Calculate accuracy of both models
  arima_accuracy <- accuracy(arima_forecast$mean, actual_values)
  sarima_accuracy <- accuracy(sarima_forecast$mean, actual_values)
  cat("ARIMA Forecast Accuracy:\n", arima_accuracy) # Print ARIMA accuracy
  cat("SARIMA Forecast Accuracy:\n", sarima_accuracy) # Print SARIMA accuracy
}

# Function to visualize the comparison of ARIMA and SARIMA forecast performance
plot_forecast_comparison <- function(actual_values, arima_forecast, sarima_forecast, time_points) {
  arima_rmse <- sqrt(mean((arima_forecast - actual_values)^2)) # Calculate RMSE for ARIMA
  sarima_rmse <- sqrt(mean((sarima_forecast - actual_values)^2)) # Calculate RMSE for SARIMA
  # Color coding for better and worse RMSE
  better_color <- ifelse(arima_rmse < sarima_rmse, "green", "red")
  worse_color <- ifelse(arima_rmse < sarima_rmse, "red", "green")

  # Plot actual values and forecasts
  plot(time_points, actual_values, type = "o", col = "blue", pch = 16, lty = 1,
       xlab = "Time", ylab = "Values", main = "Forecast Comparison")
  lines(time_points, arima_forecast, col = better_color, lty = 2, lwd = 2) # ARIMA forecast line
  lines(time_points, sarima_forecast, col = worse_color, lty = 3, lwd = 2) # SARIMA forecast line

  # Add a legend to the plot
  legend("topright",
         legend = c("Actual Values",
                    paste("ARIMA (RMSE =", round(arima_rmse, 2), ")"),
                    paste("SARIMA (RMSE =", round(sarima_rmse, 2), ")")),
         col = c("blue", better_color, worse_color),
         lty = c(1, 2, 3), lwd = c(1, 2, 2), pch = c(16, NA, NA))
}

# Monthly Milk Production Dataset Analysis
data(milk) # Load the Monthly Milk Production dataset
milk_data <- milk # Assign the dataset to a variable
cat("\n--- Monthly Milk Production Dataset ---\n")
perform_eda(milk_data, "Monthly Milk Production")
decompose_ts(milk_data, "Monthly Milk Production")
arima_milk <- fit_arima(milk_data, "Monthly Milk Production")
sarima_milk <- fit_sarima(milk_data, "Monthly Milk Production")
compare_models(arima_milk, sarima_milk, milk_data)

# Forecasting and plot comparison for Milk Production dataset
h_milk <- 12 # Define forecast horizon for Milk Production dataset (12 months ahead)
milk_actual_values <- milk_data[(length(milk_data) - h_milk + 1):length(milk_data)]
arima_milk_forecast <- forecast(arima_milk, h = h_milk)$mean
sarima_milk_forecast <- forecast(sarima_milk, h = h_milk)$mean
time_points_milk <- time(milk_data)[(length(milk_data) - h_milk + 1):length(milk_data)]
plot_forecast_comparison(milk_actual_values, arima_milk_forecast, sarima_milk_forecast, time_points_milk)