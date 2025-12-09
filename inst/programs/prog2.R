
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
