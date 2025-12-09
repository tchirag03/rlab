
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
