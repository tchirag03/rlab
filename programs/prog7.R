
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
