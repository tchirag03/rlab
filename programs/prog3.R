
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
