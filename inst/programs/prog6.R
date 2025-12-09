
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
