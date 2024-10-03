# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For gathering data
library(plotly) # For 3D plotting

# Sample datasets for demonstration
pressure <- data.frame(temperature = c(0, 25, 50, 75, 100, 125), 
                       pressure = c(0.1, 0.9, 2.1, 3.3, 5.1, 7.6))
VADeaths <- as.data.frame(VADeaths)  # Assuming VADeaths is already available
ToothGrowth <- ToothGrowth

### 1. Scatter Plot
ggplot(pressure, aes(x = temperature, y = pressure)) +
  geom_point(size = 3, shape = 16) +
  geom_text(x = 150, y = 600, label = "Pressure (mm Hg)\nversus\nTemperature (Celsius)", 
            vjust = 1, hjust = 0.5) +
  labs(x = "Temperature (Celsius)", y = "Pressure (mm Hg)") +
  theme_minimal()

### 2. Histogram
set.seed(123)  # For reproducibility
Y <- rnorm(50)
Y[Y < -3.5 | Y > 3.5] <- NA
ggplot(data.frame(Y), aes(x = Y)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "gray80", color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(Y, na.rm = TRUE), 
                                         sd = sd(Y, na.rm = TRUE)), 
                color = "blue", size = 1) +
  labs(x = "Value", y = "Density") +
  theme_minimal()

### 3. Bar Plot
VADeaths_long <- gather(VADeaths, key = "age_group", value = "death_rate", -Var1)
ggplot(VADeaths_long, aes(x = age_group, y = death_rate, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Death Rate") +
  scale_fill_manual(values = gray.colors(n = 9)) +
  theme_minimal()

### 4. Boxplot
ggplot(ToothGrowth, aes(x = as.factor(dose), y = len, fill = supp)) +
  geom_boxplot(position = position_dodge(0.8), width = 0.5) +
  labs(x = "Vitamin C dose (mg)", y = "Tooth Length") +
  theme_minimal() +
  scale_fill_manual(values = c("gray", "white")) +
  theme(legend.position = "top")

### 5. 3D Surface Plot
# Function for the surface plot
x <- seq(-10, 10, length = 30)
y <- x
f <- function(x, y) { r <- sqrt(x^2 + y^2); 10 * sin(r) / r }
z <- outer(x, y, f)

# Create 3D surface plot
plot_ly(x = ~x, y = ~y, z = ~z, type = "surface") %>%
  layout(scene = list(xaxis = list(title = 'X-axis'),
                      yaxis = list(title = 'Y-axis'),
                      zaxis = list(title = 'Z-axis')))

### 6. Pie Chart
pie_sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie_sales) <- c("Blueberry", "Cherry", "Apple", "Boston Cream", "Other", "Vanilla")
pie_data <- data.frame(flavor = names(pie_sales), share = pie_sales)

ggplot(pie_data, aes(x = "", y = share, fill = flavor)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(x = "", y = "") +
  scale_fill_manual(values = gray(seq(0.3, 1.0, length = 6))) +
  theme_void()

