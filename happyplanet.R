# Install and load necessary packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("ggrepel")
install.packages("RColorBrewer")

library(readxl)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)

# Load the data from the specified sheet, skipping the first 8 rows
data <- read_excel("/Users/malawikagopinath/Desktop/HPI_2024_public_dataset.xlsx", sheet = "1. All countries", skip = 8)

# Clean the data (e.g., remove rows with NAs)
cleaned_data <- data[complete.cases(data), ]

# Replace spaces and parentheses with underscores in column names
names(cleaned_data) <- gsub(" ", "_", names(cleaned_data))          # Replace spaces with underscores
names(cleaned_data) <- gsub("\\(|\\)", "", names(cleaned_data))    # Remove parentheses

# Check the modified column names
print(names(cleaned_data))

### 1. Cleaned up HPI vs Life Expectancy graph using ggplot2
ggplot(cleaned_data, aes(x = HPI, y = Life_Expectancy_years, label = Country, color = HPI)) +
  geom_point(size = 3) +  # Points
  geom_smooth(method = "lm", se = FALSE, color = "gray60", linetype = "solid") +  # Trendline
  geom_text_repel(size = 3, color = "gray50", max.overlaps = 10) +  # Text with automatic overlap avoidance
  labs(title = "HPI vs Life Expectancy", x = "HPI", y = "Life Expectancy (years)") +
  scale_color_gradient(low = "lightblue", high = "blue") +  # Gradient from light blue to blue
  theme_minimal() +  # Clean layout
  theme(plot.title = element_text(hjust = 0.5),  # Center title
        axis.title.x = element_text(color = "gray50"),
        axis.title.y = element_text(color = "gray50"),
        axis.text = element_text(color = "gray50")) +
  guides(color = guide_legend(title = "HPI Color Scale"))  # Add legend for HPI color scale

### 2. Histogram of HPI
par(mfrow=c(1, 1), mar=c(5, 5, 3, 3))  # Reset plot settings
hist(cleaned_data$HPI, breaks=10, col="gray80", main="Histogram of HPI", xlab="HPI", 
     ylab="Frequency", ylim=c(0, max(table(cut(cleaned_data$HPI, breaks=10)))))
lines(density(cleaned_data$HPI), col="gray50", lwd=2)  # Add density line
legend("topright", legend = c("Density"), col = "gray50", lty = 1, lwd = 2)  # Add legend for density line

### 3. Boxplot of Life Expectancy by Region
par(mfrow=c(1, 1), mar=c(5, 5, 3, 3))  # Reset plot settings
boxplot(cleaned_data$Life_Expectancy_years ~ cleaned_data$Continent, 
        main="Life Expectancy by Region", xlab="Region", ylab="Life Expectancy", 
        col="gray90", border="gray50")  # Boxplot


### 4. Pie chart for Ecological Footprint of top 5 countries
par(mfrow=c(1, 1), mar=c(2, 2, 2, 2), xpd=FALSE)  # Adjust margins for pie chart
ecological_data <- cleaned_data$Carbon_Footprint_tCO2e
names(ecological_data) <- cleaned_data$Country

# Get a range of blue colors (for example, 5 shades of blue)
blue_colors <- brewer.pal(5, "Blues")

# Create pie chart with the blue color palette
pie(ecological_data[1:5], col=blue_colors, main="Ecological Footprint of Top 5 Countries")  # Pie chart

### 5. Annotation example with shades of blue
par(mfrow=c(1, 1), mar=c(5, 5, 3, 3))  # Reset plot settings
plot(cleaned_data$HPI, cleaned_data$Life_Expectancy_years, type="n", 
     main="Annotation Example", xlab="HPI", ylab="Life Expectancy", 
     col.axis="gray50", col.lab="gray50")
# Define color based on HPI using a gradient from light blue to dark blue
colors <- scales::seq_gradient_pal(low = "lightblue", high = "blue")(cleaned_data$HPI / max(cleaned_data$HPI))

points(cleaned_data$HPI, cleaned_data$Life_Expectancy_years, pch=16, col=colors, cex=1.2)  # Points plot
text(x=cleaned_data$HPI[1], y=cleaned_data$Life_Expectancy_years[1], 
     labels=cleaned_data$Country[1], pos=4, col="gray50", cex=0.8)  # Text annotation
mtext("First country label", side=3, line=1, cex=1, col="black")  # Title

### 6. HPI vs Carbon Footprint with shades of blue
par(mfrow=c(1, 1), mar=c(5, 5, 3, 3))  # Reset plot settings
plot(cleaned_data$HPI, cleaned_data$Carbon_Footprint_tCO2e, main="HPI vs Carbon Footprint", 
     xlab="HPI", ylab="Carbon Footprint", col.axis="gray50", col.lab="gray50")
# Define color based on HPI using a gradient from light blue to dark blue
colors_fp <- scales::seq_gradient_pal(low = "lightblue", high = "blue")(cleaned_data$HPI / max(cleaned_data$HPI))

points(cleaned_data$HPI, cleaned_data$Carbon_Footprint_tCO2e, pch=21, bg=colors_fp, 
       col="gray30", cex=1.5)  # Points plot with blue background
box(bty="u")  # Box around the plot
legend("topright", legend = c("Carbon Footprint"), pch = 21, pt.bg = "lightblue", title = "Legend")  # Add legend for Carbon Footprint

### 7. 3D Perspective Plot using persp()
x <- seq(-10, 10, length=30)
y <- x
f <- function(x, y) {
  r <- sqrt(x^2 + y^2)
  10 * sin(r) / r
}
z <- outer(x, y, f)
z[is.na(z)] <- 1  # Handle NA values

# Plotting 3D perspective with custom margins and angles
par(mar=c(0, 0.5, 0, 0), lwd=0.5)  # Adjust margins for 3D plot
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, 
      col = "lightblue", border = "gray50", ltheta = 50, 
      shade = 0.5, xlab = "X-axis", ylab = "Y-axis", zlab = "Z-axis", 
      main = "3D Perspective Plot")  # Add title to 3D plot
par(mar=c(5.1, 4.1, 4.1, 2.1), lwd=1)  # Reset plot settings
