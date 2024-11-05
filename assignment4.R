install.packages("gt")
install.packages("tidyverse")

# Load ggplot2 library
library(ggplot2)
library(gt)
library(tidyverse)

# Data for charts
data <- data.frame(
  category = c("A", "B", "C", "D"),
  value = c(3, 5, 2, 8),
  width = c(1, 2, 3, 4)  
)

ggplot(data, aes(x = category, y = value, width = width)) +
  geom_col(fill = "skyblue", position = position_identity(), color = "black") +
  theme_minimal() +
  labs(title = "Variable Width Column Chart", 
       x = "Category", 
       y = "Value") +
  theme(legend.position = "none")


# 2. Table with Embedded Charts (Using dots to represent values)
ggplot(data, aes(x = category, y = value)) +
  geom_point(aes(size = value), shape = 21, fill = "skyblue") +  
  geom_text(aes(label = value), vjust = -0.5, size = 4) +  
  theme_minimal() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank()) + 
  labs(title = "Table with Embedded Charts")

# 3. Barplot
ggplot(data, aes(x = category, y = value)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Barplot", x = "Category", y = "Value")

# 4. Horizontal Barplot (correcting the column name)
ggplot(data, aes(x = category, y = value)) + 
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Horizontal Barplot", x = "Value", y = "Category")


