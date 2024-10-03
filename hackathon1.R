# Load necessary libraries
install.packages("ggplot2")
install.packages("GGally")

library(ggplot2)
library(GGally)

# Download COVID data from OWID GitHub
owidall <- read.csv("https://github.com/owid/covid-19-data/raw/master/public/data/owid-covid-data.csv")

# Deselect cases/rows with OWID
owidall <- owidall[!grepl("^OWID", owidall$iso_code), ]

# Subset by continent: Europe
owideu <- subset(owidall, continent == "Europe")

# Select relevant columns for the scatterplot matrix
data_for_plot <- owideu[, c("new_cases", "new_deaths", "total_vaccinations", "population")]

# Create scatterplot matrix
ggpairs(data_for_plot,
        title = "Scatterplot Matrix of COVID-19 Data in Europe",
        lower = list(continuous = wrap("points", alpha = 0.5, size = 0.5)),
        upper = list(continuous = wrap("cor", size = 5)),
        diag = list(continuous = wrap("barDiag", alpha = 0.5, color = "blue")))

                                        
                                        
                                        