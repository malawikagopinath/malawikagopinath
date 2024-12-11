#library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)

# Load and preprocess data
walkability_data <- read_csv("index.csv")
mental_health_data <- read_csv("locations.csv") 

# Preprocessing
walkability_clean <- walkability_data %>%
  select(GEOID10, NatWalkInd, CBSA_Name) %>%
  filter(CBSA_Name %in% c("Dallas-Fort Worth-Arlington, TX", 
                          "Austin-Round Rock-Georgetown, TX", 
                          "Houston-The Woodlands-Sugar Land, TX")) %>%
  mutate(City = case_when(
    CBSA_Name == "Dallas-Fort Worth-Arlington, TX" ~ "Dallas",
    CBSA_Name == "Austin-Round Rock-Georgetown, TX" ~ "Austin",
    CBSA_Name == "Houston-The Woodlands-Sugar Land, TX" ~ "Houston"
  ))

avg_walkability <- walkability_clean %>%
  group_by(City) %>%
  summarize(Average_WalkInd = mean(NatWalkInd, na.rm = TRUE))

depression_data_clean <- mental_health_data %>%
  filter(StateDesc == "Texas") %>%
  filter(grepl("depression", Measure, ignore.case = TRUE)) %>%
  select(LocationName, Data_Value, StateDesc, Data_Value_Type)

avg_depression <- depression_data_clean %>%
  filter(LocationName %in% c("Dallas", "Austin", "Houston")) %>%
  group_by(LocationName) %>%
  summarize(Average_Depression = mean(Data_Value, na.rm = TRUE)) %>%
  rename(City = LocationName)

city_depression <- depression_data_clean %>%
  filter(LocationName %in% c("Dallas", "Austin", "Houston")) %>%
  group_by(LocationName, Data_Value_Type) %>%
  summarize(Average_Depression = mean(Data_Value, na.rm = TRUE)) %>%
  ungroup()

combined_data <- avg_walkability %>%
  left_join(avg_depression, by = "City")

# UI
ui <- fluidPage(
  titlePanel("Walkability and Depression Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput("visualization", "Select Visualization:",
                  choices = c(
                    "Walkability Bar Chart",
                    "Depression Bar Chart",
                    "Density Plot (Walkability)",
                    "Box Plot (Walkability)",
                    "Bubble Chart (Walkability vs Depression)",
                    "Depression Bar Chart for Dallas",
                    "Depression Bar Chart for Austin",
                    "Depression Bar Chart for Houston"
                  ),
                  selected = "Walkability Bar Chart")
    ),
    mainPanel(
      plotOutput("selectedPlot")
    )
  )
)

# Server
server <- function(input, output) {
  
  output$selectedPlot <- renderPlot({
    req(input$visualization)
    
    switch(input$visualization,
           "Walkability Bar Chart" = {
             ggplot(avg_walkability, aes(x = City, y = Average_WalkInd, fill = City)) +
               geom_col() +
               scale_fill_manual(values = c("Dallas" = "#C05790", "Austin" = "#005682", "Houston" = "#4DD091")) +
               theme_minimal() +
               ggtitle("Average Walkability Index") +
               xlab("City") +
               ylab("Walkability Index")
           },
           "Depression Bar Chart" = {
             ggplot(avg_depression, aes(x = City, y = Average_Depression, fill = City)) +
               geom_col() +
               scale_fill_manual(values = c("Dallas" = "#4DD091", "Austin" = "#C05790", "Houston" = "#005682")) +
               theme_minimal() +
               ggtitle("Average Depression Rates by City") +
               xlab("City") +
               ylab("Depression Rate (%)")
           },
           "Depression Bar Chart for Dallas" = {
             ggplot(city_depression %>% filter(LocationName == "Dallas"), 
                    aes(x = Data_Value_Type, y = Average_Depression, fill = Data_Value_Type)) +
               geom_col() +
               scale_fill_manual(values = c("blue", "green")) +
               theme_minimal() +
               ggtitle("Depression Rates for Dallas by Measure Type") +
               xlab("Measure Type") +
               ylab("Depression Rate (%)")
           },
           "Depression Bar Chart for Austin" = {
             ggplot(city_depression %>% filter(LocationName == "Austin"), 
                    aes(x = Data_Value_Type, y = Average_Depression, fill = Data_Value_Type)) +
               geom_col() +
               scale_fill_manual(values = c("blue", "green")) +
               theme_minimal() +
               ggtitle("Depression Rates for Austin by Measure Type") +
               xlab("Measure Type") +
               ylab("Depression Rate (%)")
           },
           "Depression Bar Chart for Houston" = {
             ggplot(city_depression %>% filter(LocationName == "Houston"), 
                    aes(x = Data_Value_Type, y = Average_Depression, fill = Data_Value_Type)) +
               geom_col() +
               scale_fill_manual(values = c("blue", "green")) +
               theme_minimal() +
               ggtitle("Depression Rates for Houston by Measure Type") +
               xlab("Measure Type") +
               ylab("Depression Rate (%)")
           },
           "Density Plot (Walkability)" = {
             ggplot(walkability_clean, aes(x = NatWalkInd, fill = City, color = City)) +
               geom_density(alpha = 0.5) +
               scale_fill_manual(values = c("Dallas" = "#C05790", "Austin" = "#005682", "Houston" = "#4DD091")) +
               theme_minimal() +
               ggtitle("Density Plot of Walkability Index") +
               xlab("Walkability Index") +
               ylab("Density")
           },
           "Box Plot (Walkability)" = {
             ggplot(walkability_clean, aes(x = City, y = NatWalkInd, fill = City)) +
               geom_boxplot() +
               scale_fill_manual(values = c("Dallas" = "#C05790", "Austin" = "#005682", "Houston" = "#4DD091")) +
               theme_minimal() +
               ggtitle("Box Plot of Walkability Index") +
               xlab("City") +
               ylab("Walkability Index")
           },
           "Bubble Chart (Walkability vs Depression)" = {
             ggplot(combined_data, aes(x = Average_WalkInd, y = Average_Depression, color = City)) +
               geom_point(size = 10, alpha = 0.8) +
               scale_color_manual(values = c("Dallas" = "#C05790", "Austin" = "#005682", "Houston" = "#4DD091")) +
               theme_minimal() +
               ggtitle("Walkability vs Depression Rates") +
               xlab("Walkability Index") +
               ylab("Depression Rate (%)")
           }
    )
  })
}

# Run 
shinyApp(ui, server)
