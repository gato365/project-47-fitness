#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)


source("data-analysis-cardio.R")
source("data-analysis-workout.R")



# Define UI for application
ui <- fluidPage(
  titlePanel("Cardio and Workout Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "activityType",
                  label = "Choose activity:",
                  choices = c("Cardio", "Workout"), 
                  selected = "Cardio"),
      
      conditionalPanel(
        condition = "input.activityType == 'Cardio'",
        
        
        selectInput(inputId = "bpmType",
                    label = "Choose BPM type:",
                    choices = c("Avg BPM" = "Avg_BPM", "Max BPM" = "Max_BPM"),
                    selected = "Avg"),
        
        
        selectInput(inputId = "cardioType", 
                    label = "Choose cardio type:",
                    choices = c("Spinning", "Stairs", "Running"),
                    selected = "Spinning"
                    )
      )
    ),
    
    mainPanel(
      plotOutput("activityPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$activityPlot <- renderPlot({
    # Placeholder for data preparation based on activityType
    
    # Conditional plotting based on selected activityType and bpmType
    if (input$activityType == "Cardio") {
      
      type_bpm <- ifelse(input$bpmType == "Avg_BPM","Average", "Max")
      
      cardio_by_week_df %>% 
        filter(Type == input$cardioType) %>% 
        ggplot( aes_string(x = "DayOfWeek", y = input$bpmType, color = "Week", group = "Week")) +
        geom_point() +
        geom_line(aes(group = Week)) +  # This groups the line by Type, remove if not needed
        labs(x = "Date", 
             y = paste0(type_bpm," BPM"), 
             title = paste0("Date vs ",type_bpm," BPM by Type")) +
        theme_minimal() +
        # Adjust color palette as needed
        theme(
          plot.title = element_text(hjust = 0.5, size = 15, face = 'bold'),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank()
        ) +
        scale_size_manual(values = c("Three Weeks Ago" = 0.75, "Two Weeks Ago" = 1.25, "Previous Week" = 1.75, "Current Week" = 2.25)) +  # Adjust sizes here
        scale_color_manual(values = c(
          "Three Weeks Ago" = "#9ECAE1",  # Light blue
          "Two Weeks Ago" = "#6BAED6",    # Medium blue
          "Previous Week" = "#FD8D3C",    # Orange
          "Current Week" = "#E6550D"      # Dark orange
        ))
    } else if (input$activityType == "Workout") {
      # Insert your ggplot code for Workout here
      # Use input$bpmType to select between Avg_BPM and Max_BPM
      ggplot() + 
        geom_bar()  # Example, replace with your actual plot code
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

