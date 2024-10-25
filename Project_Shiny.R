#Project by R, Shiny
#Bahrae Ghasemi
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(caret)

# Load the dataset (replace with your actual dataset path)
Airline_customer_satisfaction <- read.csv("Airline_customer_satisfaction.csv")

# Example of machine learning results (replace with actual results)
confusionMatrix <- confusionMatrix(
  factor(c(rep("dissatisfied", 10137), rep("satisfied", 15838))), 
  factor(c(rep("dissatisfied", 8703), rep("satisfied", 1434), rep("dissatisfied", 3055), rep("satisfied", 12783)))
)

# Extract the confusion matrix as a table
confusionMatrixTable <- as.table(confusionMatrix$table)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Airline Customer Satisfaction Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Type of Travel Distribution", tabName = "type_of_travel", icon = icon("dashboard")),
      menuItem("Customer Type Distribution", tabName = "customer_type", icon = icon("dashboard")),
      menuItem("Age Distribution", tabName = "age_distribution", icon = icon("bar-chart")),
      menuItem("Delay", tabName = "Delay", icon = icon("bar-chart")),
      menuItem("Online Boarding", tabName = "online_boarding", icon = icon("dashboard")),
      menuItem("Flight Distance by Class", tabName = "flight_distance", icon = icon("plane")),
      menuItem("Model Evaluation", tabName = "model_evaluation", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    fluidRow(
      column(width = 12,
             h1("Airline Customer Satisfaction Dashboard", align = "center"),
             h3("Presented by Bahare Ghasemi", align = "center"),
             br()  # Add a break for spacing
      )
    ),
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Summary", status = "primary", solidHeader = TRUE,
                  plotOutput("summary")
                )
              )
      ),
      tabItem(tabName = "type_of_travel",
              fluidRow(
                box(
                  title = "Type of Travel Distribution", status = "primary", solidHeader = TRUE,
                  plotOutput("travelPieChart_type_of_travel")
                )
              )
      ),
      # Customer Type Distribution tab
      tabItem(tabName = "customer_type",
              fluidRow(
                box(
                  title = "Customer Type Distribution", status = "primary", solidHeader = TRUE,
                  plotOutput("customerTypeBarChart_customer_type")
                )
              )
      ),
      # Age Distribution tab
      tabItem(tabName = "age_distribution",
              fluidRow(
                box(
                  title = "Satisfaction by Age Distribution", status = "primary", solidHeader = TRUE,
                  plotOutput("ageHistogram")
                )
              )
      ),
      # Flight Distance by Class tab
      tabItem(tabName = "flight_distance",
              fluidRow(
                sidebarLayout(
                  mainPanel(
                    plotOutput("flightDistanceBoxPlot")
                  ),
                  sidebarPanel(
                    sliderInput("distance_filter", "Filter by Distance:",
                                min = 0, max = 10000, value = c(0, 5000),
                                step = 100, animate = TRUE)
                  )
              )
      )),
      tabItem(tabName = "Delay",
              fluidRow(
                sidebarLayout(
                  mainPanel(
                    title = "Average Departure Delay by Satisfaction", status = "primary", solidHeader = TRUE,
                    plotOutput("departureDelayHistogram")
                ),
                sidebarPanel(
                  sliderInput("departure_delay_filter", "Filter Departure Delay:",
                            min = 0, max = 300, value = c(0, 100),
                            step = 10, animate = TRUE),
                  sliderInput("arrival_delay_filter", "Filter Arrival Delay:",
                            min = 0, max = 300, value = c(0, 100),
                            step = 10, animate = TRUE)
                )
            )
          )
      ),
      # Online Boarding tab
      tabItem(tabName = "online_boarding",
              fluidRow(
                box(
                  title = "Satisfaction by Online Boarding Experience", status = "primary", solidHeader = TRUE,
                  plotOutput("onlineBoardingBarChart")
                )
              )
      ),
      # Model Evaluation tab
      tabItem(tabName = "model_evaluation",
              fluidRow(
                box(
                  title = "Confusion Matrix Heatmap", status = "primary", solidHeader = TRUE,
                  plotOutput("confusionMatrixHeatmap")
                ),
                box(
                  title = "Model Performance Metrics", status = "primary", solidHeader = TRUE,
                  verbatimTextOutput("modelPerformanceOutput")
                )
              )
      )
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Function to compute summary statistics
  output$summary <- renderPrint({
    summary_data <- list(
      Total_Customers = nrow(Airline_customer_satisfaction),
      Average_Age = round(mean(Airline_customer_satisfaction$Age, na.rm = TRUE), 1),
      Max_Flight_Distance = max(Airline_customer_satisfaction$Flight.Distance, na.rm = TRUE),
      Min_Flight_Distance = min(Airline_customer_satisfaction$Flight.Distance, na.rm = TRUE),
      Most_Common_Customer_Type = names(sort(table(Airline_customer_satisfaction$Customer.Type), decreasing = TRUE))[1],
      Average_Departure_Delay = mean(Airline_customer_satisfaction$Departure.Delay.in.Minutes, na.rm = TRUE),
      Average_Arrival_Delay = mean(Airline_customer_satisfaction$Arrival.Delay.in.Minutes, na.rm = TRUE)
      # Add more summary statistics as needed
    )
    
    # Create a character vector to store summary information
    summary_text <- paste(
      "Total Customers:", summary_data$Total_Customers, "\n",
      "Average Age:", summary_data$Average_Age, "\n",
      "Max Flight Distance:", summary_data$Max_Flight_Distance, "\n",
      "Min Flight Distance:", summary_data$Min_Flight_Distance, "\n",
      "Most Common Customer Type:", summary_data$Most_Common_Customer_Type, "\n",
      "Average Departure Delay:", summary_data$Average_Departure_Delay, "\n",
      "Average Arrival Delay:", summary_data$Average_Arrival_Delay
    )
    
    # Return the summary_text as a character vector
    return(summary_text)
  })
  
  # Plot for Online Boarding Satisfaction
  output$onlineBoardingBarChart <- renderPlot({
    ggplot(Airline_customer_satisfaction, aes(x = Online.boarding, fill = satisfaction)) +
      geom_bar(position = "stack", color = "black") +
      labs(title = "Satisfaction by Online Boarding Experience",
           x = "Online Boarding Experience", y = "Count") +
      scale_fill_manual(values = c("#FF6666", "#66FF66"),
                        labels = c("Dissatisfied", "Satisfied")) +
      theme_minimal() +
      geom_text(stat = 'count', aes(label = ..count.., group = satisfaction), position = position_stack(vjust = 0.5), color = "black")
  })
  
  
  # Departure Delay Histogram with filter
  output$departureDelayHistogram <- renderPlot({
    # Filter based on input$departure_delay_filter and input$arrival_delay_filter
    filtered_data <- Airline_customer_satisfaction %>%
      filter(Departure.Delay.in.Minutes >= input$departure_delay_filter[1] &
               Departure.Delay.in.Minutes <= input$departure_delay_filter[2],
             Arrival.Delay.in.Minutes >= input$arrival_delay_filter[1] &
               Arrival.Delay.in.Minutes <= input$arrival_delay_filter[2])
    
    # Calculate average departure delay for satisfied and dissatisfied customers
    delay_summary <- filtered_data %>%
      group_by(satisfaction) %>%
      summarise(avg_departure_delay = mean(Departure.Delay.in.Minutes, na.rm = TRUE))
    
    # Plotting
    ggplot(delay_summary, aes(x = satisfaction, y = avg_departure_delay, fill = satisfaction)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.6, alpha = 0.8) +
      labs(title = "Average Departure Delay by Customer Satisfaction",
           x = "Customer Satisfaction",
           y = "Average Departure Delay (minutes)") +
      scale_fill_manual(values = c("dissatisfied" = "red", "satisfied" = "green")) +
      theme_minimal()
  })
  
  
  output$travelPieChart_type_of_travel <- renderPlot({
    ggplot(Airline_customer_satisfaction, aes(x = "", fill = Type.of.Travel)) +
      geom_bar(width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Type of Travel Distribution") +
      scale_fill_discrete(name = "Type of Travel") +
      geom_text(aes(y = ..count.., label = scales::percent(..count.. / sum(..count..))), stat = "count", position = "stack") +
      guides(fill = guide_legend(title = "Type of Travel"))
  })
  
  
  output$customerTypeBarChart_customer_type <- renderPlot({
    percentage_data <- Airline_customer_satisfaction %>%
      group_by(Customer.Type, satisfaction) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Percentage = Count / sum(Count) * 100)
    
    ggplot(Airline_customer_satisfaction, aes(x = Customer.Type, fill = satisfaction)) +
      geom_bar(position = "stack", color = "black") +
      labs(title = "Satisfaction Levels by Customer Type",
           x = "Customer Type", y = "Count") +
      scale_fill_manual(values = c("#FF6666", "#E0E0E0", "#66FF66"),
                        labels = c("Dissatisfied", "Satisfied")) +
      theme_minimal() +
      geom_text(data = percentage_data, aes(label = paste0(round(Percentage), "%"),
                                            y = Count, group = satisfaction),
                position = position_stack(vjust = 0.5), color = "black")
  })
  
  output$ageHistogram <- renderPlot({
    customer_data_frame <- Airline_customer_satisfaction %>%
      mutate(Satisfaction = factor(satisfaction,
                                   levels = c("dissatisfied", "satisfied"))) %>%
      mutate(Age_Group = cut(Age, breaks = seq(0, 100, by = 10))) %>%
      group_by(Age_Group, Satisfaction) %>%
      summarise(Count = n()) %>%
      ungroup()
    
    ggplot(customer_data_frame, aes(x = Age_Group, y = Count, fill = Satisfaction)) +
      geom_bar(stat = "identity", position = "stack", color = "black") +
      labs(title = "Satisfaction Levels by Age Group",
           x = "Age Group", y = "Count") +
      scale_fill_manual(values = c("#FF6666", "#66FF66"),
                        labels = c("Dissatisfied", "Satisfied")) +
      theme_minimal() +
      geom_text(aes(label = Count),
                position = position_stack(vjust = 0.5), color = "black", size = 3)
  })
  
  output$flightDistanceBoxPlot <- renderPlot({
    # Default filter values
    distance_min <- 0
    distance_max <- 5000
    
    # Check if input$distance_filter exists and update filter values accordingly
    if (!is.null(input$distance_filter)) {
      distance_min <- input$distance_filter[1]
      distance_max <- input$distance_filter[2]
    }
    
    # Filter the dataset based on the distance range
    filtered_data <- Airline_customer_satisfaction %>%
      filter(Flight.Distance >= distance_min & Flight.Distance <= distance_max)
    
    # Create the plot
    ggplot(filtered_data, aes(x = Class, y = Flight.Distance, fill = Class)) +
      geom_boxplot() +
      labs(title = "Flight Distance by Class") +
      theme_minimal()
  })
  
  # Render confusion matrix heatmap
  output$confusionMatrixHeatmap <- renderPlot({
    ggplot(as.data.frame(as.table(confusionMatrix$table)), 
           aes(x = Reference, y = Prediction, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = Freq), color = "white") +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
      theme_minimal()
  })
  
  # Render model performance metrics output
  output$modelPerformanceOutput <- renderPrint({
    metrics <- list(
      Accuracy = confusionMatrix$overall["Accuracy"],
      Kappa = confusionMatrix$overall["Kappa"],
      Sensitivity = confusionMatrix$byClass["Sensitivity"],
      Specificity = confusionMatrix$byClass["Specificity"],
      Pos_Pred_Value = confusionMatrix$byClass["Pos Pred Value"],
      Neg_Pred_Value = confusionMatrix$byClass["Neg Pred Value"],
      Prevalence = confusionMatrix$byClass["Prevalence"],
      Detection_Rate = confusionMatrix$byClass["Detection Rate"],
      Detection_Prevalence = confusionMatrix$byClass["Detection Prevalence"],
      Balanced_Accuracy = confusionMatrix$byClass["Balanced Accuracy"]
    )
    print(metrics)
  })
}

# Run the app
shinyApp(ui, server)