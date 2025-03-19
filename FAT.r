library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(shinydashboard)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Food Availability Tracker", titleWidth = 350),
  dashboardSidebar(
    fileInput("file", "Upload CSV File", accept = ".csv"),
    textInput("food_item", "Food Item:"),
    actionButton("refresh", "Refresh Data", class = "btn btn-primary")
  ),
  dashboardBody(
    fluidRow(
      valueBoxOutput("total_countries", width = 4),
      valueBoxOutput("unique_markets", width = 4),
      valueBoxOutput("average_availability", width = 4)
    ),
    tabsetPanel(
      tabPanel("Data Table", DTOutput("table")),
      tabPanel("Food Availability Chart", plotOutput("food_plot")),
      tabPanel("Market Share", plotOutput("trend_plot")),
      tabPanel("Summary Statistics", verbatimTextOutput("summary_stats"))
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive data
  data <- reactiveVal()
  
  # Load data
  observeEvent(input$file, {
    req(input$file)
    df <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    colnames(df) <- tolower(gsub(" ", "_", colnames(df)))  # Standardize column names
    
    required_cols <- c("country", "food_item", "market", "availability")
    if (!all(required_cols %in% colnames(df))) {
      showNotification("Invalid file format. Please upload a valid CSV file with required columns.", type = "error")
      return()
    }
    
    df$food_item <- tolower(df$food_item)  # Convert food items to lowercase for case-insensitive comparison
    df$availability <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", df$availability)))
    
    if (all(is.na(df$availability))) {
      df$availability <- 0
      showNotification("Warning: No numeric values found in 'availability'. Defaulting to 0.", type = "warning")
    }
    
    data(df)
  })
  
  # Filtered data
  filtered_data <- reactive({
    req(data(), input$food_item)
    user_input <- tolower(input$food_item)  # Convert user input to lowercase
    filtered <- data() %>% filter(food_item == user_input)
    
    if (nrow(filtered) == 0) {
      showNotification("Food item not found. Please check your spelling.", type = "error")
    }
    
    filtered
  })
  
  # Outputs
  output$table <- renderDT({
    req(filtered_data())
    datatable(filtered_data(), 
              options = list(pageLength = 10, autoWidth = TRUE, searching = FALSE),  # Disable search bar
              colnames = c("Country" = "country", "Food Item" = "food_item", "Market" = "market", "Availability" = "availability"))
  })
  
  output$total_countries <- renderValueBox({
    req(data(), input$food_item)
    countries <- filtered_data() %>% pull(country) %>% unique()
    valueBox(length(countries), "Total Countries", icon = icon("globe"), color = "blue")
  })
  
  output$unique_markets <- renderValueBox({
    req(data(), input$food_item)
    markets <- filtered_data() %>% pull(market) %>% unique()
    valueBox(length(markets), "Unique Markets", icon = icon("store"), color = "green")
  })
  
  output$average_availability <- renderValueBox({
    req(data(), input$food_item)
    avg <- filtered_data() %>% pull(availability) %>% mean(na.rm = TRUE) %>% round(2)
    valueBox(avg, "Avg. Availability", icon = icon("chart-line"), color = "purple")
  })
  
  output$trend_plot <- renderPlot({
    req(input$food_item, filtered_data())
    market_share <- filtered_data() %>% 
      group_by(market) %>% 
      summarise(count = n()) %>% 
      mutate(market_share = count / sum(count) * 100)
    
    if (nrow(market_share) == 0) return()  # Avoid error if no data is available
    
    max_share_market <- market_share %>% arrange(desc(market_share)) %>% slice(1)
    
    ggplot(market_share, aes(x = "", y = market_share, fill = market)) + 
      geom_bar(stat = "identity", width = 1) + 
      coord_polar(theta = "y") + 
      labs(title = paste("Market Share:", max_share_market$market, "has the highest share"), x = "", y = "") + 
      theme_void() + 
      theme(legend.position = "bottom")
  })
  
  output$food_plot <- renderPlot({
    req(input$food_item, filtered_data())
    ggplot(filtered_data(), aes(x = market, y = availability, fill = market)) + 
      geom_bar(stat = "identity", color = "black") + 
      labs(title = "Food Availability Chart", x = "Market", y = "Availability") + 
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$summary_stats <- renderPrint({
    req(input$food_item, filtered_data())
    summary_stats <- filtered_data() %>% 
      summarise(
        min_availability = min(availability, na.rm = TRUE),
        max_availability = max(availability, na.rm = TRUE)
      )
    print(summary_stats)
  })
}

# Run App
shinyApp(ui, server)