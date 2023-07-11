# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

library(fs)
library(glue)
library(tibble)
library(timetk)

library(rvest)
library(quantmod)


source(file = "00_scripts/stock_analysis_functions.R")


# Once and done functions
#stock_list_tbl <- get_stock_list()

# UI ----
ui <- fluidPage(
  title = "Stock Analyzer",
  
  # 1.0 HEADER ----
  div(
    h1("Stock Analzer"),
    p("This is my second shiny project --> getting data for DAX somehow not working")
  ),
  # 2.0 APPLICATION UI -----
  div(
    column(
      width = 4,
      wellPanel(
        
        # Changes in UI  
        pickerInput(inputId = "index_selection",
                    choices = c("DAX","DOW", "sp500", "NASDAQ"),
                    multiple = F,
                    options = pickerOptions(
                      actionsBox = FALSE,
                      liveSearch = TRUE,
                      size = 10) 
                    ), # select index
        
        uiOutput("indices"), # only stock from selected index are shown (computation in server)
        
        # Add content here 
        
        actionButton("analyze", "Analyze", icon = icon("download")),
        textOutput("selected_symbol"),
        
        hr(),
        
        dateRangeInput(inputId = "date_range", 
                       label   = h4("Date Range"), 
                       start   = "2000-01-01", 
                       end     = today(),           
                       min     = "2000-01-01", 
                       max     = today(), 
                       startview = "year"),
        
        
        hr(),
        
        sliderInput("mavg_short", "Short Moving Average", min = 5, max = 40, value = 20, step = 1),
        sliderInput("mavg_long", "Long Moving Average", min = 50, max = 120, value = 50, step = 1)
        
        

        
      )
      
    ), 
    column(
      width = 8,
      div(h4(textOutput("plot_header"))),
      plotlyOutput("plotly_plot")
      
    )
  ),
  
  
  column(
    width = 12,
    div(h4("Analyst Commentary")),
    div(textOutput("analyst_commentary"))
    
  )
)
  

  



# SERVER ----
server <- function(input, output, session) {
  
  # Get Stock List
  stock_list_tbl <- eventReactive(input$index_selection, {
    get_stock_list(input$index_selection)
    
  })
  
  # Stock Symbol ----
  stock_symbol <- eventReactive(input$analyze, {
    input$stock_selection 
  })
  
  output$selected_symbol <- renderText({stock_symbol()})
  output$plot_header <- renderText(stock_symbol())
  
  # Get Moving Average from Slider

  
  
  # Get Stock Data ----
  stock_data_tbl <- reactive({
    
    stock_symbol()%>%
      get_symbol_from_user_input()%>%
      get_stock_data(from = ymd(input$date_range[1]), 
                     to   = ymd(input$date_range[2]),
                     mavg_short = input$mavg_short,
                     mavg_long  = input$mavg_long)
    
  })
  
  # Plot Stock Data ----
  stock_plot <- reactive({
    stock_symbol()%>%
      get_symbol_from_user_input()%>%
      get_stock_data(from = ymd(input$date_range[1]), 
                     to   = ymd(input$date_range[2]),
                     mavg_short = input$mavg_short,
                     mavg_long  = input$mavg_long)%>%
      plot_stock_data()
    
  })
  
  # Get Analyst Commentary
  anaylist_commentary <- reactive({
    stock_symbol()%>%
      get_symbol_from_user_input()%>%
      get_stock_data(from = ymd(input$date_range[1]), 
                     to   = ymd(input$date_range[2]),
                     mavg_short = input$mavg_short,
                     mavg_long  = input$mavg_long)%>%
      generate_commentary()
    
  })
  
  # Create stock list ----    
  output$indices <- renderUI({
    choices = stock_list_tbl() %>% purrr::pluck("label")
    pickerInput(inputId = "stock_selection", 
                choices = choices,
                multiple = F,
                options = pickerOptions(
                  actionsBox = FALSE,
                  liveSearch = TRUE,
                  size = 10)
    )
  })
  
  output$analyst_commentary <- renderPrint({anaylist_commentary()})
  
  output$stock_data <- renderPrint({stock_data_tbl()})
  
  output$plotly_plot <- renderPlotly({stock_plot()})
  


  
}

# RUN APP ----
shinyApp(ui = ui, server = server)
