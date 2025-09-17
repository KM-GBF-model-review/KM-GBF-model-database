library(shiny)
library(plotly)
library(shinyWidgets)
library(gt)

targets::tar_source("../R")

db <- LoadDatabaseInR("../input/Merged_Dataset_KMGBF_26Aug.xlsx")
name_choices <- .AttributeValues(database = db, attribute = "name") |>
  na.omit() |>
  as.character()

ui <- navbarPage(
  title = "KM-GBF Model Explorer",
  
  # ---- Page 1: Model Descriptives ----
  tabPanel("Model Descriptives",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "question", 
                           label = "KM-GBF Question", 
                           choices = name_choices)
             ),
             mainPanel(
               plotOutput("plot", width = "800px", height = "600px")
             )
           )
  ),
  
  # ---- Page 2: Model Table ----
  tabPanel("Model Table",
           sidebarLayout(
             sidebarPanel(
               pickerInput(inputId = "table_question", 
                           label = "KM-GBF Question", 
                           choices = name_choices,
                           multiple = TRUE,
                           options = list(`actions-box` = TRUE,
                                          `none-selected-text` = "Please make a selection!"))
             ),
             mainPanel(
               gt_output("model_table")
             )
           )
  )
)

server <- function(input, output, session) {
  
  plot_response <- reactive({
    .ColumnFromAttribute(database = db,
                         attribute = "name",
                         value = input$question)
  })
  
  # ---- Page 1: Plot ----
  output$plot <- renderPlot({
    plot(ResponseCount(database = db, response = plot_response()))
  })
  
  
  table_responses <- reactive({
    purrr::map_vec(input$table_question, .f = function(x){
      .ColumnFromAttribute(database = db,
                           attribute = "name",
                           value = x)
    })
  })
  
  # ---- Page 2: Table ----
  output$model_table <- render_gt({
    # Example: show the raw data for the selected question
    ColouredBinaryTable(database = db, response = table_responses())
  })
  
}

shinyApp(ui, server)
