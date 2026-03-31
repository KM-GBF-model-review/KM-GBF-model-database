library(shiny)
library(plotly)
library(shinyWidgets)
library(shinythemes)
library(gt)
library(ggpubr)
library(DT)

targets::tar_source("R")

db <- LoadDatabaseInR("input/Merged_Dataset_KMGBF_16Oct25.xlsx")


name_choices <- .AttributeValues(database = db, attribute = "name") |>
  na.omit() |>
  as.character()

ui <- navbarPage(
  title = "KM-GBF Model Explorer",
  theme = shinytheme("flatly"),
  # --- Add your custom CSS here ---
  # header = tags$style(HTML("
  #   html, body, .container-fluid, .row, .main-panel, .tab-pane {
  #     margin: 0 !important;
  #     padding: 0 !important;
  #   }
  #   .navbar-static-top {
  #     margin-bottom: 0px !important;
  #   }
  # ")),
  # ---- Page 1: Model Descriptives ---- # ---- Info Tab ----
  tabPanel(
    "About",
    br(),
    fluidRow(
      column(
        width = 10,
        offset = 1,
        # About 30by30 - Hero Section
        tags$div(
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; border-radius: 10px; margin-bottom: 30px; box-shadow: 0 4px 6px rgba(0,0,0,0.1);",
          h2("🌍 KM-GBF Model Review", style = "font-weight: bold; color: white; margin-top: 0;"),
          p(style = "font-size: 16px; line-height: 1.6;", 
            "Convening a network of modellers and scenario developers to tackle the couple biodiversity and climate crises."),
          p(style = "font-size: 16px; line-height: 1.6;", 
            "The vision of this network is to improve the use of scenarios in the design and implementation of the Global Biodiversity Framework, National Biodiversity Strategies and Action Plans, and National Reporting through collaboration between policy bodies and scientific communities."),
          p(style = "font-size: 16px; line-height: 1.6;", 
            "The motivation and need to initiate this network stemmed from the BioFutures project funded by the UK Department of Environment, Food and Rural Affairs (DEFRA) and in collaboration with UNEP-WCMC, UKCEH, NHM, and the University of Edinburgh."),
          p(style = "margin-top: 15px;", 
            tags$a(href = "https://km-gbf-model-review.github.io/", 
                   target = "_blank",
                   style = "color: #000000; font-weight: 600; text-decoration: underline;",
                   "Visit our website →"))
        ),

        # KM-GBF model review 
        tags$div(
          style = "background: #fff8e1; padding: 25px; border-radius: 10px; margin-bottom: 25px; border-left: 5px solid #ffa726;",
          h3("🔬 BioFutures review of models", style = "font-weight: bold; color: #e65100; margin-top: 0;"),
          
          tags$div(
            style = "display: flex; align-items: flex-start; gap: 20px;",
            
            # Text on the left
            tags$div(
              style = "flex: 1;",
              p(style = "font-size: 16px; line-height: 1.6; color: #5d4037;", 
                "As a key part of the BioFutures project we assessed global scenario modelling capabilities in informing the CBD Kunming-Montreal Global Biodiversity Framework using the Nature Futures Framework."),
              p(style = "margin-top: 15px;", 
                tags$a(href = "https://sciencesearch.defra.gov.uk/ProjectDetails?ProjectId=21822", 
                       target = "_blank",
                       style = "color: #e65100; font-weight: 600; text-decoration: underline;",
                       "Read the report →")),
              p(style = "color: #495057;", "This app explores the findings of this report and provides metadata and model-specific visulations of how modelling resources are distribued across:"),
              tags$ul(
                style = "color: #495057; line-height: 1.8;",
                tags$li(tags$strong("Domain"), "e.g., Biodiversity, Economy, Climate change etc"),
                tags$li(tags$strong("Realm"), "e.g., Terrestrial, Freshwater", "Marine, etc"),
                tags$li(tags$strong("Drivers of biodiversity loss"), "e.g., Land use change, Climate change, Invaisve alien species etc"),
                tags$li(tags$strong("Dimensions of Nature"), "e.g., Species, Genetic or Functional Diveristy")
              ),
              p(style = "color: #495057;", "As well as their capabilities to:"),
              tags$ul(
                style = "color: #495057; line-height: 1.8;",
                tags$li(tags$strong("Inform the goals and targets of the KM-GBF")),
                tags$li(tags$strong("Reflect different values of the NFF in their inputs and outputs"),"e.g., Intrinsic, Instrumental or Cultural values" ),
                tags$li(tags$strong("Perform different scenario analyses"), "e.g., Exploratory, Target-seeking, Policy screening etc"),
                tags$li(tags$strong("Detect values in nature and attribute these to drivers"))
              ),
              
              tags$div(
                style = "background: white; padding: 15px; border-radius: 6px; margin-top: 15px; border-left: 3px solid #28a745;",
                p(style = "margin: 0; color: #495057;", 
                  "By visualising the current modelling landscape, we can better understand the critical gaps in the currnet modelling capacity, but also highlight opportunities to develop innovative solutions and approaches to future model development and monitioring frameworks.")
              ),
              tags$div(
                style = "margin-top: 20px; text-align: center;",
                tags$a(
                  href = "#", 
                  onclick = "$('a[data-value=\"Model Descriptives\"]').tab('show'); return false;",
                  style = "background: #4a80d2; color: white; padding: 12px 30px; border-radius: 25px; text-decoration: none; display: inline-block; font-weight: 600; box-shadow: 0 2px 4px rgba(0,0,0,0.2);",
                  "🗺️ Explore the database"
                )
              )
            ),
            
            # Image on the right
            tags$img(src="report_front_page.png", alt="Report title page", style="width: 400px; flex-shrink: 0;")
          )
        ),

        
        # # Getting involved
        # tags$div(
        #   style = "background: #e3f2fd; padding: 25px; border-radius: 10px; margin-bottom: 25px; border-left: 5px solid #1976d2;",
        #   h3("📚 Contribute", style = "font-weight: bold; color: #0d47a1; margin-top: 0;"),
        #   tags$div(
        #     style = "background: white; padding: 15px; border-radius: 6px; margin-bottom: 15px;",
        #     tags$strong(style = "color: #1976d2;", "Biodiversity Intactness Index (BII) v3.1.0"),
        #     br(),
        #     tags$span(style = "color: #495057;", "Produced by The Biodiversity Futures Lab at the Natural History Museum.")
        #   )
        # ),
        
        # Contact
        tags$div(
          style = "background: #f5f5f5; padding: 20px; border-radius: 10px; text-align: center;",
          h3("📧 Contact", style = "font-weight: bold; color: #4a4e55; margin-top: 0;"),
          p(style = "margin: 0;", "For questions or issues, please contact: ", 
            tags$a(href = "mailto:hkim@ceh.ac.uk", 
                   style = "color: #4a80d2; font-weight: 600;",
                   "hkim@ceh.ac.uk")
          )
        )
      )
    )
  )
,
  tabPanel("Model Descriptives",
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "question1", 
                           label = "KM-GBF Question", 
                           choices = name_choices),
               selectInput(inputId = "question2", 
                           label = "KM-GBF Question", 
                           choices = name_choices),
               selectInput(inputId = "question3", 
                           label = "KM-GBF Question", 
                           choices = name_choices),
               selectInput(inputId = "question4", 
                           label = "KM-GBF Question", 
                           choices = name_choices),
               width = 2
             ),
             mainPanel(
               plotOutput("plot", width = "1600px", height = "1200px")
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
                           selected = name_choices[1],
                           options = list(`actions-box` = TRUE,
                                          `none-selected-text` = "Please make a selection!")),
               width = 2
             ),
             mainPanel(
               # Wrap your gt_output in a div with desired viewport height
               div(
                 DTOutput("model_table"),
                 style = "overflow-y:auto;width: 100%; display: inline-block;margin: 0 auto;"
               )
             )
           )
  ),
  
  
  # ---- Footer ----
  footer = tags$footer(
    class = "footer",
    style = "
      position: fixed;
      bottom: 0;
      width: 100%;
      background-color: #f8f9fa;
      color: #6c757d;
      text-align: center;
      padding: 10px;
      border-top: 1px solid #dee2e6;
      font-size: 14px;
    ",
    HTML("&copy; 2026 KM-GBF Model Explorer — All rights reserved.")
  )
)

server <- function(input, output, session) {
  
  plot_response <- reactive({
    c(.ColumnFromAttribute(database = db,
                         attribute = "name",
                         value = input$question1),
      .ColumnFromAttribute(database = db,
                           attribute = "name",
                           value = input$question2),
      .ColumnFromAttribute(database = db,
                           attribute = "name",
                           value = input$question3),
      .ColumnFromAttribute(database = db,
                           attribute = "name",
                           value = input$question4))
  })
  
  # ---- Page 1: Plot ----
  output$plot <- renderPlot({
    
    if(length(unique(plot_response())) == 1){
    plot(ResponseCount(database = db, response = plot_response()[1], shiny = TRUE))
    }
    
    if(length(unique(plot_response())) == 2){
      
      plot(ggpubr::ggarrange(
        plot(ResponseCount(database = db, response = plot_response()[1], shiny = TRUE)),
        plot(ResponseCount(database = db, response = plot_response()[2], shiny = TRUE)),
        nrow = 1, common.legend = TRUE, legend = "bottom", align = "hv"
      ))
      
    }
    
    if(length(unique(plot_response())) == 3){
      plot(ggpubr::ggarrange(
        plot(ResponseCount(database = db, response = plot_response()[1], shiny = TRUE)),
        plot(ResponseCount(database = db, response = plot_response()[2], shiny = TRUE)),
        plot(ResponseCount(database = db, response = plot_response()[3], shiny = TRUE)),
        common.legend = TRUE, legend = "bottom", align = "hv"
      ))
    }
    
    if(length(unique(plot_response())) == 4){
      plot(ggpubr::ggarrange(
        plot(ResponseCount(database = db, response = plot_response()[1], shiny = TRUE)),
        plot(ResponseCount(database = db, response = plot_response()[2], shiny = TRUE)),
        plot(ResponseCount(database = db, response = plot_response()[3], shiny = TRUE)),
        plot(ResponseCount(database = db, response = plot_response()[4], shiny = TRUE)),
        common.legend = TRUE, legend = "bottom", align = "hv"
      ))
    }
      
  })
  
  
  table_responses <- reactive({
    as.list(purrr::map_vec(input$table_question, .f = function(x){
      .ColumnFromAttribute(database = db,
                           attribute = "name",
                           value = x)
    }))
  })
  


  # ---- Page 2: Table ----
  output$model_table <- renderDT({
    # Example: show the raw data for the selected question
    ColouredBinaryTableDT(database = db, responses = table_responses(), question = FALSE)
  })
  
}

shinyApp(ui, server)
