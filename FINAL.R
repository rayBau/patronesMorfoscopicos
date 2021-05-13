## app.R ##
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
packageVersion('plotly')


header <- dashboardHeader(disable = FALSE,
                          title = "PAPIME",
                          titleWidth = 260
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Acerca de", tabName = "home", icon = icon("far fa-address-card")),
    menuItem("Rugoscopia", tabName = "rugos", icon = icon("align-justify"),
             menuSubItem("Temas", tabName = "tem1"),
             menuSubItem("Actividades", tabName = "act1")
             
    ),
    
    menuItem("Queiloscopia", tabName = "analy", icon = icon("chart-line"),
             menuSubItem("Temas", tabName = "tem2"),
             menuSubItem("Actividades", tabName = "morfl")
             
    )
    
  ),
  disable = FALSE,
  width = 260,
  collapsed = FALSE
  
)

body <- dashboardBody(
  theme_grey_dark,
  
  tabItems(
    tabItem(tabName = "home",
            column(width = 3, align="center",
                   img(src = "logo1.png", height = 130, width = 120)
            ),
            column(width = 6,align="center", 
                   fluidRow(width = 7,
                            h2("Análisis de Patrones Morfoscópicos"),
                            h3("Odontología y Dactiloscopía"),
                            h4(strong("UNAM")),
                            br(),
                            
                   )
            ),
            column(width = 3,align="center",
                   img(src = "logo2.png", height = 130, width = 130, alt = "This is alternate text")
            ),
            column(width = 12,align="center",
                   fluidRow(width = 12,
                            h4(strong("Proyecto:"), "PE201421	"))
            ),
            column(width = 12,align="center",
                   fluidRow(width = 12,
                            h3(strong("Objetivo")))
            ),
            
            column(width = 12, align = "center",
                   fluidRow(width = 5,
                            h4("Propuesta didáctica interactiva para la identificación humana a través del análisis comparativo de patrones morfológicos en dactiloscopía y odontología forense."))
            ),
            
            column(width = 12, align = "center",
                   fluidRow(width = 12,
                            h5("_______________________________________________________________________________________"))
            ),
            column(width = 12, align = "center",
                   fluidRow(width = 12,
                            h3(strong("METAS")))
            ),
            column(width = 12, align = "center",
                   fluidRow(width = 12,
                            h4("Propuesta:"))
            ),
            column(width = 12, align = "center",
                   fluidRow(width = 12,
                            h4("Didáctica:"))
            ),
            column(width = 12, align = "center",
                   fluidRow(width = 12,
                            h4("Interactiva:"))
            ),
            column(width = 12, align = "center",
                   fluidRow(width = 12,
                            h5("_______________________________________________________________________________________"))
            ),
            column(width = 12, align = "center",
                   fluidRow(width = 7,
                            h3(strong("Responsables")))
            ),
            
            ####################################
            
    ),
    #############################################################
    tabItem(tabName = "tem1",
            mainPanel(
              
              tabsetPanel(
                
                tabPanel("Objetivo",
                         h1("HOA;")),
                tabPanel("Responsables",
                         verbatimTextOutput("sumario")),
                tabPanel("Áreas de Interés",
                         tableOutput("tabla"))
                
              )
              
            )),
    
    #############################################################
    
    
    
    
    
    
    
    
    #############################################################
    tabItem(tabName = "act1",
            fluidRow(width = 12,
                     column(width = 12,align="center",
                            fluidRow(width = 8,
                                     tabsetPanel(type = "tabs",
                                                 tabPanel("Inputs", align="center",
                                                          br(),
                                                          h4("Ingresa la Informacion Correspondiente"),
                                                          br(),
                                                          column(width = 6,align="center",
                                                                 textInput("ID", h5("ID")),
                                                                 textInput("Evaluador", h5("Nombre del Evaluador")),
                                                                 textInput("Evaluado", h5("Nombre del evaluado")),
                                                                 numericInput("Edad",
                                                                              h5("Edad"),
                                                                              min = 0,
                                                                              max = 100,
                                                                              step = 1,
                                                                              value = 0),
                                                                 selectInput("Origen", 
                                                                             h5("Lugar de Origen"),
                                                                             choices = list("Aguascalientes" = "Aguascalientes",
                                                                                            "Baja California" = "Baja California",
                                                                                            "Campeche" = "Campeche",
                                                                                            "Chiapas" = "Chiapas",
                                                                                            "Chihuahua" = "Chihuahua",
                                                                                            "Colima" = "Colima",
                                                                                            "Durango" = "Durango",
                                                                                            "Edo. Mexico" = "Edo. Mexico",
                                                                                            "Ciudad de Mexico" = "Ciudad de Mexico",
                                                                                            "Otro" = "Otro"),
                                                                             
                                                                             selected = 1),
                                                                 numericInput("Altura",
                                                                              h5("Estatura (cm)"),
                                                                              min = 0,
                                                                              value = 0)
                                                                 
                                                          ),
                                                          column(width = 6,align="center",
                                                                 selectInput("Paladar",
                                                                             h5("Forma del paladar"),
                                                                             choices = list("Forma 1" = 1,
                                                                                            "Forma 2" = 2,
                                                                                            "forma 3" = 3),
                                                                             selected = 1),
                                                                 selectInput("Paladar",
                                                                             h5("Forma de la Papila Incisal"),
                                                                             choices = list("Redonda, separada al rafé" = 1,
                                                                                            "Redonda, unida al rafé" = 2,
                                                                                            "Alargada, separada al rafé" = 3,
                                                                                            "Alargada, unida al rafé" = 4),
                                                                             
                                                                             selected = 1),
                                                                 selectInput("Paladar",
                                                                             h5("Prominencia de las rugocidades"),
                                                                             choices = list("Elevadas" = 1,
                                                                                            "Planas" = 2,
                                                                                            "No Aplica" = 3),
                                                                             
                                                                             selected = 1),
                                                                 radioButtons("Evaluacion",
                                                                              h5("Evaluacion"),
                                                                              choices = list("Primera" = 1,
                                                                                             "Segunda" = 2),
                                                                              selected = 1),
                                                                 actionButton("save","Add")
                                                                 
                                                                 
                                                          )
                                                 ),
                                                 tabPanel(width = 12, "Table", align="center", 
                                                          br(),
                                                          h4("Dataset"),
                                                          br(),
                                                          DT::dataTableOutput("responses", width = 500), tags$hr(),
                                                          downloadButton('download1',"Download the data")
                                                 ),#align="left"
                                                 tabPanel("Summary",align="center", 
                                                          br(),
                                                          h4("Descriptive analysis"),
                                                          br(),
                                                          DT::dataTableOutput("summary",width = 300),tags$hr()#,
                                                          #downloadButton('download2',"Download the data")
                                                 ),
                                                 tabPanel("Plot", 
                                                          br(),
                                                          h4("Distribution of information"),
                                                          plotOutput("plot"),
                                                          br(),
                                                          #h4("Distribution"),
                                                          plotlyOutput("plotss"))
                                                 
                                     )
                            )
                     )
            )
            
    )
  )  
)

ui <- dashboardPage(
  
  header, 
  sidebar,
  body#,

)

server <- function(input, output){
  
  
  #create a data frame called responses
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
      responses <<- rbind(responses, data)
    } else {
      responses <<- data
    }
  }
  
  loadData <- function() {
    if (exists("responses")) {
      responses
    }
  }
  
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  # When the Save button is clicked, save the form data
  observeEvent(input$save, {
    saveData(formData())
  })
  
  # Show the previous responses
  # (update with current response when save is clicked)
  output$responses <- DT::renderDataTable({
    input$save
    DT::datatable(loadData()) # OJO Con lo editable
  }) 
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)