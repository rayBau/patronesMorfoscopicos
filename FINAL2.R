library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinythemes)
library(plotly) 
library(wesanderson)

ui = tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = shinytheme("flatly"),
    #theme = shinytheme("slate"), # <--- To use a theme, uncomment this
    "PAPIME",
    tabPanel("Acerca de", icon = icon("home"),
             
             sidebarPanel(width = 12, align="center",
                          h1("Análisis de Patrones Morfoscópicos"),
                          h2("Odontología y Dactiloscopía"),
                          p("Propuesta didáctica interactiva para la identificación 
                            humana a través del análisis comparativo de patrones 
                            morfológicos en dactiloscopía y odontología forense."),
                          
             ),
             
             sidebarPanel(width = 4, align="center",
                          strong("Propuesta"),
                          p("Propuesta didáctica interactiva para la identificación 
                            humana a través del análisis comparativo de patrones 
                            morfológicos en dactiloscopía y odontología forense."),
                          
             ),
             sidebarPanel(width = 4, align="center",
                          strong("Didáctica"),
                          p("Propuesta didáctica interactiva para la identificación 
                            humana a través del análisis comparativo de patrones 
                            morfológicos en dactiloscopía y odontología forense."),
                          
             ),
             sidebarPanel(width = 4, align="center",
                          strong("Interactiva"),
                          p("Propuesta didáctica interactiva para la identificación 
                            humana a través del análisis comparativo de patrones 
                            morfológicos en dactiloscopía y odontología forense."),
                          
             ),
             
             ###################################
             sidebarPanel(width = 3, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Nombre 1"),
                          p("Semblanza")
                          
                          
             ),
             sidebarPanel(width = 3, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Nombre 2"),
                          p("Semblanza")
                          
                          
             ),
             sidebarPanel(width = 3, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Nombre 3"),
                          p("Semblanza")
                          
                          
             ),
             sidebarPanel(width = 3, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Nombre 3"),
                          p("Semblanza")
                          
                          
             ),
             ########################################################################
             
             
             
             sidebarPanel(width = 2, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Alumno 1"),
                          p("Semblanza")
                          
                          
             ),
             sidebarPanel(width = 2, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Alumno 2"),
                          p("Semblanza")
                          
                          
             ),
             sidebarPanel(width = 2, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Alumno 3"),
                          p("Semblanza")
                          
                          
             ),
             sidebarPanel(width = 2, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Alumno 4"),
                          p("Semblanza")
                          
                          
             ),
             sidebarPanel(width = 2, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Alumno 5"),
                          p("Semblanza")
                          
                          
             ),
             
             sidebarPanel(width = 2, align="center",
                          img(src="person1.png", width = 100, align = "center"),
                          br(),
                          strong("Alumno 6"),
                          p("Semblanza")
                          
                          
             ),
             
             
             ###################################
    ),
    
    tabPanel("Rugoscopía", icon = icon("graduation-cap"),
             sidebarPanel(width = 4,
                          fluidRow(width = 12,
                                   column(width = 12,align="left",
                                          textInput("ID", p("ID")),
                                          
                                   ),
                                   column(width = 6,align="left",
                                          numericInput("Edad",
                                                       p("Edad"),
                                                       min = 0,
                                                       max = 100,
                                                       step = 1,
                                                       value = 0),
                                          selectInput("Origen", 
                                                      p("Lugar de Origen"),
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
                                          selectInput("Paladar",
                                                      p("Forma del paladar"),
                                                      choices = list("Forma 1" = 1,
                                                                     "Forma 2" = 2,
                                                                     "Forma 3" = 3),
                                                      selected = 1),
                                          selectInput("Papila",
                                                      p("Forma de la Papila Incisal"),
                                                      choices = list("Redonda, separada" = 1,
                                                                     "Redonda, unida" = 2,
                                                                     "Alargada, separada" = 3,
                                                                     "Alargada, unida" = 4),
                                                      
                                                      selected = 1),
                                          
                                          
                                   ),
                                   column(width = 6,align="left",
                                          
                                          numericInput("Altura",
                                                       p("Estatura (cm)"),
                                                       min = 0,
                                                       value = 0),
                                          selectInput("Sex",
                                                       p("Sexo"),
                                                       choices = list("Masculino" = "Hombre",
                                                                      "Femenino" = "Mujer",
                                                                      "Desconocido" = "Desconocido"),
                                                       selected = 1),
                                          selectInput("Prom",
                                                      p("Prominencia"),
                                                      choices = list("Elevadas" = "Elevadas",
                                                                     "Planas" = "Planas",
                                                                     "No Aplica" = "NA"),
                                                      
                                                      selected = 1),
                                          selectInput("Evaluacion",
                                                       p("Evaluacion"),
                                                       choices = list("Primera" = 1,
                                                                      "Segunda" = 2),
                                                       selected = 1),
                                          
                                   )
                                   
                          ),
                          fluidRow(width = 12,
                                   column(width = 12, align = "center",
                                          h4("Patrón de Registro")),
                                   column(width = 12, align = "center",
                                          h5("Derecha"))),
                          
                          fluidRow(width = 12,
                                   column(width = 3, 
                                          selectizeInput("unoad",
                                                         strong("1A"),
                                                         c("Recta" = "Recta",
                                                           "Curva" = "Curva",
                                                           "Angular" = "Angular",
                                                           "Circular" = "Circular",
                                                           "Sinuosa" = "Sinuosa",
                                                           "Punto" = "Punto",
                                                           "Horquilla" = "Horquilla",
                                                           "Islote" = "Isole",
                                                           "Delta" = "Delta",
                                                           "No Aplica" = "No Aplica"),
                                                         multiple=TRUE, selected = "No Aplica"),
                                          selectInput("unobd",
                                                             strong("1B"),
                                                             choices = list("Recta" = "Recta",
                                                                            "Curva" = "Curva",
                                                                            "Angular" = "Angular",
                                                                            "Circular" = "Circular",
                                                                            "Sinuosa" = "Sinuosa",
                                                                            "Punto" = "Punto",
                                                                            "Horquilla" = "Horquilla",
                                                                            "Islote" = "Isole",
                                                                            "Delta" = "Delta",
                                                                            "No Aplica" = "No Aplica"
                                                             ),
                                                      selected = 1),
                                          ),
                                   column(width = 3,
                                          selectInput("dosad",
                                                             strong("2A"),
                                                             choices = list("Recta" = "Recta",
                                                                            "Curva" = "Curva",
                                                                            "Angular" = "Angular",
                                                                            "Circular" = "Circular",
                                                                            "Sinuosa" = "Sinuosa",
                                                                            "Punto" = "Punto",
                                                                            "Horquilla" = "Horquilla",
                                                                            "Islote" = "Isole",
                                                                            "Delta" = "Delta",
                                                                            "No Aplica" = "No Aplica"
                                                             ),
                                                      selected = 1),
                                          selectInput("dosbd",
                                                             strong("2B"),
                                                             choices = list("Recta" = "Recta",
                                                                            "Curva" = "Curva",
                                                                            "Angular" = "Angular",
                                                                            "Circular" = "Circular",
                                                                            "Sinuosa" = "Sinuosa",
                                                                            "Punto" = "Punto",
                                                                            "Horquilla" = "Horquilla",
                                                                            "Islote" = "Isole",
                                                                            "Delta" = "Delta",
                                                                            "No Aplica" = "No Aplica"
                                                             ),
                                                      selected = 1),
                                   ),
                                   column(width = 3,
                                          selectInput("tresad",
                                                             strong("3A"),
                                                             choices = list("Recta" = "Recta",
                                                                            "Curva" = "Curva",
                                                                            "Angular" = "Angular",
                                                                            "Circular" = "Circular",
                                                                            "Sinuosa" = "Sinuosa",
                                                                            "Punto" = "Punto",
                                                                            "Horquilla" = "Horquilla",
                                                                            "Islote" = "Isole",
                                                                            "Delta" = "Delta",
                                                                            "No Aplica" = "No Aplica"
                                                             ),
                                                      selected = 1),
                                          selectInput("tresbd",
                                                             strong("3B"),
                                                             choices = list("Recta" = "Recta",
                                                                            "Curva" = "Curva",
                                                                            "Angular" = "Angular",
                                                                            "Circular" = "Circular",
                                                                            "Sinuosa" = "Sinuosa",
                                                                            "Punto" = "Punto",
                                                                            "Horquilla" = "Horquilla",
                                                                            "Islote" = "Isole",
                                                                            "Delta" = "Delta",
                                                                            "No Aplica" = "No Aplica"
                                                             ),
                                                      selected = 1),
                                   ),
                                   column(width = 3,
                                          selectInput("cuatroad",
                                                             strong("4A"),
                                                             choices = list("Recta" = "Recta",
                                                                            "Curva" = "Curva",
                                                                            "Angular" = "Angular",
                                                                            "Circular" = "Circular",
                                                                            "Sinuosa" = "Sinuosa",
                                                                            "Punto" = "Punto",
                                                                            "Horquilla" = "Horquilla",
                                                                            "Islote" = "Isole",
                                                                            "Delta" = "Delta",
                                                                            "No Aplica" = "No Aplica"
                                                             ),
                                                      selected = 1),
                                          selectInput("cuatrobd",
                                                             strong("4B"),
                                                             choices = list("Recta" = "Recta",
                                                                            "Curva" = "Curva",
                                                                            "Angular" = "Angular",
                                                                            "Circular" = "Circular",
                                                                            "Sinuosa" = "Sinuosa",
                                                                            "Punto" = "Punto",
                                                                            "Horquilla" = "Horquilla",
                                                                            "Islote" = "Isole",
                                                                            "Delta" = "Delta",
                                                                            "No Aplica" = "No Aplica"
                                                             ),
                                                      selected = 1),
                                   )
                                   
                                   
                                   
                                   
                                   
                                   
                                   ),
                          ############################################################### IZQUIERDA
                          
                          fluidRow(width = 12,
                                   column(width = 12, align = "center",
                                          h5("Derecha"))),
                          
                          fluidRow(width = 12,
                                   column(width = 3, 
                                          
                                          selectizeInput("unoad",
                                                      strong("1A"),
                                                                   c("Recta" = "Recta",
                                                                     "Curva" = "Curva",
                                                                     "Angular" = "Angular",
                                                                     "Circular" = "Circular",
                                                                     "Sinuosa" = "Sinuosa",
                                                                     "Punto" = "Punto",
                                                                     "Horquilla" = "Horquilla",
                                                                     "Islote" = "Isole",
                                                                     "Delta" = "Delta",
                                                                     "No Aplica" = "No Aplica"),
                                                      multiple=TRUE, selected = "No Aplica"),
                                          selectInput("unobd",
                                                      strong("1B"),
                                                      choices = list("Recta" = "Recta",
                                                                     "Curva" = "Curva",
                                                                     "Angular" = "Angular",
                                                                     "Circular" = "Circular",
                                                                     "Sinuosa" = "Sinuosa",
                                                                     "Punto" = "Punto",
                                                                     "Horquilla" = "Horquilla",
                                                                     "Islote" = "Isole",
                                                                     "Delta" = "Delta",
                                                                     "No Aplica" = "No Aplica"
                                                      ),
                                                      selected = 1),
                                   ),
                                   column(width = 3,
                                          selectInput("dosad",
                                                      strong("2A"),
                                                      choices = list("Recta" = "Recta",
                                                                     "Curva" = "Curva",
                                                                     "Angular" = "Angular",
                                                                     "Circular" = "Circular",
                                                                     "Sinuosa" = "Sinuosa",
                                                                     "Punto" = "Punto",
                                                                     "Horquilla" = "Horquilla",
                                                                     "Islote" = "Isole",
                                                                     "Delta" = "Delta",
                                                                     "No Aplica" = "No Aplica"
                                                      ),
                                                      selected = 1),
                                          selectInput("dosbd",
                                                      strong("2B"),
                                                      choices = list("Recta" = "Recta",
                                                                     "Curva" = "Curva",
                                                                     "Angular" = "Angular",
                                                                     "Circular" = "Circular",
                                                                     "Sinuosa" = "Sinuosa",
                                                                     "Punto" = "Punto",
                                                                     "Horquilla" = "Horquilla",
                                                                     "Islote" = "Isole",
                                                                     "Delta" = "Delta",
                                                                     "No Aplica" = "No Aplica"
                                                      ),
                                                      selected = 1),
                                   ),
                                   column(width = 3,
                                          selectInput("tresad",
                                                      strong("3A"),
                                                      choices = list("Recta" = "Recta",
                                                                     "Curva" = "Curva",
                                                                     "Angular" = "Angular",
                                                                     "Circular" = "Circular",
                                                                     "Sinuosa" = "Sinuosa",
                                                                     "Punto" = "Punto",
                                                                     "Horquilla" = "Horquilla",
                                                                     "Islote" = "Isole",
                                                                     "Delta" = "Delta",
                                                                     "No Aplica" = "No Aplica"
                                                      ),
                                                      selected = 1),
                                          selectInput("tresbd",
                                                      strong("3B"),
                                                      choices = list("Recta" = "Recta",
                                                                     "Curva" = "Curva",
                                                                     "Angular" = "Angular",
                                                                     "Circular" = "Circular",
                                                                     "Sinuosa" = "Sinuosa",
                                                                     "Punto" = "Punto",
                                                                     "Horquilla" = "Horquilla",
                                                                     "Islote" = "Isole",
                                                                     "Delta" = "Delta",
                                                                     "No Aplica" = "No Aplica"
                                                      ),
                                                      selected = 1),
                                   ),
                                   column(width = 3,
                                          selectInput("cuatroad",
                                                      strong("4A"),
                                                      choices = list("Recta" = "Recta",
                                                                     "Curva" = "Curva",
                                                                     "Angular" = "Angular",
                                                                     "Circular" = "Circular",
                                                                     "Sinuosa" = "Sinuosa",
                                                                     "Punto" = "Punto",
                                                                     "Horquilla" = "Horquilla",
                                                                     "Islote" = "Isole",
                                                                     "Delta" = "Delta",
                                                                     "No Aplica" = "No Aplica"
                                                      ),
                                                      selected = 1),
                                          selectInput("cuatrobd",
                                                      strong("4B"),
                                                      choices = list("Recta" = "Recta",
                                                                     "Curva" = "Curva",
                                                                     "Angular" = "Angular",
                                                                     "Circular" = "Circular",
                                                                     "Sinuosa" = "Sinuosa",
                                                                     "Punto" = "Punto",
                                                                     "Horquilla" = "Horquilla",
                                                                     "Islote" = "Isole",
                                                                     "Delta" = "Delta",
                                                                     "No Aplica" = "No Aplica"
                                                      ),
                                                      selected = 1),
                                   )
                                   
                                   
                                   
                                   
                                   
                                   
                          ),
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          
                          ##############################################################
                          fluidRow(width = 12,
                                   column(width = 12, align = "center",
                                          actionButton("save","Add")),
                                   ),
                          
                          
                          
                          
                          
                          
             ),
             #sidebarPanel(width = 4),
             mainPanel(width = 8,
                       tabsetPanel(
                         tabPanel("Descripción", 
                                  sidebarPanel(width = 12, align="left",
                                               h4("Rugoscopía Palatina"),
                                               p("Es una técnica de identificación que estudia, clasifica y registra las 
                            rugosidades palatinas con el propósito de ser utilizados en casos complejos 
                            de identificación en que no se pueda o sea posible la utilización de los métodos 
                            convencionales usados en la criminalística. Consta de un registro denominado ficha 
                            rugoscópica, que contiene diferentes apartados que se verán a continuación."),
                                               p(strong("ID:"), "Corresponde al identificador del modelo de estudio al cual se le
                            realizará la rugoscopía, este número es asignado según la obtención cronológica
                            de los modelos de estudio."),
                                               p(strong("Edad: "), "Corresponde a la edad del evaluado."),
                                               p(strong("Sexo: "), br(), " * Femenino", br(), "* Masculino"),
                                               p(strong("Lugar de Origen:"), "Si el lugar de origen no se encuentra dentro de la
                            república Mexicana, elegir 'otro'."),
                                               p(strong("Estatura: "), "Estatura del evaluado medida en centímetro, si se descononce
                            solo omitir dato."),
                                               p(strong("Prominencia de Rugocidades: "), "Corresponde a que tan marcadas están las rugas paltinas."),
                                               p(strong("Evaluación: "), "Corresponde a si es la primera o segunda vez que se realiza el registro.")
                                               
                                  ),
                                  sidebarPanel(width = 5, align="left",
                                               p(strong("Forma del Paladar: "), "Se anota en el recuadro correspondiente la forma a la cual la línea trazada en el 
                                       perímetro de los órganos dentarios tenga mayor parecido con la clasificación de las formas de los arcos 
                                       dentales que se muestra a continuación:", br(), "* Forma 1: Arciforme", br(), "* Forma 2: Triangular", br(), "* Forma 3: Rectangular")
                                               
                                  ),
                                  sidebarPanel(width = 7, align="center",
                                               img(src="Forma.png", width = 450, height = 200,align = "center")
                                  ),
                                  
                                  sidebarPanel(width = 5, align="left",
                                               p(strong("Forma de la Papila Incisal: "), "Se coloca en el recuadro la forma de la papila incisiva, dentro de las cuales puede ser:",
                                                 br(), "* Forma 1: Redonda y separada de rafé.", br(), "* Forma 2: Redonda y unida al rafé.", br(),
                                                 "* Forma 3: Alargada y unida al rafé.", br(), "* Forma 4: Alargada y unida al rafé.")
                                               
                                  ),
                                  sidebarPanel(width = 7, align="center",
                                               img(src="Forma.png", width = 450, height = 160,align = "center")
                                  ),
                                  sidebarPanel(width = 5, align="left",
                                               p(strong("Forma de la Papila Incisal: "), "Orientación de las rugas palatinas en base al rafé medio, opciones:",
                                                 br(), "* Forma 1: Mesial", br(), "* Forma 2: Lateral", br(),
                                                 "* Forma 3: Distal", br(), "* Forma 4: Variada")
                                               
                                  ),
                                  sidebarPanel(width = 7, align="center",
                                               img(src="forma2.png", width = 450, height = 140,align = "center")
                                  ),
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                                  
                         ),
                         tabPanel("Datos",
                                  DT::dataTableOutput("responses", width = 500), tags$hr(),
                                  downloadButton('download1',"Download the data")
                         ),
                         tabPanel("Summary",
                                  h4("Summary"),
                                  verbatimTextOutput("summary"),tags$hr(),
                                  
                                  plotlyOutput("plotss8"),
                                  plotlyOutput("plotss9")
                                  #tableOutput("view")
                                  #verbatimTextOutput("summary")
                                  
                                  #plotlyOutput("plotss",height =560)
                         ),
                         tabPanel("Plots",
                                  ############################################ Edad
                                  sidebarPanel(width = 6, align="left",
                                               plotlyOutput("plotss")
                                               
                                  ),
                                  ############################################ Altura
                                  sidebarPanel(width = 6, align="right",
                                               plotlyOutput("plotss2")
                                               
                                  ),
                                  ############################################ PAPILA
                                  sidebarPanel(width = 4, align="right",
                                               plotlyOutput("plotss3")
                                               
                                  ),
                                  
                                  sidebarPanel(width = 4, align="right",
                                               plotlyOutput("plotss4")
                                               
                                  ),
                                  sidebarPanel(width = 4, align="right",
                                               plotlyOutput("plotss5")
                                               
                                  )
                                  
                                  
                         ),
                         tabPanel("Patrón de Registro",
                                  sidebarPanel(width = 12, align="right",
                                               plotlyOutput("plotss6")
                                               
                                               
                                  )
                                  
                                  
                                  
                                  
                         )
                       )
             ),
             
    ),
    
    tabPanel("Queiloscopía", icon = icon("graduation-cap"), 
             "This panel is intentionally left blank"),
    tabPanel("Contacto", icon = icon("address-book"),
             href = "http://www.cienciaforense.facmed.unam.mx/")
  )
)


#fields <- c("ID","Evaluador","Evaluado","Edad","Origen","Altura","Paladar","Papila","Prom","check","Evaluacion")
fields <- c("ID","Sex","Edad","Origen","Altura","Paladar","Papila","Prom","Evaluacion","unoad","dosad", "tresad", "cuatroad", "unobd", "dosbd", "tresbd", "cuatrobd")

server = function(input, output) {
  #responses =  NULL
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
    DT::datatable(loadData())
  }) 
  
  
  ################################################ PLOT
  
  
  output$plotss <- renderPlotly({
    input$save
    data3 = loadData()
    
    data3[,3] <- as.integer(data3[,3])
    #data3[,5] <- as.integer(data3[,5])
    data4 <- data3[,3]
    if (is.null(loadData()))
      return(NULL)
    
    #p <- plot_ly(data3, x =~data3[,2] , y = ~data3[,5])
    
    
    p <- ggplot(data3, aes(x=Sex, y = Edad,fill = Sex))
    p <- p + geom_boxplot()
    p <- p + theme_bw()
    p <- p + labs(title = "Edad")
    p <- p + labs(subtitle = "Diagrama de caja y bigotes de edades por sexo")
    #p <- p + scale_fill_manual(values=wes_palette(n=3, name="Darjeeling"))
    #p <- p + scale_fill_brewer(palette="BrBG")
    p <- p + scale_fill_manual(values=c("#f95959", "#455d7a", "#0c3c78", "#04879c")) 
    
    #boxplot(data3$Edad)
  })

  output$plotss2 <- renderPlotly({
    input$save
    data3 = loadData()
    
    data3[,3] <- as.integer(data3[,3])
    #data3[,5] <- as.integer(data3[,5])
    data4 <- data3[,3]
    if (is.null(loadData()))
      return(NULL)
    
    #p <- plot_ly(data3, x =~data3[,2] , y = ~data3[,5])
    
    
    p <- ggplot(data3, aes(x=Sex, y=Altura, fill = Sex))
    p <- p + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
    p <- p + theme_bw()
    p <- p + labs(title = "Altura")
    p <- p + labs(subtitle = "Diagrama de caja y bigotes de edades por sexo")
    p <- p + scale_fill_manual(values=c("#f95959", "#455d7a", "#0c3c78", "#04879c")) 
    p
    #boxplot(data3$Edad)
  })

  output$plotss3 <- renderPlotly({
    input$save
    data3 = loadData()
    
    data3[,3] <- as.integer(data3[,3])
    #data3[,5] <- as.integer(data3[,5])
    if (is.null(loadData()))
      return(NULL)
    
    p <- ggplot(data3, aes(x = Paladar, fill = Paladar))
    p <- p + geom_bar(width = 1, colour = "white")
    p <- p + geom_text(aes(y = ..count.., label = ..count..),
                       stat = "count", color = "white",
                       hjust = 1.0, size = 3)
    p <- p + theme(legend.position = "none")
    #p <- p + coord_flip()
    
    
    p <- p + labs(title = "Forma del Paladar")
    p <- p + scale_fill_manual(values=c("#f95959", "#455d7a", "#233142", "#04879c")) 
    p
    
    p <- p + stat_count(width=1, colour="white", geom="bar")
  })   
  
  output$plotss4 <- renderPlotly({
    input$save
    data3 = loadData()
    
    data3[,3] <- as.integer(data3[,3])
    #data3[,5] <- as.integer(data3[,5])
    if (is.null(loadData()))
      return(NULL)
    
    p <- ggplot(data3, aes(x = Prom, fill = Prom))
    p <- p + geom_bar(width = 1, colour = "white")
    p <- p + geom_text(aes(y = ..count.., label = ..count..),
                       stat = "count", color = "white",
                       hjust = 1.0, size = 3)
    p <- p + theme(legend.position = "none")
    #p <- p + coord_flip()
    
    
    p <- p + labs(title = "Prominencia de Rugocidades")
    p <- p + scale_fill_manual(values=c("#f95959", "#455d7a", "#233142", "#04879c")) 
    p
    
    p <- p + stat_count(width=1, colour="white", geom="bar")
  }) 
  
  output$plotss5 <- renderPlotly({
    input$save
    data3 = loadData()
    
    data3[,3] <- as.integer(data3[,3])
    #data3[,5] <- as.integer(data3[,5])
    if (is.null(loadData()))
      return(NULL)
    
    p <- ggplot(data3, aes(x = Sex, fill = Sex))
    p <- p + geom_bar(width = 1, colour = "white")
    p <- p + geom_text(aes(y = ..count.., label = ..count..),
                       stat = "count", color = "white",
                       hjust = 1.0, size = 3)
    p <- p + theme(legend.position = "none")
    #p <- p + coord_flip()
    
    
    p <- p + labs(title = "Sexo")
    p <- p + scale_fill_manual(values=c("#f95959", "#455d7a", "#233142", "#04879c")) 
    p
    
    p <- p + stat_count(width=1, colour="white", geom="bar")
  }) 
  
  output$plotss6 <- renderPlotly({
    input$save
    data3 = loadData()

    if (is.null(loadData()))
      return(NULL)
    
    recta <- data3[c("Sex","unoad")] %>% filter(unoad == "Recta")
    
    
    recta
    
    table(recta$Sex)
    
    names(table(recta$Sex))
    
    
    gBarChart <- data.frame(Sexo2 = names(table(recta$Sex)),
                            GroupA = as.numeric(table(recta$Sex))
                            
    )
    head(gBarChart)
    
    
    p3 <- plot_ly(gBarChart,
                  x = "A1",
                  y = ~GroupA,
                  type = "bar",
                  name = "Recta") %>% 
      layout(yaxis = list(title = "Cities"),
             barmode = "stack")
    p3

   
  })

  output$plotss7 <- renderPlotly({
    input$save
    data3 = loadData()
    
    data3[,3] <- as.integer(data3[,3])
    #data3[,5] <- as.integer(data3[,5])
    if (is.null(loadData()))
      return(NULL)
    
    p <- ggplot(data3, aes(x = Origen, fill = Origen))
    p <- p + geom_bar(width = 1, colour = "white")
    p <- p + geom_text(aes(y = ..count.., label = ..count..),
                       stat = "count", color = "white",
                       hjust = 1.0, size = 3)
    p <- p + theme(legend.position = "none")
    #p <- p + coord_flip()
    
    
    p <- p + labs(title = "Origen")
    p <- p + scale_fill_manual(values=c("#f95959","#e84545","#903749", "#53354a",  "#233142", "#455d7a",  "#389393", "#5eaaa8", "#04879c","#f05945"))
    p

  }) 
  
  output$plotss8 <- renderPlotly({
    input$save
    data3 = loadData()
    
    data3[,3] <- as.integer(data3[,3])
    #data3[,5] <- as.integer(data3[,5])
    if (is.null(loadData()))
      return(NULL)
    table(data3$unoad)
    
    p <- ggplot(data = data3,
           mapping = aes(x = Edad, fill = factor(Sex))) + geom_histogram(bins = 9, position = 'identity', alpha = 0.8)
    p <- p + labs(title = 'Histograma Frencuancias Edades', fill = 'vs Sexo', x = 'Edad')
    p
  
  }) 
  
  output$plotss9 <- renderPlotly({
    input$save
    data3 = loadData()
    
    data3[,3] <- as.integer(data3[,3])
    data3[,5] <- as.integer(data3[,5])
    #data3[,5] <- as.integer(data3[,5])
    if (is.null(loadData()))
      return(NULL)
    table(data3$unoad)
    
    p <- ggplot(data = data3,
                mapping = aes(x = Altura, fill = factor(Sex))) + geom_histogram(bins = 9, position = 'identity', alpha = 0.8)
    p <- p + labs(title = 'Histograma Frencuancias Estatura', fill = 'vs Sexo', x = 'Estatura')
    p
    
  }) 
  
  
  
  
  
  ## Summary
  
  
  output$summary <- renderPrint({
    input$save
    data3 = loadData()
    data3[,3] <- as.integer(data3[,3])
    data3[,5] <- as.integer(data3[,5])
    #aux <- 
    summary(data3[c("Edad", "Altura")])
  })
  
  #output$view <- renderTable({
  #  head(loadData())
  
  #})
}



shinyApp(ui, server)

