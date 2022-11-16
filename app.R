#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(shinydashboard)
require(shinydashboardPlus)

source("fonctions/calculerMensualite.R")
source("fonctions/tableauAmortissement.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple-light",
  
  dashboardHeader(title = "Crédit immobilier"),  ##titre de l app 
  dashboardSidebar(
    
    tags$head(
      tags$style(
        HTML('.main-sidebar .sidebar {color : black;}')
      )
    ),
    
    # INPUTS 
    
    h3("Entrées", align = "center"),
    
    ## Durée du crédit en années
    sliderInput(inputId = "duree",
                label = "Nombre d'années :", 
                min = 1,
                max = 30,
                value = 1,
                post = " ans",
                ticks = FALSE),
    
    ## Taux d'intérêt annuel
    sliderInput(inputId = "taux_interet",
                label = "Taux d'intérêt annuel :", 
                min = 0.01,
                max = 5,
                step = 0.01,
                value = 5,
                post = "%"),
    
    ## Taux d'assurance
    sliderInput(inputId = "taux_assurance",
                label = "Taux d'assurance :", 
                min = 0.01,
                max = 3,
                step = 0.01,
                value = 0.35,
                post = "%"),
    
    ## Montant du crédit
    textInput (inputId = "montant_credit",
               label = "Montant du crédit :",
               value = "10000"),

    hr(),
    
    # ONGLETS
    
    h3("Onglets", align = "center"),
    
    sidebarMenu(
      ## premier onglet avec les résumés (cout du crédit, montant des mensualités,...)
      menuItem("Résumé", tabName = "resume"), 
      ## tableau d'amortissement 
      menuItem("Tableau d'amortissement", tabName = "amortissement")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("resume",
              fluidRow(
                valueBoxOutput("taux_interet"),
                valueBoxOutput("taeg"),
                valueBoxOutput("cout_total"),
                valueBoxOutput("mensualite"),
                valueBoxOutput("cout_credit"),
                valueBoxOutput("cout_interets"),
                valueBoxOutput("cout_assurances"),
                valueBoxOutput("taux_endettement")
                
              )
              
      )###fin du premier onglet 
      ,
      
      tabItem("amortissement", 
              box( 
                width = 12, 
                title = "Tableau d'amortissement",
                dataTableOutput("amortissement")  ##tableau d'amortisseemnet
              )
              
              
      ) ##fin amortissement
    ) ##fin tabitems
  )##fin body
)##fin app

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  amortissement <- reactive({tableauAmortissement(input$taux_interet,
                                                  as.numeric(input$montant_credit),
                                                  input$taux_assurance,
                                                  input$duree)})

  output$taux_interet <- renderValueBox({
    valueBox(
      paste(input$taux_interet,"%"), 
      toupper("Taux d'intérêt annuel"), 
      icon = icon("time", lib = "glyphicon"),
      color = "light-blue", 
      width = 4
    )
  })
  
  coutTotalCredit <- reactive({
    sum(amortissement()$mensualite) - as.numeric(input$montant_credit)
  })
  
  output$cout_credit <- renderValueBox({
    valueBox(
      paste(coutTotalCredit(),"€"), 
      toupper("Côut total du crédit"), 
      icon = icon("time", lib = "glyphicon"),
      color = "light-blue", 
      width = 4
    )
  })
  
  output$cout_interets <- renderValueBox({
    valueBox(
      paste(sum(amortissement()$interets),"€"), 
      toupper("Coût total des intérêts"), 
      icon = icon("time", lib = "glyphicon"),
      color = "light-blue", 
      width = 4
    )
  })
  
  output$cout_assurances <- renderValueBox({
    valueBox(
      paste(sum(amortissement()$assurance),"€"), 
      toupper("Côut total des assurances"), 
      icon = icon("time", lib = "glyphicon"),
      color = "light-blue", 
      width = 4
    )
  })
  
  output$mensualite <- renderValueBox({
    valueBox(
      paste(unique(amortissement()$mensualite),"€"), 
      toupper("Mensualités"), 
      icon = icon("time", lib = "glyphicon"),
      color = "light-blue", 
      width = 4
    )
  })
  
  
  output$amortissement <- renderDataTable({tableauAmortissement(input$taux_interet,
                                                               as.numeric(input$montant_credit),
                                                               input$taux_assurance,
                                                               input$duree)})
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
