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
source("fonctions/TAEG.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple-light",
  
  dashboardHeader(title = "Crédit immobilier",
                  titleWidth = 300),  ##titre de l app 
  dashboardSidebar(
    
    tags$head(
      tags$style(
        HTML('.main-sidebar .sidebar {color : black;}')
      )
    ),
    
    width = 300,
    
    # INPUTS 
    
    # h4("Entrées", align = "center"),
    
    
    ## Montant du crédit
    numericInput (inputId = "montant_credit",
               label = "Montant du crédit (frais compris) :",
               min = 1000,
               value = 20000),
    
    
    ## Montant des frais de dossiers et autres frais bancaires
    numericInput (inputId = "frais",
                  label = "Montant des frais de dossiers + autres frais :",
                  value = 1000),
    
    ## Durée du crédit en années
    sliderInput(inputId = "duree",
                label = "Durée du crédit :", 
                min = 1,
                max = 30,
                value = 10,
                post = " ans"),
    
    ## Taux d'intérêt annuel
    sliderInput(inputId = "taux_interet",
                label = "Taux d'intérêt annuel :", 
                min = 0.01,
                max = 6,
                step = 0.01,
                value = 5,
                post = "%"),
    
    ## Taux d'assurance
    sliderInput(inputId = "taux_assurance",
                label = "Taux d'assurance :", 
                min = 0,
                max = 3,
                step = 0.01,
                value = 0.5,
                post = "%"),
    
    hr(),
    
    # ONGLETS
    
    # h4("Onglets", align = "center"),
    
    sidebarMenu(
      ## premier onglet : fonctionnement du simulateur
      menuItem("Fonctionnement du simulateur", tabName = "fonctionnement"),
      
      ## deuxième onglet : les résumés (cout du crédit, montant des mensualités,...)
      menuItem("Résumé", tabName = "resume", 
               icon = icon("fa-solid fa-file", verify_fa = FALSE)), 
      ## tableau d'amortissement 
      menuItem("Tableau d'amortissement", tabName = "amortissement", 
               icon = icon("fa-regular fa-table", verify_fa = FALSE)),
      ## capacité d'emprunt
      menuItem("Capacité d'emprunt", tabName = "emprunt",
               icon = icon("fa-duotone fa-calculator", verify_fa = FALSE))

    )
  ),
  dashboardBody(
    tabItems(
    
      tabItem("fonctionnement",
              # textOutput("mode_emploi")
              h1("Binvenue sur le simulateur de crédit immobilier", align = "center"),
              h3("Fonctionnement : "),
              h4("1) Remplir les informations demandées sur la barre latérale gauche"),
              p("- Montant du crédit : la somme que vous souhaitez emprunter en euros."),
              p("- Montant de l'apport personnel (en euros)."),
              p("- Montant des charges mensuelles (en euros) : loyer, crédit maison, crédit auto, etc."),
              p("- Durée du crédit (en années)."),
              h4("2) Onglet : Résumé"),
              p("Vous trouverez les différents indicateurs calculés à l'aide des informations renseignées."),
              h4("3) Onglet : Tableau d'amortissement"),
              p("La tableau d'amortissement correspond à l'échancier de remboursement du crédit. Il est possible de le télécharger en cliquant sur le bouton de téléchargement."),
              h4("4) Onglet : Capacité d'emprunt")
      ), ## fin fonctionnement
      
      tabItem("resume",
              fluidRow(
                valueBoxOutput("montant_emprunte_total", width = 4),
                valueBoxOutput("duree_credit", width = 2),
                valueBoxOutput("taux_interet", width = 3),
                valueBoxOutput("taux_assurance", width = 3)
              ),
              fluidRow(
                valueBoxOutput("mensualite"),
                valueBoxOutput("mensualite_assurance"),
                valueBoxOutput("taeg"),
                valueBoxOutput("taux_endettement")
              ),
              fluidRow(
                valueBoxOutput("cout_credit"),
                valueBoxOutput("cout_interets"),
                valueBoxOutput("cout_assurances")
              )
              
      ), ## fin résumé
      
      tabItem("amortissement", 
              downloadButton("telechargement_tableau", label = "Télécharger le tableau d'amortissement"),
              hr(),
              box( 
                width = 12, 
                title = "Tableau d'amortissement",
                dataTableOutput("amortissement"),  ##tableau d'amortissement
                status = "danger", solidHeader = TRUE
              )
              
              
      ), ## fin amortissement
      
      tabItem("emprunt",
              
              ## Revenus de l'emprunteur 1
              numericInput (inputId = "revenu_emprunteur1",
                            label = "Revenus mensuels net de l'emprunteur 1 :",
                            value = 2000),
              
              ## Revenus de l'emprunteur 2
              numericInput (inputId = "revenu_emprunteur2",
                            label = "Revenus mensuels net de l'emprunteur 2 :",
                            value = 0),
              
              ## Montant de l'apport personnel
              numericInput (inputId = "apport_personnel",
                            label = "Montant de l'apport personnel :",
                            value = 2000),
              
              ## Charges mensuelles
              numericInput(inputId = "charges",
                           label = "Montant des charges mensuelles :",
                           value = 500)
              
      ) ## fin capacité d'emprunt
      
    ) ## fin tabitems
    
  )## fin body
   
)## fin app

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # output$mode_emploi <- renderText({
  #   str1 <- paste("Bienvenue sur le simulateur de crédit")
  #   str2 <- paste("Vous avez choisi une durée de crédit de", input$duree, "ans")
  #   HTML(paste(str1, str2, sep = "<br/>"))
  # }
  # )
  
  output$montant_emprunte_total <- renderValueBox({
    valueBox(
      paste(input$montant_credit, "€"),
      toupper("Montant total de l'emprunt (montant du crédit + frais)"),
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "purple"
    )
  })
  
  output$duree_credit <- renderValueBox({
    valueBox(
      paste(input$duree, "ans"),
      toupper("Durée du crédit"),
      # icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "purple"
    )
  })
  
  output$taux_assurance <- renderValueBox({
    valueBox(
      paste(input$taux_assurance, "%"),
      toupper("Taux d'assurance"),
      icon = icon("fa-duotone fa-percent", verify_fa = FALSE),
      color = "orange"
    )
  })
  
  amortissement <- reactive({tableauAmortissement(input$taux_interet,
                                                  as.numeric(input$montant_credit),
                                                  input$taux_assurance,
                                                  input$duree)})

  output$taux_interet <- renderValueBox({
    valueBox(
      paste(input$taux_interet,"%"), 
      toupper("Taux d'intérêt annuel"), 
      icon = icon("fa-duotone fa-percent", verify_fa = FALSE),
      color = "orange"
    )
  })
  
  coutTotalCredit <- reactive({
    (sum(amortissement()$mensualite) + sum(amortissement()$assurance) + input$frais) - input$montant_credit
  })
  
  output$cout_credit <- renderValueBox({
    valueBox(
      paste(coutTotalCredit(),"€"), 
      toupper("Côut total du crédit"), 
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "maroon"
    )
  })
  
  output$cout_interets <- renderValueBox({
    valueBox(
      paste(sum(amortissement()$interets),"€"), 
      toupper("Coût total des intérêts"), 
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "maroon"
    )
  })
  
  output$cout_assurances <- renderValueBox({
    valueBox(
      paste(sum(amortissement()$assurance),"€"), 
      toupper("Côut total des assurances"), 
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "maroon"
    )
  })
  
  output$mensualite <- renderValueBox({
    valueBox(
      paste(unique(amortissement()$mensualite),"€"), 
      toupper("Mensualités (hors assurance)"), 
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$mensualite_assurance <- renderValueBox({
    valueBox(
      paste(unique(amortissement()$mensualite) + unique(amortissement()$assurance),"€"), 
      toupper("Mensualités (avec assurance)"), 
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  calculTAEG <- reactive({
    mensualite_avec_assurance <- unique(amortissement()$mensualite) + unique(amortissement()$assurance)
    taeg <- TAEG(input$montant_credit, input$frais, mensualite_avec_assurance, input$duree) * 100
  })
  
  
  output$taeg <- renderValueBox({
    valueBox(
      paste(calculTAEG(), "%"), 
      toupper("TAEG"), 
      icon = icon("fa-duotone fa-percent", verify_fa = FALSE),
      color = "orange"
    )
  })
  
  output$taux_endettement <- renderValueBox({
    valueBox(
      paste("0"), 
      toupper("Taux d'endettement"), 
      icon = icon("fa-duotone fa-percent", verify_fa = FALSE),
      color = "orange"
    )
  })
  
  
  output$amortissement <- renderDataTable({tableauAmortissement(input$taux_interet,
                                                               as.numeric(input$montant_credit),
                                                               input$taux_assurance,
                                                               input$duree)})
  
  output$telechargement_tableau <- downloadHandler(
    filename = function() {
      paste("tableau_amortissement.csv")
    },
    content = function(file) {
      write.csv(tableauAmortissement(input$taux_interet,
                                     as.numeric(input$montant_credit),
                                     input$taux_assurance,
                                     input$duree), 
                file)
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
