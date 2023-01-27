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
require(ggplot2)
require(plotly)
require(dplyr)

source("fonctions/calculerMensualite.R")
source("fonctions/tableauAmortissement.R")
source("fonctions/TAEG.R")
source("fonctions/calculCapaciteEmprunt.R")

## Le taux moyen suivant la durée du crédit correspond au bon taux sur le site suivant :
## https://www.meilleurtaux.com/credit-immobilier/barometre-des-taux.html
## Nous avons pris les taux de la mise à jour du 28/11

vecteur_taux_moyen <- c(rep(2.05,9), rep(2.11,2), rep(2.23, 3), rep(2.3,5), 
                        rep(2.42,5), rep(2.59, 6))
vecteur_annee <- seq(1,30)


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "purple-light",
  
  dashboardHeader(title = "Crédit immobilier",
                  titleWidth = 300),
  ##titre de l app
  dashboardSidebar(
    tags$head(tags$style(
      HTML('.main-sidebar .sidebar {color : black;}')
    )),
    
    width = 300,
    
    # INPUTS
    
    h4("Entrées", align = "center"),
    
    ## Montant du crédit
    numericInput (
      inputId = "montant_credit",
      label = "Montant du crédit (frais compris) :",
      min = 1000,
      value = 80000
    ),
    
    
    ## Montant des frais de dossiers et autres frais bancaires
    numericInput (
      inputId = "frais",
      label = "Montant des frais de dossiers + autres frais :",
      value = 1000
    ),
    
    ## Durée du crédit en années
    sliderInput(
      inputId = "duree",
      label = "Durée du crédit :",
      min = 1,
      max = 30,
      value = 20,
      post = " ans"
    ),
    
    ## Taux d'intérêt annuel
    sliderInput(
      inputId = "taux_interet",
      label = "Taux d'intérêt annuel :",
      min = 0.1,
      max = 6,
      step = 0.1,
      value = 1,
      post = "%"
    ),
    
    ## Taux d'assurance
    sliderInput(
      inputId = "taux_assurance",
      label = "Taux d'assurance :",
      min = 0,
      max = 3,
      step = 0.1,
      value = 0.5,
      post = "%"
    ),
    
    hr(),
    
    # ONGLETS
    
    h4("Onglets", align = "center"),
    
    sidebarMenu(
      ## premier onglet : fonctionnement du simulateur
      menuItem(
        "Fonctionnement du simulateur", 
        tabName = "fonctionnement",
        icon = icon("fa-solid fa-info", verify_fa = FALSE)
      ),
      
      ## deuxième onglet : les résumés (cout du crédit, montant des mensualités,...)
      menuItem(
        "Résumé",
        tabName = "resume",
        icon = icon("fa-solid fa-file", verify_fa = FALSE)
      ),
      ## tableau d'amortissement
      menuItem(
        "Tableau d'amortissement",
        tabName = "amortissement",
        icon = icon("fa-regular fa-table", verify_fa = FALSE)
      ),
      ## capacité d'emprunt
      menuItem(
        "Capacité d'emprunt",
        tabName = "emprunt",
        icon = icon("fa-duotone fa-calculator", verify_fa = FALSE)
      )
      
    )
  ),
  
  dashboardBody(tabItems(
    tabItem(
      "fonctionnement",
      includeMarkdown("fonctionnement.md")
      
    ),
    ## fin fonctionnement
    
    tabItem(
      "resume",
      column(
        width = 3
      ),
      column(
        width = 6,
        valueBoxOutput("mensualite", width = NULL),
        valueBoxOutput("mensualite_assurance", width = NULL),
        valueBoxOutput("taeg", width = NULL),
        valueBoxOutput("cout_credit", width = NULL),
        valueBoxOutput("cout_interets", width = NULL),
        valueBoxOutput("cout_assurances", width = NULL),
        valueBoxOutput("taux_endettement", width = NULL)
      ),
      column(
        width = 3
      )
      
    ),
    ## fin résumé
    
    tabItem(
      "amortissement",
      downloadButton("telechargement_tableau", 
                     label = "Télécharger le tableau d'amortissement"),
      hr(),
      box(
        width = 12,
        title = "Tableau d'amortissement",
        ## Tableau d'amortissement
        dataTableOutput("amortissement"),
        status = "danger",
        solidHeader = TRUE
      )
    ),
    ## fin amortissement
    
    tabItem(
      "emprunt",
      fluidRow(
        box(
          title = p("Capacité d'emprunt", align = "center"),
          status = "purple",
          solidHeader = TRUE,
          width = 12,

          ## box pour les revenus
          box(
            title = "Revenus",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            box(
              title = "Emprunteur 1",
              status = "danger",
              solidHeader = TRUE,
              numericInput(
                inputId = "revenu_emprunteur1",
                label = "Revenus mensuels nets :",
                value = 2000
              ),
              radioButtons(
                inputId = "nb_mois_emprunteur1",
                label = "sur",
                choices = c("12 mois", "13 mois", "14 mois", "15 mois"),
                selected = "12 mois",
                inline = TRUE
              )
            ),
            box(
              title = "Emprunteur 2",
              status = "danger",
              solidHeader = TRUE,
              numericInput(
                inputId = "revenu_emprunteur2",
                label = "Revenus mensuels nets :",
                value = 0
              ),
              radioButtons(
                inputId = "nb_mois_emprunteur2",
                label = "sur",
                choices = c("12 mois", "13 mois", "14 mois", "15 mois"),
                selected = "12 mois",
                inline = TRUE
              )
            )
          ),
          ## fin box revenus
          
          ## box autres revenus
          box(
            title = "Autres revenus (allocations familiales, pension alimentaire reçue, revenus financiers, revenus locatifs, etc)",
            status = "danger",
            solidHeader = TRUE,
            width = 2,
            numericInput(
              inputId = "autres_revenus",
              label = "Montant des autres revenus :",
              value = 0
            )
          ), 
          ## box autres revenus
          
          ## box des charges mensuelles
          box(
            title = "Charges mensuelles (loyer, prêt immobilier, crédit à la consommation, crédit auto/moto, pension alimentaire versée, etc)",
            status = "success",
            solidHeader = TRUE,
            width = 2,
            numericInput(
              inputId = "charges",
              label = "Montant total des charges mensuelles :",
              value = 500
            )
          ),
          ## fin box charges
          
          ## box pour l'apport personnel
          box(
            title = "Apport personnel",
            status = "primary",
            solidHeader = TRUE,
            width = 2,
            numericInput(
              inputId = "apport_personnel",
              label = "Montant de l'apport personnel :",
              value = 5000
            )
          ),
          ## fin box apport personnel
          
          ## box taux endettement souhaité
          box(
            title = "Taux d'endettement",
            status = "warning",
            solidHeader = TRUE,
            width = 2,
            sliderInput(
              inputId = "taux_endettement",
              label = "Taux d'endettement souhaité :",
              min = 10,
              max = 35,
              step = 1,
              value = 30,
              post = "%"
            )
          ) ## fin box taux endettement
        ) 
      ), ## fin fluidrow

      column(
        width = 3,
        valueBoxOutput("capacite_emprunt_mensualite", width = NULL),
        valueBoxOutput("capacite_emprunt", width = NULL),
        valueBoxOutput("capacite_emprunt_total", width = NULL)
      ),
      column(
        width = 3,
        plotlyOutput("graphique_reste_a_vivre")
      ),
      column(
        width = 6,
        plotlyOutput("graphique_capacite_emprunt")
      )
      
    ) ## fin capacité d'emprunt
    
  ) ## fin tabitems
  ) ## fin body
) ## fin app
  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Tableau d'amortissement
  
  amortissement <- reactive({
      tableauAmortissement(
        input$taux_interet,
        as.numeric(input$montant_credit),
        input$taux_assurance,
        input$duree
      )
  })
  
  output$amortissement <- renderDataTable({ 
    amortissement() 
  })
  
  output$telechargement_tableau <- downloadHandler(
    filename = function() {
      paste("tableau_amortissement.csv")
    },
    content = function(file) {
      write.csv(
        tableauAmortissement(
          input$taux_interet,
          as.numeric(input$montant_credit),
          input$taux_assurance,
          input$duree
        ),
        file
      )
    }
  )
  
  ## Cout total du crédit
  
  coutTotalCredit <- reactive({
    (sum(amortissement()$mensualite) + sum(amortissement()$assurance) 
     + input$frais) - input$montant_credit
  })
  
  output$cout_credit <- renderValueBox({
    valueBox(
      paste(round(coutTotalCredit(),2), "€"),
      toupper("Côut total du crédit"),
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "maroon"
    )
  })
  
  ## Intérêts
  
  output$cout_interets <- renderValueBox({
    valueBox(
      paste(round(sum(amortissement()$interets),2), "€"),
      toupper("Coût total des intérêts"),
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "maroon"
    )
  })
  
  ## Assurances
  
  output$cout_assurances <- renderValueBox({
    valueBox(
      paste(round(sum(amortissement()$assurance),2), "€"),
      toupper("Côut total des assurances"),
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "maroon"
    )
  })
  
  ## Mensualités hors et avec assurance
  
  output$mensualite <- renderValueBox({
    valueBox(
      paste(round(unique(amortissement()$mensualite),2), "€"),
      toupper("Mensualités (hors assurance)"),
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  output$mensualite_assurance <- renderValueBox({
    valueBox(
      paste(round(unique(amortissement()$mensualite) + 
                    unique(amortissement()$assurance),2),"€"),
      toupper("Mensualités (avec assurance)"),
      icon = icon("calendar"),
      color = "blue"
    )
  })
  
  ## TAEG
  
  calculTAEG <- reactive({
    mensualite_avec_assurance <- unique(amortissement()$mensualite) + 
      unique(amortissement()$assurance)
    TAEG(input$montant_credit,
         input$frais,
         mensualite_avec_assurance,
         input$duree)
  })
  
  
  output$taeg <- renderValueBox({
    valueBox(
      paste(round(calculTAEG(),2), "%"),
      toupper("TAEG"),
      icon = icon("fa-duotone fa-percent", verify_fa = FALSE),
      color = "orange"
    )
  })
  
  ## Revenus des emprunteurs
  
  revenuEmprunteur1 <- reactive({
    ## revenu de l'emprunteur 1 sur 12 mois
    input$revenu_emprunteur1 * as.numeric(substr(input$nb_mois_emprunteur1, 1, 2)) / 12
  })
  
  revenuEmprunteur2 <- reactive({
    ## revenu de l'emprunteur 2 sur 12 mois
    input$revenu_emprunteur2 * as.numeric(substr(input$nb_mois_emprunteur2, 1, 2)) / 12
  })
  
  ## Taux d'endettement
  
  tauxEndettement <- reactive({
    input$charges / (revenuEmprunteur1() + revenuEmprunteur2() + input$autres_revenus) * 100
  })
  
  output$taux_endettement <- renderValueBox({
    valueBox(
      paste(round(tauxEndettement(),2), "%"),
      toupper("Taux d'endettement correspondant"),
      icon = icon("fa-duotone fa-percent", verify_fa = FALSE),
      color = "orange"
    )
  })
  
  ## Capacité d'emprunt
  
  capaciteEmpruntMensualite <- reactive({
    # (revenuEmprunteur1() + revenuEmprunteur2() + input$autres_revenus) * input$taux_endettement / 100 - input$charges
    (revenuEmprunteur1() + revenuEmprunteur2() + input$autres_revenus 
     - input$charges) * input$taux_endettement / 100
  })
  
  output$capacite_emprunt_mensualite <- renderValueBox({
    valueBox(
      paste(round(capaciteEmpruntMensualite(),2), "€"),
      toupper("Capacité d'emprunt par mois"),
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "maroon"
    )
  })
  
  capaciteEmprunt <- reactive({
    capacite <- calculCapaciteEmprunt(
      capaciteEmpruntMensualite(),
      vecteur_taux_moyen[input$duree],
      input$taux_assurance,
      input$duree
    )
    round(capacite,2)
  })
  
  output$capacite_emprunt <- renderValueBox({
    valueBox(
      paste(round(capaciteEmprunt(),2), "€"),
      toupper("Capacité d'emprunt"),
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "maroon"
    )
  })
  
  output$capacite_emprunt_total <- renderValueBox({
    valueBox(
      paste(round(capaciteEmprunt() + input$apport_personnel - input$frais,2), "€"),
      toupper("Capacité d'achat / d'acquisition"),
      icon = icon("fa-solid fa-euro-sign", verify_fa = FALSE),
      color = "maroon"
    )
  })
  
  ## Calculs et graphiques supplémentaires
  
  ### Répartition du montant pour le 'reste à vivre'
  
  tableauResteAVivre <- reactive({
    reste_a_vivre <- (revenuEmprunteur1() + revenuEmprunteur2() + 
                        input$autres_revenus) - input$charges - capaciteEmpruntMensualite()
    df <- data.frame(categorie <- c("Charges", "Emprunt", "Reste à vivre"),
                     total <- c(round(input$charges,2), 
                                round(capaciteEmpruntMensualite(),2), 
                                round(reste_a_vivre,2)))
    colnames(df) <- c("categorie", "total")
    df
  })

  repartitionResteAVivre <- reactive({
    colors <- c('rgb(211,94,96)', 'rgb(114,147,203)', 'rgb(144,103,167)')
    tableauResteAVivre() %>%
      plot_ly(labels = ~categorie, values = ~total, type = "pie",
              textposition = 'inside',
              text = ~paste(total, '€'),
              textinfo = 'label+text',
              hoverinfo = 'percent',
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1)),
              showlegend = FALSE
      ) %>%
      layout(title = list(text = "<b> Répartition des dépenses \n sur un mois </b>", 
                          align = "center"),
             margin = list(l = 10, r = 10, t = 70, b = 10)
      )
  })
  
  output$graphique_reste_a_vivre <- renderPlotly(repartitionResteAVivre())
  
  ### Répartition de la capacité d'emprunt
  
  tableauCapaciteEmprunt <- reactive({
    vecteur_emprunt <- rep(0,30)
    for (i in 1:30){
      vecteur_emprunt[i] <- 
        calculCapaciteEmprunt(
          capaciteEmpruntMensualite(),
          vecteur_taux_moyen[vecteur_annee[i]],
          input$taux_assurance,
          vecteur_annee[i]) + 
        input$apport_personnel - 
        input$frais
    }
    df <- data.frame(nb_annee = vecteur_annee, 
                     capacite_emprunt = round(vecteur_emprunt,2)
    )
  })
  
  repartitionCapaciteEmprunt <- reactive({
    p <- ggplot(data = tableauCapaciteEmprunt()) +
      aes(x = nb_annee, y = capacite_emprunt) +
      geom_line(stat = "identity", color = "deeppink3") + geom_point(color = "deeppink3") + 
      ggtitle(paste("Montant du prêt immobilier pour une mensualité de \n",
                    round(capaciteEmpruntMensualite(),2),
                    "€ / mois selon les durées de remboursement")) +
      xlab("Durée du crédit") + scale_x_continuous(breaks=vecteur_annee) +
      ylab("Capacité d'emprunt (en €)") +
      theme(plot.title = element_text(hjust = 0.5, size = 13, face = "bold"),
            plot.margin = margin(l = 10, r = 10, t = 20, b = 20)
      )
    ggplotly(p)
  })
  output$graphique_capacite_emprunt <- renderPlotly(repartitionCapaciteEmprunt())
  
}

# Run the application
shinyApp(ui = ui, server = server)
