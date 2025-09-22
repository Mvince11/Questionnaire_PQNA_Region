#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

  shinyjs::useShinyjs(),
  tags$script(type = "text/javascript", src = "index.js"), # Utile pour le lien interne
  includeCSS("www/styles.css"),#css du site
  ##########################################. 
  ## NavbarPage ----------
  ##########################################.
  navbarPage(id="tabs",
             fluid = TRUE,
             title =div(div(tags$a(href="https://www.service-public.fr/", target='_blank' ,tags$img(id="img1",title="Etat",src="Republique-francaise.png",height=80, style="top:20px;margin-left: 15px;  margin-right:5px; ")),
                            tags$a(href="https://pqn-a.fr/fr", target='_blank',tags$img(id="img1",title="PQNA",src="logo_pqna.png",height=80, style="top:20px; margin-left: 0;  margin-right:23px; ")),
                            tags$a(href="https://www.nouvelle-aquitaine.fr/", target='_blank' ,tags$img(id="img1",title="Région Nouvelle Aquitaine",src="region-nouvelle-aquitaine_logo.jpg",height=80, style="top:20px; margin-left: -15px;  "))),
                            class="conteneur_logo"),  # Navigation bar
             windowTitle = "Questionnaire d'aide à la décision", #title for browser tab
             collapsible = TRUE, #tab panels collapse into menu in small screen
             header=tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),#affiche icône onglet navigateur
            ),
  
  mainPanel(width = 12,
            fluidRow(
              column(12,style="center;",class="intro",
                    div("TEST",style="background-color: #ef7757 !important; height:450px;"))
                    )
            )
)
