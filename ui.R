
ui <- fluidPage(
   tags$head( useShinyjs(),
              includeCSS("www/styles.css"),
              tags$script(src = "normalize.js"),
              tags$script(src = "getAnswer.js"),
              tags$script(src = "updateConditionals.js"),
              tags$script(src = "initConditionals.js"),
              tags$script(src = "change_in_input.js")
   ),

  navbarPage(
    id = "tabs",
    fluid = TRUE,
    title = div(
      div(
        tags$a(href = "https://www.service-public.fr/", target = '_blank',
               tags$img(id = "img1", title = "Etat", src = "Republique-francaise.png", height = 80,
                        style = "top:20px;margin-left: 15px;  margin-right:5px;")),
        tags$a(href = "https://www.nouvelle-aquitaine.fr/", target = '_blank',
               tags$img(id = "img1", title = "Région Nouvelle Aquitaine", src = "region-nouvelle-aquitaine_logo.jpg", height = 80,
                        style = "top:20px; margin-left: -15px;")),
        tags$a(href = "https://pqn-a.fr/fr", target = '_blank',
               tags$img(id = "img1", title = "PQNA", src = "logo_pqna.png", height = 60,
                        style = "top:20px; margin-left: 0;  margin-right:0;"))
        
      ),
      class = "conteneur_logo"
    ),
    windowTitle = "Questionnaire d'aide à la décision",
    collapsible = TRUE,
    header = tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    mainPanel(width = 12,id = "main_content", uiOutput("main_ui"))
  ), tags$br(), tags$br(),tags$br(),tags$br(),uiOutput("footer_conditional")
)