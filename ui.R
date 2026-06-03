
ui <- fluidPage(
   tags$head( useShinyjs(),
              includeCSS("www/styles.css"),
              tags$script(src = "normalize.js"),
              tags$script(src = "getAnswer.js"),
              tags$script(src = "updateConditionals.js"),
              tags$script(src = "initConditionals.js"),
              tags$script(src = "change_in_input.js"),
              tags$script(src = "score.js"),
              tags$script("Shiny.addCustomMessageHandler('scrollTop', function(message) {
                              window.scrollTo({ top: 0, behavior: 'smooth' });
                            });
                          "),
              tags$script(HTML(paste0("var themeMap = {};\n", paste(themes_map, collapse="\n"))))
   ),

   tagList(
     navbarPage(
       id = "tabs",
       fluid = TRUE,
       title = div(
         div(
           tags$a(href = "https://www.nouvelle-aquitaine.fr/", target = '_blank',
                  tags$img(id = "img1", title = "Région Nouvelle Aquitaine",
                           src = "region-nouvelle-aquitaine_logo.jpg", height = 80,
                           style = "top:20px; margin-left: 20px;")),
           tags$a(href = "https://pqn-a.fr/fr", target = '_blank',
                  tags$img(id = "img1", title = "PQNA",
                           src = "logo_pqna.png", height = 60,
                           style = "top:20px; margin-left: 0; margin-right:0;"))
         ),
         class = "conteneur_logo"
       ),
       windowTitle = "Questionnaire d'aide à la décision",
       collapsible = TRUE,
       header = tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico"),
                          tagList(
                            tags$div(
                              style = "position:absolute; right:20px; top:10px;z-index:2000;",
                              actionButton(
                                "admin_mode",
                                "Mode administrateur",
                                class = "btn btn-dark",
                                style = "background:#444; color:white; border:none;"
                              )
                            )
                          )),
       
       tabPanel(
         "Questionnaire",
         mainPanel(width = 12, id = "main_content", uiOutput("main_ui"))
       ),
       
       tabPanel(
         "Résultats",
         mainPanel(width = 12, id = "resultats_page",uiOutput("resultat_ui")
    
         )
       ),
       
       tabPanel(
         "Admin",
         value = "Admin",
         
         fluidRow(
           column(
             width = 8, offset = 2,   # <-- CENTRAGE PARFAIT
             
             tags$div(
             h2("Mode administrateur")
             ),
             
             tags$div(
             h3("📁 Rapports PDF générés"),
             selectInput("selected_pdf", "Choisir un PDF :", choices = NULL, width = '430px'),
             #tableOutput("liste_pdfs"),
             downloadButton("download_pdf_admin", "Télécharger le PDF sélectionné")
             ),
             
             hr(),

             tags$div(
             h3("📊 Fichiers Excel générés"),
             selectInput("selected_excel", "Choisir un Excel :", choices = NULL, width = '430px'),
             #tableOutput("liste_excels"),
             downloadButton("download_excel_admin", "Télécharger le fichier sélectionné")
             ),
             
             hr(),
             
             tags$div(
             h3("🛠 Modifier le questionnaire"),
             fileInput("new_questions", "Importer un fichier Excel de questions"),
             actionButton("apply_questions", "Remplacer le questionnaire")
             )
           )
         )
       )
       
     ),
     
     # 👉 FOOTER ICI
     uiOutput("footer_conditional")
   )
   
)