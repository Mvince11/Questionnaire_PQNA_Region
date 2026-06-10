
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
                          uiOutput("admin_header")),          
       tabPanel(
         "Questionnaire",
         mainPanel(width = 12, id = "main_content", uiOutput("main_ui"))
       ),
       
       tabPanel(
         "Résultats",
         mainPanel(width = 12, style="overflow-x:hidden;",id = "resultats_page",uiOutput("resultat_ui")
    
         )
       ),
       
       tabPanel(
         style="
                background-image: url('home-cta-seo-4d08ff8c.webp');
                background-repeat: no-repeat;
                background-size: cover;
                padding-bottom:82px;
                margin-top:-20px;
              ",
               "Admin",
               value = "Admin",
         
         # ---- ANIMATION CSS ----
              tags$style(HTML("
                        .fade-in {
                          animation: fadeIn 0.8s ease-in-out;
                        }
                        @keyframes fadeIn {
                          from { opacity: 0; transform: translateY(10px); }
                          to   { opacity: 1; transform: translateY(0); }
                        }
                            ")
                         ),
         
                 fluidRow(
                    column(
                      width = 12,
                     
                     # Bouton retour
                     actionButton(
                       "go_home",
                       tagList(tags$i(class="fas fa-home", style="margin-right:8px;"), "Retour à l'accueil"),
                       style="background:#292574; color:white; border:none; margin:40px;"
                     ),
                     
                     # ---- CONTENEUR PRINCIPAL ----
                     div(
                       class="fade-in",
                       style="
                              border: 8px solid rgba(255,255,255,0.4);
                              border-radius: 20px;
                              margin-left: 50px;
                              margin-right: 50px;
                              padding: 40px;
                    
                              background: rgba(255,255,255,0.20);
                              backdrop-filter: blur(12px);
                              -webkit-backdrop-filter: blur(12px);
                    
                              box-shadow: 0 8px 40px rgba(0,0,0,0.25);
                            ",
                       
                           h2(
                             tagList(tags$i(class="fas fa-tools", style="margin-right:12px;")),
                             "Mode administrateur",
                             style="
                                    font-family:Marianne;
                                    font-size:42px;
                                    text-align:center;
                                    margin-bottom:50px;
                                    color:#292574;
                                    font-weight:800;
                                    letter-spacing:1px;
                                  "
                           ),
                       
                           # ---- BLOC PDF ----
                           div(
                             style="display:flex; gap:40px; flex-wrap:wrap; justify-content:center;",
                             
                             # Carte PDF 1
                             div(
                               class="fade-in",
                               style="
                                    border: 1px solid rgba(255,255,255,0.4);
                                    padding: 20px;
                                    border-radius: 20px;
                                    background: rgba(255,255,255,0.65);
                                    backdrop-filter: blur(6px);
                                    -webkit-backdrop-filter: blur(6px);
                                    box-shadow: 0 4px 20px rgba(0,0,0,0.15);
                                    width: 450px;
                                    z-index: 10;
                                    position: relative;

                                  ",
                           h3(
                             tagList(tags$i(class="fas fa-file-pdf", style="margin-right:10px; color:#292574;")),
                             "Sélectionner un rapport PDF généré",
                             style="font-family:Marianne; font-size:22px; margin-bottom:20px; color:#292574;"
                           ),
                           selectInput("selected_pdf", "Choisir un PDF :", choices = NULL, width = '420px'),
                           downloadButton("download_pdf_admin", "Télécharger",
                                          style="height:38px; margin-top:8px; background:#ef7757; color:white;")
                         ),
                         
                         # Carte PDF 2
                         div(
                           class="fade-in",
                           style="
                                  border: 1px solid rgba(255,255,255,0.4);
                                  padding: 20px;
                                  border-radius: 20px;
                                  background: rgba(255,255,255,0.65);
                                  backdrop-filter: blur(6px);
                                  -webkit-backdrop-filter: blur(6px);
                                  box-shadow: 0 4px 20px rgba(0,0,0,0.15);
                                  width: 450px;
                                  z-index: 10;
                                  position: relative;

                                ",
                           h3(
                             tagList(tags$i(class="fas fa-trash", style="margin-right:10px; color:#292574;")),
                             "Supprimer un rapport PDF généré",
                             style="font-family:Marianne; font-size:22px; margin-bottom:20px; color:#292574;"
                           ),
                           selectInput("selected_pdf_admin", "Choisir un PDF :", choices = NULL, width = '420px'),
                           actionButton("delete_pdf", "Supprimer",
                                        style="height:38px; margin-top:8px; background:#ef7757; color:white;")
                         )
                       ),
                       
                       br(),
                       
                       # ---- BLOC EXCEL ----
                       div(
                         style="display:flex; gap:40px; flex-wrap:wrap; justify-content:center;",
                         
                         # Carte Excel 1
                         div(
                           class="fade-in",
                           style="
                                  border: 1px solid rgba(255,255,255,0.4);
                                  padding: 20px;
                                  border-radius: 20px;
                                  background: rgba(255,255,255,0.65);
                                  backdrop-filter: blur(6px);
                                  -webkit-backdrop-filter: blur(6px);
                                  box-shadow: 0 4px 20px rgba(0,0,0,0.15);
                                  width: 450px;
                                ",
                           h3(
                             tagList(tags$i(class="fas fa-file-excel", style="margin-right:10px; color:#292574;")),
                             "Sélectionner un fichier Excel généré",
                             style="font-family:Marianne; font-size:22px; margin-bottom:20px; color:#292574;"
                           ),
                           selectInput("selected_excel", "Choisir un Excel :", choices = NULL, width = '420px'),
                           downloadButton("download_excel_admin", "Télécharger",
                                          style="height:38px; margin-top:8px; background:#ef7757; color:white;")
                         ),
                         
                         # Carte Excel 2
                         div(
                           class="fade-in",
                           style="
                                  border: 1px solid rgba(255,255,255,0.4);
                                  padding: 20px;
                                  border-radius: 20px;
                                  background: rgba(255,255,255,0.65);
                                  backdrop-filter: blur(6px);
                                  -webkit-backdrop-filter: blur(6px);
                                  box-shadow: 0 4px 20px rgba(0,0,0,0.15);
                                  width: 450px;
                                ",
                           h3(
                             tagList(tags$i(class="fas fa-trash", style="margin-right:10px; color:#292574;")),
                             "Supprimer un fichier Excel généré",
                             style="font-family:Marianne; font-size:22px; margin-bottom:20px; color:#292574;"
                           ),
                           selectInput("selected_excel_admin", "Choisir un Excel :", choices = NULL, width = '420px'),
                           actionButton("delete_excel", "Supprimer",
                                        style="height:38px; margin-top:8px; background:#ef7757; color:white;")
                         )
                       ),
                       
                       br(),
                       
                       # ---- TEXTE CENTRAL ----
                       h2(
                         "Vous souhaitez changer le thème du questionnaire ainsi que les éléments graphiques s’y rapportant ?",
                         style="
                                text-align:center;
                                margin-left: 15%;
                                margin-right: 15%;
                                margin-bottom: 40px;
                                margin-top: 60px;
                                font-family:Marianne;
                                color:white;
                                font-size:28px;
                                font-weight:600;
                                text-shadow:0 2px 8px rgba(0,0,0,0.4);
                              "
                       ),
                       
                       # ---- BLOC PERSONNALISATION ----
                       div(
                         style="display:flex; gap:40px; flex-wrap:wrap; justify-content:center; margin-bottom:30px;",
                         
                         # Carte personnalisation
                         div(
                           class="fade-in",
                           style="
                                  border: 1px solid rgba(255,255,255,0.4);
                                  padding: 20px;
                                  border-radius: 20px;
                                  background: rgba(255,255,255,0.65);
                                  backdrop-filter: blur(6px);
                                  -webkit-backdrop-filter: blur(6px);
                                  box-shadow: 0 4px 20px rgba(0,0,0,0.15);
                                  width: 450px;
                                ",
                           h3(
                             tagList(tags$i(class="fas fa-palette", style="margin-right:10px; color:#292574;")),
                             "Personnalisation de la page d’identification",
                             style="font-family:Marianne; font-size:22px; margin-bottom:20px; color:#292574;"
                           ),
                           textInput("admin_login_title", "Titre principal :", value = "Néo Terra"),
                           textInput("admin_login_subtitle", "Sous‑titre :", value = "Demain devient possible"),
                           fileInput("admin_login_image", "Image de fond :", accept = c(".png", ".jpg", ".jpeg", ".webp"),
                                     placeholder = "Sélectionner une image", buttonLabel = "Parcourir"),
                           actionButton("apply_login_custom", "Mettre à jour",
                                        style="background:#ef7757; color:white; margin-top:10px;")
                         ),
                         
                         # Carte modification questionnaire
                         div(
                           class="fade-in",
                           style="
                                  border: 1px solid rgba(255,255,255,0.4);
                                  padding: 20px;
                                  border-radius: 20px;
                                  background: rgba(255,255,255,0.65);
                                  backdrop-filter: blur(6px);
                                  -webkit-backdrop-filter: blur(6px);
                                  box-shadow: 0 4px 20px rgba(0,0,0,0.15);
                                  width: 450px;
                                ",
                           h3(
                             tagList(tags$i(class="fas fa-edit", style="margin-right:10px; color:#292574;")),
                             "Modifier le questionnaire",
                             style="font-family:Marianne; font-size:22px; margin-bottom:66px; color:#292574;"
                           ),
                           actionLink(
                             "open_help_modal",
                             tagList(
                               tags$i(class = "fas fa-book-open", style="margin-right:8px;"),
                               "Comment remplir le fichier Excel ?"
                             ),
                             style="font-weight:600; color:#292574; margin-top:15px; display:block;margin-bottom:20px;"
                           ),
                           fileInput("new_questions", "Importer un fichier Excel de questions",
                                     buttonLabel = "Parcourir", placeholder = "Aucun fichier sélectionné"),
                           actionButton("apply_questions", "Remplacer",
                                        style="height:38px; margin-top:10px; background:#ef7757; color:white;")
                         )
                       )
                     )
                   )
                 )
               )
      ),
     
     # 👉 FOOTER ICI
     uiOutput("footer_conditional")
   )
   
)