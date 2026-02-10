server <- function(input, output, session) {
  
  current_page <- reactiveVal("intro")
  completed_themes <- reactiveVal(character())  # stocke les noms des thèmes complétés
  answers <- reactiveValues()
  reponses_df_r <- reactiveVal(NULL)
  radar_path <- reactiveVal(NULL)
  df_scores <- reactiveVal(NULL)
  df_details_r <- reactiveVal(NULL)
  
  
  safe_id <- function(x) {
    x %>%
      iconv(to = "ASCII//TRANSLIT") %>%  # enlever les accents
      gsub("[^a-zA-Z0-9]", "_", .) %>%    # remplacer tout sauf lettres/chiffres par "_"
      gsub("_+", "_", .) %>%              # réduire les "_" multiples
      trimws()
  }
  
  
  # themes <- questions_list %>%
  #   pull(Theme) %>%
  #   unique()
  
  generateProgressBar <- function(all_themes, current_theme, completed_themes) {
    tags$div(
      id = "progress-bar",
      class = "progress-bar",
      lapply(seq_along(all_themes), function(i) {
        theme <- all_themes[i]
        label <- theme  # ou un vecteur parallèle si tu veux des noms plus lisibles
        
        state_class <- if (theme == current_theme) {
          "active"
        } else if (theme %in% completed_themes) {
          "completed"
        } else {
          "pending"
        }
        
        tags$div(
          class = paste("progress-item", state_class),
          `data-step` = theme,
          style = "display: flex; align-items: center; gap: 5px; margin-right: 15px;",
          tags$div(class = "circle", i),  # numéro du thème
          tags$span(class = "label", HTML(label))  # nom du thème
        )
      })
    )
  }
  
  ##### Page principale #####
  output$main_ui <- renderUI({
    page <- as.character(current_page())
    theme_questions <<- filter(questions_list, Theme == page)
    
    
    ##### Page d'accueil #####
    if (page == "intro") {
      # --- Page d'accueil / introduction ---
      tagList(
        # --- Bandeau orange ---
        div(
          class = "page-layout-custom",
          style = "
                background-color: #ef7757 !important;
                padding-top: 5%;
                margin-top: -2%;
                padding-bottom: 3%;
                font-family: 'Source Sans Pro', sans-serif;
              ",
          div(
            style = "color:white; margin-left:10%; font-size: 2.4em; margin-top: 1%; font-weight: bold;",
            "Auto-Diagnostic des transitions"
          ),
          div(
            style = "color:white; margin-left:10%; font-size: 2.1em; font-weight: bold; margin-top:1%;",
            "Réaliser un état des lieux de la maturité de votre territoire face aux enjeux de transition."
          ),
           tags$br(),
          # div(
          #   style = "color:white; margin-left:10%; font-size: 1.8em; font-weight: bold; margin-bottom: 0.5%; margin-top: 2%;",
          #   "Préparez l'entretien d'échanges avec votre référent Cerema en établissant un état des lieux"
          # ),
          # div(
          #   style = "color:white; margin-left:10%; font-size: 1.8em; font-weight: bold; margin-top: -9px;",
          #   "des forces et des besoins de votre territoire."
          # ),
          actionButton(
            "next_btn", "Commencer",
            class = "btn btn-bounce",
            style = "
                  margin-left: 10%;
                  box-shadow: inset 1px 1px 3px #712929, inset -1px -1px 3px #ccc7c7;
                  border-radius: 8px;
                  font-weight: bold;
                  font-size: 1.5em;
                  margin-top: 40px;
                  background-color: rgba(239,119,87,1);
                  border: none;
                "
                    )
        ),
        
        # --- Container avec colonnes ---
        div(
          class = "container",
          style = "display:flex; justify-content:space-between;width:auto; align-items:flex-start; margin:0 5%;margin-bottom:5%;",
          
          # Colonne gauche
          div(
            class = "left-column",
            style = "flex:1;margin-top:auto;",
            div(
              class = "info-block",
              tags$img(src = "main_pomme.png", alt = "Pomme", style = "width:120px; margin-right:15px;"),
              div(
                class = "info-text",
                tags$h3("Objectiver la situation de votre territoire"),
                tags$h4("À travers un questionnaire simple et synthétique, identifiez les pratiques déjà engagées et
                        les points de vigilance à l’échelle de votre territoire de contractualisation")
              )
            ),
            div(
              class = "info-block",
              tags$img(src = "cible.png", alt = "Cible", style = "width:120px; margin-right:15px;"),
              div(
                class = "info-text",
                tags$h3("Hiérarchiser vos priorités d’action"),
                tags$h4("Cet état des lieux vous permettra de définir des priorités et d’engager un dialogue
                        constructif avec votre chargé de mission territorial et l'ensemble
                        de la gouvernance du territoire de contractualisation, afin d’adapter les leviers
                        d’intervention aux spécificités locales.")
              )
            ),
            div(
              class = "info-block",
              tags$img(src = "ecran.png", alt = "Écran", style = "width:120px; margin-right:15px;"),
              div(
                class = "info-text",
                tags$h3("Accompagner le passage à l’action"),
                tags$h4("Véritable outil d’animation et de coopération, l’auto-diagnostic alimentera
                        la réflexion collective et soutiendra l’évolution des projets de territoire
                        en cohérence avec les enjeux de transition.")
              )
            )
          ),
          
          # Colonne droite
          div(
            class = "right-column",
            style = "flex:1; text-align:center;",
            tags$img(src = "Capture-1.jpg", alt = "Image droite", style = "max-width:100%; height:auto;")
          )
        ),tags$footer(
          style = "
                  position: fixed;
                  bottom: 0;
                  left: 0;
                  width: 100%;
                  padding: 10px 5%;
                  background: #f9f9f9;
                  border-top: 1px solid #ddd;
                  display: flex;
                  justify-content: space-between;
                  align-items: center;
                  font-size: 1.1rem;
                  color: #666;
                  z-index: 1000;
                ",
          div("Site créé avec ®RStudio"),
          div("© Cerema"),
          div(
            tags$a(href="https://fr.linkedin.com/company/cerema", target="_blank",
                   icon("linkedin", lib="font-awesome"), style="margin:0 8px; font-size:1.5rem; color:#0e76a8;"),
            tags$a(href="https://x.com/ceremacom", target="_blank",
                   icon("twitter", lib="font-awesome"), style="margin:0 8px; font-size:1.5rem; color:#000;"),
            tags$a(href="https://www.youtube.com/@cerema3139", target="_blank",
                   icon("youtube", lib="font-awesome"), style="margin:0 8px; font-size:1.5rem; color:#c4302b;")
          )
        )
        
      )
    } else if (page == "login") {
      return(
        div(
          style = "margin-top:8%; text-align:center; font-family:'Source Sans Pro';",
          
          h2("Connexion", style="color:#293574; font-weight:bold; margin-bottom:40px;"),
          
          div(
            style="display:inline-block; text-align:left; width:350px;",
            
            textInput("login_user", "Identifiant :", placeholder = "Votre identifiant"),
            passwordInput("login_pass", "Mot de passe :", placeholder = "Votre mot de passe"),
            
            actionButton(
              "login_btn", "Se connecter",
              style="margin-top:20px; width:100%; background-color:#ef7757; color:white;
                 border:none; padding:10px; border-radius:6px; font-size:1.4rem;"
            )
          )
        )
      )
    } else {
      
        
      texte_theme <- NA_character_
      if ("TexteTheme" %in% names(theme_questions)) {
        tmp <- theme_questions$TexteTheme
        tmp <- tmp[!is.na(tmp)]
        if (length(tmp) > 0) {
          texte_theme <- as.character(tmp[1])
        }
      }
      
      # Traitement du texte avec || comme séparateur
      texte_blocs <- if (!is.null(texte_theme) && is.character(texte_theme) && !is.na(texte_theme)) {
        unlist(strsplit(texte_theme, "\\|\\|")) |> trimws()
      } else {
        character(0)
      }
      
      
      # Le premier bloc est affiché comme paragraphe, les suivants comme liste
      texte_intro <- if (length(texte_blocs) > 0) {
        tags$p(HTML(gsub("\n", "<br>", texte_blocs[1])),
               style = "font-size:1.9rem;")
      }
      
      texte_liste <- if (length(texte_blocs) > 1) {
        tags$ul(
          lapply(texte_blocs[-1], function(item) {
            tags$li(item)
          }),
          style = "margin-left:20px; font-size:1.9rem;"
        )
      }
      
      ##### Pages questions #####
      tagList(
        tags$div(
          fluidRow(
            column(12,
          id = "progress-bar",
          class = "progress-bar",
          style = "margin-left: 10.2%; margin-right: 3.3%; padding: 0 2%; padding-bottom:14px;", # ou 0 si tu veux pleine largeur
          generateProgressBar(
            all_themes = themes,  # liste complète
            current_theme = current_page(),
            completed_themes = completed_themes()
          )
          
                  )
                  )
          ),
    div(class = "mise_en_page",
        fluidRow(id="mise_en_page",
          column(12,
            # --- Mention obligatoire ---
            div(
              style="color: red; font-size:1.2rem; margin-top: 15px; margin-bottom: 40px; margin-left: -2vw;",
              "*Champs obligatoires",
              class="champs_obligatoires"
            ),
            
            # --- Thème et texte associé ---
            tagList(
              h3(
                paste(page),
                style = "color:#293574;margin-left:5%;font-weight: bold;"
              ),
              if (!is.null(texte_theme) && !is.na(texte_theme) && nzchar(texte_theme)) {
                tags$div(
                  
                  texte_intro,
                  texte_liste
                ,style = "margin-left:5%; font-size:1.7rem; color:#293574; margin-bottom:44px; margin-top:40px;font-weight: bold;
                          font-family: Source Sans Pro;"
                )
              }
            ),
            
            ###### Génération dynamique des questions #####
            lapply(seq_len(nrow(theme_questions)), function(i) {
              q <- theme_questions[i, ]
              numero     <- as.character(q$Numero)
              question   <- q$Questions
              style      <- ifelse(is.na(q$Style), "radio", q$Style)
              condition  <- if ("Condition" %in% names(q)) q$Condition else NA
              parent_col <- if ("Parent" %in% names(q)) q$Parent else NA
              remplace <- if ("Remplace" %in% names(q)) q$Remplace else NA
              is_replacement <<- !is.na(remplace) && nzchar(remplace)
              reponses <- q$reponses[[1]]
              scores <- if ("Note" %in% names(q) && !is.na(q$Note) && nzchar(q$Note)) {
                as.numeric(unlist(strsplit(gsub("-", "NA", as.character(q$Note)), ";")))
              } else {
                rep(0, length(reponses))
              }
              has_parent <- !is.na(parent_col) && parent_col != "" && parent_col != "NA"
              is_replaced <- numero %in% theme_questions$Remplace
              is_conditional <- has_parent
              is_required <- tolower(q$Observation) == "obligatoire"
              is_required <- if (!is.na(is_required)) is_required else FALSE
              
              # --- Commentaire éventuel [ ... ] ---
              commentaire <- stringr::str_extract(question, "\\[.*?\\]")
              question_sans_commentaire <- stringr::str_replace(question, "\\[.*?\\]", "")
              if (!is.na(commentaire)) {
                commentaire <- stringr::str_replace_all(commentaire, "\\[|\\]", "")
                tooltip_html <- sprintf(
                  "<span class='tooltip'><span class='icon'>i</span><span class='tooltiptext'>%s</span></span>",
                  commentaire
                )
              } else {
                tooltip_html <- ""
              }
              
              div(
                id = paste0("q", numero),
                # Important : 'conditional' reste pour la SÉLECTION JS
                class = if (is_conditional) c("question-block", "conditional") else "question-block",
                `data-parent-question` = if (is_conditional) paste0("q", parent_col) else NULL,
                `data-condition` = if (is_conditional && !is.na(condition) && nzchar(condition)) condition else NULL,
                `data-replace` = if (is_replacement) paste0("q", remplace) else NULL,
                
                # MODIFICATION CLÉ : Le style initial est UNIFORME et SANS 'display:none'
                style = "margin:20px 0; padding:15px; margin-left:4%; margin-right:4%;",
              
                
                tags$p(HTML(sprintf(
                  "<strong style='font-size:1.6rem; color:#293574;'>%s</strong> %s <span style='color:red;'>*</span>",
                  question_sans_commentaire, tooltip_html
                ))),
                
                switch(style,
                       "radio" = tagList(
                         lapply(seq_along(reponses), function(j) {
                           id <- paste0("q", numero, "_", j)
                           val <- trimws(gsub("&nbsp;", "", reponses[j]))
                           tags$div(
                             class = "custom-radio", style = "display:contents;",
                             tags$input(
                               type = "radio", id = id, name = paste0("q", numero),is_required = is_required,
                               value = val,`data-score` = scores[j], required = if (j == 1) NA else NULL
                             ),
                             tags$label(`for` = id, val)
                           )
                         })
                       ),
                       "checkbox" = tagList(
                         lapply(seq_along(reponses), function(j) {
                           id <- paste0("q", numero, "_", j)
                           val <- trimws(gsub("&nbsp;", "", reponses[j]))
                           
                           # Récupérer StyleDetail si présent
                           style_detail <- ifelse("Affichage" %in% names(q), q$Affichage, NA)
                           
                           tags$div(
                             class = "custom-checkbox",
                             style = if (!is.na(style_detail) && style_detail == "inline-block") {
                               # Cas particulier → inline-block
                               "display: inline-block; margin-right:15px; margin-top:10px; color: #293574; font-weight: bold; opacity: 0.95;"
                             } else {
                               # Cas général → flex
                               "display: flex; align-items: flex-start; gap: 12px; margin: 10px 0; color: #293574; font-weight: bold; opacity: 0.95;"
                             },
                             
                             tags$input(
                               type = "checkbox",
                               id = id,
                               is_required = is_required,
                               name = paste0("q", numero),
                               value = val,
                               style = "transform: scale(1.3); margin-top: 4px; cursor: pointer; border-color: rgba(239,119,87,1);"
                             ),
                             tags$label(
                               `for` = id,
                               val,
                               style = "font-size: 1.6rem; font-weight: bold; margin-left: 8px; line-height: 1.4; max-width: 90%; word-break: break-word;"
                             )
                           )
                         })
                       ),
                       "textarea" = tags$textarea(
                         name = paste0("q", numero),id = paste0("q", numero), rows = 4, cols = 50, required = NA,is_required = is_required,
                         style = "width:100%; margin-top:6px; color:#293574; font-weight:bold; opacity:0.9;border:2px solid rgba(239,119,87,1);height:200px;"
                       ),
                       "textarea-alt" = tags$textarea(
                         name = paste0("q", numero),id = paste0("q", numero), rows = 4, cols = 50, required = NA,is_required = is_required,
                         style = "width:100%; margin-top:6px; color:#293574; font-weight:bold; opacity:0.9;border:2px solid rgba(239,119,87,1);
                                  height:35px;"
                       ),
                       "select" = tags$select(
                         name = paste0("q", numero), required = NA,
                         is_required = is_required,
                         style = "width:100%; margin-top:6px; border:2px solid rgba(239,119,87,1); border-radius:6px; height:35px; color:#293574; font-weight:bold; opacity:0.9;
                         font-size: 1.9rem;font-family: Apple Chancery;padding-left:10px;",
                         lapply(reponses, function(r) {
                           val <- trimws(gsub("&nbsp;", "", r))
                           tags$option(value = val, val)
                                                      }
                                )
                                            )
                          )
                      )
                  })
                )
              )
        )
      )
            }
    })
  
  ##### Footer commun #####
  output$footer_conditional <- renderUI({
    if (as.character(current_page()) != "intro") {
      tagList(
        tags$br(), tags$br(), tags$br(), tags$br(),
        uiOutput("footer")
              )
    } else {
      NULL  # rien affiché sur la page intro
            }
  })
  
  ##### Footer pages questions #####
  output$footer <- renderUI({
    p <- current_page()
    if (p %in% c("intro", "login", "resultats")) return(NULL) ### Empêche le footer de s'afficher sur la page login
    pos <- match(p, themes)
    
    step <- which(themes == p)
    if (length(step) == 0) step <- 0
    
    tagList(
    div(
      id = "footer",
      style = "position:fixed; bottom:0; left:0; width:100%;
             background:white; border-top:2px solid #EF7757; 
             padding:10px 20px; z-index:1000;",
      
      # ligne dots + bouton
      div(
        id = "footer-dots-container",
        style = "display:flex; align-items:center; justify-content:space-between;",
        
        # bouton précédent à gauche (si applicable)
        if (!is.na(pos) && pos > 1) {
          div(
            class = "left-btn",
            actionButton(
              "prev_btn", "< Précédent",
              style = "background-color:#ef7757;color:white;border:none;
                     padding:10px 20px;border-radius:6px;font-size:1.6rem;"
            )
          )
        },
        
        # dots au centre
        div(
          style = "flex-grow:1; text-align:center;",
          includeHTML("www/custom_footer.html")
        ),
        
        # bouton à droite
        div(
          class = "right-btn",
          if (!is.na(pos) && pos == length(themes)) {
            actionButton(
              "submit", "Soumettre",
              style = "background-color:#ef7757;color:white;border:none;
                     padding:10px 20px;border-radius:6px;font-size:1;6rem;"
            )
          }  else if (!is.na(pos)) {
            actionButton(
              inputId = safe_id(paste0("next_", p)), "Suivant >",
              style = "background-color:#ef7757;color:white;border:none;
                     padding:10px 20px;border-radius:6px;font-size:1.6rem;"
            )
          }
        )
      ),
      
      # ligne texte en dessous
      # ligne texte + icônes en dessous
      div(
        style = "margin-top:15px;
           display:flex; 
           align-items:center; 
           justify-content:space-between;
           font-size:1.2rem; color:#666;",
        
        # gauche
        div("Site créé avec ®RStudio"),
        
        # centre
        div("©Cerema"),
        
        # droite
        div(
          class = "footer-social",
          
          tags$a(
            href = "https://fr.linkedin.com/company/cerema",
            target = "_blank",
            icon("linkedin", lib = "font-awesome"),
            style = "margin:0 10px; font-size:1.6rem; color:#0e76a8;"
          ),
          tags$a(
            href = "https://x.com/ceremacom",
            target = "_blank",
            icon("twitter", lib = "font-awesome"),
            style = "margin:0 10px; font-size:1.6rem; color:#000;"
          ),
          tags$a(
            href = "https://www.youtube.com/@cerema3139",
            target = "_blank",
            icon("youtube", lib = "font-awesome"),
            style = "margin:0 10px; font-size:1.6rem; color:#c4302b;"
          )
        )
      )
    ),tags$script(
      HTML(
        sprintf("window.currentProgressStep = %d;", step))
          )
                )
  })
  
  validate_choix <- function(questions_list) {
    questions_list %>%
      rowwise() %>%
      mutate(
        Reponses = list(input[[paste0("q", Numero)]]),
        nb_reponses = if (is.null(Reponses)) 0 else length(Reponses),
        limite = case_when(
          Choix == "Mono" ~ 1,
          Choix == "Multichoix 2 MAX" ~ 2,
          Choix == "Multichoix 3 MAX" ~ 3,
          Choix == "Multichoix" ~ Inf,
          TRUE ~ Inf
        ),
        trop_de_reponses = nb_reponses > limite
      ) %>%
      ungroup() %>%
      filter(trop_de_reponses)
  }
  
  
  ##### Vérification questions obligatoire #####
  validate_theme <- function(th) {
    questions_theme <- questions_list %>% filter(Theme == th)%>%
      mutate(Observation = trimws(tolower(gsub("\u00A0", " ", Observation))))
    
    obligatoires <- questions_theme %>% filter(Observation == "obligatoire")
    
    non_remplies <- obligatoires %>%
      mutate(Reponse = sapply(Numero, function(id) {
        val <- input[[paste0("q", id)]]
        if (is.null(val) || val == "" || val %in% c("Sélectionner…", "Sélectionner...")) return(NA)
        return(val)
      })) %>%
      filter(is.na(Reponse))
    
    
    trop_de_reponses <- validate_choix(questions_theme)
    
    if (nrow(trop_de_reponses) > 0) {
      showModal(modalDialog(
        title = div(icon("exclamation-triangle", style="color:rgba(239,119,87,1)"), span("Trop de réponses", style = "color:#D32F2F; font-weight:bold; font-size:1.4rem;")),
        div(
          style = "font-size:1.5rem; color:#293574; margin-top:50px;",
          HTML(paste0(
            "Vous avez sélectionné trop de réponses pour les questions suivantes :<br><ul>",
            paste0("<li><strong>", trop_de_reponses$Questions, "</strong> (max ", trop_de_reponses$limite, ")</li>", collapse = ""),
            "</ul>"
          ))
        ),
        easyClose = TRUE,
        footer = modalButton("Corriger")
      ))
      return(FALSE)
    }
    
    if (nrow(non_remplies) > 0) {
      showModal(modalDialog(
        title = div(icon("exclamation-triangle"), span("Champs obligatoires manquants", style = "color:#D32F2F; font-weight:bold; font-size:1.4rem;")),
        div(
          style = "font-size:1.5rem; color:#293574; margin-top:50px;",
          HTML(paste0(
            "⚠️ Vous devez remplir les champs suivants avant de continuer :<br><ul>",
            paste0("<li><strong>", non_remplies$Questions, "</strong></li>", collapse = ""),
            "</ul>"
          ))
        ),
        easyClose = TRUE,
        footer = modalButton("Corriger")
      ))
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  
  ##### Partie ObserveEvent ####
  observe({
    lapply(themes, function(th) {
      observeEvent(input[[safe_id(paste0("next_", th))]], {
         #current_index <- which(themes == th)
        
        # ✅ Vérifier les champs obligatoires du thème courant
        if (!validate_theme(th)) return()
        
        current_index <- which(themes == th)
        
        # 🔵 Marquer le thème comme complété
        old <- completed_themes()
        if (!(th %in% old)) {
          completed_themes(c(old, th))
        }
        
        # 🔶 Passer au thème suivant si possible
        if (current_index < length(themes)) {
          current_page(themes[current_index + 1])
        }
      })
    })
  })
  
  
  
  
  observeEvent(input$prev_btn, {
    pos <- match(current_page(), themes)
    
    if (!is.na(pos) && pos > 1) {
      prev_theme <- themes[pos - 1]
      current_page(prev_theme)
      
      current_theme <- themes[pos]
      completed_themes(setdiff(completed_themes(), current_theme))
    }
  })
  
  
  
  # 🔄 Navigation
  #observeEvent(input$next_btn, current_page(themes[1]))
  observeEvent(input$next_btn, current_page("login"))
  if (length(themes) > 1) {
    for (i in seq_along(themes[-length(themes)])) {
      observeEvent(input[[paste0("next_", themes[i])]], {
        current_page(themes[i + 1])
      })
    }
  }
  
  # observeEvent(input$next_submit, current_page("submit"))
  # observeEvent(input$back_submit, current_page(themes[length(themes)]))
  
###### Permet d'enregister les réponses evec answer reactivevalue ##### 
  observe({
    lapply(questions_list$Numero, function(id) {
      inputId <- paste0("q", id)
      observeEvent(input[[inputId]], {
        answers[[inputId]] <- input[[inputId]]
      }, ignoreInit = TRUE)
    })
  })
  
##########################
#### Partie résultats ####
##########################
  
##### Enregistre toutes les Questions et réponses dans un fichier xlsx ######  
  observeEvent(input$submit, {
    th <- "Formulaire de contact"
    if (!validate_theme(th)) return()
    
    completed_themes(c(completed_themes(), th))
    
    reponses_df_r(
      questions_list %>%
        mutate(
          Reponse = sapply(Numero, function(id) {
            val <- input[[paste0("q", id)]]
            if (is.null(val)) return("")
            if (is.atomic(val)) return(paste(val, collapse = ", "))
            return(as.character(val))
          }),
          Score = sapply(Numero, function(id) {
            val <- input[[paste0("q", id)]]
            if (is.null(val)) return(NA)
            
            q <- questions_list[questions_list$Numero == id, ]
            notes <- suppressWarnings(as.numeric(unlist(strsplit(q$Note, ";"))))
            notes[notes < 0] <- NA
            
            if (q$Style == "radio") {
              idx <- match(val, q$reponses[[1]])
              return(notes[idx])
            }
            if (q$Style == "checkbox") {
              idx <- match(val, q$reponses[[1]])
              return(sum(notes[idx], na.rm = TRUE))
            }
            return(NA)
          })
        ) %>%
        select(Numero, Questions, Reponse, Score) %>%
        filter(!(Reponse %in% c("", "Sélectionner…", "Sélectionner...")))
    )
  
      df_details_r(
        reponses_df_r() %>%
          left_join(
            questions_list %>% select(Numero, Theme, Objectif),
            by = "Numero"
          ) %>%
          group_by(Theme, Objectif) %>%
          summarise(
            score_pct = round(mean(Score, na.rm = TRUE) / 3 * 100),
            .groups = "drop"
          )
      )
    
    genre <- reponses_df_r()$Reponse[reponses_df_r()$Questions == "Civilité"]
    nom <- reponses_df_r()$Reponse[reponses_df_r()$Questions == "Nom"]
    prenom <- reponses_df_r()$Reponse[reponses_df_r()$Questions == "Prénom"]
    raison_sociale <- reponses_df_r()$Reponse[reponses_df_r()$Questions == "Raison sociale de la collectivité (nom exact)"]
    adresse_mail <- reponses_df_r()$Reponse[reponses_df_r()$Questions == "Email"]
    
    genre          <- if (length(genre) == 0) NA else genre
    nom            <- if (length(nom) == 0) NA else nom
    prenom         <- if (length(prenom) == 0) NA else prenom
    raison_sociale <- if (length(raison_sociale) == 0) NA else raison_sociale
    adresse_mail   <- if (length(adresse_mail) == 0) NA else adresse_mail
    
    Identite <- data.frame(
      "Civilité"        = genre,
      Nom             = nom,
      "Prénom"          = prenom,
      `Raison sociale` = raison_sociale,
      `Adresse mail`   = adresse_mail,
      stringsAsFactors = FALSE
    )
    
    horodatage <- format(Sys.time(), "%Y-%m-%d-%H-%M")
    
    writexl::write_xlsx(
      list(
        Identite = Identite,
        Reponses = reponses_df_r()
      ),
      path = paste0("Reponses/reponses_", nom, "_", prenom, "_", horodatage, ".xlsx")
    )
    
    scores <- input$scores_par_theme
    if (is.null(scores)) return()
    
    df <- data.frame(
      theme = names(scores),
      value = as.numeric(unlist(scores))
    )
    
    df$value <- round(df$value / 30 * 100, 1)
    df_local <- df
    df_scores(df_local)
    
    output$kiviat <- renderHighchart({
      
      # --- Construction du radar (ton code existant) ---
      highchart() %>%
        hc_chart(polar = TRUE, type = "line") %>%
        hc_xAxis(
          categories = df$theme,
          tickmarkPlacement = "on",
          labels = list(
            distance = 30,
            padding = 10,
            useHTML = TRUE,
            formatter = JS("
          function () {
            var angle = this.pos * (360 / this.axis.categories.length);
            if (angle > 330 || angle < 30) {
              return '<span style=\"position:relative; font-size:12px; color:#000\">' + this.value + '</span>';
            } else if (angle > 150 && angle < 210) {
              return '<span style=\"position:relative; top:6px; font-size:12px; color:#000\">' + this.value + '</span>';
            } else {
              return '<span style=\"font-size:12px; color:#000\">' + this.value + '</span>';
            }
          }
        ")
          )
        ) %>%
        hc_yAxis(
          min = 0, max = 100,
          gridLineColor = "#D0D0D0",
          gridLineWidth = 1,
          gridLineInterpolation = "polygon",
          lineWidth = 0,
          tickInterval = 25,
          labels = list(style = list(color = "#666", fontSize = "12px"))
        ) %>%
        hc_series(
          list(
            name = "Score",
            data = df$value,
            pointPlacement = "on",
            color = "#0055A4",
            lineWidth = 3,
            marker = list(enabled = TRUE, radius = 5, fillColor = "#0055A4")
          )
        ) %>%
        hc_tooltip(
          headerFormat = "",
          pointFormat = "{point.category} : <b>{point.y}%</b>",
          backgroundColor = "white",
          borderColor = "#0055A4",
          style = list(fontSize = "14px")
        ) %>%
        hc_legend(enabled = FALSE)
    })
    
    
    updateTabsetPanel(session, "tabs", selected = "Résultats")
    shinyjs::hide("footer-dots-container")
  })

  
  # df_details <- reactive({
  #   req(reponses_df_r())
  #   
  #   reponses_df_r() %>%
  #     left_join(questions_list %>% select(Numero, Theme, Objectif), by = "Numero") %>%
  #     group_by(Theme, Objectif) %>%
  #     summarise(
  #       score_pct = round(mean(Score, na.rm = TRUE) / 3 * 100),
  #       .groups = "drop"
  #     )
  # })
  
  
  output$resultats_par_theme <- renderUI({
  req(df_details())

  blocs <- lapply(split(df_details(), df_details()$Theme), function(d) {

    titre <- sprintf(
      "<h3 style='margin-top:30px; color:#0055A4;'>%s</h3>",
      unique(d$Theme)
    )

    objectifs <- paste0(
      "<div style='margin-left:20px; font-size:16px;'>",
      paste0(
        "<p><b>", d$Objectif, "</b> : ", d$score_pct, "%</p>",
        collapse = ""
      ),
      "</div>"
    )

    paste0(titre, objectifs)
  })

  HTML(paste0(blocs, collapse = ""))
})

#### Variables pour résultats ambitions et objectifs ####
  df_details <- reactive({
    req(reponses_df_r())   # garantit que le DF existe
    
    reponses_df_r() %>%    # ⚠️ les parenthèses sont essentielles
      left_join(
        questions_list %>% select(Numero, Theme, Objectif),
        by = "Numero"
      ) %>%
      group_by(Theme, Objectif) %>%
      summarise(
        score_pct = round(mean(Score, na.rm = TRUE) / 3 * 100),
        .groups = "drop"
      )
  })
  
#### identité #####
  identite <- reactive({
    req(reponses_df_r())
    df <- reponses_df_r()
    
    get_val <- function(q) {
      v <- df$Reponse[df$Questions == q]
      if (length(v) == 0) NA_character_ else v[1]
    }
    
    list(
      civilite  = get_val("Civilité"),
      nom       = get_val("Nom"),
      prenom    = get_val("Prénom"),
      collectivite = get_val("Raison sociale de la collectivité (nom exact)"),
      email     = get_val("Email"),
      date_gen  = format(Sys.time(), "%d/%m/%Y")
    )
  })
  
 
#### Resultats par Ambition et Objectifs #####   
  output$resultats_par_theme <- renderUI({
    req(df_details())
    
    blocs <- lapply(split(df_details(), df_details()$Theme), function(d) {
      
      # Titre du thème centré
      titre <- sprintf(
        "<h3 style='
        margin-top:40px;
        margin-bottom:15px;
        color:#0055A4;
        text-align:center;
        font-weight:bold;
        font-size:26px;
      '>%s</h3>",
        unique(d$Theme)
      )
      
      # Objectifs + jauges
      objectifs <- paste0(
        "<div style='
        margin:0 auto;
        max-width:800px;
        text-align:center;
        font-size:18px;
        line-height:1.7;
        color:#293574;
      '>",
        paste0(
          "<div style='
            display:flex;
            align-items:center;
            justify-content:center;
            gap:15px;
            margin:8px 0;
        '>
            <div style='width:540px; text-align:right;'>
              <b>", d$Objectif, "</b>
            </div>

            <div style='
                width:150px;
                height:10px;
                background:#e6e6e6;
                border-radius:5px;
                overflow:hidden;
            '>
              <div style='
                  width:", d$score_pct, "%;
                  height:100%;
                  background:#0055A4;
                  border-radius:5px;
              '></div>
            </div>

            <div style='width:50px; text-align:left;'>
              ", d$score_pct, "%
            </div>
        </div>",
          collapse = ""
        ),
        "</div>"
      )
      
      paste0(titre, objectifs)
    })
    
    # Conteneur global centré
    HTML(paste0(
      "<div style='
      width:100%;
      display:flex;
      flex-direction:column;
      align-items:center;
      margin-top:30px;
      margin-bottom:60px;
    '>",
      paste0(blocs, collapse = ""),
      "</div>"
    ))
  })
  
  
  
##### Page Résultats #####
  
  output$resultat_ui<- renderUI({
    tagList(
     
      # --- Bandeau orange ---
      div(
        class = "page-layout-custom",
        style = "
                background-color: #ef7757 !important;
                padding-top: 5%;
                margin-top: -2%;
                padding-bottom: 3%;
                font-family: 'Source Sans Pro', sans-serif;
                margin-bottom: 36px;
              ",
    
    # --- TITRE ---
    tags$h2("Vos résultats", style = "margin-top:20px;
    color:white; margin-left:10%; font-size: 2.4em; font-weight: bold;"),
    
    # --- TEXTE INTRO ---
    tags$p(style = "color:white; margin-left:10%; font-size: 1.4em; margin-top: 1%; font-weight: bold;",
      "Voici une synthèse de vos scores par ambition. 
       Le graphique ci-dessous vous permet de visualiser vos forces 
       et vos axes d'amélioration.",
      style = "font-size:20px; margin-bottom:25px;"
    )),
    
    # --- GRAPHIQUE ---
    div(
      style = "margin-left:5%; margin-right:5%; width: 100%;",
      highchartOutput("kiviat", height = "650px")
    ),
    tags$br(),
    uiOutput("resultats_par_theme"),
    tags$br(),
    
    
    # --- TEXTE D'INTERPRÉTATION ---
    tags$div(style="background-color:gainsboro; padding-top:15px; padding-bottom: 37px;",
    tags$h3("Interprétation des résultats", style = "margin-left:10%; margin-right:10%; margin-top:40px; font-weight:600;"),
    tags$p(
      "Chaque axe représente une ambition Néo-Terra évaluée. 
       Plus la surface est étendue, plus votre niveau de maturité est élevé. 
       Une zone plus réduite indique un besoin d'amélioration.",
      style = "font-size:15px;margin-left:10%; margin-right:10%;"
    ),
    
    # --- BLOC CONSEILS ---
    tags$div(style = "margin-left:10%; margin-right:10%;",
      style = "
        background:#f7f7f7;
        padding:20px;
        border-radius:8px;
        margin-top:25px;
        border-left:5px solid #0055A4;",
      tags$h4("Conseils personnalisés", style="font-weight:600;"),
      tags$p(
        "En fonction de vos scores, nous vous recommandons d'examiner 
         les ambitions les plus faibles afin d'identifier des pistes d'amélioration."
      )
    )
    ),
    tags$br(),
    tags$div(
      style='
        width:100%;
        display:flex;
        flex-direction:column;
        align-items:center;
        margin-top:30px;
        margin-bottom:60px;
      ',
     tags$div(
       style="position:relative; display:inline-block;",
     downloadButton("download_pdf", "Télécharger la fiche des résultats au format PDF",
                      class = "btn btn-primary"),
    actionButton( "trigger_pdf", "", style = "position:absolute;
                                              top:0;
                                              left:0;
                                              width:100%;
                                              height:100%;
                                              opacity:0;
                                              border:none;
                                              background:transparent;
                                              cursor:pointer;
                                              z-index:10;" )
              )
            ),
    
    shinyalert::useShinyalert(force = TRUE),
    
    tags$script("
      Shiny.addCustomMessageHandler('launch-download', function(message) {
        document.getElementById('download_pdf').click();
      });
        "),
    tags$style("
        .loader {
          border: 6px solid #f3f3f3;
          border-top: 6px solid #0055A4;
          border-radius: 50%;
          width: 40px;
          height: 40px;
          animation: spin 1s linear infinite;
        }
        
        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }
        ")
    )
  })

  #### ObserveEvent login #####
  observeEvent(input$login_btn, {
    user <- input$login_user
    pass <- input$login_pass
    
    if (user == "" || pass == "") {
      showModal(modalDialog(
        title = "Erreur",
        "Veuillez remplir tous les champs.",
        easyClose = TRUE
      ))
      return()
    }
    
    # 🔐 Identifiants simples (à adapter si besoin)
    if (user == "admin" && pass == "cerema") {
      current_page(themes[1])   # on passe au premier thème
    } else {
      showModal(modalDialog(
        title = "Connexion refusée",
        "Identifiant ou mot de passe incorrect.",
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$trigger_pdf, {
    
    shinyalert::shinyalert(
      title = "Création du PDF",
      text = "
      <div style='display:flex; flex-direction:column; align-items:center;'>
        <div class='loader'></div>
        <p style='margin-top:15px;'>Votre rapport de synthèse est en cours de création…</p>
      </div>
    ",
      html = TRUE,
      closeOnClickOutside = FALSE,
      showConfirmButton = FALSE,
      timer = 10000   # fermeture automatique après 4 secondes
    )
    
    session$sendCustomMessage("launch-download", TRUE)
  })
  
  
  
  output$download_pdf <- downloadHandler(
    
    filename = function() {
      id <- identite()
      
      nom <- id$nom
      prenom <- id$prenom
      
      if (is.null(nom) || is.na(nom) || nom == "") nom <- "inconnu"
      if (is.null(prenom) || is.na(prenom) || prenom == "") prenom <- "inconnu"
      
      paste0("autodiagnostic_", nom, "_", prenom, "_", Sys.Date(), ".pdf")
    
    },
    
    content = function(file) {
      
      library(highcharter)
      library(htmlwidgets)
      library(webshot)
      
      df <- df_scores()
      
      # --- Reconstruire le radar ---
      hc <- highchart() %>% 
        hc_chart(polar = TRUE, type = "line") %>%
        hc_xAxis(
          categories = df$theme,
          tickmarkPlacement = "on",
          labels = list(
            distance = 30,
            padding = 10,
            useHTML = TRUE,
            formatter = JS("
          function () {
            var angle = this.pos * (360 / this.axis.categories.length);
            if (angle > 330 || angle < 30) {
              return '<span style=\"position:relative; font-size:14px; color:#000\">' + this.value + '</span>';
            } else if (angle > 150 && angle < 210) {
              return '<span style=\"position:relative; top:6px; font-size:14px; color:#000\">' + this.value + '</span>';
            } else {
              return '<span style=\"font-size:14px; color:#000\">' + this.value + '</span>';
            }
          }
        ")
          )
        ) %>%
        hc_yAxis(
          min = 0, max = 100,
          gridLineColor = "#D0D0D0",
          gridLineWidth = 1,
          gridLineInterpolation = "polygon",
          lineWidth = 0,
          tickInterval = 25,
          labels = list(style = list(color = "#666", fontSize = "16px"))
        ) %>%
        hc_series(
          list(
            name = "Score",
            data = df$value,
            pointPlacement = "on",
            color = "#0055A4",
            lineWidth = 3,
            marker = list(enabled = TRUE, radius = 5, fillColor = "#0055A4")
          )
        ) %>%
        hc_tooltip(
          headerFormat = "",
          pointFormat = "{point.category} : <b>{point.y}%</b>",
          backgroundColor = "white",
          borderColor = "#0055A4",
          style = list(fontSize = "14px")
        ) %>%
        hc_legend(enabled = FALSE)
      
      # 1) widget HTML temporaire
      tmp_html <- tempfile(fileext = ".html")
      saveWidget(hc, tmp_html, selfcontained = TRUE)
      
      # 2) capture PNG
      tmp_png <- tempfile(fileext = ".png")
      webshot(tmp_html, tmp_png, vwidth = 900, vheight = 700, delay = 2)
      
      # --- Chemin compatible LaTeX ---
      radar_path_safe <- normalizePath(tmp_png, winslash = "/", mustWork = TRUE)
      
      # 3. Chemin final dans Rapports/
      if (!dir.exists("Rapports")) dir.create("Rapports")
      output_path <- file.path("Rapports", basename(file))
      
      # --- Génération du PDF ---
      rmarkdown::render(
        input = "rapport.Rmd",
        output_file = output_path,
        params = list(
          radar_path = radar_path_safe,
          df_details = df_details_r(),# <-- LA CORRECTION
          identite   = identite()
        ),
        envir = new.env(parent = globalenv())
      )
      file.copy(output_path, file, overwrite = TRUE)
      
    }
  )
}