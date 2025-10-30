library(shiny)
library(readxl)
library(dplyr)
library(stringr)
library(shinyjs)
library(gtools)
library(rsconnect)
library(purrr)
library(writexl)

questions_list <- read_excel("data/question_autodiag.xlsx") %>%
  rename_with(~ gsub("é", "e", .x)) %>%      
  rename_with(trimws) %>%                   
  mutate(
    Theme      = as.character(Theme),
    Numero     = as.character(Numero),
    Questions  = as.character(Questions),
    Style      = as.character(Style),
    Reponses   = as.character(Reponses),
    Parent     = as.character(Parent),
    Condition  = as.character(Condition),
    TexteTheme = as.character(TexteTheme),
    Affichage  = as.character(Affichage),
    Remplace  = as.character(Remplace),
    Choix = as.character(Choix),
    Section = as.character(Section),
    Note = as.character(Note),
    Observation = as.character(Observation)
  ) %>%
  filter(!is.na(Questions), !is.na(Theme), !is.na(Numero)) %>%
  
  # on regroupe mais on garde les réponses "plates"
  group_by(Theme, Numero, Parent, Questions, Style, Condition, TexteTheme, Affichage, Remplace, Choix, Section, Note, Observation) %>%
  summarise(Reponses = paste(Reponses, collapse = ";"), .groups = "drop") %>%
  
  # on transforme en vecteurs propres
  mutate(
    reponses = strsplit(Reponses, ";"),
    reponses = lapply(reponses, trimws),   # nettoyer espaces
    Numero_num    = as.numeric(str_extract(Numero, "^\\d+")),
    Numero_suffix = str_extract(Numero, "[a-zA-Z]+$")
  ) %>%
  filter(!is.na(Numero_num)) %>%
  mutate(
    Numero_order = paste0(sprintf("%03d", Numero_num), Numero_suffix)
  ) %>%
  arrange(Numero_order)

themes <- questions_list %>%
  pull(Theme) %>%
  unique()