# Fichier Global
#libraires utilisées
library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(RColorBrewer) #coulors pour ggplot
library(stringr)
library(lubridate) # gestion des dates
library(xts) #serie temporelle
library(purrr) # 
library(tidyr)
library(scales) 
#library(googleVis) # Permet d'utiliser les graphiques >  Google Chart 
library(D3partitionR) # permet de générer le treemap dynamique
library(highcharter) # courbe de comparaison des CA de 2015 à 2017
library(timevis)# Gantt
library(daff) # Package pour afficher les différences enre 2 semaines
library(igraph) #permet de faire un graphe des missions gagnées
library(DT)
#chargement des données
load("data/pilotage2016_data.RData")
load("data/pilotage2015_data.RData")

load("data/staffing2017PREV.RData")
StaffinPrev <- Staffing
load("data/staffing2017.RData")




#données 2017
load("data/pilotage_data.RData")
pilotage_2017 <- pilotage_data
NomMois <- c("JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC")
nbJourMois <- c( 22,20,23,19,20,21,20,22,21,22,21,20,22,20,22)
people <- Staffing %>% arrange(CONSULTANTS) %>% distinct(CONSULTANTS) 
people <- people[people$CONSULTANTS !="(ex)",]
totem <- Staffing %>% filter(TYPE %in% c(1,2)) %>% group_by(ID_TOTEM,TYPE) %>% summarize(tot=sum(TOTAL)) %>%
  ungroup() %>% filter (tot>0) %>%
arrange(ID_TOTEM) %>% distinct(ID_TOTEM)

combo_output <- function(){
  
BT_Tree <- pilotage_data %>% filter (STEP == "4 - Gagnée",WEEK==max(pilotage_data$WEEK)) %>%
  mutate (SUJET=paste(round(CA_BT__N__KE,0),SUJET,sep=" k€<br> :")) %>%
  group_by(GROUPE, OFFRE_PRINCIPALE,ASSOCIE,SECTEUR,SUJET) %>%
  summarize(CA = sum(CA_BT__N__KE,na.rm=TRUE)) %>% mutate (DEBUT = "CA BT")

#on construit la liste pour le treemap
nom <- list()
nb_1 <- length(BT_Tree$ASSOCIE)

for (i in 1:nb_1) {
  nom[[i]] <- c(BT_Tree$DEBUT[i], BT_Tree$OFFRE_PRINCIPALE[i] ,BT_Tree$ASSOCIE[i],BT_Tree$SECTEUR[i],BT_Tree$GROUPE[i],BT_Tree$SUJET[i])
}
CA_TOTAL <- c(BT_Tree$CA)
list(path = nom, value = CA_TOTAL)

}