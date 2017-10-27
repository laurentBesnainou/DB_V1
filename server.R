server <- function(input, output, session) {
  
  
  output$TreeMapOffre <- renderD3partitionR(

    #circleTreeMap, partitionChart, treeMap, sunburst,    collapsibleIndentedTree, collapsibleTree
    D3partitionR( data=list(path=combo_output()$path,value=combo_output()$value),type="treeMap", 
                  tooltipOptions = list(showAbsolutePercent = T, showRelativePercent = T),
                  legend=list(type="sequential",no_show = FALSE, 
                              color=list("ORE"="#2E58FF", "JSO"="#5C7DFF", "OGR" = "#8BA2FF", 
                                         "BES" = "#B9C7FF", "MMO"= "#E7ECFF", "UHE"="#AB2DFF", 
                                         "Transformation"="#FF8360","ETM"="#E8E288","Data"="#21BEE9", 
                                         "Digital Innovation"="#70F4B7","Sécurité"="#C00027")), trail = TRUE,
                  Input=list(enabled=T,Id="D3Part1",clickedStep=T,
                             currentPath=T,visiblePaths=T,visibleLeaf=T,visibleNode=T),
                  
                  width = 700,height = 450)
  )
  output$Consultant_plot <- renderPlot({
   
    #Calcul des effectifs moyens sur le mois
    nbM1 <- Staffing %>% filter (TYPE == 0, !is.na(JANV)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS) %>% mutate(MOIS = "JANV")
    nbM2 <- Staffing %>% filter (TYPE == 0, !is.na(FEV)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "FEV")
    nbM3 <- Staffing %>% filter (TYPE == 0, !is.na(MAR)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "MAR")
    nbM4 <- Staffing %>% filter (TYPE == 0, !is.na(AVR)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "AVR")
    nbM5 <- Staffing %>% filter (TYPE == 0, !is.na(MAI)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "MAI")
    nbM6 <- Staffing %>% filter (TYPE == 0, !is.na(JUIN)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "JUIN")
    nbM7 <- Staffing %>% filter (TYPE == 0, !is.na(JUIL)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "JUIL")
    nbM8 <- Staffing %>% filter (TYPE == 0, !is.na(AOUT)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "AOUT")
    nbM9 <- Staffing %>% filter (TYPE == 0, !is.na(SEPT)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "SEPT")
    nbM10 <- Staffing %>% filter (TYPE == 0, !is.na(OCT)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "OCT")
    nbM11 <- Staffing %>% filter (TYPE == 0, !is.na(NOV)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "NOV")
    nbM12 <- Staffing %>% filter (TYPE == 0, !is.na(DEC)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "DEC")
    #CALCUL DU NOMBRE DE JOURS MAX POSSIBLE
    nbM1$volume <- nbM1$n * nbJourMois[1] 
    nbM2$volume <- nbM2$n * nbJourMois[2] 
    nbM3$volume <- nbM3$n * nbJourMois[3] 
    nbM4$volume <- nbM4$n * nbJourMois[4] 
    nbM5$volume <- nbM5$n * nbJourMois[5] 
    nbM6$volume <- nbM6$n * nbJourMois[6] 
    nbM7$volume <- nbM7$n * nbJourMois[7] 
    nbM8$volume <- nbM8$n * nbJourMois[8] 
    nbM9$volume <- nbM9$n * nbJourMois[9] 
    nbM10$volume <- nbM10$n * nbJourMois[10] 
    nbM11$volume <- nbM11$n * nbJourMois[11] 
    nbM12$volume <- nbM12$n * nbJourMois[12] 
    
    
    nbEffecti <- rbind(nbM1, nbM2, nbM3, nbM4, nbM5, nbM6, nbM7, nbM8, nbM9, nbM10, nbM11, nbM12)
    
    #on calcul le nombre de jours vendus par CONSULTANTS et par mois  FERME
    nbF1 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
    nbF2 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
    nbF3 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
    nbF4 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
    nbF5 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
    nbF6 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
    nbF7 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
    nbF8 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
    nbF9 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
    nbF10 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
    nbF11 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
    nbF12 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
    nbFERME <- rbind(nbF1, nbF2, nbF3, nbF4, nbF5, nbF6, nbF7, nbF8, nbF9, nbF10, nbF11, nbF12)
    
    #on calcul le nombre de jours de congès + inactivité par grade et par mois
    nbC1 <- Staffing %>% filter (TYPE %in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
    nbC2 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
    nbC3 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
    nbC4 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
    nbC5 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
    nbC6 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
    nbC7 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
    nbC8 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
    nbC9<- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
    nbC10 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
    nbC11 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
    nbC12 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
    nbCONGES <- rbind(nbC1, nbC2, nbC3, nbC4, nbC5, nbC6, nbC7, nbC8, nbC9, nbC10, nbC11, nbC12)
    
    
    #on calcul les pourcentages
    tmp <- merge(nbEffecti,nbCONGES, by=c("CONSULTANTS","MOIS"), all.x=TRUE, all.y=FALSE)
    #on supprime tous les NA du tableau
    tmp[is.na(tmp)] <- 0
    
    tmp <- tmp %>% mutate(Possible = volume - nb) %>% select (CONSULTANTS,MOIS,Possible)
    tmp <- merge(nbFERME,tmp, by=c("CONSULTANTS","MOIS"))
    tmp <- tmp %>% mutate (Pourcentage = nb/Possible)
    
    #on regarde si on filtre sur un mois pour être < 80%
    if (input$Mois !="TOUS" ) {
      if (input$Taux == "<80%") {
      personnes <- tmp %>% filter(MOIS==input$Mois,Pourcentage <0.8) %>% select(CONSULTANTS)
      } else {
        personnes <- tmp %>% filter(MOIS==input$Mois,Pourcentage >= 0.8) %>% select(CONSULTANTS)
      }
      tmp <- tmp %>% filter (CONSULTANTS %in% personnes$CONSULTANTS)
    }

    #on va entourer le mois selectionné dans le ggplot
    nbrow <- nrow(tmp%>% select(CONSULTANTS) %>% group_by(CONSULTANTS) %>% summarise(nb=n()))
    NomMois <- c("JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC")
    nbX <- which(NomMois==input$Mois )
    if (input$Mois=="TOUS") {
      nbXmin <- 1
      nbXmax <- 12
      strColor ="white"
    } else
    {
      nbXmin <- nbX
      nbXmax <- nbX
      strColor ="#00B2FF"
    }  
    
    
    ggplot(data = tmp, aes(x=MOIS, y=CONSULTANTS, fill=Pourcentage)) + 
      ggtitle("Taux de staffing Ferme (congés et inactivité déduits)") +
        geom_tile(color = "white")+
      geom_text(aes(label = round(Pourcentage, 2)))+
      labs(x = "",   y = "")+
      scale_fill_gradient2(low = "red", high = "#0C872E", mid = "yellow", 
                           midpoint = 0.6, limit = c(0,1), space = "Lab",
                           name="Staffing Ferme") +
     
      xlim(c("JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC")) +
      theme_minimal()+ 
      geom_rect(aes(xmin = nbXmin- 0.5, xmax =  nbXmax+ 0.5, ymin =  1- 0.5, ymax = nbrow+ 0.5),
                fill = "transparent", color  =strColor, size = 1.5)+
      theme(axis.text.x = element_text( 
        size = 9))
  })
  
  
  output$Consultant_plotJours <- renderPlot({

    
    #on calcul le nombre de jours vendus par CONSULTANTS et par mois  FERME
    nbF1 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
    nbF2 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
    nbF3 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
    nbF4 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
    nbF5 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
    nbF6 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
    nbF7 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
    nbF8 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
    nbF9 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
    nbF10 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
    nbF11 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
    nbF12 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
    nbFERME <- rbind(nbF1, nbF2, nbF3, nbF4, nbF5, nbF6, nbF7, nbF8, nbF9, nbF10, nbF11, nbF12)
    
    ggplot(data = nbFERME, aes(x=MOIS, y=CONSULTANTS,fill=nb)) + 
      ggtitle("Nombre de jours  Ferme (hors congés et inactivité)") +
      geom_tile(color = "white")+
      geom_text(aes(label = nb))+
 
      scale_fill_gradient2(low = "red", high = "#0C872E",mid = "yellow", midpoint = 15, limits=c(0, 23),
                           space = "Lab",guide = "colourbar",
                           name="jours Ferme") +
      labs(x = "",   y = "")+
      xlim(c("JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC")) +
      theme_minimal()+ 
      theme(axis.text.x = element_text( 
        size = 9))
  })
  
  output$Grade_plot <- renderPlot({
    #Calcul des effectifs moyens sur le mois type==0 correspond aux personnes présentes dans les effectifs
    nbM1 <- Staffing %>% filter (TYPE == 0, !is.na(JANV)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS) %>% mutate(MOIS = "JANV")
    nbM2 <- Staffing %>% filter (TYPE == 0, !is.na(FEV)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "FEV")
    nbM3 <- Staffing %>% filter (TYPE == 0, !is.na(MAR)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "MAR")
    nbM4 <- Staffing %>% filter (TYPE == 0, !is.na(AVR)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "AVR")
    nbM5 <- Staffing %>% filter (TYPE == 0, !is.na(MAI)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "MAI")
    nbM6 <- Staffing %>% filter (TYPE == 0, !is.na(JUIN)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "JUIN")
    nbM7 <- Staffing %>% filter (TYPE == 0, !is.na(JUIL)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "JUIL")
    nbM8 <- Staffing %>% filter (TYPE == 0, !is.na(AOUT)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "AOUT")
    nbM9 <- Staffing %>% filter (TYPE == 0, !is.na(SEPT)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "SEPT")
    nbM10 <- Staffing %>% filter (TYPE == 0, !is.na(OCT)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "OCT")
    nbM11 <- Staffing %>% filter (TYPE == 0, !is.na(NOV)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "NOV")
    nbM12 <- Staffing %>% filter (TYPE == 0, !is.na(DEC)) %>% group_by(CONSULTANTS) %>% count(CONSULTANTS)  %>% mutate(MOIS = "DEC")
    #CALCUL DU NOMBRE DE JOURS MAX POSSIBLE facturable
    nbM1$volume <- nbM1$n * nbJourMois[1] 
    nbM2$volume <- nbM2$n * nbJourMois[2] 
    nbM3$volume <- nbM3$n * nbJourMois[3] 
    nbM4$volume <- nbM4$n * nbJourMois[4] 
    nbM5$volume <- nbM5$n * nbJourMois[5] 
    nbM6$volume <- nbM6$n * nbJourMois[6] 
    nbM7$volume <- nbM7$n * nbJourMois[7] 
    nbM8$volume <- nbM8$n * nbJourMois[8] 
    nbM9$volume <- nbM9$n * nbJourMois[9] 
    nbM10$volume <- nbM10$n * nbJourMois[10] 
    nbM11$volume <- nbM11$n * nbJourMois[11] 
    nbM12$volume <- nbM12$n * nbJourMois[12] 
    
    
    nbEffecti <- rbind(nbM1, nbM2, nbM3, nbM4, nbM5, nbM6, nbM7, nbM8, nbM9, nbM10, nbM11, nbM12)
    
    #on calcul le nombre de jours vendus par CONSULTANTS et par mois  FERME
    nbF1 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
    nbF2 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
    nbF3 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
    nbF4 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
    nbF5 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
    nbF6 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
    nbF7 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
    nbF8 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
    nbF9 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
    nbF10 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
    nbF11 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
    nbF12 <- Staffing %>% filter (TYPE %in% input$Type2,GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
    nbFERME <- rbind(nbF1, nbF2, nbF3, nbF4, nbF5, nbF6, nbF7, nbF8, nbF9, nbF10, nbF11, nbF12)
    
    #on calcul le nombre de jours de congès + inactivité par grade et par mois
    nbC1 <- Staffing %>% filter (TYPE %in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
    nbC2 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
    nbC3 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
    nbC4 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
    nbC5 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
    nbC6 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
    nbC7 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
    nbC8 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
    nbC9<- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
    nbC10 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
    nbC11 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
    nbC12 <- Staffing %>% filter (TYPE%in% c(3,7),GRADE %in% input$uiChk4) %>% group_by(CONSULTANTS,GRADE) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
    nbCONGES <- rbind(nbC1, nbC2, nbC3, nbC4, nbC5, nbC6, nbC7, nbC8, nbC9, nbC10, nbC11, nbC12)
    
    
    tmp <- merge(nbEffecti,nbCONGES, by=c("CONSULTANTS","MOIS"), all.x=TRUE, all.y=FALSE)
    #on supprime tous les NA du tableau
    tmp[is.na(tmp)] <- 0
    
    tmp <- tmp %>% mutate(Possible = volume - nb) %>% select (CONSULTANTS,MOIS,Possible)
    tmp <- merge(nbFERME,tmp, by=c("CONSULTANTS","MOIS"))
    tmp <- tmp %>% mutate (Pourcentage = nb/Possible) %>% 
      mutate(Classification = cut(Pourcentage, breaks = c(-1, 0.8,Inf),labels = c("Staff KO","Staff OK")))
    #cela va permettre de classer le wrap
    tmp$MOIS = factor(tmp$MOIS, levels=c("JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC"))

    tmp <- tmp %>% group_by(GRADE,MOIS,Classification) %>% count()
    # browser()
    # ggplot(data = tmp) + 
    #   geom_bar(mapping = aes(x = GRADE, fill = Classification), position = "fill")+
    #     facet_wrap(~ MOIS)
    # 
    #tmp <- data.frame(GRADE= c("2-C","2-C","2-C","2-C"), Classification=c("Staff KO","Staff OK","Staff KO","Staff OK"),n=c(3,10,2,10),MOIS=c("Jan","Jan","fev","fev"))
    ggplot(data = tmp, aes(x=GRADE, y=n, fill=Classification)) + 
      geom_bar(stat = "identity",position = "dodge") +

      
      scale_fill_manual("legend", values = c("Staff KO" = "#ED4B4B", "Staff OK" = "#84DD63")) +
      geom_text(aes(label= n, y = n + 0.8), position = position_dodge(width = 0.9), vjust = 0) +
      facet_wrap(~ MOIS) +
      theme_stata() +
                   theme(strip.background =element_rect(fill="white"))+
      theme(strip.text.x = element_text(size = 12, colour = "BLUE"))
      
    
    # ggplot(data = tmp, aes(x=Mois, y=nb, fill=Etat)) + 
    #   geom_bar(stat = 'identity', position = 'dodge') + 
    #   geom_text(aes(y = nb + .5,    # nudge above top of bar
    #                 label = paste0(nb, '%')),    # prettify
    #             position = position_dodge(width = .9), 
    #             size = 3)
    # 
    #   facet_wrap(~ MOIS)

  })
  output$TotalFerme <- renderValueBox({

    data <- Staffing %>% filter (CONSULTANTS %in% input$selUI1,TYPE==1) 

    total <-   round(sum(data$TOTAL,na.rm =TRUE ),digits=0)
    
    valueBox(
      total, "J Ferme 2017", icon = icon("calendar"),
      color = "blue"
    )
  })
    output$TotalFMission <- renderValueBox({
      
      data <- Staffing %>% filter (ID_TOTEM %in% input$selUI2,TYPE==1) 
  
      total <-   round(sum(data$TOTAL,na.rm =TRUE ),digits=0)
      
      valueBox(
        paste0(total, " JH"), "Ferme", icon = icon("calendar"),
        color = "blue"
      )
    })
 
  output$TotalProvisoire <- renderValueBox({
    
    data <- Staffing %>% filter (CONSULTANTS %in% input$selUI1,TYPE==2)  
    
    total <-   round(sum(data$TOTAL,na.rm =TRUE ),digits=0)
    
    valueBox(
      paste0(total, " JH"), "Provisoire                   .", icon = icon("calendar"),
      color = "green"
    )
  })
  
  output$Holidays <- renderValueBox({
    
    data <- Staffing %>% filter (CONSULTANTS %in% input$selUI1,TYPE%in% c(3,4,5,6,7))
    
    total <-   round(sum(data$TOTAL,na.rm =TRUE ),digits=0)
    
    valueBox(
      paste0(total, " JH"), "Autres (congès, formation, maladie)", icon = icon("paper-plane"),
      color = "teal"
    )
  })
  output$TotalPMission <- renderValueBox({
    
    data <- Staffing %>% filter (ID_TOTEM %in% input$selUI2,TYPE==2) 

    total <-   round(sum(data$TOTAL,na.rm =TRUE ),digits=0)
    
    valueBox(
      paste0(total, " JH"), "Provisoire", icon = icon("calendar"),
      color = "green"
    )
  })
  
  
  output$Staff_plot <- renderPlot({
    #Calcul des effectifs moyens sur le mois
    
    
    nbM1 <- Staffing %>% filter (TYPE == 0, !is.na(JANV)) %>% group_by(GRADE) %>% count(GRADE) %>% mutate(MOIS = "JANV")
    nbM2 <- Staffing %>% filter (TYPE == 0, !is.na(FEV)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "FEV")
    nbM3 <- Staffing %>% filter (TYPE == 0, !is.na(MAR)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "MAR")
    nbM4 <- Staffing %>% filter (TYPE == 0, !is.na(AVR)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "AVR")
    nbM5 <- Staffing %>% filter (TYPE == 0, !is.na(MAI)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "MAI")
    nbM6 <- Staffing %>% filter (TYPE == 0, !is.na(JUIN)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "JUIN")
    nbM7 <- Staffing %>% filter (TYPE == 0, !is.na(JUIL)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "JUIL")
    nbM8 <- Staffing %>% filter (TYPE == 0, !is.na(AOUT)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "AOUT")
    nbM9 <- Staffing %>% filter (TYPE == 0, !is.na(SEPT)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "SEPT")
    nbM10 <- Staffing %>% filter (TYPE == 0, !is.na(OCT)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "OCT")
    nbM11 <- Staffing %>% filter (TYPE == 0, !is.na(NOV)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "NOV")
    nbM12 <- Staffing %>% filter (TYPE == 0, !is.na(DEC)) %>% group_by(GRADE) %>% count(GRADE)  %>% mutate(MOIS = "DEC")
    #CALCUL DU NOMBRE DE JOURS MAX POSSIBLE
    nbM1$volume <- nbM1$n * nbJourMois[1] 
    nbM2$volume <- nbM2$n * nbJourMois[2] 
    nbM3$volume <- nbM3$n * nbJourMois[3] 
    nbM4$volume <- nbM4$n * nbJourMois[4] 
    nbM5$volume <- nbM5$n * nbJourMois[5] 
    nbM6$volume <- nbM6$n * nbJourMois[6] 
    nbM7$volume <- nbM7$n * nbJourMois[7] 
    nbM8$volume <- nbM8$n * nbJourMois[8] 
    nbM9$volume <- nbM9$n * nbJourMois[9] 
    nbM10$volume <- nbM10$n * nbJourMois[10] 
    nbM11$volume <- nbM11$n * nbJourMois[11] 
    nbM12$volume <- nbM12$n * nbJourMois[12] 
    
    
    nbEffecti <- rbind(nbM1, nbM2, nbM3, nbM4, nbM5, nbM6, nbM7, nbM8, nbM9, nbM10, nbM11, nbM12)
    
    #on calcul le nombre de jours vendus par grade et par mois  FERME
    nbF1 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
    nbF2 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
    nbF3 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
    nbF4 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
    nbF5 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
    nbF6 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
    nbF7 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
    nbF8 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
    nbF9 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
    nbF10 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
    nbF11 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
    nbF12 <- Staffing %>% filter (TYPE %in% input$Type) %>% group_by(GRADE) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
    nbFERME <- rbind(nbF1, nbF2, nbF3, nbF4, nbF5, nbF6, nbF7, nbF8, nbF9, nbF10, nbF11, nbF12)

    
    #on calcul le nombre de jours de congès + inactivité par grade et par mois
    nbC1 <- Staffing %>% filter (TYPE %in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(JANV,na.rm =TRUE )) %>% mutate(MOIS = "JANV")
    nbC2 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(FEV,na.rm =TRUE )) %>% mutate(MOIS = "FEV")
    nbC3 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(MAR,na.rm =TRUE )) %>% mutate(MOIS = "MAR")
    nbC4 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(AVR,na.rm =TRUE ))  %>% mutate(MOIS = "AVR")
    nbC5 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(MAI,na.rm =TRUE )) %>% mutate(MOIS = "MAI")
    nbC6 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(JUIN,na.rm =TRUE ))  %>% mutate(MOIS = "JUIN")
    nbC7 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(JUIL,na.rm =TRUE ))  %>% mutate(MOIS = "JUIL")
    nbC8 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(AOUT,na.rm =TRUE )) %>% mutate(MOIS = "AOUT")
    nbC9<- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(SEPT,na.rm =TRUE )) %>% mutate(MOIS = "SEPT")
    nbC10 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(OCT,na.rm =TRUE )) %>% mutate(MOIS = "OCT")
    nbC11 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(NOV,na.rm =TRUE )) %>% mutate(MOIS = "NOV")
    nbC12 <- Staffing %>% filter (TYPE%in% c(3,7)) %>% group_by(GRADE) %>% summarise(nb = sum(DEC,na.rm =TRUE )) %>% mutate(MOIS = "DEC")
    nbCONGES <- rbind(nbC1, nbC2, nbC3, nbC4, nbC5, nbC6, nbC7, nbC8, nbC9, nbC10, nbC11, nbC12)
    
    
    #on calcul les pourcentages
    tmp <- merge(nbEffecti,nbCONGES, by=c("GRADE","MOIS"), all.x=TRUE, all.y=FALSE)
    #on supprime tous les NA du tableau
    tmp[is.na(tmp)] <- 0
    
    tmp <- tmp %>% mutate(Possible = volume - nb) %>% select (GRADE,MOIS,Possible)
    tmp <- merge(nbFERME,tmp, by=c("GRADE","MOIS"))
    tmp <- tmp %>% mutate (Pourcentage = nb/Possible)
    tmpTOTAL <-  tmp %>% group_by(MOIS) %>% summarise(Pourcentage = sum(nb)/sum(Possible)) %>% mutate(GRADE="0-Global")
    tmp <- tmp %>% select(MOIS,Pourcentage,GRADE)
    tmp <-rbind(tmp, tmpTOTAL)
    
    
    
    ggplot(data = tmp, aes(x=MOIS, y=GRADE, fill=Pourcentage)) + 
      ggtitle("Taux de staffing Ferme (congés et inactivité déduits)") +
      geom_tile(color = "white")+
      geom_text(aes(label = round(Pourcentage, 2)))+
      
      geom_rect(aes(xmin = 1- 0.5, xmax =  12+ 0.5, ymin =  1- 0.5, ymax = 1+ 0.5),
                fill = "transparent", color = "#00B2FF", size = 1.5)+
      
      scale_fill_gradient2(low = "red", high = "#0C872E", mid = "yellow", 
                           midpoint = 0.6, limit = c(0,1), space = "Lab",
                           name="Staffing Ferme") +
      
      xlim(c("JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC")) +
      theme_minimal()+ 
      theme(axis.text.x = element_text( 
        size = 9))
  })
  
  output$timelineGroupsTOTEM <- renderTimevis({
    Debut <- c("2017-01-01", "2017-02-01","2017-03-01",
               "2017-04-01", "2017-05-01","2017-06-01",
               "2017-07-01", "2017-08-01","2017-09-01",
               "2017-10-01", "2017-11-01","2017-12-01",
               "2018-01-01", "2018-02-01","2018-03-01")
    Fin <-  c("2017-01-31", "2017-02-28","2017-03-31",
              "2017-04-30", "2017-05-31","2017-06-30",
              "2017-07-31", "2017-08-31","2017-09-30",
              "2017-10-31", "2017-11-30","2017-12-31",
              "2018-01-31", "2018-02-28","2018-03-31")
    
    Staffing <- mutate(Staffing, id = row_number())
    staff <- Staffing %>% 
      arrange(CONSULTANTS,MISSIONS) %>% rowwise() %>% mutate(TOT = 
                                                               sum(JANV, FEV, MAR, AVR, MAI, JUIN, JUIL, AOUT, SEPT, OCT, NOV, DEC,
                                                                   JANV_N_PLUS_1_, FEV_N_PLUS_1_, MAR_N_PLUS_1_, na.rm=TRUE))
    
    dataGroups <- staff %>% 
      filter (TYPE %in% c(1,2)) %>% 
      mutate (start = ifelse(! is.na(JANV), Debut[1],
                             ifelse(! is.na(FEV), Debut[2],
                                    ifelse(! is.na(MAR), Debut[3],
                                           ifelse(! is.na(AVR), Debut[4],
                                                  ifelse(! is.na(MAI), Debut[5],
                                                         ifelse(! is.na(JUIN), Debut[6],
                                                                ifelse(! is.na(JUIL), Debut[7],
                                                                       ifelse(! is.na(AOUT), Debut[8],
                                                                              ifelse(! is.na(SEPT), Debut[9],
                                                                                     ifelse(! is.na(OCT), Debut[10],
                                                                                            ifelse(! is.na(NOV), Debut[11],
                                                                                                   ifelse(! is.na(DEC), Debut[12],
                                                                                                          ifelse(! is.na(JANV_N_PLUS_1_), Debut[13],
                                                                                                                 ifelse(! is.na(FEV_N_PLUS_1_), Debut[14],
                                                                                                                        ifelse(! is.na(MAR_N_PLUS_1_), Debut[15],"")))))))))))))))) %>%
      mutate (end = ifelse(! is.na(JANV_N_PLUS_1_), Fin[15],
                           ifelse(! is.na(FEV_N_PLUS_1_), Fin[14],
                                  ifelse(! is.na(MAR_N_PLUS_1_), Fin[13],
                                         ifelse(! is.na(DEC), Fin[12],
                                                ifelse(! is.na(NOV), Fin[11],
                                                       ifelse(! is.na(OCT), Fin[10],
                                                              ifelse(! is.na(SEPT), Fin[9],
                                                                     ifelse(! is.na(AOUT), Fin[8],
                                                                            ifelse(! is.na(JUIL), Fin[7],
                                                                                   ifelse(! is.na(JUIN), Fin[6],
                                                                                          ifelse(! is.na(MAI), Fin[5],
                                                                                                 ifelse(! is.na(AVR), Fin[4],
                                                                                                        ifelse(! is.na(MAR), Fin[3],
                                                                                                               ifelse(! is.na(FEV), Fin[2],
                                                                                                                      ifelse(! is.na(JANV), Fin[1],"")))))))))))))))) %>%
      mutate(content = CONSULTANTS) %>%
      mutate(group = ID_TOTEM) %>%
      mutate(type = "range")  %>%
      mutate(style =  ifelse(TYPE==1, "color: black; background-color: #39A0ED;","color: black; background-color: #7AC74F;")) %>%
      mutate(title = paste (CONSULTANTS,MISSIONS,TOT,"jh", sep = " - ")) %>%
      filter(start !="", group %in% input$selUI2) %>% 
      select (id, content, start, end, group, type, title,style) 
    groups <- dataGroups %>% distinct(group) %>% mutate (content = group)
    colnames(groups) <- c("id","content")
    timevis(data = dataGroups,  group = groups, options = list(selectable=TRUE, showCurrentTime = FALSE, orientation = "top"))
    
  })
  
  output$timelineGroups <- renderTimevis({
    Debut <- c("2017-01-01", "2017-02-01","2017-03-01",
               "2017-04-01", "2017-05-01","2017-06-01",
               "2017-07-01", "2017-08-01","2017-09-01",
               "2017-10-01", "2017-11-01","2017-12-01",
               "2018-01-01", "2018-02-01","2018-03-01")
    Fin <-  c("2017-01-31", "2017-02-28","2017-03-31",
              "2017-04-30", "2017-05-31","2017-06-30",
              "2017-07-31", "2017-08-31","2017-09-30",
              "2017-10-31", "2017-11-30","2017-12-31",
              "2017-01-31", "2018-02-28","2018-03-31")
    
    Staffing <- mutate(Staffing, id = row_number())
    staff <- Staffing %>% 
      arrange(CONSULTANTS,MISSIONS) %>% rowwise() %>% 
      mutate(TOT = sum(JANV, FEV, MAR, AVR, MAI, JUIN, JUIL, AOUT, SEPT, OCT, NOV, DEC,
                       JANV_N_PLUS_1_, FEV_N_PLUS_1_, MAR_N_PLUS_1_, na.rm=TRUE))
    
    dataGroups <- staff %>% 
      filter (TYPE %in% c(1,2)) %>% 
      mutate (start = ifelse(! is.na(JANV), Debut[1],
                             ifelse(! is.na(FEV), Debut[2],
                                    ifelse(! is.na(MAR), Debut[3],
                                           ifelse(! is.na(AVR), Debut[4],
                                                  ifelse(! is.na(MAI), Debut[5],
                                                         ifelse(! is.na(JUIN), Debut[6],
                                                                ifelse(! is.na(JUIL), Debut[7],
                                                                       ifelse(! is.na(AOUT), Debut[8],
                                                                              ifelse(! is.na(SEPT), Debut[9],
                                                                                     ifelse(! is.na(OCT), Debut[10],
                                                                                            ifelse(! is.na(NOV), Debut[11],
                                                                                                   ifelse(! is.na(DEC), Debut[12],
                                                                                                          ifelse(! is.na(JANV_N_PLUS_1_), Debut[13],
                                                                                                                 ifelse(! is.na(FEV_N_PLUS_1_), Debut[14],
                                                                                                                        ifelse(! is.na(MAR_N_PLUS_1_), Debut[15],"")))))))))))))))) %>%
      mutate (end = ifelse(! is.na(JANV_N_PLUS_1_), Fin[15],
                           ifelse(! is.na(FEV_N_PLUS_1_), Fin[14],
                                  ifelse(! is.na(MAR_N_PLUS_1_), Fin[13],
                                         ifelse(! is.na(DEC), Fin[12],
                                                ifelse(! is.na(NOV), Fin[11],
                                                       ifelse(! is.na(OCT), Fin[10],
                                                              ifelse(! is.na(SEPT), Fin[9],
                                                                     ifelse(! is.na(AOUT), Fin[8],
                                                                            ifelse(! is.na(JUIL), Fin[7],
                                                                                   ifelse(! is.na(JUIN), Fin[6],
                                                                                          ifelse(! is.na(MAI), Fin[5],
                                                                                                 ifelse(! is.na(AVR), Fin[4],
                                                                                                        ifelse(! is.na(MAR), Fin[3],
                                                                                                               ifelse(! is.na(FEV), Fin[2],
                                                                                                                      ifelse(! is.na(JANV), Fin[1],"")))))))))))))))) %>%
      mutate(content = MISSIONS) %>%
      mutate(group = CONSULTANTS) %>%
      mutate(type = "range")  %>%
      mutate(style =  ifelse(TYPE==1, "color: black; background-color: #39A0ED;","color: black; background-color: #7AC74F;")) %>%
      mutate(title = paste (CONSULTANTS,MISSIONS,TOT,"jh", sep = " - ")) %>%
      filter(start !="", group %in% input$selUI1) %>% 
      select (id, content, start, end, group, type, title,style) 
    groups <- dataGroups %>% distinct(group) %>% mutate (content = group)
    colnames(groups) <- c("id","content")
    timevis(data = dataGroups,  group = groups, options = list(selectable=TRUE, showCurrentTime = FALSE, orientation = "top"))
    
  })
  
  output$mytable = DT::renderDataTable({
    Mois <- c("JANV",	"FEV",	"MAR",	"AVR"	,"MAI",	"JUIN",	
              "JUIL",	"AOUT",	"SEPT"	,"OCT",	"NOV",	"DEC")
    nbMois <- month(Sys.Date())
    Mois <- Mois[nbMois:length(Mois)]
    colonnes <- c("CONSULTANTS",	"TYPE",	"MISSIONS",	"ID_TOTEM",
                  "TOTAL",Mois)
    x <- Staffing %>% select(colonnes) %>% filter(TYPE%in% c(1)) %>% mutate_all(funs(replace(., is.na(.), 0)))
    y <- StaffinPrev %>% select(colonnes)%>% filter(TYPE%in% c(1)) %>% mutate_all(funs(replace(., is.na(.), 0)))
    patch <- diff_data(y, x,never_show_order=TRUE)
    render_diff(patch, title="Comparaison du Staffing sur 2 semaines", pretty = TRUE) 
    Resultat <-  as.data.frame(patch$get_matrix())
    Resultat <-subset(Resultat, V1!="..." )
    Resultat <-subset(Resultat, V1!="" )
    
    colnames(Resultat) <-as.character(unlist(Resultat[Resultat$V1=="@@",]))
    Resultat <- Resultat[-1,] 

  })
  
  output$Grade_plot <- renderPlot({
    #on va regarder toutes les missions gagnées
    dataTOTEM <- Staffing %>% filter (TYPE==1,!is.na(ID_TOTEM)) %>% select(CONSULTANTS, ID_TOTEM) 
    dataPilotageGraphe <- pilotage_data %>% filter(WEEK==max(pilotage_data$WEEK), STEP=="4 - Gagnée", 
                                                   CODE_TOTEM %in% dataTOTEM$ID_TOTEM)%>%
      select(CODE_TOTEM,OFFRE_PRINCIPALE) %>% mutate(ID_TOTEM=CODE_TOTEM)
    mission <- left_join(x=dataTOTEM,y=dataPilotageGraphe)
    
    colnames(data)=c("charline", "JP", "Kath", "Gary", "Conor", "Marion", "JB", "gui", "Vincent")
    rownames(data)=c("french","phylo","latin","musique","art","sport","math","stat","R")
  })
}