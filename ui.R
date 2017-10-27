dashboardPage(skin = "yellow",
  dashboardHeader(title = "StaffViz - BT", titleWidth = 150,tags$li(a(href = 'https://demodashboard.shinyapps.io/DB_V1_LP/',
                                                                      icon("power-off"),
                                                                      title = "Back to Apps Home"),
                                                                    class = "dropdown")),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Staffing", tabName = "Staff", icon = icon("tachometer")),
      menuItem("Par personne", tabName = "StaffEquipe", icon = icon("bar-chart-o")),
      menuItem("Ecart Semaine", tabName = "Semaine", icon = icon("eye")),
      menuItem("planning People", tabName = "planning", icon = icon("calendar")),
      menuItem("planning Mission", tabName = "planningM", icon = icon("tasks", lib = "glyphicon")),
      tags$hr())
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Staff",
              h1("Calcul du taux de staffing par Grade"),
              p("Le taux de staffing calculé intègre  les jours de congés et l'inactivité (Type = '3 WBT - CONGES' ou '7 WBT - Inactivité').", style = "font-family: 'times'; font-si16pt"),

              fluidRow(plotOutput("Staff_plot"),
                         checkboxGroupInput("Type", label = "Ferme =1, Provisoire=2", 
                                            choices = list(1, 2),
                                            selected = c(1),
                                            inline = TRUE
                         ))
      ),
      tabItem(tabName = "Semaine",
              h1("Différence entre 2 semaines"),
              DT::dataTableOutput("mytable")
      ),
     
      tabItem(tabName = "StaffEquipe",
              h1("Calcul du taux de staffing par personne"),
              checkboxGroupInput("uiChk4", label = "", 
                                 choices = list("1-STA", "2-C", "3-CC", "4-CS", "5-MNG", "6-SM", "7-DIR", "8-ASS", "A1-AE", "A2-AES", "D1-DS1", "D3-DS3"),
                                 selected = c("2-C", "3-CC", "4-CS"),
                                 inline = TRUE
              ),
              checkboxGroupInput("Type2", label = "Ferme =1, Provisoire=2", 
                                 choices = list(1, 2),
                                 selected = c(1),
                                 inline = TRUE
              ),
              
                       tabsetPanel(
                         tabPanel("Nombre de jours", plotOutput("Consultant_plotJours", width = "100%", height = "600px")),
                         tabPanel("Taux de staffing",
                                  radioButtons("Mois", label = "MOIS", 
                                               choices = c("TOUS","JANV","FEV","MAR","AVR","MAI","JUIN","JUIL","AOUT","SEPT","OCT","NOV","DEC"),
                                               selected = c("TOUS"),
                                               inline = TRUE
                                  ),
                                  radioButtons("Taux", label = "Filtre Taux (actif si un mois est sélectionné)", 
                                               choices = c("<80%",">=80%"),
                                               selected = c("<80%"),
                                               inline = TRUE
                                  ),
                                  plotOutput("Consultant_plot", width = "100%", height = "600px")),
                                           
                                           
                                           
                         
                         tabPanel("Répartition du staffing par grade", plotOutput("Grade_plot"))
                       )
                       
      ),
      tabItem(tabName = "planning",
              h1("Visualisation des plannings par personnes"),
              fluidRow(column(width = 11, offset = 1, style='padding:3px;', 
                  box(width = 5,
                   selectInput("selUI1", 'ctl+A pour tous', people, 
                               multiple=TRUE, size=10, selected=people[1,1],	selectize=FALSE)),
                  box(width = 5,valueBoxOutput("TotalFerme"),valueBoxOutput("TotalProvisoire"),valueBoxOutput("Holidays")),
                  box(width = 12,
                   timevisOutput("timelineGroups"))
               ))
      ),
      tabItem(tabName = "planningM",
              h1("Visualisation des plannings par Code TOTEM"),
              fluidRow(column(width = 11, offset = 1, style='padding:3px;', 
                              box(width = 5,
                              selectInput("selUI2", 'ctl+A pour tous', totem, multiple=TRUE, size=10, 
                                          selected=totem[1,1],	selectize=FALSE)),
                              box(width = 5,valueBoxOutput("TotalFMission"),valueBoxOutput("TotalPMission")),
                              box(width = 12,
                              timevisOutput("timelineGroupsTOTEM"))
              ))
      )
      

    )

      
  )
)

