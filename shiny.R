library(shiny)
library(datamanR)
library(shinyFiles)
library(shinydashboard)
library(data.table)
library(stringr)
library(datamanR)
library(shinyFiles)
library(shinydashboard)
library(shinyBS)
library(DT)

#.sF-dirInfo
#.sF-dirWind
#.sF-dirList
#.sf-dirContent
#.may need to source shinyFiles .css from a different file

# .sF-modal .modal-content {
# overflow: auto;
# min-width: 276px;
# max-height: calc(100vh - 45px);
# }
#
# # .sF-dirWindow {
# # height: 40vmin;
# # cursor: default;
# # }
#
# .sF-dirInfo>div {
# height: calc(calc(20vw * .75) + calc(10vh * .75));
# border: 1px solid grey;
# border-radius: 5px;
# overflow: auto;
# }

ui <- dashboardPage(
  dashboardHeader(title = "Data ManageR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "homepage", icon = icon("dashboard")),
      menuItem("Data ManageR", tabName = "datamanr", icon = icon("database"),
               menuSubItem("Load Manager", "datamanr_load"),
               menuSubItem("Create Manager", "datamanr_make"),
               menuSubItem("Modify manager", "datamanr_mod")),
      menuItem("Data Tables", tabName = "datatable", icon = icon("table"),
               menuSubItem("Add", "datatable_add"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML('
             '
        )
      )
    ),
    tabItems(
      tabItem(
        tabName = "homepage",
        fluidRow(h1("Welcome to Data ManageR!"))
      ),
      tabItem(
        tabName = "datamanr",
        h1("TBD")),
      tabItem(
        tabName = "datamanr_load",
        fluidRow(box(loadDataManRUI("loadDataManR"), width = 12,
                     collapsible = T, title = "Load From File", solidHeader = T,
                     status = "primary"))
      ),
      tabItem(
        tabName = "datamanr_make",
        fluidRow(box(createDataManRUI("newDataManR"), width = 12))
      ),
      tabItem(
        tabName = "datamanr_mod",
        fluidRow(box(modDataManRUI("modDataManR"), width = 12))
      ),
      tabItem(
        tabName = "datatable_add",
        fluidRow(box(addDefUI("add_table"), width = 12))
      )
    ), #tabItems end ----
    fluidRow(
      br(),
      box(title = "Current Data ManageR", solidHeader = TRUE,
          status = "primary", collapsible = TRUE,
          verbatimTextOutput("manageR"))
    )
  )
)



server <- function(input, output, session) {



  rv <- reactiveValues(
    ManR = DataManR$new())

  create <- createDataManRServer("newDataManR", roots = c(home = "/home/datamanr"))

  observeEvent(create$set(), {
    req(create$data())
    rv$ManR <- create$data()
  })

  load <- loadDataManRServer("loadDataManR", roots = c(home = "/home/datamanr"))
  observeEvent(load$set(), {
    req(load$data())
    rv$ManR <- load$data()
  })

  #csvfile <- csvFileServer("csv", FALSE)


  output$manageR <- renderPrint(rv$ManR)

  mod <- modDataManServer("modDataManR", roots = c(home = "/home/datamanr"), datamanR = reactive(rv$ManR))

  observeEvent(mod$save(), {
    if(mod$update_dimg()=="Yes"){
      if(file.exists(rv$ManR$rds_file)){
        file.remove(rv$ManR$rds_file)
      }
    }
    rv$ManR$name <- mod$mod_name()
    rv$ManR$path <- mod$mod_path()
    rv$ManR$save()
  })

  output$manageR <- renderPrint({
    mod$save()
    rv$ManR
  })

  addDefServer("add_table")





  mod <- modDataManServer("modDataManR", roots = c(home = getwd()), datamanR = reactive(rv$ManR))

  #output$test <- renderPrint(modTest)



}



shinyApp(ui, server)


