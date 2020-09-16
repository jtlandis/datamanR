library(shiny)
library(datamanR)
library(shinyFiles)
library(shinydashboard)

#.sF-dirInfo
#.sF-dirWind
#.sF-dirList
#.sf-dirContent
#.may need to source shinyFiles .css from a different file
ui <- dashboardPage(
  dashboardHeader(title = "Data ManageR"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "homepage", icon = icon("dashboard")),
      menuItem("Data ManageR", tabName = "datamanr", icon = icon("database"),
               menuSubItem("Load Manager", "datamanr_load"),
               menuSubItem("Create Manager", "datamanr_make"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML('
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



server <- function(input, output, session){



  rv <- reactiveValues(
    ManR = DataManR$new()
  )

  create <- createDataManRServer("newDataManR")

  observeEvent(create$set(), {
    req(create$data())
    rv$ManR <- create$data()
  })

  load <- loadDataManRServer("loadDataManR")

  observeEvent(load$set(), {
    req(load$data())
    rv$ManR <- load$data()
  })

  #csvfile <- csvFileServer("csv", FALSE)

  output$manageR <- renderPrint(rv$ManR)



  #output$test <- renderPrint(modTest)



}



shinyApp(ui, server)


# uit <- fluidPage(
#   loadDataManRUI("test")
# )
#
# servert <- function(input, output, session){
#   test1 <- loadDataManRServer("test")
# }
#
# shinyApp(uit, servert)
