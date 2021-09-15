library(shiny)
library(datamanR)
library(glue)
library(shinyFiles)
library(shinydashboard)
library(data.table)
library(stringr)
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

# spring:
#   servlet:
#   multipart:
#   max-file-size: 200MB
#   max-request-size: 200MB

#options(shiny.maxRequestSize=100*1024^2)
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
      tags$style(""
                 # HTML('
                 #
                 #      '
                 # )
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
          verbatimTextOutput("manageR")),
      box(title = "Tables:", solidHeader = TRUE, status = "primary", collapsible = TRUE,
          verbatimTextOutput("TInfos"))
    )
  )
)



server <- function(input, output, session) {



  rv <- reactiveValues(
    ManR = DataManR$new())
  depend <- reactiveVal(0, "Value to update displays")

  create <- createDataManRServer("newDataManR", roots = c(home = "/home/datamanr"))

  observeEvent(create$set(), {
    req(create$data())
    rv$ManR <- create$data()
    update_disp()
  })

  load <- loadDataManRServer("loadDataManR", roots = c(home = "/home/datamanr"))
  observeEvent(load$set(), {
    req(load$data())
    rv$ManR <- load$data()
    update_disp()
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
    update_disp()
  })

  observeEvent(addTInfo$set(), {
    validate(
      need(rv$ManR$isValid(), "Current DataManR is not Valid. Please ensure it is Named and has a valid path")
    )
    isolate({
      data2 <- addTInfo$data()
      name2 <- addTInfo$name()
      path2 <- addTInfo$path()
      keys2 <- addTInfo$keys()
    })
    validate(
      need(!name2%in%names(rv$ManR$Tables), glue("There already exists a table named {name2} in {rv$ManR$name}"))
    )
    rv$ManR$addTable(def = TInfo$new(data = data2,
                                     name = name2,
                                     path = path2,
                                     keys = keys2))
    update_disp()
  })

  update_disp <- reactive(depend(depend()+1))
  output$manageR <- renderPrint({
    depend()
    rv$ManR
  })

  output$TInfos <- renderPrint({
    depend()
    if(length(rv$ManR$Tables)==0){
      print("No Tables to show")
    } else {
      print(rv$ManR$Tables)
    }
  })

  addTInfo <- addDefServer("add_table", roots = c(home = "/home/datamanr"))

  mod <- modDataManServer("modDataManR", roots = c(home = getwd()), datamanR = reactive(rv$ManR))

  #output$test <- renderPrint(modTest)



}



shinyApp(ui, server)


