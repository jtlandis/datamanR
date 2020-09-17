#' @import shiny
#' @import shinyFiles


#' @description not working
#' @export
createDataManRUI <- function(id){
  ns <- NS(id)

  tagList(
    textInput(ns("name"), "Data Manager Name:"),
    shinyDirButton(ns("directory"), "Choose Save Directory", "Select/Make a Directory for the Data Manager"),
    actionButton(ns("createDataMan"), "Create!"),
    actionButton(ns("set"), "Set Data Manager", style = "background-color: #3BBFDA;"),
    div(style="min-height:10vh;max-height:25vh;overflow-y:scroll;padding:15px;", verbatimTextOutput(ns("display")))
  )
}

#' @export
createDataManRServer <- function(id, roots = c(home = getwd()), filetypes =  c('', 'txt', 'tsv', 'csv', 'rds', 'R', 'Rmd')) {
  moduleServer(
    id,
    function(input, output, session){


      shinyDirChoose(input, "directory", roots = roots, filetypes = filetypes)

      dir <- reactive({
        validate(need(!is.null(input$directory), "Please select a Directory"))
        input$directory
        })
      nam <- reactive({
        validate(need(input$name != "", "Please give the Data Manager a Name"))
        input$name
      })

      path <- reactive({
        parseDirPath(roots = roots, selection = dir())
      })

      dataManR <- reactive({
       req(input$createDataMan)
       isolate({
          name <- nam()
          direct <- path()
        })
        datamanR::DataManR$new(name = name, path = direct)
        })

      set <- reactive({
        input$set
      })

      output$display <- renderPrint({
        req(input$createDataMan)
        dataManR()
      })

      output$info <- renderPrint({
        Sys.info()
      })

      return(list(data = dataManR, set = set))

    }
  )
}

#' @export
loadDataManRUI <- function(id){
  ns <- NS(id)

  tagList(
    shinyFilesButton(ns("file"), "Load", "Select a .rds File!", multiple = F),
    actionButton(ns("set"), "Set Data Manager", style = "background-color: #3BBFDA;"),
    br(),
    br(),
    div(style="min-height:10vh;max-height:25vh;overflow-y:scroll;padding:15px;", fluidRow(verbatimTextOutput(ns("display"))))
  )
}

#' @export
loadDataManRServer <- function(id, roots = c(home = getwd())) {
  moduleServer(
    id,
    function(input, output, session){

      shinyFileChoose(input, "file", roots = roots, filetypes = c('', 'rds'))

      dir <- reactive({
        validate(need(input$file, "Please select a File"),
                 need(!is.null(input$file), "Please select a File"))
        input$file
      })

      path <- reactive({
        parseFilePaths(roots = roots, selection = dir())
      })

      dataManR <- reactive({
        validate(need("datapath"%in%names(path()), "Select a valid File"),
                 need(nrow(path())==1, "Select a valid File"))
        readRDS(path()$datapath)
      })

      set <- reactive({
        input$set
        })

      output$display <- renderPrint({
        dataManR()
      })

      return(list(data = dataManR,set = set))

    }
  )
}


#' @export
modDataManRUI <- function(id){
  ns <- NS(id)

  tagList(
    uiOutput(ns("ui")),
    hr(),
    h3("Preview"),
    verbatimTextOutput(ns("preview")),
    actionButton(ns("save"), "Save")

  )
}

#' @export
modDataManServer <- function(id, roots = c(home = getwd()), datamanR) {
  moduleServer(
    id,
    function(input, output, session){

      #dm <- reactiveVal(datamanR)
      shinyDirChoose(input, "directory", roots = roots, filetypes = c('', 'rds'))

      dir <- reactive({
        input$directory
        ifelse(!isTruthy(input$directory),
               datamanR()$path,
               parseDirPath(roots = roots, selection = input$directory))
      })


      output$ui<- renderUI({
        ns <- session$ns
        fluidRow(
          column(width = 6,
                 textInput(ns("name"), label = "Name", value = datamanR()$name),
                 selectInput(ns("perm"), "Permission Level", choices = c("public","private"), selected = datamanR()$access)),
          column(width = 6,
                 shinyDirButton(ns("directory"), "New Directory", "Select New Directory"),
                 textOutput(ns("dirout")))
        )

      })


      output$dirout <- renderText(dir())

      previewDM <- reactive({
        browser()
        dm <- datamanR()
        dm <- dm$setManName(input$name)
        dm <- dm$setManPath(dir())
        dm <- dm$setPermission(input$perm)
        dm
      })


      output$preview <- renderPrint(previewDM())
    }
  )
}
