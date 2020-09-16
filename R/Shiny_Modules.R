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
        home <- normalizePath("~")
        file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
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
