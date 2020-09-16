#' @import shiny
#' @import shinyFiles


#' @export
createDataManRUI <- function(id){
  ns <- NS(id)

  tagList(
    textInput(ns("name"), "Data Manager Name:"),
    shinyDirButton(ns("directory"), "Choose Save Directory", title = "choose something"),
    actionButton(ns("createDataMan"), "Create!"),
    textOutput(ns("dir")),
    verbatimTextOutput(ns("info"))
  )
}

#' @export
createDataManRServer <- function(id, roots, filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw","R")) {
  moduleServer(
    id,
    function(input, output, session){

      shinyDirChoose(input, "directory", roots = roots, filetypes = filetypes)
      path <- reactive({
        if(!"path"%in%names(input$directory)){
          return()
        } else {
          home <- normalizePath("~")
          text <- file.path(home, paste(unlist(input$directory$path[-1]), collapse = .Platform$file.sep))
        }
        text
      })
      dataMan <- reactive({
        isolate({
          name <- input$name
          direct <- path()
        })
        input$createDataMan
        datamanR::DataManR$new(name = name, path = direct)
      })

      output$dir <- renderText({
        path()
      })

      output$info <- renderPrint({
        Sys.info()
      })

      return(dataMan)

    }
  )
}


server <- function(input, output, session){
  dataMan <- createDataManRServer("datMan", roots = c("Data Managers" = "/home/datamanr"))
}

ui <- fluidPage(
  createDataManRUI("datMan")
)

shinyApp(ui,server)
