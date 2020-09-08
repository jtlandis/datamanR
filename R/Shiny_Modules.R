#' @import shiny
#' @import shinyFiles


createDataManRUI <- function(id, label = "Make Data Manager"){
  ns <- NS(id)

  tagList(
    textInput(ns("name"), "Data Manager Name:"),
    shinyDirButton(ns("directory"), "Choose Save Directory", title = "choose something"),
    actionButton(ns("createDataMan"), "Create!"),
    textOutput(ns("dir"))
  )
}

createDataManRServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session){

      shinyDirChoose(input, "directory", roots = c("root"="~"), filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw","R"))

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


      return(dataMan)

    }
  )
}


server <- function(input, output, session){
  dataMan <- createDataManRServer("datMan")
}

shinyApp(ui,server)
