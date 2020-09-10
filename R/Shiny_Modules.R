#' @import shiny
#' @import shinyFiles

#' @export
createDataManRUI <- function(id, label = "Make Data Manager"){
  ns <- NS(id)

  tagList(
    textInput(ns("name"), "Data Manager Name:"),
    shinyDirButton(ns("directory"), "Choose Save Directory", title = "choose something"),
    actionButton(ns("createDataMan"), "Create!"),
    verbatimTextOutput(ns("display"))
  )
}

#' @export
createDataManRServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session){

      shinyDirChoose(input, "directory", roots = c("root"="~"), filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw","R"))

      rv <- reactiveValues(
        valid = FALSE,
        dataManR = NULL
      )
      path <- reactive({
        if(!"path"%in%names(input$directory)){
          return(NULL)
        } else {
          home <- normalizePath("~")
          text <- file.path(home, paste(unlist(input$directory$path[-1]), collapse = .Platform$file.sep))
        }
        text
      })
      dataManR <- reactive({
       isolate({
          name <- input$name
          direct <- path()
        })
        input$createDataMan
        datamanR::DataManR$new(name = name, path = direct)
        })

      valid <- reactive({
        req(input$createDataMan)
        dataManR()$isValid()
        })

      output$display <- renderPrint({
        if(dataManR()$isValid()){
          return(dataManR())
        } else {
          return(dataManR()$message)
        }
      })


      return(dataManR)

    }
  )
}


# server <- function(input, output, session){
#   dataMan <- createDataManRServer("datMan")
# }
#
# shinyApp(ui,server)
