


#' @export
addDefUI <- function(id){
  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel("Upload Local",
               uploadfileUI(ns("table_upload"))),
      tabPanel("Read  Remote"),
      tabPanel("Define New")
    )
  )
}


#' @export
addDefServer <- function(id, roots = c(home = getwd())){
  moduleServer(
    id = id,
    function(input, output, session){

      upload_def <- uploadfileServer(id = "table_upload", roots = roots)

    }
  )
}

uploadfileUI <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload File"),
    shinyDirButton(ns("dir"), "Directory", "Select A Directory"),
    uiOutput(ns("ui1")),
    uiOutput(ns("ui2"))
  )
}

uploadfileServer <- function(id, roots = c(home = getwd())){
  moduleServer(
    id,
    function(input, output, session){

      userFile <- reactive({
        validate(need(input$file, message = FALSE))
        input$file
      })

      output$ui2 <- renderUI({
        ns <- session$ns
        req(userFile())
        if(str_detect(userFile()$datapath, "\\.xlsx?$")){
          tagList(
            checkboxInput(ns("heading_xls"), "Has heading", value = TRUE),
            selectInput(ns("sheet"),
                        label = "Select Sheet",
                        choices = readxl::excel_sheets(userFile()$datapath))
          )
        } else {
          tagList(
            checkboxInput(ns("heading"), "Has heading", value = TRUE),
            selectInput(ns("quote"), "Quote", c("None" = "", "Double quote" = "\"", "Single quote" = "'")),
            selectInput(ns("sep"), "Delimeter", c("csv"=",", "tab"="\t", "None"=""))
          )
        }
      })

      def <- reactive({
        input$sheet
        if(str_detect(userFile()$datapath, "\\.rds$")){
          upload <- readRDS(userFile()$datapath)
        } else if(str_detect(userFile()$datapath, "\\.xlsx?$")){
          upload <- readxl::read_excel(userFile()$datapath, sheet = input$sheet, col_names = input$heading_xls)
        } else{
          upload <- fread(file = userFile()$datapath, header = input$heading, quote = input$quote, sep = input$sep )
        }
        if(inherits(upload, "data.frame")){
          def_ <- TableDefinition$new(data = upload, userFile()$name)
        } else if(inherits(upload, "TableDefinition")){
          def_ <- upload
        }
        return(def_)
      })

      shinyDirChoose(input, "dir", roots = roots)

      output$ui1 <- renderUI({
        ns <- session$ns
        req(def())
        tagList(
          textInput(ns("name"),
                    "Name",
                    value = str_remove(userFile()$name,
                                       str_c("(_TableDef)?\\.",
                                             tools::file_ext(userFile()$name)))),
          selectInput(ns("keys"), "Keys", choices = def()$col_names, multiple = T)
        )
      })
      file <- reactive({
        str_c(input$dir,input$name, ".csv")
      })
      def1 <- reactive({
        d <- def1()
        d$keys <- input$keys
        d$name <- input$name
        d$path <- file() #should be a path to a file.
        return(d)
      })

      return(def1)

    }
  )
}



