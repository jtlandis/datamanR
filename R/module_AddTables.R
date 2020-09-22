


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
    h2("Upload Options:"),
    br(),
    fileInput(ns("file"), "Upload File"),
    uiOutput(ns("moreUIOptions")),
    hr(),
    h4("Data Table Preview"),
    fluidRow(DT::dataTableOutput(ns("preview_table")), style = "margin-left:10px;margin-right:10px;max-height:450px;overflow-y:scroll;")
    ,
    hr(),
    h4("Data Table Definitions:"),
    fluidRow(
      column(6,
             uiOutput(ns("ui1"))
             ),
      column(6,
             fluidRow(
               column(12,
                      shinyDirButton(ns("directory"),
                                     "Table Directory", "Select A Directory"
                      ), align = "center")),
             fluidRow(
               column(12,
                      htmlOutput(ns("dirout"))))
             )
    )


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

      shinyDirChoose(input, "directory", roots = roots)
      dir <- reactive({
        validate(
          need(input$directory, "Please select a Directory for the Table to be saved.")
        )
        parseDirPath(roots = roots, selection = input$directory)
      })
      output$dirout <- renderText(str_c(p(strong(dir()), style = "word-wrap: break-word;")))
      output$moreUIOptions <- renderUI({
        ns <- session$ns
        req(userFile())
        if(str_detect(userFile()$datapath, "\\.xlsx?$")){
          tagList(
            fluidRow(
              column(6,
                     checkboxInput(ns("heading_xls"), "Has heading", value = TRUE)),
              column(6,
                     selectInput(ns("sheet"),
                                 label = "Select Sheet",
                                 choices = readxl::excel_sheets(userFile()$datapath)))
            )
          )
        } else {
          tagList(
            fluidRow(
              column(4,
                     checkboxInput(ns("heading"), "Has heading", value = TRUE)),
              column(4,
                     selectInput(ns("quote"), "Quote", c("None" = "", "Double quote" = "\"", "Single quote" = "'"))),
              column(4,
                     selectInput(ns("sep"), "Delimeter", c("csv"=",", "tab"="\t", "None"="")))
            )
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


      output$preview_table <- DT::renderDT({
        def()
        ellipse_tar <- unname(which(def()$col_types=="character"))
        DT::datatable(def()$data, class = c("compact stripe cell-border nowrap hover"), filter = 'top',
                      extensions = list('Buttons' = NULL,
                                        'FixedColumns' = NULL),
                      options = list(scrollX = TRUE,
                                     dom = 'Bfrltip',
                                     lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')),
                                     fixedColumns = TRUE,
                                     buttons = list(I('colvis'),
                                                    list(extend = c('collection'),
                                                         buttons = list(list(extend = 'csv',
                                                                             filename = paste0(Sys.Date(),"_Submitted_Export")),
                                                                        list(extend = 'excel',
                                                                             filename = paste0(Sys.Date(),"_Submitted_Export")),
                                                                        list(extend = 'pdf',
                                                                             filename = paste0(Sys.Date(),"_Submitted_Export"))),
                                                         text = 'Download')
                                     ),
                                     columnDefs = list(
                                       list(
                                         targets = ellipse_tar,
                                         render = JS(
                                           "function(data, type, row, meta) {",
                                           "return type === 'display' && data.length > 10 ?",
                                           "'<span title=\"' + data + '\">' + data.substr(0, 8) + '...</span>' : data;",
                                           "}")
                                       )
                                     )
                      ))
      })

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



