


#' @export
addDefUI <- function(id){
  ns <- NS(id)

  tagList(
    tabsetPanel(
      tabPanel("Upload Local",
               uploadfileUI(ns("table_upload"))),
      tabPanel("Read  Remote",
               readremoteUI(ns("table_remote"))
               ),
      tabPanel("Define New"#,
               # definetablUI(ns("table_define"))
               )
    ),
    hr(),
    h3("Table Definition preview:"),
    verbatimTextOutput(ns("preview"))
  )
}


#' @export
addDefServer <- function(id, roots = c(home = getwd())){
  moduleServer(
    id = id,
    function(input, output, session){
      deftab <- reactiveValues(
        data = NA_character_,
        name = NA_character_,
        path = NA_character_,
        keys = NA_character_
      )
      upload_def <- uploadfileServer(id = "table_upload", roots = roots)
      remote_def <- readremoteServer(id = "table_remote", roots = roots)
      #define_def <- definetablServer(id = "table_define")

      observe({
        upload_def$set()
        deftab$data <- upload_def$data()
        deftab$name <- upload_def$name()
        deftab$path <- upload_def$path()
        deftab$keys <- upload_def$keys()
      })

      output$preview <- renderPrint({
        cat("DataTableDefinitions:\n",
            "  Name  : ", deftab$name,"\n",
            "  Path  : ", deftab$path,"\n",
            "  md5sum: ", deftab$md5sum,"\n",
            "  keys  : ", str_c(deftab$keys, collapse =", "),"\n", sep = "")
      })


      return(list(data = reactive(deftab$data),
                  name = reactive(deftab$name),
                  path = reactive(deftab$path),
                  keys = reactive(deftab$keys)))

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
    fluidRow(DT::dataTableOutput(ns("preview_table")),
             style = "margin-left:10px;margin-right:10px;max-height:450px;overflow-y:scroll;"),
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
                      htmlOutput(ns("dirout")))),
             fluidRow(
               column(12,
                      actionButton(ns("set"), label = NULL, icon = icon("sync")), align = "center")
             )
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
                     selectInput(ns("quote"), "Quote", c("None" = "", "Double quote" = "\"", "Single quote" = "'"), selected = c("None"=""))),
              column(4,
                     selectInput(ns("sep"), "Delimeter", c("None"="", "csv"=",", "tab"="\t", "space"=" "), selected = c("None"="")))
            )
          )
        }
      })

      nam <- reactive({
        validate(
          need(input$name, "Table Must be named!")
        )
        input$name
      })

      datatab <- reactive({
        req(userFile())
        if(str_detect(userFile()$datapath, "\\.rds$")){
          upload <- readRDS(userFile()$datapath)
        } else if(str_detect(userFile()$datapath, "\\.xlsx?$")){
          req(input$sheet)
          upload <- readxl::read_excel(userFile()$datapath, sheet = input$sheet, col_names = input$heading_xls)
        } else{
          validate(
            need(is.logical(input$heading), FALSE)
          )
          upload <- fread(file = userFile()$datapath, header = input$heading, quote = input$quote, sep = input$sep )
        }
        if(inherits(upload, "data.frame")){
          def_ <- upload
        } else if(inherits(upload, "TableDefinition")){
          def_ <- copy(upload$data)
        }
        return(def_)
      })


      output$preview_table <- DT::renderDT({
        req(datatab())
        ellipse_tar <- unname(which(colclasses(datatab())=="character"))
        DT::datatable(datatab(), class = c("compact stripe cell-border nowrap hover"), filter = 'top',
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
        tagList(
          textInput(ns("name"),
                    "Name",
                    value = str_remove(userFile()$name,
                                       str_c("(_TableDef)?\\.",
                                             tools::file_ext(userFile()$name)))),
          selectInput(ns("keys"), "Keys", choices = colnames(datatab()), multiple = T)
        )
      })

      key <- reactive({
        req(datatab())
        input$keys
      })

      file <- reactive({
        str_c(dir(),nam(), ".csv")
      })



      return(list(data = datatab,
                  name = nam,
                  path = file,
                  keys = key,
                  set = reactive(input$set)))

    }
  )
}




readremoteUI <- function(id){
  ns <- NS(id)

  tagList(
    h2("Read Remote File:"),
    fluidRow(
      column(12,
             shinyFilesButton(ns("file"),
                            "Remote Table", "Select A File", F
             ), align = "center")),

    uiOutput(ns("moreUIOptions")),
    hr(),
    h4("Data Table Preview"),
    fluidRow(DT::dataTableOutput(ns("preview_table")),
             style = "margin-left:10px;margin-right:10px;max-height:450px;overflow-y:scroll;"),
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
                      htmlOutput(ns("dirout")))),
             fluidRow(
               column(12,
                      actionButton(ns("set"), label = NULL, icon = icon("sync")), align = "center")
             )
      )
    )
  )
}



readremoteServer <- function(id, roots = c(home = getwd())){
  moduleServer(
    id,
    function(input, output, session){

      userFile <- reactive({
        validate(need(input$file, message = FALSE))
        parseFilePaths(roots = roots,input$file)
      })

      shinyFileChoose(input, "file", roots = roots)
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
        if(str_detect(userFile()$datapath, "\\.rds$")){
          tagList(p("rds file."))
        } else if(str_detect(userFile()$datapath, "\\.xlsx?$")){
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
                     selectInput(ns("quote"), "Quote", c("None" = "", "Double quote" = "\"", "Single quote" = "'"), selected = c("None"=""))),
              column(4,
                     selectInput(ns("sep"), "Delimeter", c("None"="", "csv"=",", "tab"="\t", "space"=" "), selected = c("None"="")))
            )
          )
        }
      })

      nam <- reactive({
        validate(
          need(input$name, "Table Must be named!")
        )
        input$name
      })

      datatab <- reactive({
        req(userFile())
        if(str_detect(userFile()$datapath, "\\.rds$")){
          upload <- readRDS(userFile()$datapath)
        } else if(str_detect(userFile()$datapath, "\\.xlsx?$")){
          req(input$sheet)
          upload <- readxl::read_excel(userFile()$datapath, sheet = input$sheet, col_names = input$heading_xls)
        } else{
          validate(
            need(is.logical(input$heading), FALSE)
          )
          upload <- fread(file = userFile()$datapath, header = input$heading, quote = input$quote, sep = input$sep )
        }
        if(inherits(upload, "data.frame")){
          def_ <- upload
        } else if(inherits(upload, "TableDefinition")){
          def_ <- copy(upload$data)
        }
        return(def_)
      })


      output$preview_table <- DT::renderDT({
        req(datatab())
        ellipse_tar <- unname(which(colclasses(datatab())=="character"))
        DT::datatable(datatab(), class = c("compact stripe cell-border nowrap hover"), filter = 'top',
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
        tagList(
          textInput(ns("name"),
                    "Name",
                    value = str_remove(userFile()$name,
                                       str_c("(_TableDef)?\\.",
                                             tools::file_ext(userFile()$name)))),
          selectInput(ns("keys"), "Keys", choices = colnames(datatab()), multiple = T)
        )
      })

      key <- reactive({
        req(datatab())
        input$keys
      })

      file <- reactive({
        str_c(dir(),nam(), ".csv")
      })



      return(list(data = datatab,
                  name = nam,
                  path = file,
                  keys = key,
                  set = reactive(input$set)))

    }
  )
}



definetablUI <- function(id){
  ns <- NS(id)
  verbatimTextOutput(ns("tbd"), "TBD")
}

definetablServer <- function(id){
  moduleServer(
    id,
    function(input, output, session){

    }
  )
}


