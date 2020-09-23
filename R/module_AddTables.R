

#' @title addDefUI
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
      tabPanel("Define New",
               definetablUI(ns("table_define"))
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
      define_def <- definetablServer(id = "table_define", roots = roots)

      observe({
        upload_def$set()
        deftab$data <- upload_def$data()
        deftab$name <- upload_def$name()
        deftab$keys <- upload_def$keys()
        deftab$path <- upload_def$path()
      })

      observe({
        remote_def$set()
        deftab$data <- remote_def$data()
        deftab$name <- remote_def$name()
        deftab$keys <- remote_def$keys()
        deftab$path <- remote_def$path()
      })

      output$preview <- renderPrint({
        cat("DataTableInformation:\n",
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

#' @export
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

#' @export
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
                     checkboxInput(ns("manual"), "Specify File Parameters?", value = FALSE),
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
          validate(need(is.logical(input$manual), FALSE))
          if(input$manual){
            validate(need(is.logical(input$heading), FALSE))
            upload <- tryCatch(fread(file = userFile()$datapath, header = input$heading, quote = input$quote, sep = input$sep ), error = function(e){validate(e$message)})
          } else {
            upload <- tryCatch(fread(file = userFile()$datapath), error = function(e){validate(e$message)})
          }

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
        str_c(dir(),"/", nam(), ".csv")
      })



      return(list(data = datatab,
                  name = nam,
                  path = file,
                  keys = key,
                  set = reactive(input$set)))

    }
  )
}



#' @export
readremoteUI <- function(id){
  ns <- NS(id)

  tagList(
    h2("Read Remote File:"),
    fluidRow(
      column(12,
             shinyFilesButton(ns("file"),
                            "Remote Table", "Select A File", multiple =  F
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


#' @export
readremoteServer <- function(id, roots = c(home = getwd())){
  moduleServer(
    id,
    function(input, output, session){

      userFile <- reactive({
        req(input$file)
        validate(need(!is.null(input$file), FALSE),
                 need(length(input$file)>0, FALSE),
                 need(input$file, message = FALSE))
        print(input$file)
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
                     checkboxInput(ns("manual"), "Specify File Parameters?", value = FALSE),
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
        validate(need("datapath"%in%names(userFile()), "File Is not Loaded!"))
        validate(need(length(userFile()$datapath)>0, "File Is not Loaded!"))
        validate(need(fs::file_exists(userFile()$datapath), "File is not Loaded"))
        if(str_detect(userFile()$datapath, "\\.rds$")){
          upload <- readRDS(userFile()$datapath)
        } else if(str_detect(userFile()$datapath, "\\.xlsx?$")){
          req(input$sheet)
          upload <- readxl::read_excel(userFile()$datapath, sheet = input$sheet, col_names = input$heading_xls)
        } else{
          validate(need(is.logical(input$manual), FALSE))
          if(input$manual){
            validate(need(is.logical(input$heading), FALSE))
            upload <- tryCatch(fread(file = userFile()$datapath, header = input$heading, quote = input$quote, sep = input$sep ), error = function(e){validate(e$message)})
          } else {
            upload <- tryCatch(fread(file = userFile()$datapath), error = function(e){validate(e$message)})
          }
        }
        if(inherits(upload, "data.frame")){
          def_ <- upload
        } else if(inherits(upload, "TableDefinition")){
          def_ <- copy(upload$data)
        } else {
          validate(need("Please Select a _TableDef.rds file or a regular data file (.csv, .tsv, etc)"))
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
        str_c(dir(),"/", nam(), ".csv")
      })



      return(list(data = datatab,
                  name = nam,
                  path = file,
                  keys = key,
                  set = reactive(input$set)))

    }
  )
}


#' @export
definetablUI <- function(id){
  ns <- NS(id)
  tagList(
    h2("Define Table Information:"),
    fluidRow(
      column(3,
             textInput(ns("name"), "Table Name:")),
      column(3,
             textInput(ns("file_name"), "File Name:")),
      column(2,
             div(shinyDirButton(ns("directory"), "location", "Choose A Directory"), align = "center")),
      column(4,
             htmlOutput(ns("dirout")))
    ),
    fluidRow(
      column(12,
             uiOutput(ns("making")))
    ),
    fluidRow(
      column(12,
             tags$div(id="ele_indx_0")
             )
    )
  )
}

#' @export
definetablServer <- function(id, roots){
  moduleServer(
    id,
    function(input, output, session){

      count <- reactiveVal(0, "Count")
      nele <- reactiveVal(0, "N_ele")
      where <- reactiveVal("afterEnd", "insert_where")
      selec <- reactiveVal("#ele_indx_0")

      shinyDirChoose(input, id = "directory", roots = roots)
      nam <- reactive({
        validate(need(input$name, "Please provide a name for the Table."))
        input$name
      })

      file <- reactive({
        validate(need(input$file_name, "Please provide a descriptive name for disk."))
        input$file_name
      })

      dir <- reactive({
        validate(
          need(input$directory, "Please select a directory to which files will be saved."),
          need(!is.null(input$directory), "directory button is null. Try again")
        )
        parseDirPath(roots = roots, input$directory)
      })

      output$dirout <- renderPrint(str_c(p(strong(dir()), style = "word-wrap: break-word;")))



      output$making <- renderUI({
        ns <- session$ns
        if(nele()<=0){
          tagList(
            div(actionButton(ns("add"), "Start"), align = "center")
          )
        } else {
          tagList(h3(str_c("There are ", nele(), " potential columns.")))
        }
      })

      observeEvent(input$add, {
        req(input$add)
        count(count()+1)
        nele(nele()+1)
        })



      observeEvent(count(), {
        req(count()>0)
        ns <- session$ns
        select <- ifelse(nele()==1, "#ele_indx_0", selec())
        id_add <- str_c("indx_", count())
        remove_id <- str_c("remove_", id_add)
        ele_id <- str_c("ele_", id_add)
        insertUI(
          selector = select,
          where = where(),
          ui= tags$div(
            id = ele_id,

            hr(),
              fluidRow(

                column(5,
                       textInput(ns(str_c(id_add, ".colname")), label = NULL, placeholder = "New Column Name"), sytle = "max-height:25px;"),
                column(3,
                       selectInput(ns(str_c(id_add, ".coltype")),
                                   label = NULL,
                                   choices = c("character","numeric","integer","factor","logical"), selected = "character"), sytle = "max-height:25px;"),
                column(2,
                       checkboxInput(ns(str_c(id_add, ".key")), "Is key?", FALSE), sytle = "max-height:25px;"),
                column(2,
                       actionButton(ns(str_c(id_add, ".add_up")), NULL, icon = icon("arrow-up"), style = "background-color:#72B4D6;"),
                       actionButton(ns(str_c(remove_id)), NULL, icon = icon("minus-square"), style = "background-color:#FF9270;"),
                       actionButton(ns(str_c(id_add, ".add_bot")), NULL, icon = icon("arrow-down"), style = "background-color:#72B4D6;"), sytle = "max-height:25px;")
              ), hr()
          ))

        observeEvent(input[[remove_id]],{
          isolate(nele(nele()-1))
          removeUI(
            selector = str_c("#", ele_id)
          )
        })

        observeEvent(input[[str_c(id_add,".add_up")]],{
          isolate(where("beforeBegin"))
          isolate(selec(str_c("#",ele_id)))
          isolate(nele(nele()+1))
          count(count()+1)
        })

        observeEvent(input[[str_c(id_add,".add_bot")]],{
          isolate(where("afterEnd"))
          isolate(selec(str_c("#",ele_id)))
          isolate(nele(nele()+1))
          count(count()+1)
        })

      })




    }
  )
}


# define_ui <- function(id){
#   ns <- NS(id)
#   tags$div(id = environment(ns)[['namespace']],
#           tagList(
#             fluidRow(
#                      column(5,
#                             textInput(ns("colname"), label = NULL, placeholder = "New Column Name")),
#                      column(3,
#                             selectInput(ns("coltype"),
#                                         label = NULL,
#                                         choices = c("character","numeric","integer","factor","logical"), selected = "character")),
#                      column(2,
#                             checkboxInput(ns("key"), "Is key?", FALSE)),
#                      column(2,
#                             fluidRow(actionButton(ns("add_up"), NULL, icon = icon("dashboard"), style = "height:15px")),
#                             fluidRow(actionButton(ns("delete_this"), NULL, icon = icon("minus-squar"), style = "height:15px")),
#                             fluidRow(actionButton(ns("add_bot"), NULL, icon = icon("arrow-from-top"), style = "height:15px")))
#                      )
#           )
#   )
# }
#
# define_server <- function(id){
#
# }
#
# t <- fluidPage(shiny::wellPanel(define_ui("test")))
#
# s <- function(input, output, session) {
#
# }
#
# shinyApp(t,s)
