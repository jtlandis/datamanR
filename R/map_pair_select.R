

map_pair_select_UI <- function(id, choices_left = NULL, choices_right = NULL,
                               selected_left = NULL, selected_right = NULL){
  ns <- NS(id)
  s1 <- selectInput(ns("left"), NULL, choices = choices_left, selected = selected_left)
  s1[[2]]$style <- "display: inline-block;vertical-align:middle;width:40%;"
  s1[[2]]$class <- NULL
  s2 <- selectInput(ns("right"), NULL, choices = choices_right, selected = selected_right)
  s2[[2]]$style <- "display: inline-block;vertical-align:middle;width:40%;"
  s2[[2]]$class <- NULL
  i <- icon('arrows-alt-h', style = "display:inline-block;width:10%;text-align:center;horizontal-align:center;")
  b <- circleButton(ns('rm'), NULL, icon = icon('times', style = "text-align:center"), class = "btn-danger",size = 'sm', style = "display:inline-block;horizontal-align:center;position:absolute;left:91%")

  div(s1,i, s2, b, id =ns("pair_select_container"), style = "padding:10px;width:100%;min-width:200px;position:relative")
}

map_pair_select_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      left_value <- reactive({input$left})
      right_value <- reactive({input$right})

      observeEvent(input$rm, {
        remove_shiny_inputs(c(ns("left"),ns("right")), .input = input)
        removeUI(glue("#{ns('pair_select_container')}"))
      }, priority = 4L, once = TRUE, ignoreInit = T)
      zap <- reactive(input$rm)
      return(list(left = left_value,
                  right = right_value,
                  zap = zap,
                  id = id))
    }
  )
}
