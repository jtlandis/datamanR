selectMappingUI <- function(id){
  ns <- NS(id)
  style_head <- tags$head(
    tags$style(HTML("
      .selectize-control {
        margin-bottom: 0px;
      }
    "))
  )
  ui <- uiOutput(ns('mapping'))
  add_button <- actionButton(ns('add'), icon = icon('add', 'primary'), label = NULL)
  do.call('tagList', c(list(style_head),
                       list(useShinyjs()),
                       list(ui),
                       list(add_button)))
}

selectMappingServer <- function(id, choices_left = NULL, choices_right = NULL,
                                selected_left = NULL, selected_right = NULL){
  moduleServer(
    id,
    function(input, output, session){
      ns <- session$ns

      mods <- reactiveVal()
      counter <- 0L
      depend <- reactiveVal(0L)
      new_index <- function(){
        counter <<- counter + 1L
        counter
      }
      set_index <- function(x = 0L){
        counter <<- x
        invisible(counter)
      }
      removing <- 0L


      #Whenever the options change, the UI should be reindexed
      observeEvent({
        choices_left()
        choices_right()
      }, {
        removing <<- 0L
        context_local(id)
        for(i in get_mapping_index()) {
          zap_mapping_index(i)
          click(glue("{i}-rm"))
          removing <<- removing + 1L
        }
        set_index()
      }, priority = 5L)


      #if there are any default values
      #insert them when appropriate after
      #the UI has been cleared
      observeEvent({
        depend()
      }, {
        #browser()
        context_local(id)
        validate(
          need(is_empty(mods()), "No Modules"),
          need(!is.null(choices_left())&!is.null(choices_right()), "No options assigned to either right or left."),
          need(all(selected_left() %in% choices_left()), "One or more elements assigned on the left side is not apart of available options."),
          need(all(selected_right() %in% choices_right()), "One or more elements assigned on the right side is not apart of available options."),
          need(length(selected_left()) == length(selected_right()), "Both sides need equal mapping lengths")
        )
        use_NULL <- length(selected_left())==0
        set_index(length(selected_left()))
        set_mapping_index(seq_len(counter))
        pair_list_ui <- mapply(map_pair_select_UI,
                               id = if(use_NULL) NULL else ns(get_mapping_index()),
                               selected_left = selected_left(),
                               selected_right = selected_right(),
                               MoreArgs = list(
                                 choices_left = choices_left(),
                                 choices_right = choices_right()
                               ), SIMPLIFY = F)
        for(ui in pair_list_ui) {
          insertUI(selector = glue("#{ns('add')}"),
                   where = 'beforeBegin',
                   ui = ui)
        }
        mods(lapply(get_mapping_index(), map_pair_select_Server))
      },
      priority = 2L)

      # Interactively add new pair maps
      observeEvent(input$add,
                   {
                     context_local(id)
                     new_id <- new_index()
                     add_mapping_index(new_id)
                     ui <- map_pair_select_UI(ns(new_id),
                                              choices_left = choices_left(),
                                              choices_right = choices_right(),
                                              selected_left = choices_left()[1],
                                              selected_right = choices_right()[1])
                     insertUI(selector = glue("#{ns('add')}"),
                              where = 'beforeBegin',
                              ui = ui, immediate = T)
                     #isolate(.mods <- mods())
                     mods(c(mods(), list(map_pair_select_Server(new_id))))
                   })

      #Whenever something is zapped - finally remove the module's {index}-rm input
      observeEvent({
        zapped()
      }, {
        context_local(id)
        validate(
          need(any(zapped()), "No change needed")
        )
        zap_indx <- mod_ids()[which(zapped())]
        zap_mapping_index(zap_indx)
        remove_shiny_inputs(glue("{ns(zap_indx)}-rm"), input)
        isolate(.mods <- mods())
        mods(.mods[!zapped()])
        removing <<- removing - length(zap_indx)
        if(removing==0L&length(isolate(mods()))==0L) {
          onFlushed(function(){
            observeEvent(NULL, {
              depend(depend() + 1L)
            }, once = TRUE, ignoreNULL = FALSE, ignoreInit = FALSE)
          })
        }

      }, priority = 3L)

      mod_ids <- reactive({
        vapply(mods(), function(x){x[['id']]}, double(1))
      })
      zapped <- reactive({
        validate(need(!is_empty(mods()), "No Modules"))
        vals <- lapply(mods(), function(x){x[['zap']]()})
        validate(
          need(all(!vapply(vals, is.null, logical(1))), "Some inputs haven't fully registered"),
          need(!is_empty(vals), "Nothing here")
        )
        unlist(vals)!=0L
      })
      left <- reactive({
        isolate(left_choice <- selected_left() %||% choices_left()[1])
        n <- length(left_choice)
        if(length(mods())==0){
          left_choice <- NULL
        } else if (n!=length(mods())|n!=1L) {
          left_choice[(n+1L):length(mods())] <- isolate(choices_left()[1])
        }
        vals <- mapply(function(x, default){x[['left']]() %empty% default}, x = mods(), default = left_choice, SIMPLIFY = F)
        validate(
          need(all(!vapply(vals, is.null, logical(1))), "Some inputs on left haven't fully registered")
        )
        unlist(vals)
      })
      right <- reactive({
        isolate(right_choice <- selected_right() %||% choices_right()[1])
        n <- length(right_choice)
        if(length(mods())==0){
          right_choice <- NULL
        } else if (n!=length(mods())|n!=1L) {
          right_choice[(n+1L):length(mods())] <- isolate(choices_right()[1])
        }
        vals <- mapply(function(x, default){x[['right']]() %empty% default}, x = mods(), default = right_choice, SIMPLIFY = F)
        validate(
          need(!is.null(vals), "No inputs available"),
          need(all(!vapply(vals, is.null, logical(1))), "Some inputs on right haven't fully registered")
        )
        unlist(vals)
      })
      return(list(left = left, right = right))


    }
  )
}
