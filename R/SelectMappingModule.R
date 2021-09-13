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
      reindex <- function() {
        browser()

      }

      # observeEvent(depend(), {
      #   browser()
      #   context_local(id)
      #   n <- length(selected_left())
      #   ids <- seq_len(n)
      #   set_mapping_index(ids)
      #   if(n==0) {
      #     mods(NULL)
      #   } else {
      #     mods(lapply(get_mapping_index(), map_pair_select_Server))
      #   }
      # }, priority = -5L)


      #Whenever the options change, the UI should be reindexed
      observeEvent({
        choices_left()
        choices_right()
      }, {
        browser()
        context_local(id)
        for(i in get_mapping_index()) {
          zap_ns <- glue("{ns(i)}")
          remove_shiny_inputs(c(glue("{zap_ns}-left"),glue("{zap_ns}-right"),glue("{zap_ns}-rm")), .input = input)
          removeUI(glue("#{zap_ns}-pair_select_container"))
          zap_mapping_index(i)
        }
        set_index()
        mods(NULL)
        depend(depend()+1L)
      }, priority = 5L)

      observeEvent({
        depend()
      }, {
        browser()
        context_local(id)
        validate(
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

      # output$mapping <- renderUI({
      #   browser()
      #
      #
      #   mods(lapply(get_mapping_index(), map_pair_select_Server))
      #
      #
      #   do.call('tagList', if(length(get_mapping_index())<=1) list(pair_list_ui) else pair_list_ui)
      # })

      observeEvent(input$add,
                   {
                     context_local(id)
                     new_id <- new_index()
                     add_mapping_index(new_id)
                     ui <- map_pair_select_UI(ns(new_id),
                                              choices_left = choices_left(),
                                              choices_right = choices_right(),
                                              selected_left = selected_left(),
                                              selected_right = selected_right())
                     insertUI(selector = glue("#{ns('add')}"),
                              where = 'beforeBegin',
                              ui = ui, immediate = T)
                     isolate(.mods <- mods())
                     mods(c(.mods, list(map_pair_select_Server(new_id))))
                   })

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

      })

      mod_ids <- reactive({
        vapply(mods(), function(x){x[['id']]}, double(1))
      })
      zapped <- reactive({
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
        if(n!=length(mods())|n!=1L) {
          left_choice[(n+1L):length(mods())] <- ""
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
        if(n!=length(mods())|n!=1L) {
          right_choice[(n+1L):length(mods())] <- ""
        }
        vals <- mapply(function(x, default){x[['right']]() %empty% default}, x = mods(), default = right_choice, SIMPLIFY = F)
        validate(
          need(all(!vapply(vals, is.null, logical(1))), "Some inputs on right haven't fully registered")
        )
        unlist(vals)
      })
      return(list(left = left, right = right))


    }
  )
}
