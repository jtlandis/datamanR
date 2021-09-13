library(glue)
library(rlang)
`%empty%` <- function(a,b) if(is_empty(a)) b else a
context_env <- new_environment()

#put a new set of values
context_poke <- function (id) {
  old <- context_env[['id']]
  context_env[['id']] <- id
  old
}

context_peek <- function (fun)
{
  context_env[['id']] %||% abort(glue("attempted to peek a context that is NULL in `{fun}` - did you forget to use `context_local()`"))
}

context_local <- function (id, frame = caller_env())
{
  old <- context_poke(id)
  expr <- expr(on.exit(context_poke(!!old), add = TRUE))
  eval_bare(expr, frame)
}

map_env <- new_environment()

set_mapping_index <- function(index, where = NULL){
  map_env[[where %||% context_peek('set_mapping_index')]] <- index
}

get_mapping_index <- function(where = NULL) {
  map_env[[where %||% context_peek('get_mapping_index')]]
}

zap_mapping_index <- function(index, where = NULL) {
  id <- where %||% context_peek('zap_mapping_index')
  map_env[[id]] <- setdiff(map_env[[id]], index)
}

add_mapping_index <- function(index, where = NULL) {
  id <- where %||% context_peek('add_mapping_index')
  map_env[[id]] <- union(map_env[[id]], index)
}

# --- How to use
# use context_local to set the id context within a function
# use context_peek to get the current id of a function

# use (set/get/zap/add)_mapping_index functions which will access
# the current context within `map_env` environment


remove_shiny_inputs <- function(id, .input) {
  impl <- .subset2(.input, "impl")
  lgl <- id %in% impl$.values$keys()
  if(any(!lgl)) warn(glue("The following `id`s were not found in shiny server input and cannot be removed : ", glue_collapse(id[!lgl]), sep = ", ", last = ", and "))
  to_rm <- id[lgl]
  invisible(
    lapply(to_rm, function(i) {
      impl$.values$remove(i)
    })
  )
}





.rm_indx_ns <- function(x) {sub('-[0-9]+-$', "",x)}
