#' @import proto
#' @import glue
#' @import data.table
#' @importFrom rlang abort warn
#' @import stringr
#' @import shiny

"%null%" <- function(a,b){
  if(is.null(a)) a else b
}

"%na%" <- function(a,b){
  if(is.na(a)) a else b
}
colclasses <- function(data){
  data <- data %null% unlist(lapply(data, class))
  return(data)
}

"%||%" <- function(a,b){
  if(is.null(a)) b else a
}

# "%miss%" <- function(a,b) if(missing(a)) b else a
# "%!miss%" <- function(a,b) if(!missing(a)) a else b

require_scalar_char <- function(value, object){
  validate(
    need(is.character(value), glue("{object} must be a character string.")),
    need(length(value)==1, glue("{object} must be length 1."))
  )
}

require_char <- function(value, object){
  validate(
    need(is.character(value), glue("{object} must be a character string."))
  )
}

#' @name full_path
#' @description create the full path for a hypothetical directory/file
full_path <- function(path) {
  dotdot <- stringr::str_detect(path, "\\.\\.\\/?$")
  dir_ <- ifelse(dotdot, path, dirname(path))
  bas_ <- ifelse(dotdot, "", basename(path))
  return(paste(normalizePath(dir_),bas_, sep = ifelse(dotdot,"","/")))
}

#' @export
print.def <- function(x){
  .df <- data.frame(col_names = x$col_names,
                    col_types = x$col_types,
                    row.names = NULL)
  .df$keys <- ifelse(.df$col_names%in%x$keys, T, F)
  cat("name: ",x$name,
      "\npath: ", x$file,"\n")
  data <- x$data
  if(!is.null(data)) {
    cat("nrow: ", nrow(data),"\n")
  }
  print(.df)
}

#' @export
print.DataManR <- function(x){
  cat("name: ", x$name,
      "\npath: ", x$path,"\n")
  x$Tables
}
