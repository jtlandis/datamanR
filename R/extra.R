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
  if(is.null(a)) a else b
}
column_classes <- function(data){
  data <- data %null% unlist(lapply(data, class))
  return(data)
}

#' @name full_path
#' @description create the full path for a hypothetical directory/file
full_path <- function(path) {
  dir_ <- dirname(path)
  bas_ <- basename(path)
  return(paste(normalizePath(dir_),bas_, sep = "/"))
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
