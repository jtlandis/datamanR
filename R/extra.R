#' @import R6
#' @import glue
#' @import data.table
#' @importFrom rlang abort warn
#' @importFrom lubridate now
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

#' @export
"%betwn%" <- function(a, b){
  between(a, min(b), max(b))
}

#' @export
"%match%" <- function(a, b){
  str_detect(a, str_group(b))
}



"%||%" <- function(a,b){
  if(is.null(a)) b else a
}

# "%miss%" <- function(a,b) if(missing(a)) b else a
# "%!miss%" <- function(a,b) if(!missing(a)) a else b

require_scalar_char <- function(value, object){
  validate_error(
    need2(is.character(value), glue("{object} must be a character string.")),
    need2(length(value)==1, glue("{object} must be length 1."))
  )
}

require_char <- function(value, object){
  validate_error(
    need2(is.character(value), glue("{object} must be a character string."))
  )
}

#' @name full_path
#' @description create the full path for a hypothetical directory/file
full_path <- function(path) {
  slash <- if(.Platform$OS.type=="windows") "\\" else "/"
  dotdot <- stringr::str_detect(path, "\\.\\.(\\/?$|\\\\?$)|\\.")
  dir_ <- ifelse(dotdot, path, dirname(path))
  bas_ <- ifelse(dotdot, "", basename(path))
  return(paste(normalizePath(dir_),bas_, sep = ifelse(dotdot,"",slash)))
}

validate_error <- function (..., errorClass = character(0)) {
  results <- sapply(list(...), function(x) {
    if (is.null(x))
      return(NA_character_)
    else if (identical(x, FALSE))
      return("")
    else if (is.character(x))
      return(paste(as.character(x), collapse = "\n"))
    else stop("Unexpected validation result: ", as.character(x))
  })
  results <- stats::na.omit(results)
  if (length(results) == 0)
    return(invisible())
  results <- results[nzchar(results)]
  stop(paste(results, collapse = "\n"), call. = F)
}

validate_warn <- function (..., errorClass = character(0)) {
  results <- sapply(list(...), function(x) {
    if (is.null(x))
      return(NA_character_)
    else if (identical(x, FALSE))
      return("")
    else if (is.character(x))
      return(paste(as.character(x), collapse = "\n"))
    else stop("Unexpected validation result: ", as.character(x))
  })
  results <- stats::na.omit(results)
  if (length(results) == 0)
    return(invisible())
  results <- results[nzchar(results)]
  warning(paste(results, collapse = "\n"), call. = F)
}

validate_message <- function (..., errorClass = character(0)) {
  results <- sapply(list(...), function(x) {
    if (is.null(x))
      return(NA_character_)
    else if (identical(x, FALSE))
      return("")
    else if (is.character(x))
      return(paste(as.character(x), collapse = "\n"))
    else stop("Unexpected validation result: ", as.character(x))
  })
  results <- stats::na.omit(results)
  if (length(results) == 0)
    return(invisible())
  results <- results[nzchar(results)]
  warning(paste(results, collapse = "\n"), call. = F)
}

need2 <- function (expr, message) {
  force(message)
  if (!check_expr(expr))
    return(message)
  else return(invisible(NULL))
}

check_expr <- function(expr){
  if(length(expr)==0) return(FALSE)
  return(expr)
}

wrap_str <- function(str){
  special_chr <- c(" ","~","!","@","#","$","%",
                   "^","&","*","(",")","-","+",
                   "=","{","}","[","]","|","?",
                   "<",">",",","/",":",";","`")
  litteral_pattern <- str_c("^\\.?[0-9]\\|", str_c("\\",special_chr, collapse = "|"), collapse = "")
  str2 <- ifelse(str_detect(str,
                           pattern = litteral_pattern),
                str_c("`",str,"`"),
                str)
  return(str2)
}


parse_fun_list <- function(list) {
  if(is.null(list)) return("NULL")
  text <- lapply(list, function(x){
    val <- str_extract(deparse(x), "(?<=(Primitive|UseMethod)\\(\")[:alnum:]+(?=\")")
    val <- val[!is.na(val)]
    return(val)
  })
  str_c("list(",str_c(names(text), "=",text, collapse = ", "),")", collapse = "")
}

melt_meta <- function(measure.vars, variable.name, value.name){

  names(measure.vars) <- value.name
  data <- unlist(measure.vars)
  data <- data.frame(measure = data,
                     value = str_extract(names(data), pattern = str_group(value.name)),
                     variable = str_remove(names(data), pattern = str_group(value.name))) %>%
    spread(key = value, value = measure)
  colnames(data)[colnames(data)%in%"variable"] <- variable.name
  return(as.data.table(data))
}

str_group <- function(str){
  str_c("(", str_c(str, collapse = "|"),")")
}

print.dt_history <- function(x){
  cat(x, sep = "\n")
}


