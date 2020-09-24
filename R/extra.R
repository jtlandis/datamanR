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

#' @title Values Between operator
#' @name %betwen%
#' @description an infix implementation of the common
#' expression  x >= lower & x <= upper.
#' @param a Numeric Vector input
#' @param b Numeric Vector whose minimum is the left bound
#'  and maximum is the right bound.
#' @return vector of TRUE FALSE values
#' @export
"%betwn%" <- function(a, b){
  a >= min(b, na.rm = T) & a <= max(b, na.rm = T)
}

#' @title Values Match operator
#' @name %match%
#' @description an infix implementation of str_detect.
#' @param a Character Vector input
#' @param b A vector of strings to match against. b can be
#'  a regular expression. If b contains multiple elements
#'  then their values are collapsed into groups. e.i.
#'  c("hello","world") becomes c("(hello|world)")
#' @return vector of TRUE FALSE values
#' @export
"%match%" <- function(a, b){
  str_detect(a, str_group(b))
}

#' @title Values replace all operator
#' @name %replace%
#' @description an infix implementation of str_replace_all.
#' @param a Character Vector input
#' @param b A named vector of replacements, e.i. c("pattern"="replacement")
#' @return modified character vector
#' @export
"%replace%" <- function(a,b){
  str_replace_all(a,b)
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

#' @title Absolute File System Path
#' @name full_path
#' @description create the full path for a hypothetical directory/file
#' @export
full_path <- function(path) {
  path <- as.character(fs::path_abs(path))
  return(path)
}

#' @title Validation tests
#' @description set of functions that can help condense if statements
#'
#' @describeIn validate_ similar to \link[shiny]{validate} except
#'  this will throw an error message for each failed test.
#' @export
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


#' @describeIn validate_ similar to \link[shiny]{validate} except
#'  this will throw a warning message for each failed test.
#' @export
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



#' @describeIn validate_ similar to \link[shiny]{validate} except
#'  this will throw a message for each failed test.
#' @export
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
  message(paste(results, collapse = "\n"))
}



#' @describeIn validate_ similar to \link[shiny]{need} except
#'  this does not check with \link[shiny]{isTruthy}. This checks
#'  if the expression is TRUE or FALSE. If the expression is
#'  not TRUE, then the message is returned to validate_
#' @export
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


