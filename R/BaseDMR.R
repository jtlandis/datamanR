#'
#'
#' @title R6 Class used as the base class for the datamanR package.
#'
#' @description
#'  This class will contain fields and methods to be inherited
#'  to the classes 'TInfo' and 'DataManR'. Class
#'  'BaseDMR' is not intended to be instantiated directly.
#'
BaseDMR <- R6::R6Class(classname = "BaseDMR",
                       active = list(
                         #' @field name Scalar character
                         #' that specifies the name assigned to class instance.
                         name = function(value) {
                           if(missing(value)){
                             private$.name
                           } else{
                             require_scalar_char(value, "`$name`")
                             if(!identical(private$.name, value)){
                               private$.name <- value
                               self$push_history(value, field = "`$name`")
                             }
                             self
                           }
                         },
                         #' @field path Scalar character that specifies
                         #' the path this object is to be saved.
                         path = function(value) {
                           if(missing(value)){
                             private$.path
                           } else {
                             require_scalar_char(value, "`$path`")
                             value <- value %na% full_path(value)
                             if(!identical(private$.path, value)){
                               private$.path <- value
                               self$push_history(value, field = "`$path`")
                             }
                             self
                           }
                         }
                       ),
                       private = list(
                         .name = NA_character_,
                         .path = NA_character_,
                         .history = NULL,
                         .time = NULL,
                         clear_history = function(){
                           private$.history <- NULL
                           private$.time <- NULL
                           invisible(self)
                         }
                       ),
                       public = list(
                         #' @description
                         #' Create a BaseDMR object
                         #' @param name Assign the name field
                         #' @param path Assign the path field
                         initialize = function(name = NA_character_, path = NA_character_){
                           self$name <- name
                           self$path <- path
                         },
                         #' @description
                         #'  Call custom validate method. This method
                         #'  should always return TRUE unless an
                         #'  error is encountered.
                         #'  \itemize{
                         #'     \item{\code{\link{validate_error}}}
                         #'     \item{\code{\link{validate_warn}}}
                         #'     \item{\code{\link{validate_message}}}
                         #'  }
                         validate = function(){
                           validate_warn(
                             need2(file.exists(self$path), glue("file: {self$path} does not exist yet!"))
                           )
                           validate_error(
                             need2(!is.na(self$name), glue("`$name` is not defined: {self$name}")),
                             need2(!is.na(self$path), glue("`$path` is not defined: {self$path}")),
                             need2(dir.exists(dirname(self$path)), glue("directory: {dirname(self$path)} does not exist yet!"))
                           )
                           TRUE
                         },
                         #' @description
                         #'  Calls validate method within \code{\link{tryCatch}}
                         #'  expression. If an error is encountered, returns FALSE
                         #'  otherwise return TRUE from validate method.
                         isValid = function(){
                           tryCatch( expr = self$validate(),
                                     error = function(err) {
                                       message(glue("{err$message}\n")) ; F }
                           )
                         },
                         #' @description
                         #'  print information
                         print = function(){
                           cat("DMRStructure:\n",
                               "    Name: ", self$name,"\n",
                               "    Path: ", self$path,"\n", sep = "")
                         },
                         #' @description
                         #'  show history of object
                         #' @param n number of entries to print
                         #' @param fun character value of either
                         #'  "tail" or "head" to print the last
                         #'  n or top n entries respectively.
                         #' @param time logical value indicating
                         #'  if \code{\link{Sys.time}} should be
                         #'  printed along with history logs.
                         history = function(n = 5, fun = "tail", time = F){
                           if(is.null(n)){
                             n <- length(private$.history)
                           }
                           fun <- switch(fun,
                                         tail = tail,
                                         head = head)
                           .hist <- fun(private$.history, n = n)
                           if(time){
                             .time <- fun(private$.time, n = n)
                             .hist <- str_c(.time, "::", str_replace_all(.hist, "\n", "\n                     "))
                           }
                           return(structure(.hist, class = "dt_history"))
                         },
                         #' @description
                         #'  add entry to history log. params value
                         #'  field and verb are short hand entries.
                         #'  messages are logged as:
                         #'  "`field` `verb` `value`"
                         #' @param value value to which field has changed
                         #' @param field field that has changed
                         #' @param verb verb that specifies what was done
                         #' @param collapse how to collapse value if length >1
                         #' @param custom write a custom message to history.
                         push_history = function(value = "", field = "", verb = "changed to:", collapse = ", ", custom = NULL){
                           entry <- custom %||% str_c(field, verb, str_c(value, collapse = collapse), sep = " ")
                           private$.history <- c(private$.history, entry)
                           private$.time <- c(private$.time, as.character(Sys.time()))
                           invisible(self)
                         }
                       )
)


