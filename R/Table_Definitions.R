#' @title R6 Class TInfo
#'
#' @description
#' TInfo, short for "Table Information", is a Class designed to hold methods
#'  and useful information about a data table. Fields range from inforamation
#'  on the data.table attributes to details on disk location to modification
#'  history. Methods generally modify the data field in place and do not
#'  have copy on modify semantics.
#'
#' @export
TInfo <- R6::R6Class(classname = "TInfo",
                               inherit = BaseDMR,
                               private = list(
                                 .col_names = NA_character_,
                                 .col_types = NA_character_,
                                 .keys = NA_character_,
                                 .md5sum = NA_character_
                               ),
                               active = list(
                                 #' @field path Scalar character that specifies
                                 #'  a local file. This file is expected to
                                 #'  contain the data. Relative paths may
                                 #'  be assigned but will be convereted to
                                 #'  an absolute path.
                                 path = function(value){
                                   if(missing(value)){
                                     private$.path
                                   } else {
                                     require_scalar_char(value, "`$path`")
                                     value <- value %na% full_path(value)
                                     validate_error(
                                       need2(!fs::is_dir(value), "`$path` should not point to a directory for class 'TInfo'"))
                                     if(!identical(private$.path, value)){
                                       validate_warn(
                                         need2(file.exists(value), glue("`$path`: {value} does not exist yet!")))
                                       private$.path <- value
                                       self$push_history(value, field = "`$path`")
                                     }
                                     self
                                   }
                                 },
                                 #' @field col_names holds the column names of
                                 #' \code{self$data}
                                 col_names = function(value){
                                   if(missing(value)){
                                     private$.col_names
                                   } else {
                                     require_char(value, "`$col_names`")
                                     if(!identical(private$.col_names, value)){
                                       private$.col_names <- value
                                       # if(!is.null(self$data)){
                                       #   setnames(self$data, value)
                                       # }
                                       self$push_history(value, field = "`$col_names`")
                                     }
                                     self
                                   }
                                 },
                                 #' @field col_types holds the column classes of
                                 #' \code{self$data}
                                 col_types = function(value){
                                   if(missing(value)){
                                     private$.col_types
                                   } else {
                                     require_char(value, "`$col_types`")
                                     if(!identical(private$.col_types, value)){
                                       private$.col_types <- value
                                       self$push_history(value, field = "`$col_types`")
                                     }
                                     self
                                   }
                                 },
                                 #' @field keys sets which columns are keys. A group
                                 #'  of keys should uniquely identify a row.
                                 #'  keys are also helpful for identifying
                                 #'  which columns may merge well with other
                                 #'  tables.
                                 keys = function(value){
                                   if(missing(value)){
                                     private$.keys
                                   } else {
                                     require_char(value, "`$keys`")
                                     if(!identical(private$.keys, value)){
                                       private$.keys <- value
                                       self$push_history(value, feild = "`$keys`")
                                     }
                                     self
                                   }
                                 },
                                 #' @field md5sum contains the md5sum of the data table
                                 #' saved on disk. md5sum field is only updated
                                 #' when the write method or update method is called.
                                 #' It is good practice to check that the md5sum field
                                 #' matches the current image's md5sum before writing.
                                 #' If they do not match then that implies the disk image
                                 #' is different than what was originally loaded to memory.
                                 md5sum = function(value){
                                   if(missing(value)){
                                     private$.md5sum
                                   } else {
                                     abort("`$md5sum` is read only. Value depends on disk image of `$data`.")
                                   }
                                 },
                                 #' @field rds_file contains the expected rds file. This
                                 #' is expected to save in the same directory as
                                 #' `$path` but will use `$name`_TInfo.rds
                                 rds_file = function(value) {
                                   if(missing(value)){
                                     str_c(dirname(self$path),"/",self$name,"_TInfo.rds")
                                   } else {
                                     abort("`$rds_file` is read only!")
                                   }
                                 }
                               ),
                               public = list(
                                 #' @field data contains the data.table class.
                                 data = NULL,
                                 #' @description
                                 #'  TInfo Class constructor
                                 #' @param data Indicates what data to store.
                                 #' @param name Specifies the name field.
                                 #' @param path Specifies the path field.
                                 #' @param col_names Specifies the col_names field.
                                 #' @param col_types Specifies the col_types field.
                                 #' @param keys Specifies the keys field.
                                 initialize = function(data = NULL,
                                                       name = NA_character_,
                                                       path = NA_character_,
                                                       col_names = colnames(data),
                                                       col_types = colclasses(data),
                                                       keys = NA_character_) {
                                   self$data <- data
                                   self$name <- name
                                   self$path <- path
                                   self$col_names <- col_names %||% NA_character_
                                   self$col_types <- col_types %||% NA_character_
                                   self$keys <- keys
                                   self$update()
                                 },
                                 #' @description
                                 #'  print quick information about the TInfo Object
                                 print = function(){
                                   cat("DataTableInformation:\n",
                                       "  Name  : ", self$name,"\n",
                                       "  Path  : ", self$path,"\n",
                                       "  md5sum: ", self$md5sum,"\n",
                                       "  keys  : ", paste(self$keys,collapse =", "),"\n", sep = "")
                                   if(!is.null(self$data)){
                                     cat("  Data  : \n")
                                     print(x = head(self$data))
                                   }
                                 },
                                 #' @description
                                 #'  overrides \code{BaseDMR$validate} to run more specific checks.
                                 validate = function(){
                                   validate_warn(
                                     need2(file.exists(self$path), glue("file: {self$path} does not exist yet!"))
                                   )
                                   validate_error(
                                     need2(!is.na(self$name),
                                           glue("`$name` is not defined: {self$name}")),
                                     need2(!is.na(self$path),
                                           glue("`$path` is not defined: {self$path}")),
                                     need2(dir.exists(dirname(self$path)),
                                           glue("directory: {dirname(self$path)} does not exist yet!")),
                                     need2(!is.null(self$data), "No data is loaded"),
                                     need2(all(self$col_names == colnames(self$data)),
                                           "`$col_names` do not match data Column names."),
                                     need2(all(self$col_types == colclasses(self$data)),
                                           "`$col_types` do not match classes of data."),
                                     need2(all(self$keys %in% self$col_names),
                                           glue("The following `$keys` are not a subset of `$col_names`:\n",
                                                "  {paste(self$keys[!self$keys%in%self$col_names], collapse = \", \")}"))
                                   )
                                   TRUE
                                 },
                                 #' @description
                                 #'  update method for TInfo Object. This method is handy for
                                 #'  initiating field updates incase data has changed in memory
                                 #'  or on disk.
                                 update = function(){
                                   #if data has changed, update col_names and col_types
                                   if(is.null(self$data)){
                                     warn("`$data` is NULL. Please provide data before updating")
                                   } else {
                                     if(!inherits(self$data, "data.table")) self$data <- as.data.table(self$data)
                                     self$col_names <- colnames(self$data)
                                     self$col_types <- colclasses(self$data)
                                   }
                                   if(file.exists(self$path)){
                                     .md5sum <- tools::md5sum(self$path)
                                     if(!identical(private$.md5sum, .md5sum)) {
                                       private$.md5sum <- .md5sum
                                       self$push_history(.md5sum, "`$md5sum`", "updated to:")
                                     }
                                   } else {
                                     warn("`$path` does not exist. Please write table to update md5sum.")
                                   }
                                   invisible(self)
                                 },
                                 #' @description
                                 #'  Writes in data to disk
                                 #' @param data data to write.
                                 #' @param file file to write.
                                 #' @param sep delimiter to use.
                                 #' @param append if file should append current disk image.
                                 write = function(data = self$data,
                                                  file = self$path,
                                                  sep = ",",
                                                  append = F){
                                   file <- full_path(file)
                                   data.table::fwrite(x = data, file = file, sep = sep, append = append)
                                   .md5sum<- tools::md5sum(files = file)
                                   if(!identical(self$md5sum, .md5sum)){
                                     private$.md5sum <- .md5sum
                                     self$push_history(value = .md5sum, field = "`$md5sum`")
                                   }
                                   invisible(self)
                                 },
                                 #' @description
                                 #'  read data from disk. If path is invalid then
                                 #'  an empty data.table is made with the same column names and classes
                                 #'  as specified by `$col_names` and `$col_types` respetively.
                                 #'  If these fields are empty, then the function is aborted.
                                 #' @return returns a data.table object as opposed to self.
                                 read = function() {
                                   if(file.exists(self$path)){
                                     data_table <- data.table::fread(file = self$path, header = T, colClasses = self$col_types)
                                   } else if(!any(is.na(self$col_names))&&!any(is.na(self$col_types))){
                                     warn(glue("file: {self$path} is not valid.\nloading empty data.table.\n"))
                                     data_table <- data.table::fread(c(paste0(self$col_names, collapse = ","),"\n"), colClasses = self$col_types)
                                   } else {
                                     error <- c()
                                     rlang::abort(glue("Cannot read table from definition.\n",
                                                       "file: {self$path} is not valid.\n",
                                                       "No defined Column Names or Column Classes.\n"))
                                   }
                                   return(data_table)
                                 },
                                 #' @description
                                 #'  write self as an rds file. by default file is
                                 #'  written to location specified by the
                                 #'  `$rds_file` field
                                 #' @param file file location to save image
                                 save = function(file = self$rds_file){
                                   saveRDS(object = self, file = file)
                                 },
                                 #' @description
                                 #'  similar to \link[dplyr]{mutate} this function will
                                 #'  modify `$data` field in place using \link[data.table]{data.table}
                                 #'  \link[data.table]{:=}.
                                 #' @param i expression evaluating to logical vectory or row indexs.
                                 #'  This informs which rows are included in the mutation.
                                 #' @param j named list of expressions to enact. This parameter is
                                 #'  most similar to the \code{\dots} argument in \link[dplyr]{mutate}
                                 #'  except a list is expected. This which columns are made/mutated and how.
                                 #'  j is the only argument that is required, otherwise if j is missing or
                                 #'  NULL then this method returns self with no changes.
                                 #' @param by character vector of columns to group by if at all.
                                 #' @param deparse logical value indicating if i and j should be deparsed.
                                 #'  by default this method assumes the user will pass expressions
                                 #'  that need to be evaluated in the context of `$data` field. Setting
                                 #'  this value to FALSE will imply that the user will pass values
                                 #'  as a character string. This makes programming with the package
                                 #'  much easier.
                                 mutate = function(i = NULL, j = NULL, by = NULL, deparse = TRUE){
                                   #browser()
                                   if(deparse){
                                     i <- deparse(substitute(i))
                                     j <- deparse(substitute(j)) %>% str_remove(pattern = "^list")
                                   }
                                   signature <- !unlist(lapply(list(i = i, j = j, by = by), function(x) {x=="NULL"||is.null(x)}))
                                   fncID <- paste(names(signature)[signature], collapse = ".")
                                   text <- switch(fncID,
                                                  j = str_c("`:=` ", j),
                                                  j.by = str_c("`:=` ", j),
                                                  i.j = c("i" = i, "j" = str_c("`:=` ", j)),
                                                  i.j.by = c("i" = i, "j" = str_c("`:=` ", j)))

                                   switch(fncID,
                                          j = self$data[, eval(parse(text=text))],
                                          j.by = self$data[, eval(parse(text=text)), by = by],
                                          i.j = self$data[ eval(parse(text=text[1])), eval(parse(text=text[2]))],
                                          i.j.by = self$data[ eval(parse(text=text[1])), eval(parse(text=text[2])), by = by],
                                          self$data)
                                   hist <- switch(fncID,
                                                  j = str_c("Mutated `$data` column(s) by expression ", j),
                                                  j.by = str_c("Mutated `$data` column(s) by expression ", j,
                                                               " and grouped by: ", str_c(by, collapse = ", ")),
                                                  i.j = str_c("Mutated `$data` column(s) by expression ", j,
                                                              " on rows matching ", i),
                                                  i.j.by = str_c("Mutated `$data` column(s) by expression ", j,
                                                                 " on rows matching ", i,
                                                                 " and grouped by: ", str_c(by, collapse = ", ")),
                                                  NULL)

                                   if(!is.null(hist)) self$push_history(custom = hist)
                                   invisible(self)
                                 },
                                 #' @description
                                 #'  similar to \link[dplyr]{filter} this function will
                                 #'  modify `$data` field in place using \link[data.table]{data.table}.
                                 #' @param i expression evaluating to logical vectory or row indexs.
                                 #'  This informs which rows are included in the mutation.
                                 #' @param deparse logical value indicating if i should be deparsed.
                                 #'  by default this method assumes the user will pass expressions
                                 #'  that need to be evaluated in the context of `$data` field. Setting
                                 #'  this value to FALSE will imply that the user will pass values
                                 #'  as a character string. This makes programming with the package
                                 #'  much easier.
                                 filter = function(i = NULL, deparse = TRUE){
                                   if(deparse){
                                     i <- deparse(substitute(i))
                                   }
                                   if(i=="NULL"||is.null(i)){
                                     return(self)
                                   }
                                   self$data <- self$data[ eval(parse(text = i)),]
                                   self$push_history(custom = str_c("Filtered `$data` for rows matching ", i))
                                   invisible(self)
                                 },
                                 #' @description
                                 #'  similar to \link[dplyr]{summarise} this function will
                                 #'  modify `$data` field in place using \link[data.table]{data.table}
                                 #'  logic. This has a few key differences. This does not require
                                 #'  that expressions passed return a value of length 1. Evaluation
                                 #'  does NOT happen in place with \link[data.table]{:=}, thus
                                 #'  columns are subsetted in the output.
                                 #'
                                 #' @param i expression evaluating to logical vectory or row indexs.
                                 #'  This informs which rows are included in the mutation.
                                 #' @param j named list of expressions to enact. This parameter is
                                 #'  most similar to the \code{\dots} argument in \link[dplyr]{summarise}
                                 #'  except a list is expected. Describes which columns are made/summarised and how.
                                 #'  j is the only argument that is required, otherwise if j is missing or
                                 #'  NULL then this method returns self with no changes.
                                 #' @param by character vector of columns to group by if at all.
                                 #' @param deparse logical value indicating if i and j should be deparsed.
                                 #'  by default this method assumes the user will pass expressions
                                 #'  that need to be evaluated in the context of `$data` field. Setting
                                 #'  this value to FALSE will imply that the user will pass values
                                 #'  as a character string. This makes programming with the package
                                 #'  much easier.
                                 summarise = function(i = NULL, j = NULL, by = NULL, deparse = TRUE){
                                   # browser()
                                   if(deparse){
                                     i <- deparse(substitute(i))
                                     j <- deparse(substitute(j))
                                   }
                                   signature <- !unlist(lapply(list(i = i, j = j, by = by), function(x) {x=="NULL"||is.null(x)}))
                                   fncID <- paste(names(signature)[signature], collapse = ".")
                                   self$data <- switch(fncID,
                                                       j = self$data[, eval(parse(text = j))],
                                                       j.by = self$data[, eval(parse(text = j)), by = by],
                                                       i.j = self$data[eval(parse(text = i)), eval(parse(text = j))],
                                                       i.j.by = self$data[eval(parse(text = i)), eval(parse(text = j)), by = by])
                                   hist <- switch(fncID,
                                                  j = str_c("Summarised `$data` column(s) by expression ", j),
                                                  j.by = str_c("Summarised `$data` column(s) by expression ", j,
                                                               " and grouped by: ", str_c(by, collapse = ", ")),
                                                  i.j = str_c("Summarised `$data` column(s) by expression ", j,
                                                              " on rows matching ", i),
                                                  i.j.by = str_c("Summarised `$data` column(s) by expression ", j,
                                                                 " on rows matching ", i,
                                                                 " and grouped by: ", str_c(by, collapse = ", ")))
                                   self$push_history(custom = hist)
                                   invisible(self)
                                 },
                                 #' @description
                                 #'  wrapper function for \link[data.table]{dcast}.
                                 #' @param cols character vector specifying which columns whose values will become new columns.
                                 #' @param value.var character vector specifying which values will be used for new entries
                                 #' @param fun.aggregate aggregate function to be used if multiple values exist.
                                 reshape_wider = function(cols,
                                                          value.var = guess(self$data),
                                                          fun.aggregate = NULL){

                                   if(!is.null(fun.aggregate)){
                                     self$data <- dcast(self$data,
                                                        formula = str_c("... ~ ",str_c(wrap_str(cols), collapse = " + ")),
                                                        value.var = value.var,
                                                        fun.aggregate = fun.aggregate)
                                   } else {
                                     self$data <- dcast(self$data,
                                                        formula = str_c("... ~ ",str_c(wrap_str(cols), collapse = " + ")),
                                                        value.var = value.var)
                                   }
                                   hist <- str_c("`$data` was reshaped into a wider format:\n",
                                                 "  new columns mapped from: ", str_c(cols, collapse = ", "), "\n",
                                                 "  new values mapped from: ", str_c(value.var, collapse = ", "),"\n",
                                                 "  Aggregate Function(s): ", parse_fun_list(fun.aggregate))
                                   self$push_history(custom = hist)
                                   invisible(self)
                                 },
                                 #' @description wrapper function for \link[data.table]{melt}.
                                 #' @param measure.vars which columns should be melted into
                                 #'  a new column. This argument may be a list if you wish
                                 #'  to condense into multiple columns. For convenience/clarity
                                 #'  in the case of multiple melted columns, resulting column
                                 #'  names can be supplied as names to the elements measure.vars.
                                 #' @param variable.name name for the measured variable names column.
                                 #'  The default name is 'variable'
                                 #' @param value.name name for the molten data values column(s).
                                 #'  The default name is 'value'. Multiple names can be provided
                                 #'  here for the case when measure.vars is a list, though note
                                 #'  well that the names provided in measure.vars take precedence
                                 reshape_longer = function(measure.vars,
                                                           variable.name = "variable",
                                                           value.name = "value"){
                                   self$data <- melt(self$data,
                                                     measure.vars = measure.vars,
                                                     variable.name = variable.name,
                                                     value.name = value.name)
                                   meta <- capture.output(melt_meta(measure.vars = measure.vars,
                                                                    variable.name = variable.name,
                                                                    value.name = value.name))
                                   meta <- str_c("  ", meta, "\n", collapse = "")
                                   hist <- str_c("`$data` was reshaped into a longer format:\n",
                                                 "  Reshape Meta data:\n",
                                                 meta)
                                   self$push_history(custom = hist)
                                   invisible(self)

                                 },
                                 #' @description
                                 #'  TInfo's prefered clone method. The data field of
                                 #'  TInfo Object contains class data.table which
                                 #'  cannot be cloned via the base clone method. Thus copy
                                 #'  is called explicitly to ensure the cloned TInfo object
                                 #'  has a copy of the internal data.tables.
                                 #' @param deep whether it should be a deep clone.
                                 copy = function(deep = FALSE){
                                   clone_ <- self$clone(deep = deep)
                                   clone_$data <- copy(self$data)
                                   clone_
                                 }
                               ))
