#' R6 Class to represent Data Table Information
#'
#' @description
#' Container to hold methods and useful information about a
#' data table
#'
#' @export
TableDefinition <- R6::R6Class(classname = "TableDefinition",
                               inherit = BaseDMR,
                               private = list(
                                 .col_names = NA_character_,
                                 .col_types = NA_character_,
                                 .keys = NA_character_,
                                 .md5sum = NA_character_
                               ),
                               active = list(
                                 #' @field Scalar character that specifies
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
                                       need2(!fs::is_dir(value), "`$path` should not point to a directory for class 'TableDefinition'"))
                                     if(!identical(private$.path, value)){
                                       validate_warn(
                                         need2(file.exists(value), glue("`$path`: {value} does not exist yet!")))
                                       private$.path <- value
                                       self$push_history(value, field = "`$path`")
                                     }
                                     self
                                   }
                                 },
                                 #' @field holds the column names of
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
                                 #' @field holds the column classes of
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
                                 #' @field sets which columns are keys. A group
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
                                 #' @field contains the md5sum of the data table
                                 #' saved on disk. md5sum field is only updated
                                 #' when the write method or update method is called.
                                 md5sum = function(value){
                                   if(missing(value)){
                                     private$.md5sum
                                   } else {
                                     abort("`$md5sum` is read only. Value depends on disk image of `$data`.")
                                   }
                                 },
                                 #' @field contains the expected rds file. This
                                 #' is expected to save in the same directory as
                                 #' `$path` but will use `$name`_TableDef.rds
                                 rds_file = function(value) {
                                   if(missing(value)){
                                     str_c(dirname(self$path),"/",self$name,"_TableDef.rds")
                                   } else {
                                     abort("`$rds_file` is read only!")
                                   }
                                 }
                               ),
                               public = list(
                                 #' @field contains the data.table class.
                                 data = NULL,
                                 #' @description
                                 #'  TableDefinition Class constructor
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
                                 print = function(){
                                   cat("DataTableDefinitions:\n",
                                       "  Name  : ", self$name,"\n",
                                       "  Path  : ", self$path,"\n",
                                       "  md5sum: ", self$md5sum,"\n",
                                       "  keys  : ", paste(self$keys,collapse =", "),"\n", sep = "")
                                   if(!is.null(self$data)){
                                     cat("  Data  : \n")
                                     print(x = head(self$data))
                                   }
                                 },
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
                                     private$.md5sum <- tools::md5sum(self$path)
                                   } else {
                                     warn("`$path` does not exist. Please write table to update md5sum.")
                                   }
                                   invisible(self)
                                 },
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
                                 save = function(file = self$rds_file){
                                   saveRDS(object = self, file = file)
                                 },
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
                                                                 " and grouped by: ", str_c(by, collapse = ", ")))
                                   self$push_history(custom = hist)
                                   invisible(self)
                                 },
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
                                 copy = function(deep = FALSE){
                                   clone_ <- self$clone(deep = deep)
                                   clone_$data <- copy(self$data)
                                   clone_
                                 }
                               ))
