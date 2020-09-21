


#'
#' Methods to set each field
#' @export
BaseDMR <- R6::R6Class(classname = "BaseDMR",
                            active = list(
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
                              initialize = function(name = NA_character_, path = NA_character_){
                                self$name <- name
                                self$path <- path
                              },
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
                              isValid = function(){
                                tryCatch( expr = self$validate(),
                                          error = function(err) {
                                            message(glue("{err$message}\n")) ; F }
                                )
                              },
                              print = function(){
                                cat("DMRStructure:\n",
                                    "    Name: ", self$name,"\n",
                                    "    Path: ", self$path,"\n", sep = "")
                              },
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
                              push_history = function(value = "", field = "", verb = "changed", collapse = ", ", custom = NULL){
                                entry <- custom %||% str_c(field, verb, "to:", str_c(value, collapse = collapse), sep = " ")
                                private$.history <- c(private$.history, entry)
                                private$.time <- c(private$.time, as.character(now()))
                                invisible(self)
                              }
                            )
                       )

#' @title TableDef
#' @name TableDef
#' @description Prototype object that holds the defining factors
#' of a Data Table. This object is of class "def" including class
#' "proto" and "environment". Use this class to define the constraints
#' and characteristics of a Data Table.
#' @section Feilds:
#'
#' The fields specify attributes of a Data Table. Not all fields are
#' necessary, but those in bold should be defined. The associated set
#' method is displayed in which it takes one argument, its associated
#' field. Most set methods are just convienice and help standardize
#' field expectations. Not all fields have a set method as they are
#' updated by another Method. Most fields have an assocated valid{Field}
#' method that simply checks if the associated field is NA or not.
#'
#' \itemize{
#' \item{data}{ \code{setTableData(data)}.
#' holds the Data Table in memory. This field is not necessary,
#' but is a convience for programming.setMethod: setTableData}
#' \item{\strong{name}}{ \code{setTableName(name)}.
#' holds the character string that defines the name of the Data
#' Table. Each Data Table name should be unique within a Data Manager.}
#' \item{\strong{file}}{ \code{setTableFile(file)}. holds the full path to a file where the data will be stored
#' on disk.}
#' \item{\strong{col_names}}{ \code{setTableColnames(col_names)}. Specifies the Column names of the Data Table.}
#' \item{\strong{col_types}}{ \code{setTableColtypes(col_types)}. Specifies the Column classes of the Data Table.}
#' \item{keys}{ \code{setTableKeys(keys)}. Specifies which Columns are keys. Keys are not necessary,
#' but a convience. In some Data Table designs, a set of keys may express
#' a set of columns that uniquely define each row within a Data Table
#' and/or express which keys may be joined with other Data Tables in
#' a Data Manager.}
#' \item{md5sum}{ Stores the md5sum that is stored in the file field. md5sum should not be set, instead it is updated with \code{updateClass()} method.}
#'}
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
                                 col_names = function(value){
                                   if(missing(value)){
                                     private$.col_names
                                   } else {
                                     require_char(value, "`$col_names`")
                                     if(!identical(private$.col_names, value)){
                                       private$.col_names <- value
                                       self$push_history(value, field = "`$col_names`")
                                     }
                                     self
                                   }
                                 },
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
                                 md5sum = function(value){
                                   if(missing(value)){
                                     private$.md5sum
                                   } else {
                                     abort("`$md5sum` is read only. Value depends on disk image of `$data`.")
                                   }
                                 },
                                 rds_file = function(value) {
                                   if(missing(value)){
                                     str_c(dirname(self$path),"/",self$name,"_TableDef.rds")
                                   } else {
                                     abort("`$rds_file` is read only!")
                                   }
                                 }
                                 ),
                               public = list(
                                 data = NULL,
                                 initialize = function(data = NULL,
                                                       name = NA_character_,
                                                       path = NA_character_,
                                                       col_names = colnames(data),
                                                       col_types = colclasses(data),
                                                       keys = NA_character_,
                                                       md5sum = NA_character_) {
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
                                     private$md5sum <- tools::md5sum(self$path)
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
                                   if(!identical(private$md5sum, .md5sum)){
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
                                   saveRDS(object = self, file = paste0(location,"/", name))
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
                               ), )
# d <- TableDefinition$new(us_rent_income)
# d$reshape_wider(cols = "variable", value.var = c("estimate","moe"))
# d$reshape_longer(measure.vars = list(c("estimate_income","estimate_rent"),c("moe_income","moe_rent")), value.name = c("estimate","moe"))
# new_level <- c("income","rent")
# for(i in 1:length(new_level)){
#   d$data[variable%in% i, `:=`(variable = new_level[i])]
# }

#' @title TableDef Methods
#' @name TableDef_methods
#' @description Methods of TableDef

#' @name write_table
#' @rdname TableDef_methods
#' @param data data to write. Defaults to data field.
#' @param file file to write. Defaults to file field.
#' @param sep delimiter to write as. Defaults to ",".
#' @param append logical value. Defaults to FALSE.
#'

#' @name read_table
#' @rdname TableDef_methods Read data from disk based on fields.
#'

#' @title DataManR
#' @name DataManR
#' @export
DataManR <- R6::R6Class(classname = "DataManR",
                        inherit = BaseDMR,
                        private = list(
                          links = data.frame(LeftTable = character(),
                                             LeftKey = character(),
                                             RightTable = character(),
                                             RightKey = character()),
                          join = function(x, y,
                                          by.x = NULL,
                                          by.y = NULL,
                                          all.x = FALSE,
                                          all.y = FALSE,
                                          sort = TRUE,
                                          suffixes = c(".x",".y")){
                            merge(x, y, by.x = by.x, by.y = by.y, all.x = all.x, all.y = all.y)
                          }
                        ),
                        active = list(
                          rds_file = function(value) {
                            if(missing(value)){
                              str_c(self$path,"/",self$name,"_DataManR.rds")
                            } else {
                              abort("`$rds_file` is read only!")
                            }
                          }
                        ),
                        public = list(
                          view = NULL,
                          Tables = list(),
                          show_link = function(){
                            private$links
                          },
                          setTableLink = function(LeftTable, LeftKey, RightTable, RightKey) {
                            if(!all(names(self$Tables)%in%c(LeftTable, RightTable))){
                              rlang::abort("{LeftTable} and/or {RightTable} are not accessible by Data Manager {self$name}.\n")
                            }
                            errors <- c()
                            if(!LeftKey%in%self$Tables[[LeftTable]]$col_names){
                              errors <- c(errors, glue("{LeftKey} is not a column name in {LeftTable}."))
                            }
                            if(!RightKey%in%self$Tables[[RightTable]]$col_names){
                              errors <- c(errors, glue("{RightKey} is not a column name in {RightTable}."))
                            }
                            if(length(errors)>0){
                              rlang::abort(glue_collapse(errors, sep = "\n"))
                            }
                            private$links <- rbind(private$links,
                                             data.frame(LeftTable = LeftTable,
                                                        LeftKey = LeftKey,
                                                        RightTable = RightTable,
                                                        RightKey = RightKey))
                          },
                          pull_key_pairs = function(x,y){
                            private$links %>%
                              filter((LeftTable %in% x & RightTable %in% y)|
                                      (LeftTable %in% y & RightTable %in% x)) %>%
                              mutate(by.x = case_when(LeftTable %in% x ~ LeftKey,
                                                      RightTable %in% x ~ RightKey,
                                                      TRUE ~ NA_character_),
                                     by.y = case_when(LeftTable %in% y ~ LeftKey,
                                                      RightTable %in% y ~ RightKey,
                                                      TRUE ~ NA_character_)) %>%
                              select(by.x, by.y)
                          },
                          full_join = function(x, y){
                            self$view <- x
                            keys_pairs <- self$pull_key_pairs(x$name, y$name)
                            validate_error(
                              need2(nrow(keys_pairs)>0, glue("{x$name} and {y$name} do not have any common keys set!")),
                              need2(any(is.na(keys_pairs$by.x)|is.na(keys_pairs$by.y)),
                                    glue("Check `$pull_key_pairs(x = \"{x$name}\", y = \"{y$name}\")` and correct key pairs."))
                            )
                            self$view$data <- private$join(x = x$data,
                                                           y = y$data,
                                                           by.x = keys_pairs$by.x,
                                                           by.y = keys_pairs$by.y,
                                                           all.x = T,
                                                           all.y = T,
                                                           suffixes = c(str_c(".",x$name)), str_c(".",y$name))

                            invisible(self)
                          },
                          addTable = function(def){
                            validate_error(
                              need2(!is.na(def$name), "Table Definition provided MUST have a name!"),
                              need2(!def$name%in%names(self$Tables), glue("There already exists a table named {def$name} in {self$name}"))
                            )
                            self$Tables[[def$name]] <- def
                          },
                          rmTable = function(def){
                            def$save()
                            self$Tables[[def$name]] <- NULL
                          },
                          save = function(file = self$rds_file,
                                          saveData = F){
                            clone_ <- self$copy(deep = T)
                            if(!saveData){
                              clone_$Tables <- lapply(clone_$Tables, function(x){
                                x$data <- NULL
                                return(x)
                              })
                            }
                            saveRDS(object = clone_, file = file)
                            # fs::file_chown(path = path, user_id = .$owner, group_id = .$group)
                            # fs::file_chmod(path = path, mode = .$access)
                            invisible(self)
                          },
                          copy = function(deep = FALSE){
                            clone_ <- self$clone(deep = deep)
                            clone_$Tables <- lapply(clone_$Tables, function(x){x$copy()})
                            clone_
                          },
                          initialize = function(name = NA_character_,
                                                path = NA_character_,
                                                Tables = list()){
                            self$name <- name
                            self$path <- path
                            self$Tables <- Tables
                            self
                          },
                          validate = function(){
                            validate_warn(
                              need2(file.exists(self$path), glue("file: {self$path} does not exist yet!"))
                            )
                            validate_error(
                              need2(!is.na(self$name), glue("`$name` is not defined: {self$name}")),
                              need2(str_length(self$name)>0, glue("`$name` must have at least 1 character.")),
                              need2(!is.na(self$path), glue("`$path` is not defined: {self$path}")),
                              need2(dir.exists(dirname(self$path)), glue("directory: {dirname(self$path)} does not exist yet!"))
                            )
                            lapply(self$Tables, function(x) x$validate())
                            TRUE
                          },
                          isValid = function(){
                            tryCatch( expr = self$validate(),
                                      error = function(err) {
                                        message(glue("{err$message}\n")) ; F }
                            )
                          },
                          print = function(){
                            cat("DMRStructure:\n",
                                "    Name    : ", self$name,"\n",
                                "    Path    : ", self$path,"\n",
                                "    Managing: ", length(self$Tables), sep = "")
                          }
                        ))

# dm <- DataManR$new("Test1", path = ".")
# dm$addTable(d)

#' @title load_DataManR
#' @rdname load_DataManR
#' @description Main function to read the specified rds file.
#' @param file rds file to read in
#' @param force_load_data logical defaults to F. Indicates
#' if each table managed by DataManR should be read into
#' memory.
#' @export
load_DataManR <- function(file, force_load_data = F){
  DataMan <- readRDS(file = file)
  if(force_load_data){
    DataMan$Tables <- lapply(DataMan$Tables, function(x){
      x$data <- x$read()
      return(x)
    })
  }
  return(DataMan)
}


# ,
# message = NA_character_,
# permission = "public",
# owner = NA_character_,
# group = NA_character_,
# access = "774", #a convience value
# setManMess = function(., message = .$message) { .$message <- message},
# setGroup = function(., group = .$group) { .$group <- group},
# setPermission = function(., permission = .$permission) {
#   if(!is.element(permission, c("public","private"))){
#     abort("{permission} is not a valid permission")
#   }
#   .$access <- switch (permission,
#                       public = "774", #Anyone in group can modify
#                       private = "744" #Only user can modify
#   )
#   .$permission <- permission
# },
# validManName = function(.) !is.na(.$name)&&str_length(.$name)>0,
# validManPath = function(.) !is.na(.$path)&&dir.exists(.$path),
# updateClass = function(.) {
#   .$setManPath(.$path)
# },
# isValid = function(.){
#   errors <- c()
#   if(!.$validManName()){
#     errors <- c(errors, glue("Data Manager name is not defined."))
#   }
#   if(is.na(.$path)){
#     errors <- c(errors, glue("Data Manager path is not defined."))
#   } else if(!dir.exists(.$path)) {
#     rlang::warn(glue("{.$path} does not exist yet."))
#   }
#   if(length(errors)>0) {
#     rlang::warn(glue_collapse(errors, sep = "\n"))
#     .$setManMess(errors)
#     return(FALSE)
#   } else {
#     .$setManMess(NULL)
#     return(TRUE)
#   }
#
# },
# new = function(.,
#                Tables = .$Tables,
#                name = .$name,
#                links = .$links,
#                path = .$path) {
#   tmp <- .$proto(Tables = Tables, name = name, links = links, path = path, owner = Sys.info()[["user"]], owner = Sys.info()[["user"]])
#   tmp$isValid()
#   class(tmp) <- c("DataManR",class(tmp))
#   return(tmp)
# }



