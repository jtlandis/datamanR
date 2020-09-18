


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
                                  private$.name <- value
                                  self
                                }
                              },
                              path = function(value) {
                                if(missing(value)){
                                  private$.path
                                } else {
                                  require_scalar_char(value, "`$path`")
                                  value <- value %na% full_path(value)
                                  private$.path <- value
                                  self
                                }
                              }
                              ),
                            private = list(
                              .name = NA_character_,
                              .path = NA_character_
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
                                     private$.col_names <- value
                                     self
                                   }
                                 },
                                 col_types = function(value){
                                   if(missing(value)){
                                     private$.col_types
                                   } else {
                                     require_char(value, "`$col_types`")
                                     private$.col_types <- value
                                     self
                                   }
                                 },
                                 keys = function(value){
                                   if(missing(value)){
                                     private$.keys
                                   } else {
                                     require_char(value, "`$keys`")
                                     private$.keys <- value
                                     self
                                   }
                                 },
                                 md5sum = function(value){
                                   if(missing(value)){
                                     private$.md5sum
                                   } else {
                                     abort("`$md5sum` is read only. Value depends on disk image of `$data`.")
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
                                 },
                                 print = function(){
                                   cat("DataTableDefinitions:\n",
                                       "  Name  : ", self$name,"\n",
                                       "  Path  : ", self$path,"\n",
                                       "  md5sum: ", self$md5sum,"\n",
                                       "  keys  : ", paste(self$keys,collapse =", "),"\n", sep = "")
                                  if(!is.null(self$data)){
                                    cat("  Data  : \n")
                                    print(head(self$data))
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
                                     self$col_names <- colnames(self$data)
                                     self$col_types <- colclasses(self$data)
                                   }
                                   if(file.exists(self$path)){
                                     self$md5sum <- tools::md5sum(self$path)
                                   } else {
                                     warn("`$path` is not exist. Please write table to update md5sum.")
                                   }
                                   self
                                 }
                                 write = function(data = self$data,
                                                  file = self$path,
                                                  sep = ",",
                                                  append = F){
                                   file <- full_path(file)
                                   data.table::fwrite(x = data, file = file, sep = sep, append = append)
                                   private$md5sum <- tools::md5sum(files = file)
                                 },
                                 read = function() {
                                   if(file.exists(self$path)){
                                     data_table <- data.table::fread(file = self$file, header = T, colClasses = self$col_types)
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
                                 save = function(location = dirname(self$path), name = paste0(self$name, "_tableDef.rds")){
                                   saveRDS(object = self, file = paste0(location,"/", name))
                                 }
                               ))


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
DataManR <- proto::proto(Tables = list(),
                  links = data.frame(LeftTable = character(),
                                     LeftKey = character(),
                                     RightTable = character(),
                                     RightKey = character()),
                  name = NA_character_,
                  path = NA_character_,
                  message = NA_character_,
                  permission = "public",
                  owner = NA_character_,
                  group = NA_character_,
                  access = "774", #a convience value
                  setManName = function(., name = .$name){ .$name <- name},
                  setManPath = function(., path = .$path) { .$path <- path %na% full_path(path)},
                  setManMess = function(., message = .$message) { .$message <- message},
                  setGroup = function(., group = .$group) { .$group <- group},
                  setPermission = function(., permission = .$permission) {
                    if(!is.element(permission, c("public","private"))){
                      abort("{permission} is not a valid permission")
                    }
                    .$access <- switch (permission,
                      public = "774", #Anyone in group can modify
                      private = "744" #Only user can modify
                    )
                    .$permission <- permission
                  },
                  validManName = function(.) !is.na(.$name)&&str_length(.$name)>0,
                  validManPath = function(.) !is.na(.$path)&&dir.exists(.$path),
                  setTableLink = function(., LeftTable, LeftKey, RightTable, RightKey) {
                    if(!all(names(.$Tables)%in%c(LeftTable, RightTable))){
                      rlang::abort("{LeftTable} and/or {RightTable} are not accessible by Data Manager {.$name}.\n")
                    }
                    errors <- c()
                    if(!LeftKey%in%.$Tables[[LeftTable]]$col_names){
                      errors <- c(errors, glue("{LeftKey} is not a column name in {LeftTable}."))
                    }
                    if(!RightKey%in%.$Tables[[RightTable]]$col_names){
                      errors <- c(errors, glue("{RightKey} is not a column name in {RightTable}."))
                    }
                    if(length(errors)>0){
                      rlang::abort(glue_collapse(errors, sep = "\n"))
                    }
                    .$links <- rbind(.$links,
                                     data.frame(LeftTable = LeftTable,
                                                LeftKey = LeftKey,
                                                RightTable = RightTable,
                                                RightKey = RightKey))
                  },
                  addTable = function(., def){
                    if(def$name%in%names(.$Tables)){
                      rlang::abort(glue("There already exists a table named {def$name} in {.$name}.\n"))
                    }
                    .$Tables[[def$name]] <- def
                  },
                  rmTable = function(., def){
                    def$save(location = .$path)
                    .$Tables[[def$name]] <- NULL
                  },
                  save = function(., location, saveData = F){
                    if(!save_data){
                      .$Tables <- lapply(.$Tables, function(x){
                        x$setTableData(data = NULL)
                        return(x)
                      })
                    }
                    path <- paste0(location,"/",.$name,"_DataManR.rds")
                    saveRDS(., file = path)
                    fs::file_chown(path = path, user_id = .$owner, group_id = .$group)
                    fs::file_chmod(path = path, mode = .$access)
                  },
                  updateClass = function(.) {
                    .$setManPath(.$path)
                  },
                  isValid = function(.){
                    errors <- c()
                    if(!.$validManName()){
                      errors <- c(errors, glue("Data Manager name is not defined."))
                    }
                    if(is.na(.$path)){
                      errors <- c(errors, glue("Data Manager path is not defined."))
                    } else if(!dir.exists(.$path)) {
                      rlang::warn(glue("{.$path} does not exist yet."))
                    }
                    if(length(errors)>0) {
                      rlang::warn(glue_collapse(errors, sep = "\n"))
                      .$setManMess(errors)
                      return(FALSE)
                    } else {
                      .$setManMess(NULL)
                      return(TRUE)
                    }

                  },
                  new = function(.,
                                 Tables = .$Tables,
                                 name = .$name,
                                 links = .$links,
                                 path = .$path) {
                    tmp <- .$proto(Tables = Tables, name = name, links = links, path = path, owner = Sys.info()[["user"]], owner = Sys.info()[["user"]])
                    tmp$isValid()
                    class(tmp) <- c("DataManR",class(tmp))
                    return(tmp)
                  })

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
      x$setTableData(data = x$read_table())
      return(x)
    })
  }
  return(DataMan)
}

# TableDef$read_table()
# test <- TableDef$new(data = iris, name = "test", file = "test.csv")
# test$data
# test$write_table()
# test$col_types
#
# new_dataman <- DataManR$proto()
