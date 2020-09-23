
#' @title R6 Class DataManR
#' @name DataManR
#'
#' @description The DataManR object is responsible for containing
#'  and managing the TableDefinition class. While most methods that affect
#'  the data exist on the TableDefinition object, DataManR's fields
#'  and methods describe and affect how TableDefinition$data relate to
#'  each other.
#'
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
                          #' @field rds_file shorthand value for the default .rds
                          #' file name for this object. This field is
                          #' read only and a concatination of
                          #' \code{\link{DataManR$path}}/\cod{\link{DataManR$name}}_DataManR.rds
                          #' This object is only saved to disk when \code{\link{DataManR$save}} is
                          #' called.
                          rds_file = function(value) {
                            if(missing(value)){
                              str_c(self$path,"/",self$name,"_DataManR.rds")
                            } else {
                              abort("`$rds_file` is read only!")
                            }
                          }
                        ),
                        public = list(
                          #' @field view The active TableDefinition Object. This field
                          #'  should be treated as a scratch space as operations on view
                          #'  should not affect the original TableDefinition Object.
                          view = NULL,
                          #' @field Tables List of TableDefinition Objects that DataManR
                          #'  is watching.
                          Tables = list(),
                          #' @description
                          #'  Get method for private links data.frame
                          show_link = function(){
                            private$links
                          },
                          #' @description
                          #'  appends the private links data.frame with new TableDefinition
                          #'  link information. The links data.frame describes how two
                          #'  TableDefinitions will be merged if the join method is called.
                          #'  The terms "Left" and "Right" are to inform which arguments are
                          #'  paird together and do not imply which direction the merge occurs.
                          #'
                          #'  @param LeftTable vector of TableDefinition names
                          #'  @param LeftKey vector of TableDefinition col_names
                          #'  @param RightTable vector of TableDefinition names
                          #'  @param RightKey vector of TableDefinition col_names
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
                          #' @description
                          #'  Convience function to test which keys will be used on
                          #'  a pair of TableDefinition names.
                          #'  @param x TableDefinition name
                          #'  @param y TableDefinition name
                          #'  @return data.frame of corresponding linked keys. data.fram
                          #'  has column names "by.x" and "by.y"
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
                          #' @description
                          #'  performs a full join on two TableDefinitions. Results are assigned
                          #'  to DataManR$view
                          #'  @param x TableDefinition object
                          #'  @param y TableDefinition object
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
                          #' @description
                          #'  adds a TableDefinition object to the list in
                          #'  \code{\link{DataManR$Tables}}. The Definition must
                          #'  be named and cannot have the same name of a table
                          #'  managed by this DataManR object.
                          #' @param def A TableDefinition object
                          addTable = function(def){
                            validate_error(
                              need2(!is.na(def$name), "Table Definition provided MUST have a name!"),
                              need2(!def$name%in%names(self$Tables), glue("There already exists a table named {def$name} in {self$name}"))
                            )
                            self$Tables[[def$name]] <- def
                          },
                          #' @description
                          #'  removes a Table Definition object from the list in
                          #'  \code{\link{DataManR$Tables}}. It's save method
                          #'  is called and then removed from this list.
                          #'  Note: The removed Table's .rds file will remain
                          #'  on disk, but since it isn't managed by this
                          #'  DataManR object, it could be overwritten easily.
                          #'  @param def TableDefinition object
                          rmTable = function(def){
                            def$save()
                            self$Tables[[def$name]] <- NULL
                          },
                          #' @description
                          #'  saves a copy of this R Object as an .rds file
                          #'  in the \code{\link{DataManR$rds_file}} as default.
                          #'  @param file file path to save R Object
                          #'  @param saveData Logical to indicate if TableDefinition's
                          #'  data field should be saved too. Default is set to FALSE
                          #'  to prevent the resulting _DataManR.rds file from being
                          #'  too large. Set to TRUE if you want a reproducable object.
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
                          #' @description
                          #'  DataManR's prefered clone method. The data field of
                          #'  TableDefinition Object contains class data.table which
                          #'  cannot be cloned via the base clone method. Thus copy
                          #'  is called explicitly to ensure the cloned DataManR object
                          #'  has a copy of the internal data.tables.
                          #'  @param deep whether it should be a deep clone.
                          copy = function(deep = FALSE){
                            clone_ <- self$clone(deep = deep)
                            clone_$Tables <- lapply(clone_$Tables, function(x){x$copy()})
                            clone_
                          },
                          #' @description
                          #'  instantiate a DataManR object.
                          #'  @param name Specifies the \code{\link{DataManR$name}} field
                          #'  @param path Specifies the \code{\link{DataManR$path}} field
                          #'  @param Tables Specifies the \code{\link{DataManR$Tables}} field
                          initialize = function(name = NA_character_,
                                                path = NA_character_,
                                                Tables = list()){
                            self$name <- name
                            self$path <- path
                            self$Tables <- Tables
                            self
                          },
                          #' @description
                          #'  internal validate function for running internal checks
                          #'  Method will return TRUE or an error.
                          #'  Note: DataManR object is not valid unless all TableDefinitions
                          #'  are also valid.
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
                          #' @description
                          #'  prints quick information about DataManR object.
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
#' memory, thus attempting to access the file locations.
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





