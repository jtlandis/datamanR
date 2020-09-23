
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



