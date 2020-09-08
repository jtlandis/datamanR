
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

full_path <- function(path) {
  dir_ <- dirname(path)
  bas_ <- basename(path)
  return(paste(normalizePath(dir_),bas_, sep = "/"))
}

def_table <- function(data = NULL,
                      name = "",
                      col_names = colnames(data),
                      col_types = column_classes(data),
                      keys = NULL,
                      path = NULL) {
  structure(name,
            path = path %null% full_path(path),
            data = data,
            col_names = col_names,
            col_types = col_types,
            keys = keys,
            class = "def")
}

print.def <- function(x){
  .df <- data.frame(col_names = attr(x,"col_names"),
                    col_types = attr(x, "col_types"),
                    row.names = NULL)
  .df$keys <- ifelse(.df$col_names%in%attr(x,"keys"), T, F)
  cat("name: ",x,
      "\npath: ", attr(x,"path"),"\n")
  data <- attr(x,"data")
  if(!is.null(data)) {
    cat("nrow: ", nrow(data),"\n")
  }
  print(.df)
}

read_table <- function(def) {
  data_table <- attr(def,"path") %null% data.table::fread(file = attr(def,"path"),header = T, colClasses = attr(def, "col_types"))
  return(data_table)
}

write_table <- function(def, data, sep = ",", append = F){
  data.table::fwrite(x = data, file = attr(def, "path"), append = append, sep = sep)
}

test <- def_table(iris, "TestTable", path = "./test.csv")
test
write_table(test, attr(test,"data"))
readingIn <- read_table(test)


ldbm <- function(def, ...) { #add name?
  li <- list(def, ...)
  class(li) <- c("ldbm", "list")
  names(li) <- unlist(li)
  return(li)
}

save_ldbm <- function(ldbm, location = getwd(), save_data = F){
  if(!save_data){
    ldbm <- lapply(ldbm, function(x){
      attr(x, "data") <- NULL
      return(x)
    })
    class(ldbm) <- c("ldbm","list")
  }
  saveRDS(ldbm, file = paste0(location,"/ldbm.rds"))
}

load_ldbm <- function(file, force_load_data = T){
  ldbm <- readRDS(file = file)
  if(force_load_data){
    ldbm <- lapply(ldbm, function(x){
      if(is.null(attr(x,"data"))){
        attr(x,"data") <- read_table(x)
      }
      return(x)
    })
    class(ldbm) <- c("ldbm","list")
  }
  return(ldbm)
}
