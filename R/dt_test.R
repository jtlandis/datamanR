# library(data.table)
#
#
# DT <- data.table(V1 = rep(c(1L, 2L), 5)[-10],
#                  V2 = 1:9,
#                  V3 = c(0.5, 1.0, 1.5),
#                  V4 = rep(LETTERS[1:3], each = 3))
#
# select2 <- function(dt, column){
#   dt[,.SD, .SDcols = column]
# }
#
#
# summarise2 <- function(dt, exp, name = exp){
#
# }
#
#
# mutate2 <- function(dt, expr, name, rows = rep(T, nrow(dt)), by = NULL) {
#   by <- eval(substitute(by), envir = dt)
#   text <- paste0("c(\"",paste0(name,collapse = "\",\""),"\") := ", deparse(substitute(expr)))
#   copy(dt)[rows, eval(parse(text = text)), by = by][]
# }
#
# mutate2(DT, list(V1+V2, V2^2), name = c("a","b"))
#
# DT[V3==0.5, `:=`(V1=sum(V1))]


