
dict2 <- function(...) {
  namedList <- list(...)
  l <- lapply(X = namedList, FUN = function(x) { x[2]; } )
  ln<- lapply(X = namedList, FUN = function(x) { x[1]; } )
  list <- setNames( object = l, nm = ln )
}
