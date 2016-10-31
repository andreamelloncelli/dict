fun <- function(x, env ) {  eval( as.name( x ) , envir = env ) 	 }

evalStringVec <- function( vec, env = parent.frame() ) {
	resultingList <- lapply(X = vec, FUN =  fun, env = env)
	unlist(resultingList)
}

listToDict <- function( namedList, env = parent.frame() ) {
	listNames <- names(namedList)
	names( namedList ) <- evalStringVec( listNames, env = env )
	namedList
}

dict <- function(...) {
  namedList <- list(...)
  listToDict(namedList = namedList, env = parent.frame() )
}
