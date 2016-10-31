library(dplyr)
library(dict)

## load example data
data("iris")
iris <- tbl_df(iris)
iris

## NSE
iris_proportion <- iris %>% mutate(Petal.proportion = Petal.Length * Petal.Width, Sepal.proportion = Sepal.Length * Sepal.Width)

## SE
list <- list(Petal.proportion = "Petal.Length * Petal.Width", Sepal.proportion = "Sepal.Length * Sepal.Width")
mutate_(iris, .dots= list)


## SE in function as usual
proportion_old_way <- function(df, name1 , val1, name2, val2) {
  list <- setNames(
    list(
      val1,
      val2
    ),
    c(name1, name2)
  )
  mutate_(df, .dots= list) %>% collect()
}

proportion_old_way(iris,
                   name1 = "Petal.proportion", val1 = "Petal.Length * Petal.Width",
                   name2 = "Sepal.proportion", val2 = "Sepal.Length * Sepal.Width")

# SE in function with dict
proportion_new_way <- function(df, name1 , val1, name2, val2) {
  list <-
    dict(
      name1 = val1,
      name2 = val2
    )
  mutate_(df, .dots= list) %>% collect()
}

proportion_new_way(iris,
                   name1 = "Petal.proportion", val1 = "Petal.Length * Petal.Width",
                   name2 = "Sepal.proportion", val2 = "Sepal.Length * Sepal.Width")

###############################################################
## benchmark of the whole datamanipulation

##install.packages("microbenchmark")
require(microbenchmark)
microbenchmark(
  proportion_old_way(iris,
                     name1 = "Petal.proportion", val1 = "Petal.Length * Petal.Width",
                     name2 = "Sepal.proportion", val2 = "Sepal.Length * Sepal.Width"),
  proportion_new_way(iris,
                     name1 = "Petal.proportion", val1 = "Petal.Length * Petal.Width",
                     name2 = "Sepal.proportion", val2 = "Sepal.Length * Sepal.Width")
)

###############################################################
## benchmark of making a named list
fun <- compiler::comfun(fun)
evalStringVec <- compiler::cmpfun(evalStringVec)
listToDict <- compiler::cmpfun(listToDict)
dict <- compiler::cmpfun(dict)

name1 = "Petal.proportion"
val1 = "Petal.Length * Petal.Width"
name2 = "Sepal.proportion"
val2 = "Sepal.Length * Sepal.Width"
microbenchmark(
  list <- setNames(
    list(
      val1,
      val2
    ),
    c(name1, name2)
  ),
  list<- dict(
    name1 = val1,
    name2 = val2
  )
)
dict
