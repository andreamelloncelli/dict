###############################################################
### library and data loading

library(dplyr)
library(dict)

## load example data
data("iris")
iris <- tbl_df(iris)
iris

###############################################################
### premise
###
### The goal of this tool is to have more readability in code
### using dplyr using standard evaluation SE

## NSE
iris_proportion <-
  iris %>%
  mutate(Petal.proportion = Petal.Length / Petal.Width, Sepal.proportion = Sepal.Length / Sepal.Width)

## SE
list <- list(
    Petal.proportion = "Petal.Length / Petal.Width",
    Sepal.proportion = "Sepal.Length / Sepal.Width")

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
                   name1 = "Petal.proportion", val1 = "Petal.Length / Petal.Width",
                   name2 = "Sepal.proportion", val2 = "Sepal.Length / Sepal.Width")

### Now we wanto to write the last function (proportion_old_way) in a new
### way, more readable using the dict function

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
                   name1 = "Petal.proportion", val1 = "Petal.Length / Petal.Width",
                   name2 = "Sepal.proportion", val2 = "Sepal.Length / Sepal.Width")

###############################################################
### benchmark of the whole datamanipulation

##install.packages("microbenchmark")
require(microbenchmark)
microbenchmark(
  proportion_old_way(iris,
                     name1 = "Petal.proportion", val1 = "Petal.Length / Petal.Width",
                     name2 = "Sepal.proportion", val2 = "Sepal.Length / Sepal.Width"),
  proportion_new_way(iris,
                     name1 = "Petal.proportion", val1 = "Petal.Length / Petal.Width",
                     name2 = "Sepal.proportion", val2 = "Sepal.Length / Sepal.Width"),
  times = 1000
)
identical(
  proportion_old_way(iris,
                     name1 = "Petal.proportion", val1 = "Petal.Length / Petal.Width",
                     name2 = "Sepal.proportion", val2 = "Sepal.Length / Sepal.Width"),
  proportion_new_way(iris,
                     name1 = "Petal.proportion", val1 = "Petal.Length / Petal.Width",
                     name2 = "Sepal.proportion", val2 = "Sepal.Length / Sepal.Width")
)

###############################################################
### benchmark of making a named list
###
### Now benchmarking the dict new function in comparison to the
### old behaviour and other two methods

name1 = "Petal.proportion"
val1 = "Petal.Length / Petal.Width"
name2 = "Sepal.proportion"
val2 = "Sepal.Length / Sepal.Width"
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
  ),
  list<- dict2(
    c(name1, val1),
    c(name2, val2)
  ),
  {
    list <- list()
    list[[name1]] <- val1
    list[[name2]] <- val2
    list
  },
  times = 10000
)
