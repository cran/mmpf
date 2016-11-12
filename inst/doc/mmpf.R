## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(error = TRUE)

## ------------------------------------------------------------------------
library(mmpf)
library(randomForest)

data(swiss)

fit = randomForest(Fertility ~ ., swiss)
marginalPrediction(swiss[, -1], "Education", c(10, 25), fit)

## ------------------------------------------------------------------------
marginalPrediction(swiss[, -1], "Education", c(10, 25), fit, aggregate.fun = identity)

## ------------------------------------------------------------------------
data(iris)

fit = randomForest(Species ~ ., iris)
marginalPrediction(iris[, -ncol(iris)], "Sepal.Width", c(10, 25), fit,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "prob"))

## ------------------------------------------------------------------------
marginalPrediction(iris[, -ncol(iris)], c("Sepal.Width", "Sepal.Length"), c(5, 25), fit,
  predict.fun = function(object, newdata) predict(object, newdata = newdata, type = "prob"))

## ------------------------------------------------------------------------
permutationImportance(iris, "Sepal.Width", "Species", fit)

## ------------------------------------------------------------------------
permutationImportance(iris, "Sepal.Width", "Species", fit,
  loss.fun = function(x, y) {
    mat = table(x, y)
    n = colSums(mat)
    diag(mat) = 0
    rowSums(mat) / n
  },
  contrast.fun = function(x, y) x - y)

