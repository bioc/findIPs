library(testthat)
library(IPSR)

#- generate data --

X <- miller05$X
y <- miller05$y
surv <- miller05$surv

#- 1, function: getdrop1ranks
#- 1.1 test getdrop1ranks using "t.test", "cox", and specified function

test_that("getdrop1ranks using t.test",
          {expect_equal(
            names(getdrop1ranks(X, y, fun = "t.test", 
                                decreasing = F, 
                                topn = 100, 
                                ncores = NULL)),
            c("orig", "drop1rank"))})

test_that("getdrop1ranks using cox",
          {expect_equal(
            names(getdrop1ranks(X, surv, fun = "cox", 
                                decreasing = F, 
                                topn = 100, 
                                ncores = NULL)),
            c("orig", "drop1rank"))})

# using log-fold change function
log2fc <- function(x, y){
  
  y <- as.factor(y)
  abs(log2(mean(x[y == levels(y)[1]]) / mean(x[y == levels(y)[2]])))
  
}

test_that("getdrop1ranks using specified function",
          {expect_equal(
            names(getdrop1ranks(X, y, fun = log2fc, 
                                decreasing = T, 
                                topn = 100, ncores = NULL)),
            c("orig", "drop1rank"))})


#- 2, rank.compare

out <- getdrop1ranks(X, y, fun = "t.test", 
                     decreasing = F, 
                     topn = 100, 
                     ncores = NULL)


test_that("rank commpare using L1 or L2", {
  
  expect_error(rank.compare(refr = out$orig[-1], matr = out$drop1rank, 
                                  dist = "L1"),
               "lists in refr and matr should be equal in length")
  
  expect_error(rank.compare(refr = c(out$orig[-1], "a"), matr = out$drop1rank, 
                            dist = "L1"),
               "refr and matr should contain the same elements")
  
  expect_equal(names(rank.compare(refr = out$orig, matr = out$drop1rank, 
                                  dist = "L1")),
               c("kappa", "orig.ranks", "weighted.ranks", "score"))
  
  expect_equal(names(rank.compare(refr = out$orig, matr = out$drop1rank, 
                                  dist = "L2")),
               c("kappa", "orig.ranks", "weighted.ranks", "score"))})

test_that("data type of matr", {
   expect_error(rank.compare(refr = out$orig, matr = list(), dist = "L2"),
               "matr should be matrix or data.frame")
  })

#- 


