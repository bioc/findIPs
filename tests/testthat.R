
library(testthat)
library(IPSR)

# get data
data(miller05)
X <- miller05$X
y <- miller05$y
surv <- miller05$surv

# save expected results

# obj <- getdrop1ranks(X, y,
#                      fun = "t.test",
#                      decreasing = FALSE,
#                      topN = 100)
#
# expected.t.adaptive <- sumRanks(origRank = obj$origRank,
#                                 drop1Rank = obj$drop1Rank,
#                                 topN = 100,
#                                 method = "adaptive")
#
# expected.t.unweighted <- sumRanks(origRank = obj$origRank,
#                                   drop1Rank = obj$drop1Rank,
#                                   topN = 100,
#                                   method = "unweighted")
#
# expected.t.weighted <- sumRanks(origRank = obj$origRank,
#                                 drop1Rank = obj$drop1Rank,
#                                 topN = 100,
#                                 method = "weightedSpearman")
#
# expected.t.adaptive.ipsr <- ipsr(X, y,
#                                  fun = "t.test",
#                                  decreasing = FALSE,
#                                  topN = 100,
#                                  method = "adaptive")
#
#
# expected.cox.adaptive.ipsr <- ipsr(X, surv,
#                                    fun = "cox",
#                                    decreasing = FALSE,
#                                    topN = 100,
#                                    method = "adaptive")
#
# save(list = c("expected.t.adaptive", "expected.t.unweighted",
#               "expected.t.weighted", "expected.t.adaptive.ipsr",
#               "expected.cox.adaptive.ipsr"),
#      file = "./expectedResults/expectedResults.RData")

# get observed results

load("./expectedResults/expectedResults.RData")

obj <- getdrop1ranks(X, y,
                     fun = "t.test",
                     decreasing = FALSE,
                     topN = 100)

observed.t.adaptive <- sumRanks(origRank = obj$origRank,
                                drop1Rank = obj$drop1Rank,
                                topN = 100,
                                method = "adaptive")

observed.t.unweighted <- sumRanks(origRank = obj$origRank,
                                  drop1Rank = obj$drop1Rank,
                                  topN = 100,
                                  method = "unweighted")

observed.t.weighted <- sumRanks(origRank = obj$origRank,
                                drop1Rank = obj$drop1Rank,
                                topN = 100,
                                method = "weightedSpearman")

observed.t.adaptive.ipsr <- ipsr(X, y,
                                 fun = "t.test",
                                 decreasing = FALSE,
                                 topN = 100,
                                 method = "adaptive")

observed.cox.adaptive.ipsr <- ipsr(X, surv,
                                   fun = "cox",
                                   decreasing = FALSE,
                                   topN = 100,
                                   method = "adaptive")

test_that("test getdrop1ranks and sumRanks",{
  expect_equal(expected.t.adaptive, observed.t.adaptive)
  expect_equal(expected.t.unweighted, observed.t.unweighted)
  expect_equal(expected.t.weighted, expected.t.weighted)
})

test_that("stepwise(getdrop1ranks and sumRanks) and one-step procedure",{
  expect_equal(observed.t.adaptive.ipsr, observed.t.adaptive)
})

test_that("test ipsr",{
  expect_equal(expected.t.adaptive.ipsr, observed.t.adaptive.ipsr)
})

test_that("test ipsr for survival outcomes (ranked by univariate cox)",{
  expect_equal(expected.cox.adaptive.ipsr, observed.cox.adaptive.ipsr)
})













