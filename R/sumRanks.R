
#' Summarize the weighted rank changes caused by case-deletion
#'
#' @description
#' This function measures the overall rank changes due to case deletion.
#' A large rank changes indicates more influence of the deleted case on feature
#' rankings. \code{sumRanks()} provides three methods to compute the overall
#' rank changes: unweighted, weighted Spearman, and adaptive weights.
#'
#' @param origRank vectors, reference rankings. For influential observation
#' detection, origRank denotes the original ranking obtained using the whole
#' data.
#' @param drop1Rank matrix or data.frame, Each column is a feature list with
#' a case removed.
#' @param topN the top n features in origRank will be used for rank comparison.
#' If null, include all features.
#' @param method method to summarize rank changes. Both 'adaptive' and
#' 'weightedSpearman' are weighted rank comparison method, but former employs
#' the weight that are adaptive to the distribution of rank changes.
#' 'unweighted' denotes a direct comparison of ranks without considering
#' weights.
#' @param ... other arguments
#'
#' @return \item{kappa}{The weight function's shape is controlled by kappa,
#' which ranges from 0 to 1. Weighted rank changes are calculated using kappa,
#' with higher values indicating more weight on top features.}
#' @return \item{score}{The influence of each observation on feature rankings,
#' with larger values indicating more influence.}
#' @return \item{origRank}{The original ranking. origRank is exactly the input.
#' Here it is re-output for visualization purposes.}
#' @return \item{drop1Rank}{The leave-one-out rankings. }
#' @return \item{origRankWeighted}{The weighted original ranking.
#' origRankWeighted will be returned when method = 'adaptive'. }
#' @return \item{drop1RankWeighted}{The weighted leave-one-out rankings.
#' drop1RankWeighted will be returned when method = 'adaptive'. }
#'
#' @examples
#'
#' data(miller05)
#' X <- miller05$X
#' y <- miller05$y
#' obj <- getdrop1ranks(X, y,
#'                      fun = 't.test',
#'                      decreasing = FALSE,
#'                      topN = 100)
#'
#' rks <- sumRanks(origRank = obj$origRank,
#'                 drop1Rank = obj$drop1Rank,
#'                 topN = 100,
#'                 method = 'adaptive')
#'
#' plotIPs(rks, topn = 5, ylim = NULL)
#'
#' @importFrom stats optim
#' @import methods
#' @export sumRanks

sumRanks <- function(origRank, drop1Rank, topN = NULL,
                     method = c("adaptive", "weightedSpearman", "unweighted"),
                     ...) {

  ## drop1Rank should be a matrix or data.frame
  if (!(is.matrix(drop1Rank) | is.data.frame(drop1Rank)))
    stop("drop1Rank should be a matrix or a data.frame")
  ## transfrorm to matrix
  drop1Rank <- as.matrix(drop1Rank)

  ## check
  if (length(origRank) != nrow(drop1Rank))
    stop("origRank and drop1Rank should be equal in length")

  ## select top n features in the reference ranking
  if (is.null(topN))
    topN <- length(origRank)
  n <- topN

  ## features to ranks
  if (!all(apply(drop1Rank, 2, function(x) setequal(x, origRank))))
    stop("origRank and drop1Rank should contain the same elements")

  ## select topn features
  origRank <- origRank[seq_len(topN)]

  ## select top n feature in leave-one-out matrix
  drop1Rank <- apply(drop1Rank, 2, intersect, y = origRank)

  ## get the rank in the leave-one-out matrix
  drop1Rank <- apply(drop1Rank, 2, match, table = origRank)

  ## rank changes based on adaptive weights
  if (method == "adaptive") {
    ## combine the rank changes in leave-1-out matrix
    combR <- c()
    for (i in seq_len(ncol(drop1Rank))) {
      combR <- rbind(combR, cbind(seq_len(nrow(drop1Rank)),
                                  drop1Rank[, i]))
    }
    colnames(combR) <- c("y", "x")

    ## remove non-changed ranks, and transform to data frame
    ## format for optim
    combR <- as.data.frame(combR[combR[, 1] != combR[, 2], ])

    ## weight optimization function
    optf <- function(kappa, data, n) {
      # weight function
      wfun <- function(x) {
        exp(-kappa * x) * (1 - exp(kappa))/(exp(-kappa * n) - 1)
      }

      with(data, -(1 - sum((wfun(x) - wfun(y))^2)/sum((wfun(x) -
                                                         mean(wfun(x)))^2)))
    }

    kappa <- optim(par = 0.01, fn = optf, data = combR, n = n, method = "Brent",
                   lower = 0, upper = 1)$par

    ## kappa = 0 or 1 indicates abnormal distribution of rank
    ## changes kappa may not be right for this situation
    if (kappa == 0 | kappa == 1)
      warning(bquote(kappa ~ "=" ~ .(m) ~ "may not be right"))

    ## weight function
    wfun <- function(x) {
      exp(-kappa * x) * (1 - exp(kappa))/(exp(-kappa * n) - 1)
    }

    drop1RankWeighted <- apply(drop1Rank, 2, wfun)

    ## get weighted ranks with the estimated kappa
    if (is.null(colnames(drop1RankWeighted)))
      colnames(drop1RankWeighted) <- seq_len(ncol(drop1Rank))

    ## transform origRank to weighted origRank
    origRankWeighted <- wfun(seq_len(topN))

    score <- apply(drop1RankWeighted, 2, function(x)
      sqrt(sum((origRankWeighted - x)^2)))
    score <- score/sd(score)
  }

  ## rank changes based on weighted Spearman
  if (method == "unweighted") {

    n <- length(origRank)
    m <- seq_len(n)
    score <- apply(drop1Rank, 2, function(x) (x - m)^2)
    score <- colSums(score)
    score <- score/sd(score)
  }

  ## weighted Spearman
  if (method == "weightedSpearman") {

    n <- length(origRank)
    m <- seq_len(n)
    score <- apply(drop1Rank, 2,
                   function(x) (x - m)^2 * (2 * n + 2 - x - m))
    score <- colSums(score)
    score <- score/sd(score)
  }


  ## return
  if (method == "adaptive") {

    out <- list(kappa = kappa,
               score = score,
               origRank = origRank,
               drop1Rank = drop1Rank,
               origRankWeighted = origRankWeighted,
               drop1RankWeighted = drop1RankWeighted)
  } else {

    out <- list(score = score,
               origRank = origRank,
               drop1Rank = drop1Rank)
  }

  return(out)
}














