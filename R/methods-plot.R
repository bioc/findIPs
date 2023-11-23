
#' ipsAdaptive
#' @slot kappa a numeric. The shape parameter for the weight function.
#' @slot score a numeric vector. The influential scores.
#' @slot origRank a character vector. The original ranking.
#' @slot drop1Rank a matrix. Leave-one-out rankings.
#' @slot origRankWeighted a numeric vector. The weighted original ranking.
#' @slot drop1RankWeighted a matrix. Weighted leave-one-out rankings.
#' @description ipsAdaptive is an S4 class object. The object is the output of
#' the \code{rank.compare} or \code{findIPs} function, when "adaptive" is
#' selected for method.
#' @return S4 class object.
#' @name ipsAdaptive
#'
setClass(
  "ipsAdaptive",
  slots= c(
    kappa = "numeric",
    score = "numeric",
    origRank = "character",
    drop1Rank = "matrix",
    origRankWeighted  = "numeric",
    drop1RankWeighted = "matrix"
  )
)


#' ipsOthers
#' @slot score a numeric vector. The influential scores.
#' @slot origRank a character vector. The original ranking.
#' @slot drop1Rank a matrix. Leave-one-out rankings.
#' @description ipsOthers is an S4 class object. The object is the output of
#' the \code{rank.compare} or \code{findIPs} function, when "weightedSpearman"
#' or "unweighted" is selected.
#' @return S4 class object.
#' @name ipsOthers
#'
setClass(
  "ipsOthers",
  slots = c(
    score = "numeric",
    origRank = "character",
    drop1Rank = "matrix"
  )
)


#' Visualize the influential scores
#'
#' @description
#' Visualize influential score using lollipop plot. The function is an S4 class
#' method for object obtained from \code{rank.compare} or \code{findIPs}
#' function.
#'
#' @param x the object obtained from \code{rank.compare} or \code{findIPs}
#' function.
#' @param topn the top n most influential points to be labelled in the plot.
#' @param ylim y coordinates ranges
#' @param ... other arguments
#' @return plot based on basic graph
#' @examples
#'
#' data(miller05)
#' X <- miller05$X
#' y <- miller05$y
#' obj <- getdrop1ranks(X, y,
#'                      fun = 't.test',
#'                      decreasing = FALSE,
#'                      topN = 100)
#' rks <- sumRanks(origRank = obj$origRank,
#'                 drop1Rank = obj$drop1Rank,
#'                 topN = 100,
#'                 method = 'adaptive')
#' plot(rks, topn = 5, ylim = NULL)
#'
#' @export
#' @import graphics
#' @import methods

setMethod(

  "plot",
  signature(x = "ipsAdaptive", y = "missing"),

  function(x, topn = 5, ylim = NULL, ... ) {

    # object score
    yy <- x@score
    yy <- yy / sd(yy)
    xx <- seq_len(length(yy))

    # lollipop plot
    lollipop(xx, yy, topn = topn, ylim = ylim)
  }
)

#' Plot method for S4 class ipsOthers
#'
#' @method plot ipsOthers
#' @description Plot method for S4 class ipsOthers.
#' @param x An object of S4 class ipsOthers.
#' @param topn the top n most influential points to be labelled in the plot.
#' @param ylim y coordinates ranges.
#' @param ... Additional parameters.
#' @return plot based on basic graph.

setMethod(

  "plot",
  signature(x = "ipsOthers", y = "missing"),

  function(x, topn = 5, ylim = NULL, ... ) {

    # object score
    yy <- x@score
    yy <- yy / sd(yy)
    xx <- seq_len(length(yy))

    # lollipop plot
    lollipop(xx, yy, topn = topn, ylim = ylim)
  }
)








