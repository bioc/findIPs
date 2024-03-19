
#' Visualize the influential scores
#'
#' @description
#' Visualize influential score using lollipop plot. The function uses the output
#' obtained from \code{rank.compare} or \code{findIPs} function.
#'
#' @param obj the object obtained from \code{rank.compare} or \code{findIPs}
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
#' plotIPs(rks, topn = 5, ylim = NULL)
#'
#' @export plotIPs
#' @import graphics


plotIPs <- function(obj, topn = 5, ylim = NULL, ... ) {

    y <- obj$score
    y <- y / sd(y)
    x <- seq_len(length(y))

    ## lollipop plot
    lollipop(x, y, topn = topn, ylim = ylim)

}







