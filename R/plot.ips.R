
#' Visualize the influential scores
#'
#' @description
#' Visualize influential score using lollipop plot
#'
#' @param obj the objective obtained from \code{rank.compare} or \code{findIPs} function
#' @param topn the top n most influential points to be labelled in the plot.
#' @param ylim y coordinates ranges
#' @examples
#'
#' data(miller05)
#' X <- miller05$X
#' y <- miller05$y
#' obj <- getdrop1ranks(X, y,
#'                      fun = "t.test",
#'                      decreasing = FALSE,
#'                      topN = 100)
#' rks <- sumRanks(origRank = obj$origRank,
#'                 drop1Rank = obj$drop1Rank,
#'                 topN = 100,
#'                 method = "adaptive")
#' plot(rks, topn = 5, ylim = NULL)
#'
#' @export
#' @export plot.ips
#' @import graphics

plot.ips <- function(obj, topn = 5, ylim = NULL){

  # function for lolliop plot
  lollipop <- function(x, y, topn = 5, ylim = NULL){

    plot.new()
    if (is.null(ylim)) ylim = c(0, max(y) * 1.1)
    plot.window(xlim = c(min(x), max(x)), ylim = ylim, yaxs = "i")
    axis(1)
    axis(2)
    box()

    points(x, y, pch = 19, cex = 0.8)

    for(i in seq_along(x)){
      lines(c(x[i], x[i]), c(0, y[i]),  lwd = 0.7)
    }

    topn <- order(y, decreasing = TRUE)[seq_len(topn)]
    text(x[topn], y[topn], pos = 3, cex = 0.8, labels = topn)
    mtext("Observations", side = 1, line = 2.2)
    mtext("Scores", side = 2, line = 2.2)
  }

  # obj score
  y <- obj$score
  y <- y / sd(y)
  x <- seq_len(length(y))

  # lollipop plot
  lollipop(x, y, topn = topn, ylim = ylim)
}










