#' Visualize the unweighted rank changes
#'
#' @description
#' Visualize the unweighted rank changes using scatter plot. The plot displays
#' the original ranking and leave-one-out rankings.
#'
#' @param obj the objective obtained from \code{findIPs()} or \code{sumRanks()}
#' functions
#' @param top logical, whether the most influential case needs to be plot in
#' black
#' @param points.arg a list. Arguments in \code{graphics::points()} can be used
#' to define the points.
#' @param top.arg a list. Arguments in \code{graphics::points()} can be used
#' to define the top points.
#'
#' @examples
#'
#' data(miller05)
#' X <- miller05$X
#' y <- miller05$y
#'
#' obj <- getdrop1ranks(X, y,
#'                      fun = 't.test',
#'                      decreasing = FALSE,
#'                      topN = 100)
#' rks <- sumRanks(origRank = obj$origRank,
#'                 drop1Rank = obj$drop1Rank,
#'                 topN = 100,
#'                 method = 'adaptive')
#' plotRankScatters(rks)
#'
#' @import graphics
#' @export plotRankScatters
#'
#'

plotRankScatters <- function(obj, top = TRUE, points.arg = list(), top.arg = list()) {

    # get origRank and drop1Rank
    origRank <- obj@origRank
    drop1Rank <- obj@drop1Rank

    # combine the rank changes in the leave-1-out matrix
    combR <- c()
    for (i in seq_len(ncol(drop1Rank))) {
        combR <- rbind(combR, cbind(seq_len(nrow(drop1Rank)), drop1Rank[, i]))
    }
    colnames(combR) <- c("y", "x")

    # set color for points
    if (!"col" %in% names(points.arg))
        points.arg$col <- ifelse(top, "grey", "black")

    # points argument
    points.arg <- c(points.arg, list(x = combR[, "x"], y = combR[, "y"], cex = 0.8,
        pch = 19, xlab = "", ylab = ""))

    # remove duplicated arguments, e.g., pch is set twice
    points.arg <- points.arg[!duplicated(names(points.arg))]

    do.call("plot", points.arg)

    # plot top points
    if (top) {
        ord <- order(obj@score, decreasing = TRUE)[1]
        top.arg <- c(top.arg, list(x = seq_len(nrow(drop1Rank)), y = drop1Rank[,
            ord], cex = 0.8, pch = 19))
        # remove duplicated arguments, e.g., pch is set twice
        top.arg <- top.arg[!duplicated(names(top.arg))]

        do.call("points", top.arg)

        legend("topleft", pch = 19, col = c("black", "grey"), legend = c(paste0("obs",
            ord), "The others"))
    }

    mtext("Original ranking", side = 1, line = 2.2)
    mtext("Leave-one-out rankings", side = 2, line = 2.2)
}



















