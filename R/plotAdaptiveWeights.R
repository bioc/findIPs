
#' Visualize the weight function for adaptive weights
#'
#' @description
#' Plot the weight function for the adaptive weights with given kappa
#' and the list length (n).
#'
#' @param kappa a shape parameter of the weight function.
#' @param n the length list.
#' @param type draw line or points. Both line and points will be plotted if
#' type = c('line', 'points').
#' @param ylim y coordinates ranges.
#' @examples
#'
#' par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#' plotAdaptiveWeights(kappa = 0.01, n = 100, type = 'line', ylim = c(0, 0.025))
#' plotAdaptiveWeights(kappa = 0.02, n = 100, type = 'line', ylim = c(0, 0.025))
#'
#' @import graphics
#' @export plotAdaptiveWeights

plotAdaptiveWeights <- function(kappa, n, type = c("line", "points"), ylim = NULL) {

    # generate x and y
    x <- seq_len(n)
    y <- exp(-kappa * x) * (1 - exp(kappa))/(exp(-kappa * n) - 1)

    if (is.null(ylim))
        ylim <- c(0, max(y))

    # plot
    plot.new()
    plot.window(xlim = c(1, n), ylim = ylim)
    axis(1)
    axis(2)
    box()

    # plot type
    if ("line" %in% type)
        lines(x, y)
    if ("points" %in% type)
        points(x, y)

    # legend and axis labels
    mtext("Ranks", side = 1, line = 2.3)
    mtext("Weighted ranks", side = 2, line = 2.3)
    kappa <- format(round(kappa, 3), nsmall = 3)
    legend("topright", inset = 0.02, bty = "n", legend = bquote(kappa ~ " = " ~ .(kappa)),
        cex = 1.2)

}



