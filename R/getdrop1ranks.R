#' Derive ranking lists including original and leave-one-out rankings
#'
#' @description
#' This function calculates the original and leave-one-out feature rankings
#' using a predefined rank method
#'
#' @param X A data matrix, with rows being the variables and columns being
#' samples.
#' @param y Groups or survival object (for cox regression)
#' @param fun *fun* can either be a character or a function. *fun* should be one
#' of the 't.test', 'cox', 'log2fc', and 'kruskal.test' when it is a character.
#' \code{findIPs()} incorporates four widely used ranking criteria: t-test,
#' univariate cox model, log2fc, and kruskal test, whose outputs are p values
#' except log2fc (absolute log2 fold changes). The features would be ordered by
#' specifying the argument \code{decreasing}. For instance,
#' if \code{fun = 't.test'}, the \code{decreasing = F}, such that features are
#' order by the pvalues of t.test in the increasing manner.
#'
#' *fun* can also be a function to obtain ranking criteria with x and y being
#' the only input and the ranking criteria, such as p-values being the only
#' output.
#'
#' @param decreasing logical. How the rank criteria are ordered? For instance,
#' p-value should be ordered increasingly, while fold-change should be ordered
#' decreasingly.
#' @param topN the number of important features included for comparison.
#' The top n features in the original ranking list.
#' @param nCores the number of CPU cores used for parallel running.
#' If nCores = NULL, a single core is used.
#'
#' @return \item{orig}{vector:,original ranking}
#' @return \item{drop1rank}{matrix, Leave-one-out rankings}
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
#' rks <- sumRanks(origRank = obj$origRank,
#'                 drop1Rank = obj$drop1Rank,
#'                 topN = 100,
#'                 method = 'adaptive')
#' plotIPs(rks, topn = 5, ylim = NULL)
#'
#' @importFrom survival coxph is.Surv
#' @importFrom BiocParallel SnowParam bplapply
#' @importFrom parallel detectCores
#' @importFrom stats sd t.test kruskal.test
#' @importFrom utils data
#' @export getdrop1ranks
#'

getdrop1ranks <- function(X, y,
                          fun,
                          decreasing = FALSE,
                          topN = 100,
                          nCores = NULL) {

    ## check the dimension of X and y
    if (dim(X)[2] != length(y))
        stop("The row of X should be identical to the length of y")

    ## Argument check for fun
    if (!class(fun) %in% c("function", "character"))
        stop("fun should be either a character between 't.test' and 'cox',
        or a function with the ranking criteria (e.g., P values),
             being the output")

    ## check for topn
    if (topN > nrow(X))
        warning(sprintf("topN exceeds the number of variables:
                   all variables (%s) will be included", nrow(X)))

    ## get the function (fun) for feature ranking
    if (is.function(fun)) {
        f <- function(X, y) apply(X, 1, fun, y = y)
    } else {
        ## match arguments
        fun <- match.arg(fun, c("t.test", "cox", "log2fc", "kruskal.test"))

        if (fun == "cox") {
            if (!is.Surv(y))
                stop("y should be a survival object")

            f <- function(X, y) apply(X, 1, function(x)
                summary(coxph(y ~ x))$logtest[3])
        }

        if (fun == "t.test") {
            f <- function(X, y) apply(X, 1, function(x)
                t.test(x ~ y, var.equal = TRUE)$p.value)
        }

        if (fun == "log2fc") {
            ## check y should be binary outcome
            if (length(unique(y)) != 2)
                stop("Log2fc requires binary outcomes")

            f <- function(X, y) apply(X, 1, function(x) {
                re <- split(x, y)
                abs(log2(mean(re[[1]])) - log2(mean(re[[2]])))
            })
        }

        if (fun == "kruskal.test") {
            ## check y should be binary outcome
            if (length(unique(y)) != 2)
                stop("kruskal.test requires binary outcomes")

            f <- function(X, y) apply(X, 1, function(x)
                kruskal.test(x, y)$p.value)
        }
    }

    ## apply the function to X and y
    xp <- do.call("f", list(X = X, y = y))

    ## order and select the topn features
    topRank <- order(xp, decreasing = decreasing)[seq_len(topN)]

    ## if no row names, use 1:ncol(X) as row names
    if (is.null(rownames(X))) {
        v <- seq_len(ncol(X))
    } else {
        v <- rownames(X)
    }

    ## select topn features based on row names Row names, here, will
    ## be used to rank features
    topRank <- v[topRank]
    Xtop <- X[topRank, ]

    ## Set parallel running, default 1
    if (is.null(nCores))
        nCores <- 1

    ## Set the maximum number of used cores
    if (nCores > detectCores() - 1) {
        nCores <- detectCores() - 1
        warning(sprintf("A maximum of %s CPU cores are used", nCores))
    }

    ## run fun in parallel set number of workers
    param <- SnowParam(workers = nCores)

    ## function to run in parallel
    paraFun <- function(x, f) {
        Xdrop1 <- Xtop[, -x]
        y0 <- y[-x]
        nr <- do.call("f", list(X = Xdrop1, y = y0))
        rf <- order(nr, decreasing = decreasing)
        rf <- topRank[rf]
        return(rf)
    }

    ## parallel runing
    drop1Rank <- bplapply(seq_len(ncol(Xtop)),
                          FUN = paraFun,
                          BPPARAM = param,
                          f = f)
    drop1Rank <- Reduce(cbind, drop1Rank)
    colnames(drop1Rank) <- colnames(X)

    ## output
    return(list(origRank = topRank, drop1Rank = drop1Rank))
}



