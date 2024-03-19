#' Function to detect influential points for feature rankings
#'
#' @description
#' \code{findIPs} employs two important functions: \code{getdrop1ranks} and
#' \code{sumRanks}. \code{getdrop1ranks} can calculate the original feature
#' ranking and leave-one-out feature rankings. The outputs are subsequently
#' taken to \code{sumRanks}, which computes the overall rank changes for
#' each observation, indicating their influence on feature rankings.
#'
#' @param X A data matrix, with rows being the variables and columns being
#' samples.
#' @param y Groups or survival object (for cox regression).
#' @param fun *fun* can either be a character or a function. *fun* should be one
#' of the 't.test', 'cox', 'log2fc', and 'kruskal.test' when it is a character.
#' \code{findIPs()} incorporates four widely used ranking criteria: t-test,
#' univariate cox model, log2fc, and kruskal test, whose outputs are p values
#' except log2fc (absolute log2 fold changes). The features would be ordered by
#' specifying the argument \code{decreasing}. For instance, if
#' \code{fun = 't.test'}, the \code{decreasing = F}, such that features are
#' order by the pvalues of t.test in a increasing manner.
#'
#' *fun* can also be a function to obtain ranking criteria with x and y being
#' the only input and the ranking criteria, such as p-values being the only
#' output.
#' @param decreasing logical. How the rank criteria are ordered? For instance,
#' p-value should be ordered increasingly, while fold-change should be ordered
#' decreasingly.
#' @param topN the number of important features included for comparison.
#' @param method method to summarize rank changes. It should be one of the
#' 'adaptive', 'weightedSpearman', and 'unweighted'. Both 'adaptive' and
#' 'weightedSpearman' are weighted rank comparison method, but former employs
#' the weight that are adaptive to the distribution of rank changes.
#' 'unweighted' denotes a direct comparison of ranks without considering
#' weights.
#'
#' @param nCores the number of CPU cores used for parallel running.
#' If nCores = NULL, a single core is used.
#'
#' @return \item{kappa}{The weight function's shape is controlled by kappa,
#' which ranges from 0 to 1. Weighted rank changes are calculated using kappa,
#' with higher values indicating more weight on top features.}
#' @return \item{score}{The influence of each observation on feature rankings,
#' with larger values indicating more influence.}
#' @return \item{origRank}{The original ranking. origRank is exactly the input.
#' Here it is re-output for visualization purposes.}
#' @return \item{drop1Rank}{The leave-one-out rankings. }
#' @return \item{origRankWeighted}{The weighted original ranking}
#' @return \item{drop1RankWeighted}{The weighted leave-one-out rankings}
#'
#'
#' @import Biobase
#' @import SummarizedExperiment
#' @importFrom stats sd t.test
#' @importFrom utils data
#' @export findIPs
#' @examples
#'
#' data(miller05)
#' X <- miller05$X
#' y <- miller05$y
#'
#' obj <- findIPs(X, y,
#'                fun = 't.test',
#'                decreasing = FALSE,
#'                topN = 100,
#'                method = 'adaptive')
#'
#' par(mfrow = c(1, 3), mar = c(4, 4, 2, 2))
#' plotRankScatters(obj, top = TRUE)
#' plotAdaptiveWeights(kappa = obj$kappa,
#'                     n = nrow(obj$drop1Rank),
#'                     type = 'line',
#'                     ylim = NULL)
#' plotIPs(obj, topn = 5, ylim = NULL)
#'
#' ## Interop with ExpressionSet class
#' library(Biobase)
#' data(sample.ExpressionSet)
#' design <- phenoData(sample.ExpressionSet)$type
#' IPs <- findIPs(exprs(sample.ExpressionSet), design, fun = "t.test",
#'                method = "adaptive")
#' plotIPs(IPs)
#'
#' ## Interop with SummarizedExperiment class
#' library(SummarizedExperiment)
#' ## Make a SummarizedExperiment class
#' sample.SummarizedExperiment <- makeSummarizedExperimentFromExpressionSet(
#'   sample.ExpressionSet)
#'
#' design <- colData(sample.SummarizedExperiment)$type
#' IPs <- findIPs(assay(sample.SummarizedExperiment), design, fun = "t.test",
#'                method = "adaptive")
#' plotIPs(IPs)
#'

findIPs <- function(X, y, fun,
                    decreasing = FALSE,
                    topN = 100,
                    method = "adaptive",
                    nCores = NULL) {

    # derive original ranking and leave-one-out rankings
    obj <- getdrop1ranks(X, y,
                         fun = fun,
                         decreasing = decreasing,
                         topN = 100,
                         nCores = nCores)

    # calculate the influential scores for each observatoin
    out <- sumRanks(obj$origRank, obj$drop1Rank, topN = topN, method = method)

    return(out)

}

