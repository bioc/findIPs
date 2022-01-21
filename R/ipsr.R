#' Derive ranking lists including original and leave-1-out rankings
#' 
#' @description 
#' This function calculate the feature rankings and the features rankings with 
#' one-case deletion with a given ranking criteria function. 
#' 
#' @param X A data matrix, with columns being the variables and rows being samples 
#' @param y Groups or survival object (for cox regression)
#' @param fun function to obtain ranking criteria with x and y being the input, where 
#' x a variables in X, y is the response. "t.test" and "cox" are two simple options 
#' that rank features according to the p.values of t.test or univariate cox regression.
#' @param decreasing logical. How the rank criteria are ordered? For instance, 
#' p-value should be ordered increasingly, while fold-change should be ordered decreasingly.  
#' @param topn the number of important features included for comparison. 
#' The top n features in the original ranking list.
#' @param ncores the number of CPU cores used for parallel running. 
#' If ncores = NULL, a single core is used.
#' 
#' @value orig feature rankings of the whole data
#' @value drop1rank feature ranking matrix of the leave-1-out data
#' @author Shuo Wang
#' @examples 
#' X <- miller05$X
#' y <- miller05$y
#' 
#' obj <- getdrop1ranks(X, y, topn = 100, fun = "t.test")
#' rks <- rank.compare(refr = obj$orig, matr = obj$drop1rank, topn = 100, dist = "L2")
#' par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#' plot_unweighted_ranks(rks, top = TRUE)
#' plot_kappa_weight(rks$kappa, n = 100, type = "line")
#' plot(rks, topn = 5, ylim = NULL)
#' 
#' @import survival 
#' @import doParallel
#' @import foreach
#' @import parallel
#' @importFrom stats sd t.test
#' @importFrom utils data
#' @export getdrop1ranks
#' 

getdrop1ranks <- function(X, y, fun = c("t.test", "cox"), 
                          decreasing = FALSE, 
                          topn = 100, ncores = NULL){

  if (dim(X)[1] != length(y))
    stop("The row of X should be identical to the length of y")
  
  if (!class(fun) %in% c("function", "character"))
    stop("fun should be function with the statistics being the output 
         or character between 't.test' and 'cox'")
  
  if (is.function(fun)) {
    f <- fun} else{
      
      if (fun == "cox" & !is.Surv(y))
        stop("y should be a survival object")
      
      if (fun == "cox")
        f <- function(x, y)
          summary(survival::coxph(y ~ x))$logtest[3]
      
      if (fun == "t.test")
        f <- function(x, y)
          t.test(x ~ y, var.equal = TRUE)$p.value
    }
  
  xp <- apply(X, 2, f, y = y)
  
  topr <- order(xp, decreasing = decreasing)[seq_len(topn)]
  
  if (is.null(colnames(X))){ 
    v <- seq_len(ncol(X))
  }else{
    v <- colnames(X)
  }
  
  topr <- v[topr]
  Xtop <- X[, topr]
  
  #- parallel running
  ncores <- ifelse(is.null(ncores), 1, ncores)
  
  #- detect cores in computer
  ecores <- parallel::detectCores() - 1 #- left 1 core by default
  if (ncores > 1 & ncores <= ecores)
    warning(paste0("Parallel running: ", ncores, " cores are used"))
  if (ncores > detectCores())
    warning(paste0("A maximum of ", ecores, " CPU cores are used"))
  
  ncores <- pmin(ncores, ecores)
  
  cl <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  
  drop1rank <- foreach::foreach(j = seq_len(nrow(Xtop)), .combine = cbind) %dopar% {
    
    Xdrop1 <- Xtop[-j,]
    y0 <- y[-j]
    nr <- apply(Xdrop1, 2, f, y = y0)
    rf <- order(nr, decreasing = decreasing) 
    rf <- topr[rf]
    return(rf)}
  parallel::stopCluster(cl)
  
  colnames(drop1rank) <- rownames(X)
  return(list(orig = topr, 
              drop1rank = drop1rank))
}


#' Measure the influence of the cases on feature rankings
#' 
#' @description 
#' This function measures the rank changes due to case deletion. The process incorporates
#' a rank comparison method taking the feature importance into account. The weight function
#' is determined by the distribution of rank changes.  
#' 
#' @param refr vectors, reference rankings.For influential observation detection, 
#' refr is the original ranking obtained with the whole data.
#' @param matr matrix or data.frame, Each column is a feature list with a case
#' removed.  
#' @param topn the top n features in refr will be used for rank comparison. 
#' If null, include all features.
#' @param dist the strategy to calculate the rank changes. Choose between L1 and L2 norm.
#' @param ... other arguments
#' @value kappa a optimized parameter in weight function. It is used to calculate 
#' the weighted rank changes.
#' @value orig.ranks the ranks or the leave-1-out rankings without weight transformation
#' @value weighted.ranks the weighted original ranking (first column) 
#' and leave-1-out rankings (from the second column on). 
#' @value score the weighted rank changes for each case
#' @author Shuo Wang
#' @examples  
#' X <- miller05$X
#' y <- miller05$y
#' 
#' obj <- getdrop1ranks(X, y, topn = 100, fun = "t.test")
#' rks <-rank.compare(refr = obj$orig, matr = obj$drop1rank, topn = 100, dist = "L2")
#' par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#' plot_unweighted_ranks(rks, top = TRUE)
#' plot_kappa_weight(rks$kappa, n = 100, type = "line")
#' plot(rks, topn = 5, ylim = NULL)
#' @import reshape2
#' @importFrom stats optim
#' @export rank.compare

rank.compare <- function(refr, matr, topn = NULL, dist = c("L1", "L2"), 
                         ...){
  
  if (!(is.matrix(matr) | is.data.frame(matr)))
    stop("matr should be matrix or data.frame")

  matr <- as.matrix(matr)
  if (length(refr) != nrow(matr)) 
    stop("lists in refr and matr should be equal in length")
  
  #- topn features in reference list
  if (is.null(topn)) topn <- length(refr)
  n <- topn
  
  #- features to ranks
  if (!all(apply(matr, 2, function(x) setequal(x, refr)))) 
    stop("refr and matr should contain the same elements")
  
  refr <- refr[seq_len(topn)]
  matr <- apply(matr, 2, function(x) match(refr, x))
  matr <- apply(matr[seq_len(topn), ], 2, rank) # get the new ranks within topn
  
  #- select changed ranks
  all_r <- reshape2::melt(matr)
  sel_r <- all_r[all_r[,1] != all_r[,3],]
  colnames(sel_r) <- c("y", "group", "x")
  
  #- weight function optimization
  optf <- function(kappa, data, n){

    #- weight function
    wfun <- function(x){
      exp(-kappa * x) * (1 - exp(kappa)) / (exp(-kappa * n) - 1)
    }

    with(data,
         -(1 - sum((wfun(x) - wfun(y))^2) / sum((wfun(x) - mean(wfun(x)))^2)))
  }

  kappa <- optim(par = 0.01, fn = optf, data = sel_r, n = n, method = "Brent",
                 lower = 0, upper = 1)$par
  
  if (kappa == 0 | kappa == 1) 
    warning(bquote(kappa ~ "=" ~ .(m) ~ "may not be right"))
  
  #- weight function
  wfun <- function(x) {
    exp(-kappa * x) * (1 - exp(kappa)) / (exp(-kappa * n) - 1)
  }
  
  #- get weighted ranks
  w.rank <- function(){
    
    weighted_matr <- apply(matr, 2, wfun)
    
    if (is.null(colnames(weighted_matr))) 
      colnames(weighted_matr) <- 1:ncol(matr)
    
    reference <- wfun(seq_len(topn))
    return(cbind("ref" = reference, weighted_matr))
  }
  
  weighted.ranks <- w.rank()
  
  if (dist == "L1") {
    score <- apply(weighted.ranks[, -1], 2, 
                function(x) sum(abs(weighted.ranks[,1] - x)))
  }
  
  if (dist == "L2"){
    score <- apply(weighted.ranks[, -1], 2, 
                function(x) sum((weighted.ranks[,1] - x)^2))
  }
  
  orig.ranks <- matr
  
  out <- list("kappa" = kappa,
              "orig.ranks" = matr,
              "weighted.ranks" = weighted.ranks,
              "score" = score)
  
  class(out) <- append(class(out), "ips")
  return(out)
}

#' Visualize the weight function
#' 
#' @description 
#' Plot the weight function given kappa and the list length (n)
#' 
#' @param kappa a parameter of the weight function.
#' @param n the length list.
#' @param type draw line or points. 
#' @param ylim y coordinates ranges
#' @author Shuo Wang
#' @examples
#' X <- miller05$X
#' y <- miller05$y
#' 
#' obj <- getdrop1ranks(X, y, topn = 100, fun = "t.test")
#' rks <-rank.compare(refr = obj$orig, matr = obj$drop1rank, topn = 100, dist = "L2")
#' par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#' plot_unweighted_ranks(rks, top = TRUE)
#' plot_kappa_weight(rks$kappa, n = 100, type = "line")
#' plot.ips(rks, topn = 5, ylim = NULL) 
#' @import graphics
#' @export plot_kappa_weight

plot_kappa_weight <- function(kappa, n, type = c("line", "points"), ylim = NULL){
  
  x <- seq_len(n)
  y <- exp(-kappa * x) * (1 - exp(kappa)) / (exp(-kappa * n) - 1)
  
  if (is.null(ylim)) ylim = c(0, max(y))
  
  plot.new()
  plot.window(xlim = c(1, n),  ylim = ylim)
  axis(1)
  axis(2)
  box()
  
  if ("line" %in% type)
    lines(x, y)
  if ("points" %in% type)
    points(x, y)
  
  mtext("Original ranks", side = 1, line = 2.3)
  mtext("Weighted ranks", side = 2, line = 2.3)
  kappa <- format(round(kappa, 3), nsmall = 3)
  legend("topright", inset = 0.02, bty = "n", 
         legend = bquote(kappa ~ " = " ~ .(kappa)), cex = 1.2)
}

#' Visualize the influential scores
#' 
#' @description 
#' Display the influential score using lollipop plot
#' 
#' @param obj the objective obtained from rank.compare function
#' @param topn the number of influential cases labelled in the plot
#' @param ylim y coordinates ranges
#' @author Shuo Wang
#' @examples 
#' X <- miller05$X
#' y <- miller05$y
#' 
#' obj <- getdrop1ranks(X, y, topn = 100, fun = "t.test")
#' rks <-rank.compare(refr = obj$orig, matr = obj$drop1rank, topn = 100, dist = "L2")
#' par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#' plot_unweighted_ranks(rks, top = TRUE)
#' plot_kappa_weight(rks$kappa, n = 100, type = "line")
#' plot(rks, topn = 5, ylim = NULL)
#' @export
#' @export plot.ips
#' @import graphics

plot.ips <- function(obj, topn = 5, ylim = NULL){
  
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
  
  y <- obj$score

  y <- y / sd(y)
  x <- seq_len(length(y))
  
  lollipop(x, y, topn = topn, ylim = ylim)
  
}

#' Visualize the unweighted rank changes
#' 
#' @description 
#' Compare the unweighted rank changes. The plot displays the original ranking 
#' leave-1-out rankings.
#' 
#' @param obj the objective obtained from rank.compare function
#' @param top logical, whether the most influential case needs to be plot
#' @param points.arg arguments pass to points
#' @param top.arg arguments pass to points of top feature
#' @author Shuo Wang
#' 
#' @examples 
#' X <- miller05$X
#' y <- miller05$y
#' 
#' obj <- getdrop1ranks(X, y, topn = 100, fun = "t.test")
#' rks <-rank.compare(refr = obj$orig, matr = obj$drop1rank, topn = 100, dist = "L2")
#' par(mfrow = c(1, 2), mar = c(4, 4, 2, 2))
#' plot_unweighted_ranks(rks, top = TRUE)
#' plot_kappa_weight(rks$kappa, n = 100, type = "line")
#' plot(rks, topn = 5, ylim = NULL)
#' @import reshape2
#' @import graphics
#' @export plot_unweighted_ranks
#' 
#' 

plot_unweighted_ranks <- function(obj, top = TRUE, points.arg = list(),
                                  top.arg = list()){
  
  orig <- obj$orig.ranks
  meltr <- melt(orig)
  
  if (!"col" %in% names(points.arg))
    points.arg$col <- ifelse(top, "grey", "black") 
  
  points.arg <- c(points.arg, list(x = meltr[,1], y = meltr[,3], cex = 0.8, 
                                pch = 19, xlab = "", ylab = ""))
  do.call("plot", points.arg)
  
  if (top){
    ord <- order(obj$score, decreasing = TRUE)[1]
    top.arg <- c(top.arg, list(x = seq_len(nrow(orig)), y = orig[,ord], cex = 0.8, 
                      pch = 19))
    do.call("points", top.arg)
    
    legend("topleft", pch = 19, col = c("black", "grey"), 
           legend = c(paste0("obs", ord), "The others"))
  }
  
  mtext("Original ranking", side = 1, line = 2.2)
  mtext("Leave-one-out rankings", side = 2, line = 2.2)
}

#' miller05 data
#' 
#' @description 
#' miller05 is gene expression data with 1000 genes randomly sampled from 22283 genes 
#' and 236 samples since removing the case with missing response. 
#' The data has binary and survival response. The binary response 
#' contains 58 case with p53 mutant and 193 wild type mutant. The survival response has a total of 55 events.
#' 
#' @export miller05
#' @references Miller, Lance D., et al. "An expression signature for p53 status in human breast cancer predicts mutation status, transcriptional effects, and patient survival." Proceedings of the National Academy of Sciences 102.38 (2005): 13550-13555.doi:10.1073pnas.0506230102
#' @author Shuo Wang
#' 
 
miller05 <- function(){
  data("miller05")
}

























