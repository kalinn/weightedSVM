#' @title fit.svm
#' @description Fit support vector machine with optional subject-level weights
#' @author Kristin Linn
#' @param tr.x Matrix or data.frame of features for training
#' @param tr.y Vector of group labels for training
#' @param ts.x Matrix or data.frame of features for testing
#' @param grid.C Vector of C values for tuning the SVM
#' @param w optional vector of subject weights
#' @import rPython
#' @examples 
#' if (require(MASS)){
#' ##################################################
#' # Generate data
#' ##################################################
#' set.seed(1)
#' n = 500
#' p = 2
#' sigma = matrix(.25, p, p)
#' diag(sigma) = 1
#' eps1 = MASS::mvrnorm(n, rep(0, p), sigma)
#' eps2 = MASS::mvrnorm(n, rep(0, p), sigma)
#' tr.y = rbinom(n, 1, .5)
#' x1 = 5 - .25*tr.y + .2*tr.y*eps1[,1] + .1*eps2[,1]
#' x2 = 5 - tr.y + .3*eps2[,1] + .5*(eps1[,2] + eps2[,2])
#' tr.x = cbind(x1, x2)
#' ts.eps1 = MASS::mvrnorm(n, rep(0, p), sigma)
#' ts.eps2 = MASS::mvrnorm(n, rep(0, p), sigma)
#' ts.y = rbinom(n, 1, .5)
#' ts.x1 = 5 - .25*ts.y + .2*ts.y*ts.eps1[,1] + .1*ts.eps2[,1]
#' ts.x2 = 5 - ts.y + .3*ts.eps2[,1] + .5*(ts.eps1[,2] + ts.eps2[,2])
#' ts.x = cbind(ts.x1, ts.x2)
#' grid.C = 10^c(-3:3)
#' out = fit.svm(tr.x=tr.x, tr.y=tr.y, ts.x=ts.x, grid.C=grid.C)
#' w = c(rep(2, 250), rep(1.5, 125), rep(3, 125))
#' w.out = fit.svm(tr.x=tr.x, tr.y=tr.y, ts.x=ts.x, grid.C=grid.C, w=w)
#' }
#' @seealso \code{data.frame}, \code{\link{data.frame}}
#' @export
fit.svm = function(tr.x, tr.y, ts.x, grid.C, w=NULL){
  tr.x = as.matrix(tr.x)  
  colnames(tr.x) = NULL
  ts.x = as.matrix(ts.x)  
  colnames(ts.x) = NULL
  py_file = system.file("exec", "fit_svm.py", package = "weightedSVM")
  python.load(py_file)
  if(is.null(w)){
    py.svm = python.call("fit_svm", tr.x, tr.y, ts.x, grid.C)
    return(list('w'=py.svm[[1]], 'rho'=py.svm[[2]], 
                'predicted'=py.svm[[3]], 'bestC'=py.svm[[4]]))
  }
  py.svm = python.call("fit_weighted_svm", tr.x, tr.y, ts.x, grid.C, w)
  return(list('w'=py.svm[[1]], 'rho'=py.svm[[2]], 
              'predicted'=py.svm[[3]], 'bestC'=py.svm[[4]]))
}

#' @title fit.ipw
#' @description Estimate inverse probability weights using logistic regression
#' @author Kristin Linn
#' @param tr.c Matrix or data.frame of confounders
#' @param tr.y Vector of group labels for training
#' @export
#' @examples
#' # Total in confounded sample
#' n = 200
#' # Number of noise features
#' k = 10
#' # a1 and a2 are confounders
#' a1 = runif (n, 0, 1)
#' a2 = rbinom(n, 1, .5)
#' # d is a vector of class labels
#' ld = -1 + a1 + a2 + rnorm(n, 0, .5)
#' d = 1*(exp(ld)/(1+exp(ld))>.5)
#' # covariance structure for features
#' # x1 and x2 are  
#' covmat = matrix (c (2, .5, .5, 2), 2, 2)
#' errs = mvrnorm (n, mu=rep (0, 2), Sigma=covmat)
#' # x1 and x2 are features
#' x1mean = 5 - 2*d - .5*a1
#' x2mean = -3*a1 + .5*a2 - .5*d*(a1 + .5*a2 + .25*a1*a2) 
#' x1 = scale(x1mean + errs[,1])
#' x2 = scale(x2mean + errs[,2])
#' noise = matrix (rnorm(n*k), n, k)
#' features = data.frame(x1=x1, x2=x2, noise=noise)
fit.ipw = function(tr.c, tr.y){
  ps.model = glm(tr.y~tr.c)
  ps.fits = ps.model$fitted.values
  ps.scores = (tr.y*ps.fits + (1-tr.y)*(1-ps.fits))
  ipweights = as.numeric(as.character(1/ps.scores))
  return(ipweights)
}




