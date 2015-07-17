#' Fitting stuff
#' @description Fit unweighted SVM
#' @author Kristin Linn
#' @param tr.x Matrix or data.frame of features
#' @param tr.y Vector of group labels
#' @param ts.x Matrix or data.frame of features for testing
#' @param ts.y Vector of group labels in test set
#' @param grid.C Vector of C values for tuning the SVM
#' @import rPython
#' @examples 
#' if (require(MASS)){
#' ##################################################
#' # Generate data
#' ##################################################
#' set.seed(5000)
#' n = 500
#' p = 2
#' sigma = matrix(.25, p, p)
#' diag(sigma) = 1
#' eps1 = mvrnorm(n, rep(0, p), sigma)
#' eps2 = mvrnorm(n, rep(0, p), sigma)
#' d = rbinom(n, 1, .5)
#' x1 = 5 - .25*d + .2*d*eps1[,1] + .1*eps2[,1]
#' x2 = 5 - d + .3*eps2[,1] + .5*(eps1[,2] + eps2[,2])
#' par(mfrow=c(1,1))
#' plot(x1, x2, col=d+2)
#' X = cbind(x1, x2)
#' ts.eps1 = mvrnorm(n, rep(0, p), sigma)
#' ts.eps2 = mvrnorm(n, rep(0, p), sigma)
#' ts.d = rbinom(n, 1, .5)
#' ts.x1 = 5 - .25*ts.d + .2*ts.d*ts.eps1[,1] + .1*ts.eps2[,1]
#' ts.x2 = 5 - ts.d + .3*ts.eps2[,1] + .5*(ts.eps1[,2] + ts.eps2[,2])
#' par(mfrow=c(1,1))
#' plot(ts.x1, ts.x2, col=ts.d+2)
#' ts.X = cbind(ts.x1, ts.x2)
#' tr.x = X
#' tr.y = d
#' ts.x = ts.X
#' ts.y = ts.d
#' grid.C = 10^c(-3:3)
#' out = fit.unadj.svm(tr.x = tr.x, tr.y = tr.y, ts.x = ts.x, ts.y = ts.y, grid.C = grid.C)
#' print(out)
#' }
#' @seealso \code{data.frame}, \code{\link{data.frame}}
#' @export
fit.unadj.svm = function(tr.x, tr.y, ts.x, ts.y, grid.C){
  trScale = scale(tr.x)
  testData = as.matrix(scale(ts.x, 
                             center=attr(trScale, "scaled:center"), 
                             scale=attr(trScale, "scaled:scale")))
  colnames(testData) = NULL
  trainData = as.matrix(trScale)
  colnames(trainData) = NULL
  py_file = system.file("exec", "fit_svm.py", package = "ipwsvm")
  python.load(py_file)
  py.svm = python.call("fit_svm", trainData, tr.y,
                       testData, grid.C)
  acc = mean((py.svm[[3]]==ts.y))
  return(list('w'=py.svm[[1]], 'rho'=py.svm[[2]], 
              'acc'=acc, 'bestC'=py.svm[[4]]))
}

