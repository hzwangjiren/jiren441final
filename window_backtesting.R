#' Moving Window Backtesting Function
#'
#' @param model This is the linear regression model used on time series
#' @param data This is the time series data
#' @param orig This is the number of data in the window
#' @param h This is the number of steps in prediction
#' @param xreg Usually go by default
#' @param fixed Usually go by Null
#' @param inc.mean Usually go by true
#' @param reest Time for the reestimation, usually go by 1.
#'
#' @return This function returns the RMSE and Mean Absolute Error of the model
#' @export
#'
#' @examples
window_backtesting <- function(model, data, orig, h, xreg=NULL,fixed = NULL, inc.mean = TRUE,
                               reest = 1){
  if(!inherits(data,"ts"))stop("data must be a time series object")
  arma_order <- model$arma
  regor = arma_order[c(1, 6, 2)]
  seaor = list(order = arma_order[c(3, 7, 4)],  period = arma_order[5])
  T = length(data)
  if (orig > T)
    orig = T
  if (h < 1)
    h = 1
  rmse = numeric(h)
  mabso = numeric(h)

  nori = T - orig
  err = matrix(0, nori, h)
  fcst = matrix(0, nori, h)
  jlast = T - 1
  time_vec <- time(data)
  ireest <- reest
  for (n in orig:jlast) {
    jcnt = n - orig + 1
    x <- window(data, time_vec[jcnt], time_vec[n])
    if (is.null(xreg))
      pretor = NULL
    else pretor = xre[jcnt:n]
    if (ireest == reest) {
      mm = arima(x, order = regor, seasonal = seaor, xreg = pretor,
                 fixed = fixed, include.mean = inc.mean)
      ireest <- 0
    }
    else {
      ireest <- ireest + 1
    }
    if (is.null(xreg)) {
      nx = NULL
    }
    else {
      nx = xreg[(n + 1):(n + h)]
    }
    fore = predict(mm, h, newxreg = nx)
    kk = min(T, (n + h))
    nof = kk - n
    pred = fore$pred[1:nof]
    obsd = data[(n + 1):kk]
    err[jcnt, 1:nof] = obsd - pred
    fcst[jcnt, 1:nof] = pred
  }
  for (i in 1:h) {
    iend = nori - i + 1
    tmp = err[1:iend, i]
    mabso[i] = sum(abs(tmp))/iend
    rmse[i] = sqrt(sum(tmp^2)/iend)
  }
  print("RMSE of out-of-sample forecasts")
  print(rmse)
  print("Mean absolute error of out-of-sample forecasts")
  print(mabso)
  backtest <- list(origin = orig, error = err, forecasts = fcst,
                   rmse = rmse, mabso = mabso, reest = reest)
}
