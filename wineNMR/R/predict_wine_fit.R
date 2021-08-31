#' Predict the glmnet results
#'
#' Predict the glmnet results given all attributes (response_names = NULL) or specific results.
#'
#' @param wine_fit A wine_fit class.
#'
#' @param response_names The response names to predict
#'
#'
#' @return A wine_predict class.
#'
#'
#' @export
#'


predict.wine_fit = function(wine_fit, wine_data, response_names = NULL, ...){
  pred_vals = predict(wine_fit$fit, wine_data$X, s = 'lambda.min')

  response_values = colnames(wine_data$Y)

  value = list(
    observed = NULL,
    predicted = NULL,
    fit_residuals = NULL,
    pred_names = NULL,
    r2 = NULL,
    rmse = NULL
  )


  for(i in response_names){
    temp_index = which(response_values == i)
    temp_observed = wine_data$Y[,temp_index]
    temp_predicted = pred_vals[, temp_index, 1]
    temp_residuals = temp_observed - temp_predicted
    temp_pred_names = rep(i, length(temp_observed))

    rss = sum((temp_observed - temp_predicted)^2)
    tss = sum((temp_observed - mean(temp_observed))^2)
    temp_r2 = 1 - rss / tss
    temp_rmse = sqrt(mean((temp_observed - temp_predicted)^2))

    # assignment
    value$observed = c(value$observed, temp_observed)
    value$predicted = c(value$predicted, temp_predicted)
    value$fit_residuals = c(value$fit_residuals, temp_residuals)
    value$pred_names = c(value$pred_names, temp_pred_names)
    value$r2 = c(value$r2, temp_r2)
    value$rmse = c(value$rmse, temp_rmse)
  }

  attr(value, "class") = "wine_predict"
  return(value)
}
