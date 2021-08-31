#' Fit the wine data
#'
#' Fit the wine data using glmnet with the mgaussian family
#'
#' @param wine_data A wine_data class object.
#'
#'
#' @return A wine_fit object containing the glmnet class.
#'
#'
#' @export
#'

fit_wine = function(wine_data){
  library(glmnet)
  fit = cv.glmnet(wine_data$X, wine_data$Y, alpha = 0.5, family = 'mgaussian', nfolds = dim(wine_data$X)[1])
  beta_coefs = fit$glmnet.fit$beta$malicAcid[, which(fit$lambda == fit$lambda.min)]
  percent_removed = length(beta_coefs[beta_coefs == 0]) / dim(wine_data$X)[2]

  value = list(
    'fit' = fit,
    'cvm' = fit$cvm,
    'coefs' = beta_coefs,
    'percent_removed' = percent_removed
  )

  attr(value, "class") = "wine_fit"
  return(value)
}
