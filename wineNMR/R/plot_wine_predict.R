#' Plot the wine prediction
#'
#' Plots the wine prediction
#'
#' @param pred_wine A wine_predict class.
#'
#' @param resid_plot A boolean to plot the residuals vs fitted or the fitted vs the observed.
#'
#' @return A plot of the wine_predict class.
#'
#'
#' @export
#'

plot.wine_predict = function(pred_wine, resid_plot = F, ...){
  library(ggplot2)

  plot_df = data.frame(
    'observed' = pred_wine$observed,
    'residuals' = pred_wine$fit_residuals,
    'fitted' = pred_wine$predicted,
    'Color' = pred_wine$pred_names
  )

  if(resid_plot){
    g = ggplot(plot_df, aes(x = fitted, y = residuals, color = Color)) + geom_point(size = 2) +
      theme(text=element_text(size=20))
  }else{
    g = ggplot(plot_df, aes(x = observed, y = fitted, color = Color)) + geom_point(size = 2) +
      theme(text=element_text(size=20))
  }
  return(g)
}
