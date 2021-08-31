#' Load the wine data
#'
#' Loads the wine data given a directory location.  After which, the data
#' colnames and rownames are assigned based on the wine_labels.  Both the
#' responses (Y) and features (X) are normalized by their column wise mean and
#' standard deviation.
#'
#' @param dir_path The folder containing the NMR_40wines.mat and wine_labels.csv
#'
#' @return A wine_data class with the wine data.
#'
#'
#' @export
#'

load_wine = function(dir_path){

  data = R.matlab::readMat(paste0(dir_path,'/NMR_40wines.mat'))
  wine_labels = read.csv(paste0(dir_path,'/wine_labels.csv'))

  X = data$X
  ppm = as.numeric(data$ppm)
  rownames(X) = wine_labels$Country

  # response information
  Y = data$Y
  y_labels = rep('a', length(data$Label))
  for(i in 1:length(data$Label)){
    y_labels[i] = unlist(data$Label[[i]])
  }
  colnames(Y) = y_labels

  value = list(
    'X' = scale(X),
    'Y' = scale(Y),
    'X_unscaled' = X,
    'Y_unscaled' = Y,
    'ppm' = ppm,
    'wine_labels' = wine_labels
  )

  attr(value, "class") = "wine_data"
  return(value)
}
