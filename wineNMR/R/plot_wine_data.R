#' Plot wine data
#'
#' Plots the wine data based on the input parameters
#'
#' @param wine_data A wine_data class object.
#'
#' @param plot_type A string choice of the plot type.  Options include: map, dendrogram, correlation
#'
#' @return A plot of the wine_data class.
#'
#'
#' @export
#'

plot.wine_data = function(wine_data, plot_type = NULL, ...){


  if(is.null(plot_type)){
    library(plotly)

    plot_df = data.frame(
      'NMR' = as.numeric(wine_data$X_unscaled[1:3,]),
      'ppm' = rep(wine_data$ppm, 3),
      'Country' = rep(c('Germany', 'France', 'Italy'), each = dim(wine_data$X)[2])
    )

    p = plot_ly(plot_df, x = ~ppm, y = ~NMR, color = ~Country, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = 'Parts Per Million'), yaxis = list(title = 'Nuclear Magnetic Resonance'))

    return(p)


    ############################
  }else if(plot_type == 'map'){
    library(leaflet)

    wine_labels = wine_data$wine_labels
    unique_country = unique(wine_labels$Country)
    unique_lat = rep(0, length(unique_country))
    unique_long = unique_lat
    wine_types = unique_lat
    for(i in 1:length(unique_country)){
      sub_df = wine_labels[which(wine_labels$Country == unique_country[i]), ]
      # only selecting the first one, could be replaced later by a jitter
      unique_lat[i] = sub_df$Lat[1]
      unique_long[i] = sub_df$Long[1]
      wine_types[i] = paste0(unique_country[i], ": ", paste(sub_df$Type, collapse = ', '))
    }

    m <- leaflet() %>%
      addTiles() %>%
      addMarkers(lat=unique_lat, lng=unique_long, popup=wine_types)

    return(m)


    ############################
  }else if(plot_type == 'dendrogram'){
    library(protoclust)

    temp_dendrogram = protoclust(dist(scale(wine_data$X)))

    return(plot(temp_dendrogram))

    ############################
  }else if(plot_type == 'correlation'){
    library(corrplot)

    response_cor = cor(wine_data$Y)

    return(corrplot(response_cor, method = 'color', type = 'upper', order = 'hclust'))
  }




}
