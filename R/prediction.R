#' @title 
#' Read and analyze univariate time series data for EU datathon
#'  
#' @description
#' Extract and analyze univariate time series data for EU datathon
#' 
#' @usage predict_onrg(json_data, time="years", amount="values", order=NULL,
#'  prediction_steps= 5, tojson= TRUE)
#' 
#' @param json_data The json text or link with the EU Energy data frame or matrix
#' @param time Time label of the json time series data
#' @param amount Amount label of the json time series data
#' @param order An integer vector of length 3 specifying the order of the Arima model
#' @param prediction_steps The number of prediction steps
#' @param tojson If FALSE returns a list object
#' 
#' @details 
#' This function is used to predict eu energy statistics/market data in OpEnergy application.
#' 
#' @return A json string with the resulted parameters of the ts.analysis function.
#'
#' @author Kleanthis Koupidis
#' 
#' @rdname predict_onrg
#' @import stats
#' @export


predict_onrg <- function(json_data, time="years", amount="values", order=NULL, prediction_steps= 5, tojson= TRUE) {
  
  data <- jsonlite::fromJSON(json_data)
  
  data=data.frame(amount = data[paste0(amount)],time = data[paste0(time)])
  
  df<-as.data.frame(data)
  
  df<-df[order(df[,paste(time)],decreasing=F),]
  
  tsdata <- stats::ts(df[,paste0(amount)],start=min(df[,paste(time)]),end=max(df[,paste(time)]))
  tsdata <- stats::na.omit(tsdata)
  
  ts.result <- TimeSeries.OBeu::ts.analysis(tsdata, x.order=order ,prediction_steps = prediction_steps, tojson = F)
  

  ts.result$forecasts$predict_values = c(utils::tail(ts.result$forecasts$data,1),unname(ts.result$forecasts$predict_values))

  
  ts.result= list(
    prediction = ts.result$forecasts$predict_values,
    low.interval = unname(ts.result$forecasts$low80),
    up.interval = unname(ts.result$forecasts$up80)
  )
  
  if (tojson==TRUE) ts.result = jsonlite::toJSON(ts.result)
  
  return(ts.result)  
}
