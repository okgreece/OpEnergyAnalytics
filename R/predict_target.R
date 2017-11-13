if(getRversion() >= "2.15.1")  utils::globalVariables(c("predict_values","low80","up80"))
#' @title 
#' Predictions
#'  
#' @description
#' Extract and analyze univariate time series data in OpEnergy application http://openergy.okfn.gr
#' 
#' @usage predict_res_target(json, targets2020="target", forward_steps= 5, toJSON= TRUE)
#' 
#' @param json The json text or link with the EU Energy data frame or matrix
#' @param targets2020 The 2020 targets.
#' @param forward_steps The prediction steps
#' @param toJSON If FALSE returns a list object
#' 
#' @details 
#' This function is used to predict eu energy market data used in OpEnergy application
#' 
#' @return A json string with the necessary parameters to visualize the results in OpEnergy application.
#'
#' @author Kleanthis Koupidis
#' 
#' @rdname predict_res_target
#' 
#' @seealso \code{\link[TimeSeries.OBeu]{ts.analysis}}
#' 
#' @importFrom foreach %do%
#' @importFrom stats na.omit
#' @export

predict_res_target <- function(json, targets2020="target", forward_steps= 5, toJSON= TRUE) {
  
  data <- jsonlite::fromJSON(json)
  
  EU=c("EU-28", "Belgium", "Bulgaria", "Czech Republic", "Denmark", "Germany", "Estonia", "Ireland", 
       "Greece", "Spain", "France", "Croatia", "Italy", "Cyprus", "Latvia", "Lithuania", 
       "Luxembourg", "Hungary", "Malta", "Netherlands", "Austria", "Poland", "Portugal",
       "Romania", "Slovenia", "Slovak Republic", "Finland", "Sweden", "United Kingdom")
  
  data = data[data[,"Country"] %in% EU,] 

  data[,colnames(data)!="Country"] = as.data.frame(apply(data[,colnames(data)!="Country"],2,as.numeric))
  
  if ( any(grepl(targets2020, colnames(data))) ) {
    
    target=as.data.frame(data[,c(1,grep(targets2020, colnames(data)))])
    
    data2 = DescriptiveStats.OBeu::nums(data)
    
    data2 = data2[,-grep(targets2020, colnames(data2))]
    
    rownames(data2)=data[,"Country"]
    
  } else {
    
    data = stats::na.omit(data)
    data2 = DescriptiveStats.OBeu::nums(data)
    
    rownames(data2)=data[,"Country"]
    
  }
  
  data2=round(data2,2)
  
  ts.result = apply(data2, 1, TimeSeries.OBeu::ts.analysis, prediction.steps = forward_steps, tojson = FALSE)

  ts.result = purrr::map(ts.result,"forecasts")
  
  ts.result = rlist::list.select(ts.result, data, predict_values, low80, up80)
  
  i=j=NULL
  foreach::foreach(j = 1:length(ts.result) ) %do% {
    
    foreach::foreach(i = 1:length(ts.result[[j]]) ) %do% { 
      
    ts.result[[j]][[i]] = round(ts.result[[j]][[i]],2)
    }}
  
  foreach::foreach(j = 1:length(ts.result) ) %do% {
    ts.result[[j]]$low80 = c(unname(ts.result[[j]]$data[12]),ts.result[[j]]$low80)
    ts.result[[j]]$up80 = c(unname(ts.result[[j]]$data[12]),ts.result[[j]]$up80)
    ts.result[[j]]$predict_values = c(unname(ts.result[[j]]$data[12]),unname(ts.result[[j]]$predict_values))
    ts.result[[j]]$target2020 = target[j,2] # to comment for non targets
    ts.result[[j]]$datain2015 =  ts.result[[j]]$data[12]
    ts.result[[j]]$name =  names(ts.result)[j]
  }
  # extract 2015 data
  for_top_data  = purrr::map(ts.result, "data")
  for_top_data = purrr::map(for_top_data, 12)
  first5data = data.frame(values=utils::head(unlist(for_top_data)[order(unlist(for_top_data), decreasing = T)], 5))
  last5data = data.frame(values=utils::tail(unlist(for_top_data)[order(unlist(for_top_data),decreasing = T)],5))
  # extract future estimations
  for_top_predicted = purrr::map(ts.result, "predict_values")
  for_top_predicted = purrr::map(for_top_predicted, 5)
  first5predicted = data.frame(values=utils::head(unlist(for_top_predicted)[order(unlist(for_top_predicted), decreasing = T)], 5))
  last5predicted = data.frame(values=utils::tail(unlist(for_top_predicted)[order(unlist(for_top_predicted),decreasing = T)],5))
  # add top/last 2015 data in eu component
  ts.result$`EU-28`$first5data = data.frame(Country = rownames(first5data), values = first5data)
  ts.result$`EU-28`$last5data = data.frame(Country = rownames(last5data), values = last5data)
  # add future estimations in eu component
  ts.result$`EU-28`$first5predicted = data.frame(Country = rownames(first5predicted), values = first5predicted)
  ts.result$`EU-28`$last5predicted = data.frame(Country = rownames(last5predicted), values = last5predicted)  

  if (toJSON==TRUE) ts.result2 = jsonlite::toJSON(ts.result, pretty = TRUE)
  
  return(ts.result)  
  
}