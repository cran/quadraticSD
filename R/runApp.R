#' @title Visualizing the SD using a Quadratic Curve
#' @description Given a dataset, the user is invited to utilize the Empirical Cumulative 
#'              Distribution Function (ECDF) to guess interactively the mean and the mean 
#'              deviation. Thereafter, using the quadratic curve the user can guess the 
#'              Root Mean Sqaured Deviation (RMSD) and visualize the Standard Deviation (SD).
#'              For details, see Sarkar and Rashid (2019), Have You Seen the Standard Deviaiton?, Nepalese Journal of Statistics, Vol. 3, 1-10 
#' @param data a data vector, ideally of a moderate size (say, 10), to be provided by the user. Adventurous users may increase the size.
#' @import shiny
#' @importFrom utils assignInMyNamespace
#' @import ggplot2
#' @return An interactive shiny application. 
#' @export runApp
#'
#' @examples
#' \donttest{
#'    data <- c(12,13,15,17,20,21,23)
#'    runApp(data)
#'    #end of example
#'  }   
runApp<- function(data){
  assignInMyNamespace("data1",data)
  shiny::runApp(system.file(package = "quadraticSD","app"))
}

#' @title data1: the namespace variable.
#' @description the namespace variable that stores data provided by the user.
#' @export
data1 <- c(1,2,3,4,5)