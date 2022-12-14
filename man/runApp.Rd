% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runApp.R
\name{runApp}
\alias{runApp}
\title{Visualizing the SD using a Quadratic Curve}
\usage{
runApp(data)
}
\arguments{
\item{data}{a data vector, ideally of a moderate size (say, 10), to be provided by the user. Adventurous users may increase the size.}
}
\value{
An interactive shiny application.
}
\description{
Given a dataset, the user is invited to utilize the Empirical Cumulative 
             Distribution Function (ECDF) to guess interactively the mean and the mean 
             deviation. Thereafter, using the quadratic curve the user can guess the 
             Root Mean Sqaured Deviation (RMSD) and visualize the Standard Deviation (SD).
             For details, see Sarkar and Rashid (2019), Have You Seen the Standard Deviaiton?, Nepalese Journal of Statistics, Vol. 3, 1-10
}
\examples{
\donttest{
   data <- c(12,13,15,17,20,21,23)
   runApp(data)
   #end of example
 }   
}
