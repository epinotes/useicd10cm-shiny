#' @title Run the Shiny application
#' @description Launch a browser for uploading the file
#'   with ICD-10-CM and adding non-fatal drug overdose indicators.
#' @details 
#' Below are the ICD10-CM Definitions of the Non-Fatal Drug Overdose Indicators
#' 
#'    1.Any drug:
#' 
#'  Any diagnosis of T36-T50
#'  AND a 6th character of 1,2,3, or 4 for T36.9, T37.9, T39.9, T41.4, T42.7, T43.9, T45.9, T47.9, and T49.9,
#'  AND a 7th character of A or missing
#'  
#'    2.Any Opioid:
#'  
#'  Any diagnosis of T40.0X, T40.1X, T40.2X, T40.3X, T40.4X, T40.60, T40.60
#'  AND a 6th character of 1,2,3, or 4
#'  AND a 7th character of A or missing
#'  
#'    3.Heroin:
#'  
#'  Any diagnosis of T40.1X
#'  AND a 6th character of 1,2,3, or 4
#'  AND a 7th character of A or missing
#'  
#'    4.Non-Heroin Opioid:
#'  
#'  Any diagnosis of T40.0X, T40.2X, T40.3X, T40.4X,T40.60, T40.69
#'  AND a 6th character of 1,2,3, or 4
#'  AND a 7th character of A or missing
#'  Then exclude any cases of heroin as defined above
#'  
#'    5.Stimulant:
#'  
#'  Any diagnosis of T40.5X, T43.60, T43.61, T43.62, T43.63, T43.64, T43.69
#'  AND a 6th character of 1,2,3, or 4
#'  AND a 7th character of A or missing
#'  
#'    6.Cocaine:
#'  
#'  Any diagnosis of T40.5X
#'  AND a 6th character of 1,2,3, or 4
#'  AND a 7th character of A or missing
#'  
#'    7.Non-Cocaine Stimulant:
#'  
#'  Any diagnosis of T43.60, T43.61, T43.62, T43.63, T43.64, T43.69
#'  AND a 6th character of 1,2,3, or 4
#'  AND a 7th character of A or missing
#'  Then exclude any cases of cocaine as defined above  
#'  
#'   
#' @source For more details:  
#' \url{https://resources.cste.org/Injury-Surveillance-Methods-Toolkit/}  
#' 
#'   

#' @export runshiny
#' @examples
#' \dontrun{
#' runshiny()
#' }
#'@import shiny

runshiny <- function() {
  shiny::shinyApp(ui, server)
}


