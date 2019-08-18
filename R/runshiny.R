#' @title Run the Shiny application
#' @description Launch a browser for uploading the file
#'   with ICD-10-CM.
#' @export runshiny
#' @examples
#' \dontrun{
#' runshiny()
#' }
#'@import shiny

runshiny <- function() {
  shiny::shinyApp(ui, server)
}