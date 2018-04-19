AEGIS <- function() {
  path <- paste0(.libPaths()[1],"/AEGIS/data")
  shiny::runApp(path)
}
