AEGIS <- function() {
  path <- paste0(.libPaths()[1],"/AEGIS")
  shiny::runApp(path)
}
