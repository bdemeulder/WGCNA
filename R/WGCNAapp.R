#' Launch WGCNA Shiny App
#' @export
#' @import shiny
#' @import WGCNA
#' @examples
#' \dontrun{WGCNAapp()}
#' 

WGCNAapp <- function(){
  appDir <- system.file("wgcna_shiny",package="WGCNAapp")
  if (appDir == "") {
    stop("Could not find application directory. Try re-installing `WGCNAapp` package.", call. = FALSE)
  }
  options(shiny.maxRequestSize = -1, shiny.launch.browser = TRUE)
  tmpfiles <- dir(tempdir())
  runApp(appDir, quiet = TRUE)
  tmpfilesend <- dir(tempdir())
  wd <- getwd()
  setwd(tempdir())
  unlink(x = setdiff(tmpfilesend,tmpfiles),recursive=TRUE)
  setwd(wd)
} 
