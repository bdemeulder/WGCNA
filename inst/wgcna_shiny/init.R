## Function taken fom radiant package from radiant package

# Intialise part of r_data when opening a new session, such as plot width, ...
# not needed here but kept in case
init_state <- function(r_data) {
  r_data
}

# Get previous SSUID from the url 
isolate({
  prevSSUID <- parseQueryString(session$clientData$url_search)[["SSUID"]]
})

# Set the session id (from Joe Cheng's shiny-resume)
r_ssuid <-
  if (r_local) {
    if (is.null(prevSSUID)) {
      paste0("local-",shiny:::createUniqueId(3))
    } else {
      mrsf <- "0000"
      prevSSUID
    }
  } else {
    ifelse (is.null(prevSSUID), shiny:::createUniqueId(5), prevSSUID)
  }

# (Re)start the session and push the id into the url
session$sendCustomMessage("session_start", r_ssuid)

# Load for previous state if available but look in global memory first
if (exists("r_state") && exists("r_data")) {
  r_data  <- do.call(reactiveValues, r_data)
  r_state <- r_state
  rm(r_data, r_state, envir = .GlobalEnv)
} else if (!is.null(r_sessions[[r_ssuid]]$r_data)) {
  r_data  <- do.call(reactiveValues, r_sessions[[r_ssuid]]$r_data)
  r_state <- r_sessions[[r_ssuid]]$r_state
} else {
  r_data  <- init_state(reactiveValues())
  r_state <- list()
}

# identify the current shiny environment
r_env <- environment()



