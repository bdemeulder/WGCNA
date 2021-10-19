# remove warnings in the app
options(warn = -1, show.error.messages=FALSE)

# set global variables to be able to load data
r_local <- TRUE
r_sessions <- new.env(parent = emptyenv())
