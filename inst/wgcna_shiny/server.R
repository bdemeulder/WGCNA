shinyServer( # run once when app is launched
  function(input, output, session){ # each time a user visits the app
    # source various button and input observer to launch alert or change pages
    # disabling buttons ...
    source(file = 'sources/servers/input_observers.R', local = TRUE)$value
    
    # source fucntions needed and initialise the app
    source('functions.R', local = TRUE)
    source(file = 'init.R', local = TRUE)$value
  
    # Data Input (tab1) 
    source(file = 'sources/servers/tab1.R', local = TRUE)$value
    
    # Network construction (tab2) 
    source(file = 'sources/servers/tab2.R', local = TRUE)$value
    
    # Module Trait relationship (tab3) 
    source(file = 'sources/servers/tab3.R', local = TRUE)$value
})
