## change tab when pushing buttons
observeEvent(input$GoTab1, {
  updateTabsetPanel(session = session, 
                    inputId = "myTabsetPanel", 
                    selected = 'Data Input')
})

observeEvent(input$GoTab2, {
  updateTabsetPanel(session = session, 
                    inputId = "myTabsetPanel", 
                    selected = "Network Construction")
})
        
observeEvent(input$BackTab0, {
  updateTabsetPanel(session = session, 
                    inputId = "myTabsetPanel", 
                    selected = "Introduction")
})

observeEvent(input$GoTab3, {
  updateTabsetPanel(session = session, 
                    inputId = "myTabsetPanel", 
                    selected = 'Module - Trait Relationship')
})

observeEvent(input$BackTab1, {
  updateTabsetPanel(session = session, 
                    inputId = "myTabsetPanel", 
                    selected = 'Data Input')
})

observeEvent(input$BackTab2, {
  updateTabsetPanel(session = session, 
                    inputId = "myTabsetPanel", 
                    selected = "Network Construction")
})

## disable download buttons 
observe({ 
  if(tryCatch(is.null(r_data$sft), error = function(e){TRUE})){
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = TRUE, 
                                             div = '#downloadsft', 
                                             parent = FALSE))
  }else{
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = FALSE, 
                                             div = '#downloadsft', 
                                             parent = FALSE))
  }
})

observe({ 
  if(tryCatch(is.null(moduleassignment()$assignment), error = function(e){TRUE})){
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = TRUE, 
                                             div = '#downloadmoduleassignment', 
                                             parent = FALSE))
  }else{
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = FALSE, 
                                             div = '#downloadmoduleassignment',
                                             parent=FALSE))
  }
})

observe({ 
  if(tryCatch(is.null(r_data$moduleTrait), error = function(e){TRUE})){
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = TRUE, 
                                             div = '#downloadmoduletrait', 
                                             parent = FALSE))
  }else{
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = FALSE, 
                                             div = '#downloadmoduletrait', 
                                             parent = FALSE))
  }
})

observe({ 
  if(tryCatch(is.null(r_data$gsMM), error = function(e){TRUE})){
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = TRUE, 
                                             div = '#downloadgsmm', 
                                             parent = FALSE))
  }else{
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = TRUE, 
                                             div = '#downloadgsmm', 
                                             parent =FALSE))
  }
})

observe({
  if(tryCatch(is.null(r_data$network_to_show), error = function(e){TRUE})){
      session$sendCustomMessage(type = 'disable', 
                                message = list(value = TRUE, 
                                               div = '#downloadshownnetwork', 
                                               parent = FALSE))
  }else{
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = FALSE, 
                                             div = '#downloadshownnetwork', 
                                             parent = FALSE))
  }
})

observe({
  if(tryCatch(is.null(r_data$net), error = function(e){TRUE})){
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = TRUE, 
                                             div = '#downloadwholenetwork',
                                             parent = FALSE))
  }else{
    session$sendCustomMessage(type = 'disable', 
                              message = list(value = FALSE, 
                                             div = '#downloadwholenetwork', 
                                             parent = FALSE))
  }
})

## Other alerts
# alert if only 3 or less modules are defined
observe(priority=100,{
  if(tryCatch({dim(as.matrix(MEDiss()))[1] <= 3}, error = function(e){FALSE})){
    session$sendCustomMessage(type = 'only_one_module', 
                              message = list(a = MEDiss()))
  }
})


