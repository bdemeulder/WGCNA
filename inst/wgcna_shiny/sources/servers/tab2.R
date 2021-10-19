#########
## UIs ##
#########

# network type ui
output$network_type_ui <- renderUI({
  wellPanel( 
    flowLayout(
      selectInput(inputId = 'network_type', 
                  label = "Choose Network Type", 
                  choices = c('Unsigned' = "unsigned", 
                              'Signed' = "signed", 
                              "Signed Hybrid" = "signed hybrid"),
                  selected = state_init('network_type', 'unsigned'))
    ),
    flowLayout(
      actionButton(inputId = 'calc_sft', label = 'Compute')
    )
  )
})

# network construction ui
output$network_construction_ui <- renderUI({
  wellPanel(
    HTML("<h3 style='margin-top: 10px'>Compute Network</h3>"),
    flowLayout(
      selectInput(inputId = 'network_construction_method', 
                  label = 'Construction Method', 
                  choices = c('Automatic', 'Manual'), 
                  selected = state_init('network_construction_method', 'Automatic')),
      numericInput(inputId = 'soft_threshold', 
                   label = "Choose Soft Power", 
                   min = 1, 
                   max = 30, 
                   value = state_init('soft_threshold', 6), 
                   step = 0.01),
      uiOutput('ui_tomtype')
    ), 
    flowLayout(
      actionButton(inputId = 'calc_network', label = 'Calculate Network')
    )
  )
})

# Add TOM type choice for unsigned network
output$ui_tomtype <- renderUI({ 
  if(req(input$network_type == 'unsigned')){
    selectInput(inputId = 'tom_type', 
                label = 'Choose TOM Type', 
                choices = c('Unsigned' = 'unsigned', 'Signed' = 'signed'),
                selected = state_init('tom_type', 'unsigned'))     
  }
})

# Add parameter box for manual construction with additional parameters
output$ui_man_param <- renderUI({
  if (req(input$network_construction_method == 'Manual')){
    wellPanel(
      HTML('<h3 style="margin-top: 10px">Detect Modules</h3>'),
      # Additional Parameters for Module Detection
      flowLayout(
        numericInput(inputId = 'minModuleSize', 
                     label = 'Min Module Size', 
                     value = state_init('minModuleSize', 30), 
                     min = 0), 
        numericInput(inputId = 'cutheight', 
                     label = 'Max Joining Height', 
                     value = state_init('cutheight', 0.995), 
                     min = 0, 
                     max = 1, 
                     step = 0.001),
        numericInput(inputId = 'deepSplit', 
                     label = 'Depth of Split', 
                     value = state_init('deepSplit', 2), 
                     min = 0, 
                     max = 4, 
                     step = 1)
      ),
      # Compute initial modules button
      flowLayout(
        actionButton(inputId = 'define_modules', 
                     label = 'Define Initial Modules')
      ),
      # Additional parameter for module merge
      flowLayout(
        numericInput(inputId = 'MEDissThres', 
                     label = 'Threshold to Merge Modules', 
                     value = state_init('MEDissThres', 0.25), 
                     min = 0, max = 1, 
                     step = 0.001)
      ),
      # Merge module button
      flowLayout(
        actionButton(inputId = 'merge_modules', label = 'Merge Close Modules')
      )
    )
  }
})

###################
## Calculate SFT ##
###################
# set possible power
powers <- c(1:30)

# Calculate sfts values for the dataset and network type
sft <- eventReactive(req(input$calc_sft), {
  # Get precomputed sft values
  req(input$dataset, input$network_type)
  if(input$dataset == "fml"){
    load(file = 'data/sfts.Rdata') # pre-computed sfts
    if(input$network_type == "unsigned"){
      tmp <- sft_fem_unsigned
    }else if(input$network_type == "signed"){
      tmp <- sft_fem_signed
    }else if(input$network_type == "signed hybrid"){
      tmp <- sft_fem_signed_hybrid
    }
  }else if(input$dataset == "mml"){
    load(file = 'data/sfts.Rdata') # pre-computed sfts
    if(input$network_type == "unsigned"){
      tmp <- sft_male_unsigned
    }else if(input$network_type == "signed"){
      tmp <- sft_male_signed
    }else if(input$network_type == "signed hybrid"){
      tmp <- sft_male_signed_hybrid
    }
  # If own data calculate values => not available yet
  }else if(input$dataset == 'own'){
    req(r_data$data$datExpr)
    longProcessStart(session)
    pt <- proc.time()
    tmp <- tryCatch(
      expr = {pickSoftThreshold(data = r_data$data$datExpr, 
                                powerVector = powers, 
                                verbose = 0, 
                                networkType = input$network_type)},
      finally = longProcessStop(session)
    )
    pt <- as.numeric((proc.time() - pt)['elapsed'])
    process_time(pt)
  }
  return(tmp)
})

# update r_data to be able to save
observe({
  r_data$sft <- req(sft())
})

#######################
## Choose Soft Power ##
#######################
# Plot the sft figures
output$soft_power_plot <- renderPlot(height = 800, width = "auto", {
  req(r_data$sft)
  plot_soft_threshold(sft = r_data$sft, powers = powers)
})

# render sft download button active
observeEvent(req(r_data$sft),{
  session$sendCustomMessage(type = 'disable', 
                            message = list(value = FALSE, 
                                           div = '#downloadsft', 
                                           parent = FALSE))
})

# Download sft table
output$downloadsft <- downloadHandler(
  filename = function(){'sft.csv'},
  content = function(file){
    longProcessStart(session)
    tryCatch(
      expr = {
        if(!is.null(r_data$sft)){
          write.csv(x = r_data$sft$fitIndices, file = file, row.names = FALSE)
        }else{
          return()
        }
      }, 
      finally = longProcessStop(session)
    )
  }
)

# Update default value of soft power
observe({
  req(r_data$sft)
  tmp <- tryCatch(
    expr = {powers[which.max((-sign(r_data$sft$fitIndices[, 3]) * r_data$sft$fitIndices[, 2]) >= 0.8)]},
    error = function(e){NULL}
  )
  if(!is.null(tmp)){
    updateNumericInput(session, inputId = 'soft_threshold', value = tmp)
  }
})

# initialise downstream analysis quand r_data$sft
observe({
  req(r_data$sft)
  req(input$calc_sft != 0) # not reseting in case of new session ie load
  isolate({
    r_data$net <- NULL
    r_data$METree <- NULL
    r_data$merge_modules <- NULL
    r_data$module_initial <- NULL
    r_data$module_current <- NULL
    r_data$merge <- NULL
    r_data$module <- NULL
    r_data$MEDiss <- NULL
    r_data$color_mes <- NULL
    r_data$moduleTrait <- NULL
    r_data$gsMM <- NULL
    r_data$network_to_show <- NULL
    merge_modules$initial <- NULL
    merge_modules$value <- NULL
    merge_modules$true_merged <- NULL
  })
})

#######################
## Calculate Network ##
#######################

## Set parameters ##
# Set TOM type unsigned otherwise
observe({
  r_data$tom_type
  input$tom_type
  if(!is.null(input$tom_type)){
    r_data$tom_type <- input$tom_type
  }else{
    r_data$tom_type <- "unsigned"
  }
})

# Add help description for manual construction describing additional parameters
output$textManual <- renderUI({
  if(req(input$network_construction_method == 'Manual')){
    includeHTML(path = "www/html_text/network_construction/create_network2.html")
  }
})

# Set default values for Automatic construction method
observe({
  if(!('man_param' %in% names(r_data)) | is.null(r_data$man_param)){
    r_data$man_param$minModuleSize <- 30
    r_data$man_param$MEDissThres <- 0.25
    r_data$man_param$deepSplit <- 2
    r_data$man_param$cutheight <- 0.995
  }else{
    r_data$man_param$minModuleSize <- req(input$minModuleSize)
    r_data$man_param$MEDissThres <- req(input$MEDissThres)
    r_data$man_param$deepSplit <- req(input$deepSplit)
    r_data$man_param$cutheight <- req(input$cutheight)
  }
})

# Keep track of initialisation, merging asked and done 
merge_modules <- reactiveValues()

observe({
  if(('merge_modules' %in% names(r_data)) & !is.null(r_data$merge_modules) & 
     !('true_merged' %in% names(merge_modules))){
    merge_modules$true_merged <-  r_data$merge_modules$true_merged
    merge_modules$value <-  r_data$merge_modules$value
    merge_modules$initial <-  r_data$merge_modules$initial
  }else if(is.null(r_data$merge_modules) & !('true_merged' %in% names(merge_modules))){
    merge_modules$true_merged <-  0
    merge_modules$value <-  0
    merge_modules$initial <-  0
  }
})

# update r_data to be able to save
observe({
  req(merge_modules$initial, merge_modules$value, merge_modules$true_merged)
  r_data$merge_modules
  isolate({
    r_data$merge_modules <- merge_modules
  })
})

# Keep track of value in case something goes wrong
observe({
  if(!('module' %in% names(r_data)) | is.null(r_data$module)){
    r_data$module <- list('current' = NULL, 'initial' = NULL)
  }
})

# Set everything to 0 for Manual, and 1 for Automatic to allow computation
observe(priority = 100,{
  req(r_data$net)
  req(input$calc_network >= 1)
  isolate({
    req(input$network_construction_method)
    if(input$network_construction_method == 'Automatic'){
      merge_modules$value <- 1
      merge_modules$initial <- 1
      merge_modules$true_merged <- 0 # set to 0 no merge happened yet
      r_data$module_current <- NULL
      r_data$module_initial <- NULL
      r_data$module$merge_diss_it <- c()
    }else{
      merge_modules$value <- 0
      merge_modules$initial <- 0
      merge_modules$true_merged <- 0
      r_data$module_current <- NULL
      r_data$module_initial <- NULL
      r_data$module$merge_diss_it <- c()
    }
  })
})

# Increment initial value when define initial module button is pressed, 
# and reset the others
observe({
  req(input$define_modules)
  if(!is.null(input$define_modules)){
    if(input$define_modules != 0){
      isolate({
	      merge_modules$initial <- merge_modules$initial + 1
	      merge_modules$value <- 0
        merge_modules$true_merged <- 0
        r_data$module_current <- NULL
        r_data$module_initial <- NULL
        r_data$module$merge_diss_it <- c()
      })
    }
  }
})

# Increment merge value when merge module button is pressed
observe({
  req(input$merge_modules)
  if(input$merge_modules != 0){
	  isolate({
	      merge_modules$value <- merge_modules$value + 1
	  })
  }
})

# Use absolute correlation or not
observe({
  if(req(input$network_type) %in% c("unsigned")){
      r_data$useAbs <- TRUE
  }else{
      r_data$useAbs <- FALSE
  }
})

## calculate network ##
# calculate network
net <- eventReactive(req(input$calc_network), {
  if( tryCatch(expr = { is.na(input$soft_threshold) |
                        !is.numeric(input$soft_threshold) | 
                        input$soft_threshold < 1 |
                        input$soft_threshold > 30 }, 
               error = function(e){TRUE})){
    a <- '\nSoft power parameter should be a numeric value between 1 and 30.'
    session$sendCustomMessage(type = 'invalid', message = a)
    updateNumericInput(session, inputId = 'soft_threshold', value = 6)
    return(NULL)
  }else{
    pt <- proc.time()
    longProcessStart(session)
    tmp <- tryCatch(
      expr = {create_network(datExpr = req(r_data$data$datExpr), 
                             power = input$soft_threshold,
                             network_construction_method = input$network_construction_method,
                             network_type = input$network_type, 
                             man_param = req(r_data$man_param),
                             tom_type = req(r_data$tom_type))},
      error = function(e){NULL})
    longProcessStop(session)
    pt <- as.numeric((proc.time() - pt)['elapsed'])
    process_time(pt)
    return(tmp)
  }
})

# update r_data to be able to save
observeEvent(req(net()),{
  r_data$net <- net()
})

# initialise downstream analysis quane r_data$net change
observe(priority = 1000, {
  req(r_data$net)
  req(input$calc_network != 0) # not reseting in case of new session ie load
  isolate({
    r_data$METree <- NULL
    r_data$merge_modules <- NULL
    r_data$module_initial <- NULL
    r_data$module_current <- NULL
    r_data$merge <- NULL
    r_data$module <- NULL
    r_data$MEDiss <- NULL
    r_data$color_mes <- NULL
    r_data$moduleTrait <- NULL
    r_data$gsMM <- NULL
    r_data$network_to_show <- NULL
    merge_modules$initial <- NULL
    merge_modules$value <- NULL
    merge_modules$true_merged <- NULL
  })
})

## Calculate modules ##
# Get initial modules
initial_mod <- reactive({
  req(merge_modules$initial != 0)
  isolate({
    req(r_data$data$datExpr, 
        input$soft_threshold, 
        input$network_construction_method, 
        r_data$man_param, 
        r_data$net)
    
    if(tryCatch(expr = { is.na(r_data$man_param$minModuleSize) | 
                         !is.numeric(r_data$man_param$minModuleSize) | 
                         r_data$man_param$minModuleSize < 0}, 
                error = function(e){TRUE})){
      a <- '\nMin module size parameter should be a positive numeric value.'
      session$sendCustomMessage(type = 'invalid', message = a)
      updateNumericInput(session, inputId = 'minModuleSize', value = 30)
      return(NULL)
    }
    if(tryCatch(expr = { is.na(r_data$man_param$cutheight) | 
                         r_data$man_param$cutheight < 0 | 
                         r_data$man_param$cutheight > 1 },
                error = function(e){TRUE})){
      a <- '\nMax joining height parameter should be a numeric value between 0 and 1.'
      session$sendCustomMessage(type = 'invalid', message = a)
      updateNumericInput(session, inputId = 'cutheight', value = 0.995)
      return(NULL)
    }
    if(tryCatch(expr = { is.na(r_data$man_param$deepSplit) | 
                         !r_data$man_param$deepSplit %in% c(0, 1, 2, 3, 4) },
                error = function(e){TRUE})){
  	  a <- '\nDepth of split parameter should be 0, 1, 2, 3 or 4.'
  	  session$sendCustomMessage(type = 'invalid', message = a)
  	  updateNumericInput(session, inputId = 'deepSplit', value = 2)
  	  return(NULL)
    }
    
    tmp <- tryCatch(
      expr = { initial_modules(datExpr = r_data$data$datExpr, 
                               power=input$soft_threshold,
                               network_construction_method = input$network_construction_method,
                               man_param = r_data$man_param, 
                               net = r_data$net)}, 
      error = function(e){NULL})
    return(tmp)
  })
})

# update r_data to be able to save
observeEvent(req(initial_mod()),{
  r_data$module_initial <- initial_mod()
})

# merge modules
merge<-reactive({
  req(r_data$module_initial$dynamicColors, merge_modules$value)
  isolate({
    if(merge_modules$value != 0){
      req(merge_modules$true_merged, r_data$data$datExpr) 
      req(input$network_construction_method, r_data$man_param)
      if( tryCatch( 
            expr = { is.na(r_data$man_param$MEDissThres) | 
                     r_data$man_param$MEDissThres < 0 | 
                     r_data$man_param$MEDissThres > 1 },
            error = function(e){TRUE})){
        a <- '\nThreshold to merge modules parameter should be a numeric value between 0 and 1.'
        session$sendCustomMessage(type = 'invalid', message = a)
        updateNumericInput(session, inputId = 'MEDissThres', value = 0.25)
        return(NULL)
      }
      # for the first use initial modules definition
      if(merge_modules$true_merged == 0){
        req(r_data$module_initial$dynamicColors)
        color <- r_data$module_initial$dynamicColors
      # else use previous merge iteration
      }else if(merge_modules$true_merged >= 1){ 
        req(r_data$module_current$moduleColors)
        color <- r_data$module_current$moduleColors
      }
      
      merge_new <- tryCatch(
        expr = { mergeModules(datExpr = r_data$data$datExpr, 
                              useAbs = r_data$useAbs,
                              network_construction_method = input$network_construction_method,
                              colors = color,
                              man_param = r_data$man_param)},
        error = function(e){NULL})
      
      # r_data$merge <- merge_new
      return(req(merge_new))
    }
  })
})

# update r_data to be able to save
observeEvent(req(merge()),{
  isolate({
    r_data$module_current <- merge()
    r_data$module$merge_diss_it <- c(r_data$module$merge_diss_it, r_data$man_param$MEDissThres)
    merge_modules$true_merged <- merge_modules$true_merged + 1
  })
})

# Calculate dissimilarity between module eigengenes   
MEDiss <-reactive({
  # use initial definition if no merged done
  req(r_data$merge_modules$true_merged, r_data$merge_modules$initial)
  tryCatch(
    expr = { 
      ME <- NULL
      if(r_data$merge_modules$true_merged == 0 & r_data$merge_modules$initial != 0){
        ME <- req(r_data$module_initial$MEs$eigengenes)
        # use merge definition if already done
      }else if(r_data$merge_modules$true_merged >= 1){
        ME <- req(r_data$module_current$MEs)
      }
      # calculate correlation between eigengenes
      tmp <- NULL
      if(ncol(ME) > 1){
        if(r_data$useAbs){
          tmp <- 1 - abs(cor(ME))
        }else {
          tmp <- 1 - cor(ME)
        }
      }
      return(tmp)
    },
    error = function(e){ return(NULL) }
  )
})

# Update r_data to be able to save
observeEvent(req(MEDiss()),{
  r_data$MEDiss <- MEDiss()
})

# Calculate eigengenes dendrogram
METree <- eventReactive(req(r_data$MEDiss), {
  tmp <- hclust(as.dist(r_data$MEDiss), method = "average")
  return(tmp)
})

# update r_data to be able to save
observeEvent(req(METree()), {
  r_data$METree <- METree()
})

## Plot Figures ##
# plot module eigengene dendrogram
output$METree_plot <- renderPlot(
  height = function() {
    ifelse(tryCatch({length(r_data$METree$height) > 0}, error = function(e){FALSE}), 1000, 0)
  },
  width = "auto",
  {
    tryCatch({
      req(r_data$METree)
      if(length(r_data$METree$height) > 0){
        par(mar = c(5.1, 5.1, 4.1, 2.1))
        plot(x = r_data$METree, 
             hang = -1, 
             main = "Clustering of module eigengenes", 
             cex = 2,
             cex.main = 2,
             cex.axis = 2,
             cex.lab = 2,
             cex.sub = 2,
             xlab = "",
             sub = "")
        if(input$network_construction_method == 'Manual'){
          if(is.numeric(r_data$man_param$MEDissThres)){
            abline(h = r_data$man_param$MEDissThres, col = "red")
          }
        }
      }
    }, error=function(e){NULL})
  }
)

# plot gene dendrogram with module colors
output$network_dendrogram <- renderPlot(height = 1000, width = "auto", {
  req(r_data$net)
  tryCatch({
    if(r_data$merge_modules$true_merged >= 1){
      color <- cbind(r_data$module_initial$dynamicColors,  
                    r_data$module_current$moduleColors)
      gp_lab <- c("Initial Modules", "Merged Modules")
    }else if (r_data$merge_modules$true_merged == 0 & r_data$merge_modules$initial!= 0){
      color <- r_data$module_initial$dynamicColors
      gp_lab <- "Initial Modules"
    }else{
      color <- NULL
      gp_lab <- NULL
    }
    
    plotDendroAndColors(dendro = r_data$net$geneTree, 
                        colors = color,
                        groupLabels = gp_lab,
                        cex.colorLabels = 1, 
                        cex.main = 2, 
                        cex.axis = 1, 
                        cex.lab = 2,
                        dendroLabels = FALSE, 
                        hang = 0.03,
                        addGuide = TRUE, 
                        guideHang = 0.05, 
                        abHeight = r_data$man_param$cutheight, 
                        marAll = c(1, 7, 3, 1))
    }, error = function(e){NULL})
  }
)

## Downloads ##
# prepare module assignment table
moduleassignment <- reactive({
  req(r_data$merge_modules$true_merged, r_data$data$datExpr, r_data$module_initial$dynamicColors)
  if(r_data$merge_modules$true_merged >= 1){
    req(r_data$module_current$moduleColors)
    assigned <- tryCatch({data.frame('Features' = names(r_data$data$datExpr), 
                                     "Initial Module" = r_data$module_initial$dynamicColors, 
                                     "Merged Module" = r_data$module_current$moduleColors)},
                         error = function(e){NULL})
  }else if (r_data$merge_modules$true_merged ==0){
    assigned <- tryCatch({data.frame('Features' = names(r_data$data$datExpr), 
                                     "Initial Module" = r_data$module_initial$dynamicColors)}, 
                         error = function(e){NULL})
  }
  return(list("assignment" = assigned))
})

# download module assignment table
output$downloadmoduleassignment <- downloadHandler(
  filename = function(){'module_assignment.csv'},
  content = function(file){
    longProcessStart(session)
    tryCatch(
      write.csv(x = moduleassignment()$assignment, file = file, row.names = FALSE), 
      finally = longProcessStop(session), 
      error = function(e){return()}
    )
  }
)

# download report
output$downloadReportNetworkConstruction <- downloadHandler(
  filename = 'Report_Network_Construction.html',
  content = function(file) {
    longProcessStart(session)
    tryCatch({
      src <- normalizePath(path = 'sources/reports/network_construction.Rmd')
	  src2 <- normalizePath(path = 'www/Images/etriks.png')
	  src3 <- normalizePath(path = 'www/Images/EISBM.png')
      owd <- getwd()
      setwd(tempdir())
      file.copy(src,'network_construction.Rmd')
	  file.copy(src2, 'etriks.png')
	  file.copy(src3, 'EISBM.png')
      out <- rmarkdown::render('network_construction.Rmd',rmarkdown::html_document(), quiet = TRUE)
      file.rename(out, file)
      setwd(owd)
    }, finally = longProcessStop(session))
  }
)
