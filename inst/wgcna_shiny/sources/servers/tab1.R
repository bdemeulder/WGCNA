##########
## Quit ##
##########

# Ask if sure before closing
observe({
  req(input$quit > 0)
  session$sendCustomMessage(type = 'sure', message = list(value = 'quit'))
})

# close window when answer is yes
output$close_window <- renderUI({
  if(req(input$answer_sure_quit)){
    tags$script("setTimeout(function(){window.close();},100);")
  }
})

# remove file from the environment and stop the app from running when answer is
# yes
observe({
  req(input$answer_sure_quit)
  Sys.sleep(0.2)
  rm(list=c("r_local", 'r_sessions'), envir = .GlobalEnv)
  rm(list=ls())
  stopApp()
})

###########
## Reset ##
###########

# Ask are you sure when clicking reset or quit
observe({
  req(input$reset > 0)
  session$sendCustomMessage(type = 'sure', message = list(value = 'reset'))
})

# reload if answer is yes
output$reset_window <- renderUI({
  if(req(input$answer_sure_reset)){
    tags$script("window.location.reload();")
  }
})

# re-initialize r_data and r_state values on reset when answer is yes
observe({
  req(input$answer_sure_reset)
  r_data  <- init_state(reactiveValues())
  r_state <- list()
  r_sessions[[r_ssuid]] <- list(
    'r_data' = r_data,
    'r_state' = r_state
  )
})

################
## Save state ##
################

# Save r_data and r_state to rda file when save button is pushed from radiant
output$save_session <- downloadHandler(
  filename = function(){paste0('Analysis_State', Sys.Date(), '.rda')},
  content = function(file){
    longProcessStart(session)
    tryCatch(
      saveState(file),
      finally = longProcessStop(session)
    )
  }
)

#########################
## Load previous state ##
#########################

# import previous state into r_data and r_state when rda session files are
# given from radiant
observe({
  inFile <- input$load_session
  if (!is.null(inFile)) {
    isolate({
      tmpEnv <- new.env(parent = emptyenv())
      load(file = inFile$datapath, envir = tmpEnv)

      r_sessions[[r_ssuid]] <- list(
        'r_data' = tmpEnv$r_data,
        'r_state' = tmpEnv$r_state,
        'timestamp' = Sys.time()
      )
      rm(tmpEnv)
    })
  }
})

# reload the app when rda files are given from radiant
output$refreshOnUpload <- renderUI({
  inFile <- input$load_session
  if (!is.null(inFile)) {
    tags$script("window.location.reload();")
  }
})

####################################
## Choose data and preprocess UIs ##
####################################

# choose data UI initialized to r_state$dataset exists from radiant
output$dataset_ui <- renderUI({
  radioButtons(inputId = 'dataset',
               label = 'Datasets',
               choices = c('Female Mice Liver'='fml', 'Male Mice Liver'="mml",
                           'Own Dataset'='own','Load Previous State from .Rda file'='load'),
               selected = state_init('dataset','own'))
})

# upload own files UI if "own" is chosen
output$own_upload <- renderUI({
  if(req(input$dataset == "own")){
    flowLayout(
      fileInput(inputId = 'own_genes',
                label = 'Choose genes expression data',
                accept = c('.txt','.csv','.tsv'),
                width = 'inherit'),
      fileInput(inputId = 'own_traits',
                label = 'Choose trait data',
                accept = c('.txt','.csv','.tsv'),
                width = 'inherit'), style="margin-top: 20px; white-space: nowrap"
    )
  }
})

# upload rda file UI if load session is chosen
output$load_data <- renderUI({
  if(req(input$dataset=="load")){
    flowLayout(
      fileInput(inputId = 'load_session',
                label =  'Load session',
                accept = c('.rda'))
    )
  }
})

# preprocessing UI if own data chosen
output$preprocessing <- renderUI({
  if(req(input$dataset=='own')){
    wellPanel(
      fluidRow(
        column(width = 6,
               radioButtons(inputId = 'missing',
                            label = 'Missing Data',
                            choices = c('impute','keep'),
                            selected = state_init('missing','keep')),
               radioButtons(inputId = 'normalisation',
                            label = 'Normalisation',
                            choices = c('Z-score','none'),
                            selected=state_init('normalisation','none')),
               actionButton(inputId = 'preprocess_btn',
                            label = "Preprocess Data")
        ),
        column(width = 6,
               radioButtons(inputId = 'categories',
                            label = 'Qualitative Data',
                            choices=c('binary','none'),
                            selected=state_init('categories','none'))
        )
      )
    )
  }
})

#################################
## Observe change of datasets ##
#################################
# initiate
datasets_changes <- reactiveValues(value=0)

# observe dataset changes and remove r_data when changing dataset except when
# equal 0 (begin of a session or loaded session)
observeEvent(req(input$dataset), {
  datasets_changes$value <- datasets_changes$value + 1
  if(datasets_changes$value > 1){
    r_data$data_raw <- NULL
    r_data$data <- NULL
    r_data$sampleTree <- NULL
    r_data$traitColors <- NULL
    r_data$METree <- NULL
    r_data$man_param <- NULL
    r_data$net <- NULL
    r_data$merge_modules <- NULL
    r_data$module_initial <- NULL
    r_data$module_current <- NULL
    r_data$sft <- NULL
    r_data$tom_type <- NULL
    r_data$merge <- NULL
    r_data$module <- NULL
    r_data$useAbs <- NULL
    r_data$MEDiss <- NULL
    r_data$color_mes <- NULL
    r_data$moduleTrait <- NULL
    r_data$gsMM <- NULL
    r_data$network_to_show <- NULL
  }
})

####################
## Get data ready ##
####################

# Set object to receive files path when own is chosen
files <- reactiveValues(genes=NULL,traits=NULL)

## Get gene data ##
# Get gene data path when file is given
observe({
  req(input$dataset == "own")
  if(!is.null(input$own_genes)){
    if(file.exists(input$own_genes$datapath)){
      files$genes <- input$own_genes
    }else{
      files$genes <- NULL
    }
  }else{
    files$genes <- NULL
  }
})

# Get and load gene data from files
datExpr <- reactive({
  req(input$dataset)
  if(input$dataset == "fml"){
    isolate({
      if(!exists('fem_mice_liver_datExpr')){
        load(file = 'data/fem_mice_liver_datExpr.Rdata')
      }
      datExpr <- fem_mice_liver_datExpr
      rm('fem_mice_liver_datExpr')
    })
  }else if(input$dataset == "mml"){
    isolate({
      if(!exists('male_mice_liver_datExpr')){
        load(file = 'data/male_mice_liver_datExpr.Rdata') # expression data
      }
      datExpr <- male_mice_liver_datExpr
      rm('male_mice_liver_datExpr')
    })
  }else if(input$dataset == "own"){
    if(!is.null(files$genes)){
      isolate({
        # determine separator and decimal
        fmt <- determine_format(file = files$genes$datapath)
        datExpr <- read.table(file = files$genes$datapath,
                              header=TRUE,
                              row.names=1,
                              sep = fmt$sep,
                              dec = fmt$dec)
		
        # verify that there is only numeric data
        if(!all(sapply(X = datExpr, FUN = function(x){is.numeric(x)}))){
          session$sendCustomMessage(type = 'datExpr_num',
                                    message = list(value = FALSE))
          file.remove(files$genes$datapath)
          tmp <- gregexpr('/', files$genes$datapath)
          unlink(substr(files$genes$datapath, 1, tmp[[1]][length(tmp[[1]])]-1),
                 recursive = T)
          files$genes <- NULL
          return(NULL)
        }
        # remove genes and samples with excessive missing data
        gsg <- goodSamplesGenes(datExpr, verbose=0);
        if (!gsg$allOK){
          datExpr <- datExpr[gsg$goodSamples, gsg$goodGenes]
          session$sendCustomMessage(type = 'missing_data',
                                    message = list(genes = sum(!gsg$goodGenes),
                                                   samples = sum(!gsg$goodSamples)))
        }
        # send message for big matrices
        if(fmt$nvar > 5000){
          session$sendCustomMessage(type = "too_big",
                                    message = list(genes = nvar))
        }
        file.remove(files$genes$datapath)
        tmp <- gregexpr('/', files$genes$datapath)
        unlink(substr(files$genes$datapath, 1, tmp[[1]][length(tmp[[1]])]-1),
               recursive = T)
        files$genes <- NULL
      })
    }else{
      datExpr <- NULL
    }
  }
  return(datExpr)
})

# update r_data to be able to save
observe({
  r_data$data_raw$datExpr <- req(datExpr())
})

## Get trait data ##
# get trait data path when file is given
observe({
  req(input$dataset == "own")
  if(!is.null(input$own_traits)){
    if(file.exists(input$own_traits$datapath)){
      files$traits <- input$own_traits
    }else{
      files$traits <- NULL
    }
  }else{
    files$traits <- NULL
  }
})


# Get and load traits data from files
datTraits <- reactive({
  req(input$dataset)
  if(input$dataset == "fml"){
    isolate({
      if(!exists('fem_mice_liver_datTraits')){
        load(file = 'data/fem_mice_liver_datTraits.Rdata')
      }
      datTraits <- fem_mice_liver_datTraits
      rm('fem_mice_liver_datTraits')
    })
  }else if(input$dataset == "mml"){
    isolate({
      if(!exists('male_mice_liver_datTraits')){
        load(file = 'data/male_mice_liver_datTraits.Rdata')
      }
      datTraits <- male_mice_liver_datTraits
      rm('male_mice_liver_datTraits')
    })
  }else if(input$dataset == "own"){
    if(!is.null(files$traits)){
      isolate({
        # determine separator and decimal
        fmt <- determine_format(file = files$traits$datapath)
        datTraits <- read.table(file = files$traits$datapath,
                                header=TRUE,
                                row.names = 1,
                                sep = fmt$sep,
                                dec = fmt$dec)
        file.remove(files$traits$datapath)
        tmp <- gregexpr('/', files$traits$datapath)
        unlink(substr(files$traits$datapath, 1, tmp[[1]][length(tmp[[1]])]-1),
               recursive = T)
        files$traits <- NULL
      })
    }else{
      datTraits <- NULL
    }
  }
  return(datTraits)
})

# update r_data to be able to save
observe({
  r_data$data_raw$datTraits <- req(datTraits())
})

## Preprocess data if needed ##
# preprocess gene data for own input else keep as is
datExpr_preproc <- reactive({
  req(input$dataset)
  datExpr <- NULL
  if(!is.null(r_data$data_raw$datExpr)){
    datExpr <- r_data$data_raw$datExpr
    if(input$dataset == "own"){
      req(input$preprocess_btn)
      if(input$preprocess_btn > 0){
        isolate({
          if(input$missing == 'impute'){
            tmp <- impute::impute.knn(data = t(datExpr), rowmax = 0.5, colmax = 0.5)
            datExpr <- t(tmp$data)
          }
          if(input$normalisation == 'Z-score'){
            tmp <- apply(X = datExpr, MARGIN = 2, FUN = scale)
            attributes(tmp)$dimnames <- list(row.names(datExpr),colnames(datExpr))
            datExpr <- tmp
          }
		datExpr<-as.data.frame(datExpr)
        })
      }
    }
  }
  return(datExpr)
})

# preprocess traits data for own input else keep as is
datTraits_preproc <- reactive({
  req(input$dataset)
  datTraits <- NULL
  if(!is.null(r_data$data_raw$datTraits)){
    datTraits <- r_data$data_raw$datTraits
    if(input$dataset == "own"){
      req(input$preprocess_btn)
      if(input$preprocess_btn > 0){
        isolate({
          if(input$categories == 'binary'){
            fac <- sapply(X = datTraits, FUN = function(x){is.factor(x)})
            if(any(fac)){
              len_lev_fac <- sapply(X = which(fac), FUN = function(x){length(levels(datTraits[, x]))})
              # Convert manually variable with only 1 factor
              if(any(len_lev_fac == 1)){
                for(i in which(fac)){
                  if(len_lev_fac[i == which(fac)] == 1){
                    lev <- levels(datTraits[, i])
                    levels(datTraits[, i]) <- 1
                    datTraits[, i] <- as.numeric(as.character(datTraits[, i]))
                    names(datTraits)[i] <- paste0(names(datTraits)[i],'_',lev)
                  }
                }
              }
              # Convert all others factors
              tmp <- model.matrix(~ . +0, model.frame(~. ,data = datTraits, na.action=na.pass))
              datTraits <- tmp
            }
          }
        })
      }
    }
  }
  return(datTraits)
})

# reorder clinical data if needed
data <- reactive({
  req(input$dataset != "load")
  datExpr <- datExpr_preproc()
  datTraits <- datTraits_preproc()

  isolate({
    # order and subset data
    if(!is.null(datExpr) & !is.null(datTraits)){
      subject <- row.names(datExpr)
      if(any(subject != row.names(datTraits))){
        a <- "Clinical data subjects don't match expression data subjects.\nReordering clinical data."
        session$sendCustomMessage(type = 'subject_match',
                                  message = list(value = a))
        datTraits <- datTraits[subject, ]
        row.names(datTraits) <- subject
      }
    }
  })
  return(list('datExpr' = datExpr, 'datTraits' = datTraits))
})

# update r_data to be able to save
observe({
  r_data$data$datExpr <- req(data()$datExpr)
})

observe({
  r_data$data$datTraits <- req(data()$datTraits)
})

# keep track of which column are numeric
datTraits_num <- reactive({
  req(r_data$data$datTraits)
  if(!is.null(r_data$data$datTraits)){
    numeric <- sapply(X = r_data$data$datTraits, FUN = function(x){is.numeric(x)})
  }else{
    numeric <- NULL
  }
  return(numeric)
})

# update r_data to be able save
observe({
  r_data$data$datTraits_num <- req(datTraits_num())
})

####################
## Visualize Data ##
####################

## Render data tables ##
# render tables UI
output$datatables_ui <- renderUI({
  fluidRow(id = "datatables",
           column(6,
                  h4(paste0('Table of Gene Expression (100 first / ',
                            tryCatch(ncol(req(r_data$data$datExpr)), error = function(e){""}),
                            ' genes)')),
                  DT::dataTableOutput(outputId = 'datatablegenes',
                                      height = 'auto')
           ),
           column(6,
                  h4(paste0('Table of Clinical Traits (',
                            tryCatch(ncol(req(r_data$data$datTraits)), error = function(e){""}),
                            ' variables)')),
                  DT::dataTableOutput(outputId = 'datatableclinical',
                                      height = 'auto')
           )
  )
})

# render the gene table
output$datatablegenes <- DT::renderDataTable({
  req(input$dataset, r_data$data$datExpr)
  longProcessStart(session) # show loading while rendering
  pt <- proc.time()
  tmp <- tryCatch({
    DT::datatable(data = data.frame('Subject' = row.names(r_data$data$datExpr), r_data$data$datExpr[, 1:100]),
                  extensions = list(ColReorder = NULL, FixedColumns = NULL),
                  options = list(pageLength = 5,
                                 dom = 'R<"clear">lftipr',
                                 scrollX = '100%',
                                 scrollY = '300px',
                                 scrollCollapse = TRUE,
                                 autoWidth = FALSE,
                                 processing = FALSE),
                  rownames = FALSE)},
    error = function(e){NULL})
  pt <- as.numeric((proc.time() - pt)['elapsed'])
  if(input$dataset == 'own' & !is.null(tmp)){
    process_time(pt)
  }
  longProcessStop(session)
  return(tmp)
})


# render the clinical trait table
output$datatableclinical <- DT::renderDataTable({
  req(r_data$data$datTraits, input$dataset)
  longProcessStart(session)
  pt <- proc.time()
  tmp <- tryCatch(
    DT::datatable(data.frame('Subject' = row.names(r_data$data$datTraits), r_data$data$datTraits),
                             extensions = list(ColReorder = NULL, FixedColumns = NULL),
                             options = list(pageLength = 5,
                                            dom = 'R<"clear">lfrtip',
                                            scrollX = '100%',
                                            scrollY = '300px',
                                            scrollCollapse = TRUE,
                                            autoWidth = FALSE,
                                            processing = FALSE),
                             rownames = FALSE),
    error = function(e){NULL})
  pt <- as.numeric((proc.time() - pt)['elapsed'])
  if(input$dataset=='own' & !is.null(tmp)){
    process_time(pt)
  }
  longProcessStop(session)
  return(tmp)
})

## Prepare sample dendrogram and clinical trait figure ##
# Do the sample dendrogram
sampleTree <- reactive({
  req(r_data$data$datExpr)
  sampleTree <- hclust(dist(r_data$data$datExpr), method = "average")
  return(sampleTree)
})

# update the sample dendrogram to be able to save
observe({
  r_data$sampleTree <- req(sampleTree())
})

# Convert traits to a color representation: blue means low, white means medium, red means
# high and grey means missing entry
traitColors <- reactive({
  req(r_data$data$datTraits, r_data$data$datTraits_num)
  tryCatch({
    tmp <- apply(X = r_data$data$datTraits[, r_data$data$datTraits_num],
                 MARGIN = 2,
                 FUN =  function(x){sum(!is.na(x)) >= 2})
    tmp2 <- apply(X = r_data$data$datTraits[, r_data$data$datTraits_num][, tmp],
                  MARGIN = 2,
                  FUN = function(x){sd(x, na.rm = TRUE) > 0})
    traits_tmp <- r_data$data$datTraits[, r_data$data$datTraits_num][, tmp][, tmp2]
    col <- numbers2colors(scale(traits_tmp), signed = TRUE)
    labels <-  colnames(traits_tmp)
    return(list("col" = col, "labels" = labels))
  }, error = function(e){ return(list('col' = NULL, 'labels' = NULL))})
})

# update the traitColors to be able to save
observe({
  r_data$traitColors <- list('col' = req(traitColors()$col),
                             'labels' = req(traitColors()$labels))
})

# Plot the sample dendrogram with clinical traits colors underneath
output$visualize_plot <- renderPlot(width = "auto", height = 800,
  {
    tryCatch({
      # set cex to adapt text size to the number of samples
      cex.text <- min(118 / nrow(r_data$data$datExpr), 2)
      plotDendroAndColors(dendro = r_data$sampleTree,
                          colors = r_data$traitColors$col,
                          groupLabels = r_data$traitColors$labels,
		                      main = "Sample Dendrogram and Trait Heatmap",
		                      marAll = c(1, 6, 3, 1),
		                      cex.dendroLabels = cex.text,
		                      cex.colorLabels = cex.text,
		                      cex.main = 2.4)
    },error = function(e){NULL})
  }
)

## Prepare PCA ##
# Color scheme and PCA rendering UIs
output$pca <- renderUI({
  req(r_data$data$datExpr, !any(is.na(r_data$data$datExpr)))
  if(!is.null(r_data$data$datTraits)){
    fluidRow(
      selectInput(inputId = 'pca_col',
                  label = "Color by",
                  choices = colnames(r_data$data$datTraits),
                  multiple = FALSE,
                  selected = state_init('pca_col', colnames(r_data$data$datTraits)[1])),
      plotlyOutput('visualize_pca_plotly', height = "auto", width = "auto")
    )
  }else{
    fluidRow(
      plotlyOutput('visualize_pca_plotly',height = "auto", width = "auto")
    )
  }
})

# render PCA
output$visualize_pca_plotly <- renderPlotly({
  req(r_data$data$datExpr, !any(is.na(r_data$data$datExpr)))
  # calculate principal components
  pc <- tryCatch({prcomp(r_data$data$datExpr)}, error = function(e){NULL})
  if(!is.null(pc)){
    longProcessStart(session)
    # calculate associated variance
    var <- pc$sdev^2 / sum(pc$sdev^2)

    if(!is.null(r_data$data$datTraits)){
      req(input$pca_col)
      data <- data.frame(pc$x,r_data$data$datTraits)

      p <- plot_ly(data = data,
                   x = ~PC1,
                   y = ~PC2,
                   z = ~PC3,
                   text = row.names(r_data$data$datExpr),
                   type = "scatter3d",
                   mode = "markers",
                   hoverinfo = "text",
                   marker = list(size = 3,
                                 line = list(width = 0.5),
                                 colorbar = list(title = input$pca_col)#,
                                 #showscale = is.numeric(data[, input$pca_col])
                                 ),
                   color = ~get(input$pca_col),
                   projection = list(y = list(opacity = 0.1, show = TRUE),
                                     x = list(opacity = 0.1, show = TRUE),
                                     z = list(opacity = 0.1, show = TRUE)))
    }else{
      p <- plot_ly(data = data.frame(pc$x),
                   x = ~PC1,
                   y = ~PC2,
                   z = ~PC3,
                   text = row.names(r_data$data$datExpr),
                   type = "scatter3d",
                   mode = "markers",
                   hoverinfo = "text",
                   marker = list(size = 3, line = list(width = 0.5)),
                   projection = list(y = list(opacity = 0.1, show = TRUE),
                                     x = list(opacity = 0.1, show = TRUE),
                                     z = list(opacity = 0.1, show = TRUE)))
    }
    # configure layout
    p <- p %>% layout(title = 'Samples PCA',
                      scene = list(aspectmode = "cube",
                                   xaxis = list(title = paste0("PC1 (", round(var[1] * 100, 2), "%)")),
                                   yaxis = list(title = paste0("PC2 (", round(var[2] * 100, 2), "%)")),
                                   zaxis = list(title = paste0("PC3 (", round(var[3] * 100, 2), "%)"))))
    longProcessStop(session)
    p <- p %>% config(modeBarButtonsToRemove = list('sendDataToCloud', 'toImage'))
    p
  }else{
    req(FALSE)
  }
})

#####################
## Download report ##
#####################

## Handler to download report
output$downloadReportDataInput <- downloadHandler(
  filename = 'Report_Data_Input.html',
  content = function(file){
    longProcessStart(session)
    tryCatch({
      src <- normalizePath(path = 'sources/reports/data_input.Rmd')
	    src2 <- normalizePath(path = 'www/Images/etriks.png')
	    src3 <- normalizePath(path = 'www/Images/EISBM.png')
      owd <- getwd()
      setwd(tempdir())
      file.copy(src, 'data_input.Rmd')
	    file.copy(src2, 'etriks.png')
	    file.copy(src3, 'EISBM.png')
      out <- rmarkdown::render(input = 'data_input.Rmd',
                               output_format = rmarkdown::html_document(),
                               quiet = T)
      file.rename(out, file)
      setwd(owd)
    }, finally = longProcessStop(session))
  }
)
