## Function to determine sep and dec from input text files ##
determine_format <- function(file){
  # read first 2 lines
  l1 <- readLines(con = file,n = 2)
  # determine most common separator in header
  if (stringr::str_count(l1[1], '\"') > 0){
    count <- c(stringr::str_count(l1[1], '\" \"'),  # count number of " "
               stringr::str_count(l1[1], '\"\"'),   # count number of ""
               stringr::str_count(l1[1], '\",\"'),  # count number of ","
               stringr::str_count(l1[1], stringr::fixed('\".\"')), # count number of "."
               stringr::str_count(l1[1], '\"\t\"'), # count number of "\t"
               stringr::str_count(l1[1], '\";\"'))  # count number of ";"
    sep <- c(" ", "", ",", ".", "\t", ";")[which.max(count)]
  }else{
    count <- cbind(stringr::str_count(l1, " "),  # count number of space
                   stringr::str_count(l1, ","),  # count number of ,
                   stringr::str_count(l1, stringr::fixed(".")), # count number of .
                   stringr::str_count(l1, "\t"), # count number of \t
                   stringr::str_count(l1, ";"))  # count number of ;
    # compare ligne 1 and 2 (they should have the same number of separators)
    sep <- c(" ", ",", ".", "\t", ";")[which.max(count[1,] * as.integer(count[1, ] == count[2, ]))]
  }
  # read second line knowning the separator
  l2 <- scan(file = file, what = character(), nlines = 1, 
             quote = "", sep = sep, skip = 1)
  # determine most common decimal  in the first line
  count <- c(sum(stringr::str_count(l2, '[:digit:],[:digit:]'), na.rm = T), # count number of ,
             sum(stringr::str_count(l2, '[:digit:][.][:digit:]'), na.rm = T)) # count number of .
  dec <- c(",", ".")[which.max(count)] 
  
  return(list('sep' = sep, 'dec'= dec, 'nvar'=length(l2)))
}

## Function to show the loading div when begining a long calculus ##
longProcessStart <- function(session) {
  session$sendCustomMessage(type = 'showWaitMessage', 
                            message = list(value = TRUE))
}

## Function to hide the loading div when finishing a long calculus ##
longProcessStop <- function(session) {
  session$sendCustomMessage(type = 'showWaitMessage', 
                            message = list(value = FALSE))
}

## Function to render processing time ##
process_time <- function(pt){
  if (pt > 1){
    pt <- round(pt)
    pt <- c(((pt %/% 60) %/% 60) %/% 24, ((pt %/% 60) %/% 60) %% 24, (pt %/% 60) %% 60, pt %% 60)
    pt2 <- pt[pt != 0]
    pt3 <- c('day', 'hour', 'minute', 'second')[pt != 0]
    pt3[pt2 > 1] <- paste0(pt3[pt2 > 1],"s")
    pt4 <- paste(pt2, pt3, sep = ' ', collapse = ' ')
    session$sendCustomMessage('time', message = list(value = pt4))
  }
}

## Functions take from radiant package ##
# Function to save input state and data by converting it to normal R objects
saveState <- function(filename) {
  isolate({
    # Tranfom reactive inputs to normal list
    LiveInputs <- reactiveValuesToList(input)
    r_state[names(LiveInputs)] <- LiveInputs
    # Tranfom reactive values to normal list
    r_data <- reactiveValuesToList(r_data)
    # save both
    save(r_state, r_data , file = filename)
  })
}

# Functions to initialize input to their loaded value if it exists
state_init <- function(var, init = "") {
  isolate({
    ivar <- input[[var]]
    if (var %in% names(input)) {
      ivar <- input[[var]]
      if (is.null(ivar)){
        r_state[[var]] <<- NULL
      }
    } else {
      ivar <- .state_init(var, init)
    }
    ivar
  })
}

.state_init <- function(var, init = "") {
  rs <- r_state[[var]]
  if (is_empty(rs)){
    init
  }else{
    rs
  }
}

is_empty <- function(x, empty = ""){
  if (length(x) == 0 || is.na(x) || x == empty){
    TRUE
  }else{
    FALSE
  }
}

## Function for ploting the sft with the 4 plots ##
plot_soft_threshold <- function(sft, powers){
  # columns of the sft matrix to be plotted and their names
  plotCols <- c(2, 5, 6, 7)
  colNames <- c("Scale Free Topology Model Fit", 
                "Mean Connectivity", 
                "Median Connectivity", 
                "Max Connectivity")
  sft_mat <- sft$fitIndices[, plotCols]
  sft_mat[, 1] <- -sign(sft$fitIndices[, 3]) * sft_mat[,1]
  
  # setting ylim for each plot
  ylim <- matrix(NA, nrow = 2, ncol = 4)
  for(col in 1:length(plotCols)){
    ylim[1, col] <- min(ylim[1, col], sft_mat[, col], na.rm=T)
    ylim[2, col] <- max(ylim[2, col], sft_mat[, col], na.rm=T)
  }
  
  # defining a 2x2 plot and specify margins
  par(mfcol = c(2,2), mar=c(5.1, 5.1, 4.1, 2.1));
  
  # add each individual plot
  for(col in 1:length(plotCols)){
    # adding an emply plot with title and labels
    plot(x = sft$fitIndices[, 1], 
         xlab = "Soft Threshold (Power)", 
         ylab = colNames[col], 
         type = "n", # not plotting the curve
         ylim = ylim[, col], 
         main = colNames[col],
         cex.main = 2, cex.axis = 2, cex.lab = 2)
    addGrid()
    
    # get the first value >= 0.8 in red
    tmp <- which.max(sft_mat[, 1] >= 0.8)
    
    # for the first plot writing in adding a horizontal line for representing 
    # a fit of 0.8
    if(col == 1){
      abline(h = 0.8, col = 'red')
    }
    
    # adding power value instead of points
    text(x = sft$fitIndices[, 1], 
         y = sft_mat[, col], 
         labels = powers, 
         col = replace(rep('black', length(powers)), tmp, 'red'), 
         cex=1)
  }
}

## Function to create the network ##
# returning simultaneously adjacency, TOM, dissTOM, and geneTree
create_network <- function(datExpr, power, network_construction_method, 
                           network_type, man_param, tom_type){
  if(network_construction_method %in% c('Automatic','Manual')){
    # Calculate adjacency
    adjacency <- adjacency(datExpr = datExpr, power = power, type = network_type)
    
    # Caculate TOM
    TOM <- TOMsimilarity(adjacency, TOMType = tom_type, verbose = 0)

    # Calculate TOM dissimilarity  
    dissTOM <- 1 - TOM
    
    # Calculate geneTree
    geneTree <- hclust(as.dist(dissTOM), method = "average")
  }else if(network_construction_method == 'Block'){
    # add Block wise computation?
  }
  
  return(list("adjacency" = adjacency,
              "TOM" = TOM,
              "dissTOM" = dissTOM,
              "geneTree" = geneTree))
}

## Function to define initial modules ##
# return simulatneously dynamicsMods, dynamicColors and MEs
initial_modules <- function(datExpr, power, network_construction_method, 
                            net, man_param){
  if(network_construction_method %in% c('Automatic','Manual')){
    # Calculate modules
    dynamicMods <-  cutreeDynamic(dendro = net$geneTree, 
                                  distM = net$dissTOM, 
                                  cutHeight = man_param$cutheight,
                                  deepSplit = man_param$deepSplit,
                                  pamRespectsDendro = FALSE, # from tuto 1
                                  minClusterSize = man_param$minModuleSize,
                                  verbose = 0)
    # Convert modules to colors
    dynamicColors <- labels2colors(dynamicMods)
    # Calculate modules eigengenes
    MEList <- moduleEigengenes(expr = datExpr, 
                               colors = dynamicColors, 
                               softPower = power)
  }else if(network_construction_method=='Block'){
    # add Block wise computation?
  }
  
  return(list("dynamicMods" = dynamicMods, 
              "dynamicColors" = dynamicColors,
              "MEs" = MEList))
}

## Function to merge modules ##
# return simultaneously moduleColors, moduleLabels and MEs
mergeModules <- function(datExpr, useAbs, network_construction_method, 
                         colors, man_param){
  # Do automatically iterations or not
  if(network_construction_method == 'Automatic'){
    it <- TRUE
  }else if(network_construction_method == 'Manual'){
    it <- FALSE
  }
  # Calculate merged modules
  tmp <- mergeCloseModules(exprData = datExpr, 
                           colors = colors,
                           cutHeight = man_param$MEDissThres, 
                           verbose = 0, 
                           useAbs = useAbs, 
                           iterate = it)
  moduleColors <- tmp$colors
  colorOrder <- c("grey", standardColors(50));
  moduleLabels <- match(moduleColors, colorOrder)-1
  MEs <- tmp$newMEs
    
  return(list("moduleColors"=moduleColors,"moduleLabels"=moduleLabels,"MEs"=MEs))
}

## Function to cross clinical traits with modules ##
# returning simultaneously modNames, geneModuleMembership, MMPvalue, 
# geneTraitSignificance, GSPvalue
gene_relationship <- function(datTraits, datExpr, MEs){
  # Get modules Names
  modNames <- substring(names(MEs), 3)
  # Calculate gene module membership (pearson correlation of gene and eigengene)
  geneModuleMembership <- as.data.frame(cor(datExpr, MEs, use = "p"));
  names(geneModuleMembership) <- paste("MM", modNames, sep="");
  # Calculate associated p-value
  MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), nrow(datExpr)));
  names(MMPvalue) <- paste("p.MM", modNames, sep="");
  # Calculate gene-trait significance (pearson correlation of gene and trait)
  geneTraitSignificance <- as.data.frame(cor(datExpr, as.data.frame(datTraits), use = "p"));
  names(geneTraitSignificance) <- paste("GS.", colnames(datTraits), sep="");
  # Calculate associated p-value
  GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance), nrow(datExpr)));
  names(GSPvalue) = paste("p.GS.", colnames(datTraits), sep="");

  return(list('modNames'=modNames, 'geneModuleMembership'=geneModuleMembership,
          'MMPvalue'=MMPvalue, 'geneTraitSignificance'=geneTraitSignificance,
          'GSPvalue'=GSPvalue))  
}
