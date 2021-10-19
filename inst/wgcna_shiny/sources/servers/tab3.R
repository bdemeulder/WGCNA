#########################################
## Module Eigengene Trait Relationship ##
#########################################

# get module color list
observe({
  req(r_data$merge_modules$true_merged)
  tryCatch({
    if(r_data$merge_modules$true_merged >= 1){
      if(!is.null(r_data$module_current)){
        MEs <- orderMEs(r_data$module_current$MEs)
        color <- r_data$module_current$moduleColors
      }else{
        req(r_data$module_initial)
        MEs <- orderMEs(r_data$module_initial$MEs$eigengenes)
        color <- r_data$module_initial$dynamicColors
        # if merge happened but initial modules are used send alert
        session$sendCustomMessage(type = 'nomerge',
                                  message = list(a = r_data$module_initial))
      }
    }else{
      req(r_data$module_initial)
      MEs <- orderMEs(r_data$module_initial$MEs$eigengenes)
      color <- r_data$module_initial$dynamicColors
    }
    r_data$color_mes <- list('color' = color, 'MEs' = MEs)
  }, error = function(e){NULL})
})

# calculate trait module eigengene correlations and p-values
observe({
  req(r_data$color_mes, r_data$data$datTraits)
  isolate({
    tryCatch({
      color <- r_data$color_mes$color
      MEs <- r_data$color_mes$MEs
      moduleTraitCor <- cor(MEs, r_data$data$datTraits[, r_data$data$datTraits_num], use = "p")
      moduleTraitPvalue <- corPvalueStudent(cor = moduleTraitCor,
                                            nSamples = t(!is.na(MEs)) %*% (!is.na(r_data$data$datTraits[, r_data$data$datTraits_num])))
      textMatrix <- paste0(signif(moduleTraitCor, 2), "\n(", signif(moduleTraitPvalue, 1), ")");
      dim(textMatrix) <- dim(moduleTraitCor)
      r_data$moduleTrait <- list("moduleTraitCor" = moduleTraitCor,
                                 "moduleTraitPvalue" = moduleTraitPvalue,
                                 "MEs" = MEs,
                                 "textMatrix" = textMatrix,
                                 'colors' = color)
    }, error = function(e){NULL})
  })
})

# enable download moduleTrait button
observeEvent(req(r_data$moduleTrait), {
  session$sendCustomMessage(type = 'disable',
                            message = list(value = FALSE,
                                           div = '#downloadmoduletrait',
                                           parent = FALSE))
})

# download module trait values
output$downloadmoduletrait <- downloadHandler(
  filename = function(){'module_trait_association.csv'},
  content = function(file){
    longProcessStart(session)
    tryCatch({
      dim(r_data$moduleTrait$moduleTraitCor)
      tmp <- paste0(signif(r_data$moduleTrait$moduleTraitCor, 2),
                    " (",
                    signif(r_data$moduleTrait$moduleTraitPvalue, 1),
                    ")")
      dim(tmp) <- dim(r_data$moduleTrait$moduleTraitCor)
      rownames(tmp) <- names(r_data$moduleTrait$MEs)
      colnames(tmp) <- colnames(r_data$data$datTraits)[r_data$data$datTraits_num]
      write.csv(x = tmp, file = file, row.names = TRUE)
    }, finally = longProcessStop(session))
  }
)

# plot heatmap
output$moduleTraitMatrix <- renderPlot(height = 1000, width = "auto",
  {
    req(r_data$moduleTrait)
    par(mar = c(8, 7, 2, 0))
    isolate({
      tryCatch({
        # adjust cex according to number of modules and number of clinical traits
        cex.text = min(min(21 / length(r_data$moduleTrait$MEs), 29 / length(r_data$data$datTraits_num)), 2)
        labeledHeatmap(Matrix = r_data$moduleTrait$moduleTraitCor,
                       xLabels = colnames(r_data$data$datTraits)[r_data$data$datTraits_num],
                       yLabels = names(r_data$moduleTrait$MEs),
                       ySymbols = names(r_data$moduleTrait$MEs),
                       colorLabels = FALSE,
                       colors = gplots::bluered(50),
                       textMatrix = r_data$moduleTrait$textMatrix,
                       setStdMargins = FALSE,
                       cex.text = cex.text,
                       cex.lab = cex.text,
                       cex.main = 2,
                       zlim = c(-1, 1),
                       main = "Module-trait relationships",
                       xColorOffset = 0.01,
                       yColorWidth = 0.01,
                       yColorOffset = 0)
      })
    })
  }
)

#######################################
## Module genes - Trait relationship ##
#######################################
# ui to choose module and trait of interest
output$trait_ui <- renderUI({
  req(r_data$data$datTraits)
  traits_num <- r_data$data$datTraits[, r_data$data$datTraits_num]
  choices <- colnames(traits_num)[apply(X = traits_num,
                                        MARGIN = 2,
                                        FUN = function(x){sd(x, na.rm = TRUE) != 0})]
  selectInput(inputId = 'trait_interest',
              label = 'Trait of interest',
              choices = choices,
              multiple = FALSE,
              selected = state_init('trait_ui', choices[1]))
})

output$module_ui <- renderUI({
  req(r_data$color_mes$color)
  selectInput(inputId = 'module_interest',
              label = 'Choose Module',
              choices = unique(r_data$color_mes$color),
              selected = state_init('module_interest', unique(r_data$color_mes$color)[1]),
              multiple = FALSE)
})

# calculate gene correlation with trait (GS) and module eigengene (MM)
gsMM <- reactive({
  req(r_data$data$datTraits, r_data$data$datExpr)
  req(r_data$color_mes$MEs, input$trait_interest)
  tmp <- tryCatch(
    expr = { gene_relationship(datTraits = r_data$data$datTraits,
                               datExpr = r_data$data$datExpr,
                               MEs = r_data$color_mes$MEs)},
    error = function(e){NULL})
  return(tmp)
})

# update r_data to be able to save
observe({
  r_data$gsMM <- req(gsMM())
})

# render download able
observe({
  req(r_data$gsMM)
  session$sendCustomMessage(
    type = 'disable',
    message = list(value = FALSE, div = '#downloadgsmm', parent = FALSE)
  )
})

## plot sample eigengene value for chosen module ##
# interactive
output$plot_GSvsMM3_plotly <- renderPlotly({
  req(input$module_interest, input$static_interactive, r_data$color_mes)

  isolate({
    if((input$static_interactive == "Interactive")){
      tryCatch(
        expr = {
          p <- plot_ly(x = row.names(r_data$data$datExpr),
                       y = r_data$color_mes$MEs[, paste0("ME", input$module_interest)],
                       type = "bar",
                       marker = list(color = gplots::col2hex(input$module_interest),
                                     line = list(width = 1)))
          p <- p %>% layout(title ="Module Eigengene Expression by Sample",
                            xaxis = list(title = "Array samples", showticklabels = FALSE),
                            yaxis = list(title = "Eigengene Expression"))
          p
        },
        error = function(e){req(FALSE)})
    }else{req(FALSE)}
  })
})

# static
output$plot_GSvsMM3 <- renderPlot(height = function(){session$clientData$output_plot_GSvsMM2_width},
                                  width = function(){session$clientData$output_plot_GSvsMM2_width}, 
								  {
  req(input$module_interest, input$static_interactive, r_data$color_mes)
  isolate({
    if((input$static_interactive == "Static")){
      tryCatch(
        expr = {
          dat <- data.frame(x = row.names(r_data$data$datExpr),
                            y = r_data$color_mes$MEs[, paste0("ME",input$module_interest)])
          p <- ggplot(data = dat, mapping = aes(x = dat$x, y = dat$y))
          p <- p + geom_bar(stat = "identity",
                            fill = gplots::col2hex(input$module_interest),
                            colour = "black",
                            lwd = 1)
          p <- p + theme_bw(base_size = 15)
          p <- p + theme(legend.position = "none")
          p <- p + scale_x_discrete(name = "Array samples", labels = dat$x)
          p <- p + scale_y_continuous(name = "Eigengene Expression")
          p <- p + theme(axis.text.x  = element_text(angle = 90,
                                                     vjust = 0.5,
                                                     size = 760 / nrow(dat)),
                         line = element_line(size = 1),
                         rect = element_rect(size = 1))
          p <- p + ggtitle("Module Eigengene Expression by Sample")
          print(p)
        },
        error = function(e){}
      )
    }else{}
  })
})

## Boxplot of Genes Significance with chosen trait for all modules ##
# Static
output$plot_GSvsMM1 <- renderPlot(height = function(){session$clientData$output_plot_GSvsMM2_width},
                                  width = function(){session$clientData$output_plot_GSvsMM2_width}, {
    req(r_data$gsMM, input$trait_interest, input$static_interactive)

    isolate({
      if((input$static_interactive == "Static")){
        tryCatch({
          x <- factor(x = r_data$color_mes$color,
                      levels = unique(r_data$color_mes$color),
                      ordered = TRUE)
          y <- abs(r_data$gsMM$geneTraitSignificance[, paste0("GS.",input$trait_interest)])

          p <- ggplot(data = data.frame(x = x, y = y),
                      mapping = aes(x = x, y = y))
          p <- p + geom_boxplot(mapping = aes(fill = x),
                                lwd = 1,
                                outlier.size = 6)
          p <- p + scale_fill_manual(values = alpha(levels(x)))
          p <- p + theme_bw(base_size = 15)
          p <- p + theme(legend.position = "none")
          p <- p + scale_x_discrete(name = "",
                                    labels = paste(
                                      levels(x),
                                      '(n=',
                                      sapply(levels(x), function(i){sum(x == i)}),
                                      ')'))
          p <- p + scale_y_continuous(name = "Gene Significance")
          p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size=rel(0.5)),
                         line = element_line(size = 1),
                         rect = element_rect(size = 1))
          p <- p + ggtitle("Gene Significance Across Modules")
          print(p)
        }, error=function(e){})
      }else{}
    })
  }
)

# interactive
output$plot_GSvsMM1_plotly <- renderPlotly({
  req(r_data$gsMM, input$trait_interest, input$static_interactive)
  isolate({
    if((input$static_interactive == "Interactive")){
      col <- unique(r_data$color_mes$color)[1]
      y <- abs(r_data$gsMM$geneTraitSignificance[r_data$color_mes$color == col,
                                                 paste0("GS.",input$trait_interest)])

      p <- plot_ly()
      # p <- plot_ly(y = y,
      #              x0 = col,
      #              type = "box",
      #              name = col,
      #              fillcolor = gplots::col2hex(col),
      #              marker = list(color = gplots::col2hex(col),
      #                            line = list(color = "black", width = 1)),
      #              line = list(color = "black", width = 1),
      #              legendgroup = col, hoverinfo = "none")
      # p <- add_trace(p = p,
      #                x0 = col,
      #                name = col,
      #                hoverinfo ="text",
      #                text = paste("Number: ", sum(r_data$color_mes$color == col)),
      #                y = median(y),
      #                type = "scatter",
      #                mode = "markers",
      #                legendgroup = col,
      #                evaluate = TRUE,
      #                showlegend = FALSE,
      #                marker = list(color = gplots::col2hex(col), opacity = 0))

      for (col in unique(r_data$color_mes$color)){
        y <- abs(r_data$gsMM$geneTraitSignificance)[r_data$color_mes$color == col,
                                                    paste0("GS.",input$trait_interest)]

        p <- add_trace(p = p,
                       y = y,
                       type = "box",
                       name = col,
                       fillcolor = gplots::col2hex(col),
                       evaluate = TRUE,
                       hoverinfo = "none",
                       line = list(color = "black", width = 1),
                       marker = list(color = gplots::col2hex(col),
                                     line = list(color = "black", width = 1)))
        p <- add_trace(p = p,
                       x0 = col,
                       name = col,
                       hoverinfo = "text",
                       text = paste("Number: ", sum(r_data$color_mes$color == col)),
                       y = median(y),
                       type = "scatter",
                       mode="markers",
                       legendgroup = col,
                       evaluate = TRUE,
                       showlegend = FALSE,
                       marker = list(color = gplots::col2hex(col), opacity = 0))
      }

      p <- p %>% layout(title="Gene Significance Across Modules",
                        xaxis = list(title = "",
                                     showticklabels = TRUE,
                                     titlefont = list(size = 1)),
                        yaxis = list(title = "Gene Significance"),
                        showlegend = FALSE,
                        margin = list(b = 120))
      p
    }else{
      req(FALSE)
    }
  })
})

## plot Genes Significance vs Module Membership for chosen trait and module ##
# static
output$plot_GSvsMM2 <- renderPlot(height = function(){session$clientData$output_plot_GSvsMM2_width},
                                  width = function(){session$clientData$output_plot_GSvsMM2_width},
  {
    req(r_data$gsMM, input$trait_interest, input$module_interest, input$static_interactive)

    isolate({
      if((input$static_interactive == "Static")){
        tryCatch({
          res <- session$clientData$output_plot_GSvsMM2_width / 1000
          dat <- data.frame(y = abs(r_data$gsMM$geneTraitSignificance[(r_data$color_mes$color == input$module_interest),
                                                                      paste0("GS.",input$trait_interest)]),
                            x = abs(r_data$gsMM$geneModuleMembership[(r_data$color_mes$color == input$module_interest),
                                                                     match(input$module_interest, r_data$gsMM$modNames)]),
                            row.names = row.names(r_data$gsMM$geneModuleMembership)[(r_data$color_mes$color == input$module_interest)])
          p <- ggplot(data = dat, mapping = aes(x = dat$x, y = dat$y))
          p <- p + geom_point(shape = 21,
                              fill = gplots::col2hex(input$module_interest),
                              size = 6 * res,
                              stroke = 2 * res)
          p <- p + theme_bw(base_size = 15) + theme(legend.position = "none")
          p <- p + scale_x_continuous(name = paste("Module Membership in", input$module_interest, "module"))
          p <- p + scale_y_continuous(name = paste("Gene significance for", input$trait_interest))
          p <- p + theme(line = element_line(size = 1 * res),
                         rect = element_rect(size = 1 * res))
          p <- p + ggtitle("Module membership vs. gene significance")
          p <- p + geom_smooth(method = 'lm',
                               se = FALSE,
                               color = "orange",
                               size = 2 * res)
          p
        }, error=function(e){})
      }else{}
    })
  }
)

# interactive
output$plot_GSvsMM2_plotly <- renderPlotly(
  {
    req(r_data$gsMM, input$trait_interest, input$module_interest, input$static_interactive)

    isolate({
      if(!is.null(input$trait_interest) & (input$static_interactive == "Interactive")){
        tryCatch({
          dat <- data.frame(y = abs(r_data$gsMM$geneTraitSignificance[(r_data$color_mes$color == input$module_interest),
                                                                      paste0("GS.",input$trait_interest)]),
                            x = abs(r_data$gsMM$geneModuleMembership[(r_data$color_mes$color == input$module_interest),
                                                                     match(input$module_interest, r_data$gsMM$modNames)]),
                            row.names = row.names(r_data$gsMM$geneModuleMembership)[(r_data$color_mes$color == input$module_interest)])

          p <- plot_ly()
          p <- p %>% add_markers(x = ~dat$x,
                       y = ~dat$y,
                       marker = list(color = gplots::col2hex(input$module_interest),
                                     line = list(width = 1)),
                       source = "gene_selected",
                       showlegend = FALSE,
                       hoverinfo = "text",
                       text = paste0(row.names(dat),
                                     "\nMM: ", signif(dat$x, 2),
                                     "\nGS: ", signif(dat$y, 2)))
          m <- lm(y ~ x, data = dat)
          corm <- cor(dat$x, dat$y, use = "p")
          p <- p %>% add_lines(y = ~predict(m),
                               x = ~dat[names(predict(m)),'x'],
                               showlegend = FALSE,
                               hoverinfo = "text",
                               text = paste("R: ", signif(corm, 2),
                                            "\nR<sup>2</sup>: ", signif(summary(m)$r.squared, 2),
                                            "\nP-val: ", signif(summary(m)$coefficients["x", 4], 2)))
      	  p <- p %>% layout(dragmode = "select",
      	                    xaxis = list(title = paste("Module Membership in", input$module_interest, "module")),
      	                    yaxis = list(title = paste("Gene significance for", input$trait_interest)),
      	                    title = paste("Module membership vs. gene significance\n"))
      	  p
          },
	        error=function(e){req(FALSE)}
	      )
      }else{req(FALSE)}
    })
  }
)

## scatterplot Module Eigengene versus clinical trait ##
# interactive
output$plot_GSvsMM4_plotly <- renderPlotly({
  req(r_data$gsMM, input$trait_interest, input$module_interest, input$static_interactive)

  isolate({
    if(!is.null(input$trait_interest) & (input$static_interactive == "Interactive")){
      tryCatch(
        expr = {
          dat <- data.frame(y = r_data$color_mes$MEs[, paste0("ME",input$module_interest)],
                            x = r_data$data$datTraits[, input$trait_interest])
          p <- plot_ly()
          p <- p %>% add_markers(x = ~dat$x,
                       y = ~dat$y,
                       marker = list(color = gplots::col2hex(input$module_interest),
                                     line = list(width = 1)),
                       hoverinfo = "text",
                       text = row.names(r_data$data$datTraits),
                       showlegend = FALSE)
          tryCatch({
            m <- lm(y ~ x,dat)
            corm <- cor(dat$x, dat$y, use = "p")
            p <- p %>% add_lines(y = ~predict(m),
                                 x= ~dat[as.numeric(names(predict(m))), 'x'],
                                 showlegend = FALSE,
                                 hoverinfo = "text",
                                 text = paste("R: ", signif(corm, 2),
                                              "\nR<sup>2</sup>: ", signif(summary(m)$r.squared, 2),
                                              "\nP-val: ", signif(summary(m)$coefficients["x", 4], 2)))
          })
          p <- p %>% layout(title = "Module eigengene vs. clinical trait",
                            xaxis = list(title = input$trait_interest),
                            yaxis = list(title = paste("Module", input$module_interest, "eigengene", sep = " ")))
          p
        },
        error = function(e){req(FALSE)}
      )
    }else{req(FALSE)}
  })
}
)

# Static
output$plot_GSvsMM4 <- renderPlot(height = function(){session$clientData$output_plot_GSvsMM2_width},
                                  width = function(){session$clientData$output_plot_GSvsMM2_width},
  {
    req(r_data$gsMM, input$trait_interest, input$module_interest, input$static_interactive)

    isolate({
      if(!is.null(input$trait_interest) & (input$static_interactive == "Static")){
        tryCatch({
          dat <- data.frame(y = r_data$color_mes$MEs[, paste0("ME",input$module_interest)],
                            x = r_data$data$datTraits[, input$trait_interest])
          p <- ggplot(data = dat, mapping = aes(x = dat$x, y = dat$y))
          p <- p + geom_point(shape = 21,
                              fill = gplots::col2hex(input$module_interest),
                              stroke = 2,
                              size = 6)
          p <- p + theme_bw(base_size = 15) + theme(legend.position = "none")
          p <- p + scale_x_continuous(name = input$module_interest)
          p <- p + scale_y_continuous(name = paste0("Module ", input$module_interest, " eigengene"))
          p <- p + theme(line = element_line(size = 1),
                         rect = element_rect(size = 1))
          p <- p + ggtitle("Module eigengene vs. clinical trait")
          p <- p + geom_smooth(method = 'lm', se = FALSE, color = "orange", size = 2)
          print(p)
        },
        error = function(e){NULL}
        )
      }else{}
    })
  }
)

# render table of selected genes
gene_selected <- reactive({
  event.data <- event_data("plotly_selected", source = "gene_selected")
  return(event.data)
})

selected_Genes_table <- reactive({
  req(r_data$gsMM, input$module_interest, input$trait_interest)
  req(r_data$color_mes, input$static_interactive)
  if((input$static_interactive == "Static")){
    if(is.null(input$brushed_genes)){
      session$sendCustomMessage(type = 'show',
                                message = list(value = FALSE, div = '#data_selected'))
      req(FALSE)
    }
  }else if (input$static_interactive == "Interactive"){
    if(is.null(gene_selected())){
      session$sendCustomMessage(type = 'show',
                                message = list(value = FALSE, div = '#data_selected'))
      req(FALSE)
    }
  }

  isolate({
    dat <- data.frame(y = r_data$gsMM$geneTraitSignificance[(r_data$color_mes$color == input$module_interest),
                                                            paste0("GS.", input$trait_interest)],
                      x = r_data$gsMM$geneModuleMembership[(r_data$color_mes$color == input$module_interest),
                                                           match(input$module_interest, r_data$gsMM$modNames)],
                      row.names = row.names(r_data$gsMM$geneModuleMembership)[(r_data$color_mes$color == input$module_interest)])

    if((input$static_interactive == "Static")){
      genes <- which(abs(dat$x) <= input$brushed_genes$xmax &
                     abs(dat$x) >= input$brushed_genes$xmin &
                     abs(dat$y) <= input$brushed_genes$ymax &
                     abs(dat$y) >= input$brushed_genes$ymin)
    }else{
      genes <- gene_selected()$pointNumber + 1
    }

    if(length(genes) > 0){
      session$sendCustomMessage(type = 'show',
                                message = list(value = TRUE, div = '#data_selected'))
      isolate({
        genes_table <- data.frame(
          'Feature' = colnames(r_data$data$datExpr)[(r_data$color_mes$color == input$module_interest)][genes],
          'Module'= r_data$color_mes$color[r_data$color_mes$color == input$module_interest][genes],
          'Module Membership' = dat$x[genes],
          'Module Membership P-value' = r_data$gsMM$MMPvalue[(r_data$color_mes$color == input$module_interest),
                                                             match(input$module_interest, r_data$gsMM$modNames)][genes],
          'Correlation with Trait' = dat$y[genes],
          'Correlation with Trait P-value' = r_data$gsMM$GSPvalue[(r_data$color_mes$color == input$module_interest),
                                                                  paste0("p.GS.",input$trait_interest)][genes])
      })
      return(genes_table)
    }else{
      session$sendCustomMessage(type = 'show', message = list(value = FALSE, div = '#data_selected'))
      return(FALSE)
    }
  })
})

output$data_selected <- DT::renderDataTable({
  req(selected_Genes_table())
  DT::datatable(data = selected_Genes_table(),
                rownames = FALSE,
                extensions = list(ColReorder = NULL,
                                  FixedColumns = list(leftColumns = 1)),
                options = list(dom = 'R<"clear">lftip',
                               scrollX = TRUE,
                               scrollXInner = "100%",
                               scrollCollapse = FALSE,
                               autoWidth = FALSE))
})

outputOptions(x = output, name = 'data_selected', suspendWhenHidden = FALSE)

## put the plots in UI ##
output$GSvsMM_ui <- renderUI({
  if((input$static_interactive == "Static")){
    fluidRow(
      column(width = 6,
             # plot eigengene value by sample for specified module
             plotOutput('plot_GSvsMM3', height = "auto", width = "auto"),
             # plot module significance for specified trait
             plotOutput('plot_GSvsMM1', height = "auto", width = "auto")
      ),
      column(width = 6,
             # plot eigengene value vs clinical trait
             plotOutput('plot_GSvsMM4', height = "auto", width = "auto"),
             # plot gene significance for specified trait and gene module Membership
             # for specified module
             plotOutput('plot_GSvsMM2', height = "auto",width = "100%", brush = "brushed_genes")
      )
    )
  }else{
    fluidRow(
      column(width = 6,
             # plot eigengene value by sample for specified module
             plotly::plotlyOutput('plot_GSvsMM3_plotly', height = "auto", width = "auto"),
             # plot module significance for specified trait
             plotly::plotlyOutput('plot_GSvsMM1_plotly', height = "auto", width = "auto")
      ),
      column(width = 6,
             # plot eigengene value vs clinical trait
             plotly::plotlyOutput('plot_GSvsMM4_plotly',height="auto", width="auto"),
             # plot gene significance for specified trait and gene module Membership
             # for specified module
             plotly::plotlyOutput('plot_GSvsMM2_plotly',height="auto",width="auto")
      )
    )
  }
})

## Downloads ##
# download whole table
output$downloadgsmm <- downloadHandler(
  filename = function(){paste0('module_', input$module_interest, '_membership_gene_significance_', input$trait_interest, '.csv')},
  content = function(file){
    longProcessStart(session)
    tryCatch({
      xs <- r_data$gsMM$geneModuleMembership[(r_data$color_mes$color == input$module_interest),
                                             match(input$module_interest, r_data$gsMM$modNames)]
      ys <- r_data$gsMM$geneTraitSignificance[(r_data$color_mes$color == input$module_interest),
                                              paste0("GS.",input$trait_interest)]
      tmp <- data.frame(names(r_data$data$datExpr)[(r_data$color_mes$color == input$module_interest)],
                        r_data$color_mes$color[r_data$color_mes$color == input$module_interest],
                        xs,
                        r_data$gsMM$MMPvalue[(r_data$color_mes$color == input$module_interest),
                                             match(input$module_interest, r_data$gsMM$modNames)],
                        ys,
                        r_data$gsMM$GSPvalue[(r_data$color_mes$color == input$module_interest),
                                             paste0("p.GS.",input$trait_interest)])
      names(tmp) <- c('Feature',
                      'Module',
                      'Module Membership',
                      'Module Membership P-value',
                      paste0('Correlation with ', input$trait_interest),
                      paste0('Correlation with ', input$trait_interest, ' P-value'))

      write.csv(x = tmp, file = file, row.names = FALSE)
    },finally = longProcessStop(session))
  }
)

################
## Show graph ##
################
# panel for graph parameters
output$network_param_ui <- renderUI({
  wellPanel(
    flowLayout(
      selectInput(inputId = 'modules_for_network',
                  label = 'Modules',
                  choices =unique(r_data$color_mes$color),
                  selected = state_init('modules_for_network','') ,
                  multiple = TRUE),
      numericInput(inputId = 'link_threshold',
                   label = 'Link threshold',
                   min = 0,
                   max = 1,
                   value = state_init('link_threshold', 0.25),
                   step = 0.01),
      checkboxInput(inputId = 'remove_unlink',
                    label = 'Remove Unlinked Nodes',
                    value = state_init('remove_unlink', TRUE))
    ),
    actionButton(inputId = 'plot_graph',
                 label = 'Plot Network')
  )
})

# make the subset for the network
network_to_show <- reactive({
  req(input$plot_graph >= 1)
  isolate({
    req(r_data$color_mes$color, input$link_threshold)
    if(tryCatch( expr = { is.na(input$link_threshold) | input$link_threshold < 0 | input$link_threshold > 1 },
                 error = function(e){TRUE})){
      a <- '\nLink threshold paramerer should be a numeric value between 0 and 1.'
      session$sendCustomMessage(type = 'invalid', message = a)
      updateNumericInput(session, inputId = 'link_threshold', value = 0.25)
      return(NULL)
    }

    tryCatch({
      genes <- which(r_data$color_mes$color %in% input$modules_for_network)
      tmp <- r_data$net$adjacency[genes, genes]
      diag(tmp) <- NA
      tmp2 <- apply(X = tmp, MARGIN = 1, FUN = function(x){all(x < input$link_threshold, na.rm = TRUE)})
      if(input$remove_unlink){
        genes <- genes[!tmp2]
      }

      if(length(genes) > 0){
        value <- as.vector(as.dist(r_data$net$adjacency[genes, genes]))
        tmp <- (value >= input$link_threshold)
        nodes <- row.names(r_data$net$adjacency)[genes]
        srce <- rep(c(0:(length(genes) - 1)), times = c((length(genes) - 1):0))
        tgt <- c(0:(length(genes) - 1))[unlist(lapply(X = 2:length(genes), FUN = function(i){i:length(genes)}))]
        group <- r_data$color_mes$color[genes]
        MisLinks <- data.frame("source" = srce, "target" = tgt, "value" = value)[tmp, ]
        MisNodes <- data.frame("name" = nodes, "nodeid" = c(0:(length(genes) - 1)), "group" = group)
        return(list("MisLinks" = MisLinks, "MisNodes" = MisNodes))
      }else{
        session$sendCustomMessage(type = 'nonode', message = list(a = genes))
      }
    }, error = function(e){NULL})
  })
})

# update r_data to be able to save
observe({
  r_data$network_to_show <- req(network_to_show())
})

# send a warning when trying to plot big networks
observe({
  req(input$plot_graph >= 1)
  isolate({
    session$sendCustomMessage(type = 'networksize',
                              message = list(value = nrow(r_data$network_to_show$MisNodes)))
  })
})

# render Graph
output$networkPlot <- networkD3::renderForceNetwork({
  req(r_data$network_to_show)
  if(nrow(r_data$network_to_show$MisNodes) > 100){
    req(input$network_answer)
  }
  isolate({
    if(nrow(r_data$network_to_show$MisNodes) < 100 || input$network_answer){
      tmp <- tryCatch(r_data$network_to_show$MisLinks,error=function(e){NULL});
      tmp2 <- tryCatch({r_data$network_to_show$MisNodes}, error=function(e){NULL});
      net <- tryCatch(
        expr = {
          networkD3::forceNetwork(Links = tmp,
                                  Nodes = tmp2,
                                  opacity = 0.8,
                                  Source = "source",
                                  Target = "target",
                                  Value = "value",
                                  NodeID = "name",
                                  Group = "group",
                                  colourScale = networkD3::JS(paste0("d3.scaleOrdinal().range(['", paste(gplots::col2hex(unique(tmp2$group)), collapse = " ' , ' "), "'])")),
                                  zoom = TRUE,
                                  bounded = FALSE,
                                  linkDistance = networkD3::JS("function(d){return (1-d.value) * 100}"),
                                  linkWidth = networkD3::JS("function(d) { return d.value*3; }"))
        },
        error = function(e){NULL})
      return(net)
    }
  })
})

## Downloads
# download shown network
output$downloadshownnetwork <- downloadHandler(
  filename = function(){'network.zip'},
  content = function(file){
    longProcessStart(session)
    tryCatch({
      tmp <- r_data$network_to_show$MisLinks
      tmp2 <- r_data$network_to_show$MisNodes
      tmpdir <- tempdir()
      wd <- getwd()
      setwd(tmpdir)
      fs <- c("nodes.csv", "links.csv")
      write.csv(x = tmp,  file = "links.csv", row.names = FALSE)
      write.csv(x = tmp2, file = "nodes.csv", row.names = FALSE)

      zip(zipfile = file, files = fs)

      file.remove(fs)
      setwd(wd)
    }, finally = longProcessStop(session))
  },
  contentType = "application/zip"
)

# download whole network
output$downloadwholenetwork <- downloadHandler(
  filename = function(){'network_all.zip'},
  content = function(file){
    longProcessStart(session)
    tryCatch({
      # make tables
      value <- as.vector(as.dist(r_data$net$adjacency))
      nodes <- row.names(r_data$net$adjacency)
      srce <- rep(nodes, times = c((length(nodes) - 1):0))
      tgt <- nodes[unlist(lapply(X = 2:length(nodes), FUN = function(i){i:length(nodes)}))]
      group <- r_data$moduleTrait$colors
      MisLinks <- data.frame("source" = srce, "target" = tgt, "value" = value)
      MisNodes <- data.frame("name" = nodes, "group" = group)

      tmpdir <- tempdir()
      wd <- getwd()
      setwd(tmpdir)
      fs <- c("nodes_all.csv", "links_all.csv")
      write.csv(MisLinks, file = "links_all.csv", row.names = FALSE)
      write.csv(MisNodes, file = "nodes_all.csv", row.names = FALSE)

      zip(zipfile = file, files = fs)

      file.remove(fs)
      setwd(wd)
    }, finally = longProcessStop(session))
  },
  contentType = "application/zip"
)

# download report
output$downloadReportModuleTrait <- downloadHandler(
  filename = function() { paste0('Report_Module_', input$module_interest, '_Trait_', input$trait_interest, '_Network_', paste0(input$modules_for_network, collapse = "_"), '_th_', input$link_threshold, '.html')},
  content = function(file) {
    longProcessStart(session)
    tryCatch({
      src <- normalizePath(path = 'sources/reports/module_trait.Rmd')
      src2 <- normalizePath(path = 'www/Images/etriks.png')
      src3 <- normalizePath(path = 'www/Images/EISBM.png')
      owd <- getwd()
      setwd(tempdir())
      file.copy(from = src, to = 'module_trait.Rmd')
      file.copy(src2, 'etriks.png')
      file.copy(src3, 'EISBM.png')
      out <- rmarkdown::render('module_trait.Rmd', rmarkdown::html_document(), quiet = TRUE)
      file.rename(out, file)
      setwd(owd)},
      finally = longProcessStop(session)
    )
  }
)
