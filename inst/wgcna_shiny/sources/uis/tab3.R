tabPanel('Module - Trait Relationship',
  h1('3. Module - Trait Relationship'),
  HTML("In this section, the user will explore relationships between gene 
       modules and clinical traits."
  ),
  
  ###########################################
  ## Heatmap Correlation Eigengene - Trait ##
  ###########################################
  h2('a. Eigengene - Trait Analysis'),
  fluidRow( 
    style = "margin: 0px",
    div(
      id = "help3", 
      style = "width: calc(33.33% - 45px); float: left", 
      class = 'help collapsible collapse in', 
      # help description
      includeHTML(path = "www/html_text/module_trait/eigengene_trait.html")
    ),
    # arrow to show/hide help
    HTML({
      '<div style = "width: 30px; float: left; right: 0px; position: relative; 
      padding: 0px;" class = "help arrow-left" data-toggle = "collapse" 
      data-target = "#help1, #help2, #help2b, #help2c, #help3, #help3b, #help3c">
      </div>'
    }), 
    div(
      class = "main",
      # visualise heatmap
      plotOutput(outputId = 'moduleTraitMatrix', height = "auto", width = "auto"),
      # download table of correlations with p-values
      downloadButton(
        outputId = 'downloadmoduletrait', 
        label = 'Download Module Trait Association'
      )
    )
  ),
  # horizontal line
  hr(),
  
  #############################################
  ## Gene Significance and Module Membership ##
  #############################################
  h2('b. Gene - Trait Analysis'),
  fluidRow(
    style = "margin:0px",
    div(
      id = "help3b", 
      style = "width: calc(33.33% - 45px); float: left", 
      class = 'help collapsible collapse in', 
      # help description
      includeHTML(path = 'www/html_text/module_trait/gene_trait.html')
    ),
    # arrow to show/hide help
    HTML({
      '<div style = "width: 30px; float: left; right: 0px; 
      position: relative; padding: 0px;" class="help arrow-left" 
      data-toggle="collapse" data-target="#help1, #help2, #help2b, 
      #help2c, #help3, #help3b, #help3c">
      </div>'
    }),
    div(
      class = "main",
      # choose module and trait to analyse
      wellPanel(
        flowLayout(
          radioButtons(
            inputId = 'static_interactive', 
            label = "",
            choices = c("Static", "Interactive"), 
            selected = "Static"
          ),
          uiOutput(outputId = 'module_ui'),
          uiOutput(outputId = 'trait_ui')
        )
      ),
      # plot GS MM
      uiOutput(outputId = 'GSvsMM_ui'),
      # show detail information for genes selected in plot_GSvsMM2 
      DT::dataTableOutput(outputId = 'data_selected'),
      # downloads
      tags$div(style = 'margin-bottom: 10px; margin-top: 10px',
        downloadButton(
          outputId = 'downloadgsmm', 
          label = 'Dowload Gene Significance and Module Membership'
        )
      ),
      
      # anchor back to top
      tags$div(
        style = 'margin-bottom: 10px; margin-top: 10px',
        HTML(text = "<a href = '#top'>Back to Top</a>")
      )
    )
  ),
  # horizontal line
  hr(),
  
  #########################
  ## Show Module Network ##
  #########################
  h2('c. Network of Modules'),
  fluidRow( 
    style = "margin: 0px",
    div(
      id = "help3c", 
      style = "width: calc(33.33% - 45px); float: left", 
      class = 'help collapsible collapse in', 
      # help description
      includeHTML(path = 'www/html_text/module_trait/network_module.html')
    ),
    # arrow to show/hide help
    HTML({
      '<div style = "width: 30px; float: left; right: 0px; 
      position: relative; padding: 0px;" class = "help arrow-left" 
      data-toggle = "collapse" data-target = "#help1, #help2, #help2b, 
      #help2c, #help3, #help3b, #help3c">
      </div>'
    }),
    div(
      class = "main",
      # choose parameter for the network (module, link threshold, singleton)
      uiOutput(outputId = 'network_param_ui'),
      # buttons to download shown and whole network
      flowLayout(
        downloadButton(
          outputId = 'downloadshownnetwork', 
          label = 'Download Shown Network'
        ),
        downloadButton(
          outputId = 'downloadwholenetwork', 
          label = 'Download Whole Network'
        )
      ),
      # show network 
      networkD3::forceNetworkOutput(outputId = 'networkPlot'),
      # anchor back to top
      tags$div(
        style = 'margin-bottom: 10px; margin-top: 10px',
        HTML(text = "<a href='#top'>Back to Top</a>")
      ),
	  tags$div(
        style = 'margin-bottom: 10px; margin-top: 10px',
        downloadButton(
          outputId = 'downloadReportModuleTrait', 
          label = 'Download Report'
        )
      ),
      # button previous step
      tags$div(
        actionButton(
          inputId = 'BackTab2', 
          label = 'Go to Previous Step',
          class = 'btn-left'
        )
      )
    )
  ), 
  
  #############################
  ## Go Further Explications ##
  #############################
  h1("4. To Go Further"),
  # help description
  tags$div(
    class = "help",
    includeHTML(path = 'www/html_text/module_trait/go_further.html')
  )
)