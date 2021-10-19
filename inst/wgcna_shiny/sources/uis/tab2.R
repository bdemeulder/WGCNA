tabPanel('Network Construction',
  h1('2. Network Construction'),
  div(
    HTML(
      '<p>In this section, the user will define the values of parameters 
      and choose options to build the correlation network and define gene 
      modules.</p>'
    )
  ),
  
  #########################
  ## Choose Network Type ##
  #########################
  h2('a. Choose Network Type'),
  fluidRow(
    style = "margin: 0px",
    div(
      id = "help2", 
      style = "width: calc(33.33% - 45px); float: left", 
      class = 'help collapsible collapse in', 
      # help description
      includeHTML(path = 'www/html_text/network_construction/network_type.html')
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
      # choose network type and add button
      uiOutput(outputId = 'network_type_ui')
    )
  ),
  
  # horizontal line to separate next section
  hr(),
  
  #######################
  ## Choose Soft Power ##
  #######################
  h2('b. Choose Soft Power'),
  fluidRow(
    style = "margin:0px",
    div(
      id = "help2b", 
      style = "width: calc(33.33% - 45px); float: left", 
      class = 'help collapsible collapse in', 
      # help description
      includeHTML(path = 'www/html_text/network_construction/soft_power.html')
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
      # visualise soft power result
      plotOutput(outputId = 'soft_power_plot', height = "800px", width = "800px"),
      # download soft power result as table
      downloadButton(outputId = 'downloadsft', label = 'Download Table'),
      # anchor back to top
      tags$div(
        style = 'margin-bottom: 10px; margin-top: 10px',
        HTML(text = "<a href='#top'>Back to Top</a>")
      )
    )
  ),
  # horizontal line
  hr(),
  
  ####################
  ## Create Network ##
  ####################
  h2('c. Create Network'),
  fluidRow(
    style = "margin:0px",
    div(
      id = "help2c", 
      style = "width: calc(33.33% - 45px); float: left", 
      class = 'help collapsible collapse in', 
      # help description with optional paragraph for manual construction
      includeHTML(path = 'www/html_text/network_construction/create_network1.html'),
      uiOutput(outputId = 'textManual'),
      includeHTML(path = 'www/html_text/network_construction/create_network3.html')
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
      # choose construction method, soft power and tom type for unsigned 
      # network and calc button
      uiOutput(outputId = 'network_construction_ui'),
      # additional parameters for manual construction method
      uiOutput(outputId = 'ui_man_param'),
      # visualise module eingene tree
      plotOutput(outputId = 'METree_plot', height = "1000", width = "auto"),
      div(style = "height:15px"),
      # visualise gene tree with module colors
      plotOutput(outputId = 'network_dendrogram', height = "1000", width = "auto"),
      # download module assignment button
      tags$div(
        style = 'margin-bottom:10px; margin-top:10px',  
        downloadButton(
          outputId = 'downloadmoduleassignment', 
          label = 'Download Module Assignments'
        )
      ),
      tags$div(
        style = 'margin-bottom: 10px; margin-top: 10px',
        downloadButton(
          outputId = 'downloadReportNetworkConstruction', 
          label = 'Download Report'
        )
      ),
      # anchor back to top
      tags$div(
        style = 'margin-bottom: 10px; margin-top: 10px',
        HTML(text = "<a href='#top'>Back to Top</a>")
      ),
      # buttons previous and next step
      tags$div(
        actionButton(
          inputId = 'BackTab1', 
          label = 'Go to Previous Step', 
          class = 'btn-left'
        ),
        actionButton(
          inputId = 'GoTab3', 
          label = 'Go to Next Step', 
          class = 'btn-right'
        )
      )
    )
  )
)