tabPanel('Data Input',
  h1('1. Data Input'),
  HTML(
    "In this section, the user chooses and gets a first look at the dataset 
    to use in the analysis."
  ),
  
  ####################
  ## Select Dataset ##
  ####################
  
  h2('a. Choose Demo Dataset'),                    
  fluidRow(
    style = "margin: 0px",
    div(
      id = "help1", 
      style = "width: calc(33.33% - 45px); float: left",
      class = 'help collapsible collapse in',
      # help description
      includeHTML(path = 'www/html_text/data_input.html')
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
      # button to choose datasets
      wellPanel(
        uiOutput(outputId = 'dataset_ui'),
        uiOutput(outputId = 'own_upload'),
        uiOutput(outputId = 'load_data')
      ),
      # preprocessing panel for own data
      uiOutput(outputId = 'preprocessing'),
      # visualise data tables (genes and clinical traits)
      HTML(
        "<h4> <a data-toggle = 'collapse', href = '#datatables_ui'> Data Tables 
        of Input Data</a></h4>"
      ),
      uiOutput(outputId = 'datatables_ui', class = "collapse"),
      br(),
      # visualise sample tree
      plotOutput(outputId = 'visualize_plot', height = "100%", width = "100%"),
	  br(),
      # visualize pca
      uiOutput(outputId = 'pca'),
      br(),
      
      # anchor back to top
      tags$div(
        style = 'margin-bottom:20px',
        downloadButton(outputId = 'downloadReportDataInput', label = 'Download Report')
      ),
      tags$div(
        style = 'margin-bottom:20px',
        HTML(text = "<a href = '#top'>Back to Top</a>")
      ),
      
      # buttons previous and next step
      tags$div(
      	actionButton(inputId = 'BackTab0', label = 'Go to Previous Step', class = 'btn-left'),   
      	actionButton(inputId = 'GoTab2', label = 'Go to Next Step', class = 'btn-right')
      )
    )
  )
)