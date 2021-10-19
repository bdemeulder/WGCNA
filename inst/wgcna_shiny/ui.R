shinyUI(
  # Create a navbar page
  
  navbarPage(
    title = div(img(id = "header", src = 'etriks_logo.png', height = "30px")), 
    windowTitle = 'WGCNA - Shiny', fluid = TRUE, id = "main",selected = 'wgcna',
    
    # Set top anchor
    div(HTML(text="<a name = 'top'></a>")),
    
    # Creat a tab panel
    tabPanel(
      title = strong('Weighted Gene Correlation Network Analysis'), 
      value="wgcna",
      
      #create the main page         
      fluidPage(
        # insert js and css
        tagList(
          tags$head(
            tags$script(src = "handlers.js"),
            tags$script(src = "sessions.js"),
            tags$link(rel = "stylesheet", href = "dt.css")
          )
        ),
        uiOutput(outputId = 'reset_window'),
        uiOutput(outputId = 'close_window'),
        
        # put save, reset and quit buttons at the top right of the page
        fluidRow(
          style = "float:right;",
          downloadButton(outputId = 'save_session', label = "Save"),
          actionButton(inputId = 'reset', label = 'Reset', icon = icon("stop")),
          actionButton(inputId = 'quit', label = 'Quit', icon = icon("close"))
        ),
        
        h1(strong('Weighted Gene Correlation Network Analysis')),
        
        # include waiting box
        includeHTML(path = "www/waiting.html"),
        
        # include small description of WGCNA
        includeHTML(path='www/html_text/welcome.html'), 
        
        # insert div for refreshing app on upload invible by user but needed 
        #to be on the main page
        uiOutput('refreshOnUpload'),
        
        # add tabs
        tabsetPanel(id = 'myTabsetPanel',
          # Introduction Tab
          tabPanel('Introduction',
            fluidPage(
              includeHTML(path='www/html_text/introduction.html'),
              tags$div(
                actionButton(inputId = 'GoTab1', label = 'Begin!', class = 'btn-right')   
              )
            )
          ),
          
          # Data Input Tab
          source(file = 'sources/uis/tab1.R',local = TRUE)$value, 
          
          # Network Construction Tab
          source(file = 'sources/uis/tab2.R', local=TRUE)$value, 
          
          # Module Trait Relationship Tab
          source(file = 'sources/uis/tab3.R', local=TRUE)$value
        ),
        hr(),
        HTML('<p><strong>Disclaimer:</strong> This application is offered to the public as a freely available resource, 
          for non-commercial research use. Some aspects of this experimental module 
          may still be under development, and there are no warranties about the 
          completeness, reliability, accuracy, and security of this software. Please send an email to <a href="mailto:bdemeulder@eisbm.org">bdemeulder@eisbm.org</a> for any questions. </p>')
      )
    )
  )
)