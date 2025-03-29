library(shiny)

server <- function(input, output, session) {
  # Hide loading screen when app loads
  waiter_hide()
  
  # Notification menu
  output$notificationMenu <- renderMenu({
    dropdownMenu(
      type = "notifications", 
      badgeStatus = "warning",
      icon = icon("bell"),
      notificationItem(
        text = "New blog post coming soon!",
        icon = icon("blog")
      ),
      notificationItem(
        text = "You have 3 unread messages",
        icon = icon("envelope")
      )
    )
  })
  
  # Plot for career transition blog
  output$careerTransitionPlot <- renderPlotly({
    df <- data.frame(
      Stage = c("Academic\nBackground", "Technical\nSkill Building", 
                "Portfolio\nDevelopment", "Industry\nApplications"),
      Importance = c(8, 9, 7, 6),
      Focus = c("Theory", "Tools", "Projects", "Business")
    )
    
    plot_ly(df, x = ~Stage, y = ~Importance, type = 'scatter', mode = 'lines+markers',
            line = list(color = '#3498db', width = 3),
            marker = list(color = '#3498db', size = 10),
            text = ~Focus, hoverinfo = 'text') %>%
      layout(
        title = "My Data Science Journey Stages",
        xaxis = list(title = ""),
        yaxis = list(title = "Perceived Importance (1-10)"),
        showlegend = FALSE
      )
  })
  
  # Trend results output
  output$trendResults <- renderText({
    if (input$submitTrend > 0) {
      paste("Thanks for voting! You selected:", isolate(input$trendOpinion))
    }
  })
  
  # Button actions
  observeEvent(input$exploreBtn, {
    updateTabItems(session, "tabs", "about")
  })
  
  observeEvent(input$contactBtn, {
    updateTabItems(session, "tabs", "about")
  })
  
  observeEvent(input$viewProject1, {
    updateTabItems(session, "tabs", "obesity_education")
  })
  
  observeEvent(input$viewBlog1, {
    updateTabItems(session, "tabs", "b1")
  })
  
  observeEvent(input$downloadCV, {
    showModal(modalDialog(
      title = "Download CV",
      "This would typically link to your CV download.",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Close"),
        downloadButton("downloadCVBtn", "Download", class = "btn-primary")
      )
    ))
  })
  
  output$downloadCVBtn <- downloadHandler(
    filename = function() {
      "Tony_Irose_CV.pdf"
    },
    content = function(file) {
      file.copy("path/to/my/cv.pdf", file)
    }
  )
  
  # Social link handlers
  observeEvent(input$linkedinBtn, {
    # Handled by JavaScript onclick
  })
  
  observeEvent(input$githubBtn, {
    # Handled by JavaScript onclick
  })
  
  observeEvent(input$emailBtn, {
    # Handled by JavaScript onclick
  })
  
  # Interactive projects handlers
  # Reactive value to store the loaded data
  loadedData <- reactiveVal(NULL)
  
  # Observe file upload
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    loadedData(NULL)  # Reset loaded data when new file is selected
  })
  
  # Check if file is uploaded
  output$fileUploaded <- reactive({
    !is.null(input$fileUpload)
  })
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  
  # Load data when button is clicked
  observeEvent(input$loadData, {
    req(input$fileUpload)
    
    tryCatch({
      file <- input$fileUpload
      ext <- tools::file_ext(file$datapath)
      
      data <- switch(ext,
                     csv = read.csv(file$datapath, 
                                    header = as.logical(input$header),
                                    sep = input$sep,
                                    quote = input$quote),
                     xls = readxl::read_excel(file$datapath),
                     xlsx = readxl::read_excel(file$datapath),
                     xpt = haven::read_xpt(file$datapath),
                     sas7bdat = haven::read_sas(file$datapath),
                     dat = read.table(file$datapath, 
                                      header = as.logical(input$header)),
                     txt = read.table(file$datapath, 
                                      header = as.logical(input$header),
                                      sep = input$sep,
                                      quote = input$quote),
                     rds = readRDS(file$datapath),
                     {
                       # Default case for unsupported extensions
                       stop("Unsupported file format")
                     }
      )
      
      loadedData(data)
      output$loadError <- reactive(FALSE)
    }, error = function(e) {
      output$loadError <- reactive(TRUE)
      output$errorMessage <- renderText({
        paste("Error loading file:", e$message, 
              "\nPlease try a different file format.")
      })
    })
    
    outputOptions(output, "loadError", suspendWhenHidden = FALSE)
  })
  
  # Check if data is loaded
  output$dataLoaded <- reactive({
    !is.null(loadedData())
  })
  outputOptions(output, "dataLoaded", suspendWhenHidden = FALSE)
  
  # Data summary output
  output$dataSummary <- renderPrint({
    req(loadedData())
    cat("Dataset dimensions:", dim(loadedData()), "\n\n")
    cat("Variable names and types:\n")
    str(loadedData())
  })
  
  # Data preview output
  output$dataPreview <- renderDT({
    req(loadedData())
    datatable(head(loadedData(), 10), 
              options = list(scrollX = TRUE, dom = 't'))
  })
}