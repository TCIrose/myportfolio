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
                     csv = read.csv(file$datapath),
                     xls = readxl::read_excel(file$datapath),
                     xlsx = readxl::read_excel(file$datapath),
                     xpt = haven::read_xpt(file$datapath),
                     sas7bdat = haven::read_sas(file$datapath),
                     dat = read.table(file$datapath),
                     txt = read.table(file$datapath),
                     rds = readRDS(file$datapath),
                     stop("Unsupported file format")
      )
      
      # Convert to data.frame if it isn't already
      if (!is.data.frame(data)) {
        data <- as.data.frame(data)
      }
      
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
  
  # Update variable selection dropdown
  observeEvent(loadedData(), {
    req(loadedData())
    updateSelectInput(session, "varSelect", 
                      choices = names(loadedData()))
  })
  
  # Data dimensions output
  output$dataDims <- renderPrint({
    req(loadedData())
    dim(loadedData())
  })
  
  # Variable types table
  output$varTypes <- renderDT({
    req(loadedData())
    var_info <- data.frame(
      Variable = names(loadedData()),
      Type = sapply(loadedData(), function(x) class(x)[1]),
      stringsAsFactors = FALSE
    )
    datatable(var_info, options = list(dom = 't'))
  })
  
  # Variable summary output
  output$varSummary <- renderPrint({
    req(loadedData(), input$varSelect)
    var <- loadedData()[[input$varSelect]]
    
    if (is.numeric(var)) {
      cat("Numeric Variable Summary:\n")
      cat("------------------------\n")
      cat("Mean:", mean(var, na.rm = TRUE), "\n")
      cat("Median:", median(var, na.rm = TRUE), "\n")
      cat("Min:", min(var, na.rm = TRUE), "\n")
      cat("Max:", max(var, na.rm = TRUE), "\n")
      cat("SD:", sd(var, na.rm = TRUE), "\n")
      cat("Variance:", var(var, na.rm = TRUE), "\n")
      cat("NA values:", sum(is.na(var)), "\n")
    } else if (is.factor(var) || is.character(var)) {
      cat("Categorical Variable Summary:\n")
      cat("---------------------------\n")
      print(table(var))
      cat("\nNA values:", sum(is.na(var)), "\n")
    } else if (is.logical(var)) {
      cat("Logical Variable Summary:\n")
      cat("------------------------\n")
      print(table(var))
      cat("\nNA values:", sum(is.na(var)), "\n")
    } else {
      cat("Variable type:", class(var)[1], "\n")
      print(summary(var))
    }
  })
  
  # Variable plot
  output$varPlot <- renderPlotly({
    req(loadedData(), input$varSelect)
    var <- loadedData()[[input$varSelect]]
    
    if (is.numeric(var)) {
      p <- plot_ly(x = ~var, type = "histogram", 
                   nbinsx = 30) %>%
        layout(xaxis = list(title = input$varSelect),
               yaxis = list(title = "Count"))
    } else if (is.factor(var) || is.character(var) || is.logical(var)) {
      freq <- as.data.frame(table(var))
      p <- plot_ly(freq, x = ~var, y = ~Freq, type = "bar") %>%
        layout(xaxis = list(title = input$varSelect),
               yaxis = list(title = "Count"))
    } else {
      p <- plot_ly(x = 1, y = 1, type = "scatter", mode = "text",
                   text = paste("No plot available for", class(var)[1], "variables"),
                   textposition = "middle center") %>%
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    }
    p
  })
  
  # Data preview output
  output$dataPreview <- renderDT({
    req(loadedData())
    datatable(loadedData(), 
              options = list(scrollX = TRUE, 
                             pageLength = 10))
  })
}