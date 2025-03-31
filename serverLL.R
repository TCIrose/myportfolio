library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(shinydashboard)
library(bslib)
library(stringdist)

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
  loadedData <- reactiveVal(NULL)
  cleanData <- reactiveVal(NULL)
  
  # Observe file upload
  observeEvent(input$fileUpload, {
    req(input$fileUpload)
    loadedData(NULL)
    cleanData(NULL)
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
      
      if (!is.data.frame(data)) {
        data <- as.data.frame(data)
      }
      
      if (is.null(data) || nrow(data) == 0) {
        stop("Data loaded but appears empty")
      }
      
      loadedData(data)
      cleanedData(data)
      
      # Update variable selections with actual columns
      updateSelectInput(session, "varSelect", choices = names(data))
      updateSelectInput(session, "visX", choices = names(data))
      updateSelectInput(session, "visY", choices = c("None" = "none", names(data)))
      updateSelectInput(session, "group1", choices = c("None" = "none", names(data)))
      updateSelectInput(session, "group2", choices = c("None" = "none", names(data)))
      
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
  
  # Check if selected variable is numeric
  output$isVarNumeric <- reactive({
    req(cleanedData(), input$varSelect)
    var <- cleanedData()[[input$varSelect]]
    is.numeric(var)
  })
  outputOptions(output, "isVarNumeric", suspendWhenHidden = FALSE)
  
  # Data dimensions output
  output$dataDims <- renderPrint({
    req(cleanedData())
    dim(cleanedData())
  })
  
  # Variable types table
  output$varTypes <- renderDT({
    req(cleanedData())
    var_info <- data.frame(
      Variable = names(cleanedData()),
      Type = sapply(cleanedData(), function(x) class(x)[1]),
      stringsAsFactors = FALSE
    )
    datatable(var_info, options = list(dom = 't'))
  })
  
  # Clean individual variable
  observeEvent(input$cleanVar, {
    req(cleanedData(), input$varSelect)
    
    tryCatch({
      data <- cleanedData()
      var_name <- input$varSelect
      var <- data[[var_name]]
      
      # Handle NA values
      if (input$removeNAVar) {
        data <- data[!is.na(var), ]
      }
      
      # Handle outliers for numeric variables
      if ((input$treatAs == "numeric" || 
           (input$treatAs == "auto" && is.numeric(var))) && 
          input$removeOutliersVar) {
        qnt <- quantile(var, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- IQR(var, na.rm = TRUE)
        lower <- qnt[1] - (input$outlierThresholdVar * iqr)
        upper <- qnt[2] + (input$outlierThresholdVar * iqr)
        data <- data[var >= lower & var <= upper, ]
      }
      
      cleanedData(data)
      showNotification(paste("Cleaning applied to variable", var_name), 
                       type = "message")
    }, error = function(e) {
      showNotification(paste("Error in cleaning:", e$message), type = "error")
    })
  })
  
  # Variable summary output
  output$varSummary <- renderPrint({
    req(cleanedData(), input$varSelect)
    var <- cleanedData()[[input$varSelect]]
    
    # Determine how to treat variable
    treat_as <- input$treatAs
    if (treat_as == "auto") {
      treat_as <- ifelse(is.numeric(var), "numeric", "categorical")
    }
    
    if (treat_as == "numeric") {
      if (!is.numeric(var)) var <- as.numeric(var)
      cat("Numeric Variable Summary:\n")
      cat("------------------------\n")
      cat("Mean:", mean(var, na.rm = TRUE), "\n")
      cat("Median:", median(var, na.rm = TRUE), "\n")
      cat("Min:", min(var, na.rm = TRUE), "\n")
      cat("Max:", max(var, na.rm = TRUE), "\n")
      cat("SD:", sd(var, na.rm = TRUE), "\n")
      cat("Variance:", var(var, na.rm = TRUE), "\n")
      cat("NA values:", sum(is.na(var)), "\n")
      cat("Outliers (using 1.5*IQR):", sum(boxplot.stats(var)$out), "\n")
    } else {
      if (!is.factor(var)) var <- as.factor(var)
      cat("Categorical Variable Summary:\n")
      cat("---------------------------\n")
      print(table(var))
      cat("\nNA values:", sum(is.na(var)), "\n")
    }
  })
  
  # Variable plot
  output$varPlot <- renderPlotly({
    req(cleanedData(), input$varSelect)
    var <- cleanedData()[[input$varSelect]]
    
    # Determine how to treat variable
    treat_as <- input$treatAs
    if (treat_as == "auto") {
      treat_as <- ifelse(is.numeric(var), "numeric", "categorical")
    }
    
    if (treat_as == "numeric") {
      if (!is.numeric(var)) var <- as.numeric(var)
      p <- plot_ly(x = ~var, type = "histogram", 
                   nbinsx = 30) %>%
        layout(xaxis = list(title = input$varSelect),
               yaxis = list(title = "Count"))
    } else {
      if (!is.factor(var)) var <- as.factor(var)
      freq <- as.data.frame(table(var))
      p <- plot_ly(freq, x = ~var, y = ~Freq, type = "bar") %>%
        layout(xaxis = list(title = input$varSelect),
               yaxis = list(title = "Count"))
    }
    p
  })
  
  # Prepare data for advanced visualization
  plotData <- reactive({
    req(cleanedData(), input$visX)
    
    data <- cleanedData()
    x_var <- input$visX
    y_var <- if (input$visY != "none") input$visY else NULL
    group1 <- if (input$group1 != "none") input$group1 else NULL
    group2 <- if (input$group2 != "none") input$group2 else NULL
    
    # Convert grouping variables to factors
    if (!is.null(group1)) {
      data[[group1]] <- as.factor(data[[group1]])
    }
    if (!is.null(group2)) {
      data[[group2]] <- as.factor(data[[group2]])
    }
    
    # Create summary data if needed
    if (!is.null(y_var) || !is.null(group1)) {
      summary_vars <- c()
      if (!is.null(y_var)) summary_vars <- c(summary_vars, y_var)
      if (is.null(y_var) && input$plotType %in% c("bar", "box", "violin")) {
        summary_vars <- c(summary_vars, x_var)
      }
      
      group_vars <- c()
      if (!is.null(group1)) group_vars <- c(group_vars, group1)
      if (!is.null(group2)) group_vars <- c(group_vars, group2)
      
      if (length(summary_vars) > 0 && length(group_vars) > 0) {
        # Create summary using the selected function
        summary_func <- switch(input$summaryFunc,
                               "mean" = function(x) mean(x, na.rm = TRUE),
                               "median" = function(x) median(x, na.rm = TRUE),
                               "sum" = function(x) sum(x, na.rm = TRUE),
                               "count" = function(x) length(x))
        
        # Only proceed if all variables exist
        if (all(c(summary_vars, group_vars) %in% names(data))) {
          plot_data <- data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(across(all_of(summary_vars), 
                             summary_func, 
                             .names = "{.col}"),
                      .groups = "drop")
          
          return(plot_data)
        }
      }
    }
    
    return(data)
  })
  
  # Advanced visualization
  observeEvent(input$generatePlot, {
    req(cleanedData(), input$visX)
    
    output$advancedPlot <- renderPlotly({
      tryCatch({
        data <- plotData()
        if (is.null(data)) {
          stop("No data available for plotting")
        }
        
        x_var <- input$visX
        y_var <- if (input$visY != "none") input$visY else NULL
        group1 <- if (input$group1 != "none") input$group1 else NULL
        group2 <- if (input$group2 != "none") input$group2 else NULL
        
        # Validate variables exist
        validate(
          need(x_var %in% names(data), paste("X variable", x_var, "not found in data"))
        )
        if (!is.null(y_var)) {
          validate(
            need(y_var %in% names(data), paste("Y variable", y_var, "not found in data"))
          )
        }
        
        # Create plot based on type
        p <- switch(input$plotType,
                    "hist" = {
                      if (!is.null(group1)) {
                        plot_ly(data, x = ~get(x_var), type = "histogram", 
                                color = ~get(group1), barmode = "stack")
                      } else {
                        plot_ly(data, x = ~get(x_var), type = "histogram")
                      }
                    },
                    "box" = {
                      if (!is.null(y_var)) {
                        if (!is.null(group1)) {
                          plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
                                  color = ~get(group1), type = "box")
                        } else {
                          plot_ly(data, x = ~get(x_var), y = ~get(y_var), type = "box")
                        }
                      } else {
                        if (!is.null(group1)) {
                          plot_ly(data, y = ~get(x_var), color = ~get(group1), type = "box")
                        } else {
                          plot_ly(data, y = ~get(x_var), type = "box")
                        }
                      }
                    },
                    "bar" = {
                      if (!is.null(y_var)) {
                        if (!is.null(group1)) {
                          plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
                                  color = ~get(group1), type = "bar")
                        } else {
                          plot_ly(data, x = ~get(x_var), y = ~get(y_var), type = "bar")
                        }
                      } else {
                        # Count by x_var
                        agg_data <- data %>% 
                          group_by(across(all_of(c(x_var, group1)))) %>% 
                          summarise(count = n(), .groups = "drop")
                        
                        if (!is.null(group1)) {
                          plot_ly(agg_data, x = ~get(x_var), y = ~count, 
                                  color = ~get(group1), type = "bar")
                        } else {
                          plot_ly(agg_data, x = ~get(x_var), y = ~count, type = "bar")
                        }
                      }
                    },
                    "scatter" = {
                      req(y_var)
                      if (!is.null(group1)) {
                        plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
                                color = ~get(group1), type = "scatter", mode = "markers")
                      } else {
                        plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
                                type = "scatter", mode = "markers")
                      }
                    },
                    "violin" = {
                      if (!is.null(y_var)) {
                        if (!is.null(group1)) {
                          plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
                                  color = ~get(group1), type = "violin")
                        } else {
                          plot_ly(data, x = ~get(x_var), y = ~get(y_var), type = "violin")
                        }
                      } else {
                        if (!is.null(group1)) {
                          plot_ly(data, y = ~get(x_var), color = ~get(group1), type = "violin")
                        } else {
                          plot_ly(data, y = ~get(x_var), type = "violin")
                        }
                      }
                    },
                    "line" = {
                      req(y_var)
                      if (!is.null(group1)) {
                        plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
                                color = ~get(group1), type = "scatter", mode = "lines+markers")
                      } else {
                        plot_ly(data, x = ~get(x_var), y = ~get(y_var), 
                                type = "scatter", mode = "lines+markers")
                      }
                    },
                    stop("Unknown plot type selected")
        )
        
        # Add trend line if requested
        if (input$plotType %in% c("scatter", "line") && input$addTrend) {
          p <- p %>% add_lines(y = ~fitted(loess(get(y_var) ~ get(x_var))),
                               line = list(color = 'red'),
                               name = "Trend")
        }
        
        # Add points if requested
        if (input$plotType == "violin" && input$showPoints) {
          p <- p %>% add_markers(jitter = 0.2, marker = list(opacity = 0.5))
        }
        
        # Final layout adjustments
        p <- p %>% layout(
          plot_bgcolor = "#e5ecf6",
          xaxis = list(title = x_var),
          yaxis = list(title = if (!is.null(y_var)) y_var else "Count"),
          showlegend = !is.null(group1)
        )
        
        p
        
      }, error = function(e) {
        plot_ly(x = 1, y = 1, type = "scatter", mode = "text",
                text = paste("Error creating plot:", e$message),
                textposition = "middle center") %>%
          layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      })
    })
    
    # Show plot data summary
    output$plotSummary <- renderPrint({
      data <- plotData()
      if (is.null(data)) {
        cat("No data available for summary\n")
        return()
      }
      
      cat("Plot Data Summary:\n")
      cat("------------------\n")
      cat("Number of observations:", nrow(data), "\n")
      cat("Variables used:\n")
      vars <- c(input$visX)
      if (input$visY != "none") vars <- c(vars, input$visY)
      if (input$group1 != "none") vars <- c(vars, input$group1)
      if (input$group2 != "none") vars <- c(vars, input$group2)
      
      for (var in unique(vars)) {
        if (var %in% names(data)) {
          cat("\n", var, " (", class(data[[var]])[1], "):\n", sep = "")
          if (is.numeric(data[[var]])) {
            cat("  Min:", min(data[[var]], na.rm = TRUE), "\n")
            cat("  Mean:", mean(data[[var]], na.rm = TRUE), "\n")
            cat("  Max:", max(data[[var]], na.rm = TRUE), "\n")
            cat("  NA's:", sum(is.na(data[[var]])), "\n")
          } else {
            cat("  Categories:", length(unique(data[[var]])), "\n")
            cat("  NA's:", sum(is.na(data[[var]])), "\n")
          }
        } else {
          cat("\n", var, ": Variable not found in data\n", sep = "")
        }
      }
    })
  })
  
  # Data preview output
  output$dataPreview <- renderDT({
    req(cleanedData())
    datatable(cleanedData(), 
              options = list(scrollX = TRUE, 
                             pageLength = 10))
  })
  
  # Data Cleaning Functions
  
  # Reactive value for data used in Data Cleaning
  cleanData <- reactiveVal(NULL)
  
  # Load data when cleanLoadData button is clicked
  observeEvent(input$cleanLoadData, {
    req(input$cleanFileUpload)
    
    tryCatch({
      file <- input$cleanFileUpload
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
      
      if (!is.data.frame(data)) {
        data <- as.data.frame(data)
      }
      
      if (is.null(data) || nrow(data) == 0) {
        stop("Data loaded but appears empty")
      }
      
      cleanData(data) # Update cleanData with uploaded data
      
      # Update variable selections with actual columns
      updateSelectInput(session, "convertVar", choices = names(data))
      updateSelectInput(session, "missingVar", choices = names(data)[sapply(data, is.numeric)])
      updateSelectInput(session, "outlierVar", choices = names(data))
      updateSelectInput(session, "subsetVars", choices = names(data))
      updateSelectInput(session, "filterVar", choices = names(data))
      updateSelectInput(session, "categoricalCleanVar", choices = names(data)[sapply(data, is.character)])
      
      output$cleanLoadError <- reactive(FALSE)
    }, error = function(e) {
      output$cleanLoadError <- reactive(TRUE)
      output$cleanErrorMessage <- renderText({
        paste("Error loading file:", e$message,
              "\nPlease try a different file format.")
      })
    })
    
    outputOptions(output, "cleanLoadError", suspendWhenHidden = FALSE)
  })  
  
  # Convert Data Types
  observeEvent(input$applyConvert, {
    req(cleanData(), input$convertVar, input$convertTo)
    
    data <- cleanData()
    var_name <- input$convertVar
    var <- data[[var_name]]
    
    if (input$convertTo == "categorical") {
      data[[var_name]] <- as.factor(var)
    } else if (input$convertTo == "continuous") {
      data[[var_name]] <- as.numeric(var)
    }
    
    cleanData(data)
  })
  
  # Handle Duplicates
  observeEvent(input$removeDuplicates, {
    req(cleanData())
    
    data <- cleanData()
    data <- distinct(data)
    cleanData(data)
  })
  
  # Handle Missing Values
  observeEvent(input$applyMissing, {
    req(cleanData(), input$missingVar, input$missingAction)
    
    data <- cleanData()
    var_name <- input$missingVar
    var <- data[[var_name]]
    
    if (input$missingAction == "remove") {
      data <- data[!is.na(var), ]
    } else if (input$missingAction == "mean") {
      data[[var_name]][is.na(var)] <- mean(var, na.rm = TRUE)
    } else if (input$missingAction == "median") {
      data[[var_name]][is.na(var)] <- median(var, na.rm = TRUE)
    } else if (input$missingAction == "mode") {
      mode_val <- names(sort(table(var), decreasing = TRUE))[1]
      data[[var_name]][is.na(var)] <- as.numeric(mode_val) # Assuming numeric mode
    }
    
    cleanData(data)
  })
  
  # Outlier Detection & Removal
  observeEvent(input$applyOutlier, {
    req(cleanData(), input$outlierVar, input$outlierMethod)
    
    data <- cleanData()
    
    for (var_name in input$outlierVar) {
      var <- data[[var_name]]
      
      if (input$outlierMethod == "iqr") {
        qnt <- quantile(var, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- IQR(var, na.rm = TRUE)
        lower <- qnt[1] - (input$iqrDegree * iqr)
        upper <- qnt[2] + (input$iqrDegree * iqr)
        data <- data[var >= lower & var <= upper, ]
      } else if (input$outlierMethod == "zscore") {
        z_scores <- abs(scale(var))
        data <- data[z_scores <= input$zscoreThreshold, ]
      }
    }
    
    cleanData(data)
  })
  
  # Outlier Plot
  output$outlierPlot <- renderPlot({
    req(cleanData(), input$outlierVar)
    
    plots <- lapply(input$outlierVar, function(var_name) {
      var <- cleanData()[[var_name]]
      
      # Before outlier removal
      p1 <- boxplot(var, main = paste("Before Outlier Removal:", var_name), plot = FALSE)
      
      # After outlier removal (Using IQR)
      if(input$outlierMethod == "iqr") {
        qnt <- quantile(var, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- IQR(var, na.rm = TRUE)
        lower <- qnt[1] - (input$iqrDegree * iqr)
        upper <- qnt[2] + (input$iqrDegree * iqr)
        
        var_clean <- var[var >= lower & var <= upper]
        p2 <- boxplot(var_clean, main = paste("After Outlier Removal (IQR):", var_name), plot = FALSE)
      } else { #Zscore
        z_scores <- abs(scale(var))
        var_clean <- var[z_scores <= input$zscoreThreshold]
        p2 <- boxplot(var_clean, main = paste("After Outlier Removal (Z-score):", var_name), plot = FALSE)
      }
      
      
      
      par(mfrow = c(1, 2)) # Display plots side by side
      boxplot(var, main = paste("Before Outlier Removal:", var_name))
      boxplot(var_clean, main = paste("After Outlier Removal:", var_name))
      par(mfrow = c(1, 1)) # Reset to single plot layout
    })
    
    # Return a dummy plot if there are no variables selected
    if (length(plots) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Select variable(s) to view outlier plots.")
    }
  })
  
  # Subsetting Data
  observeEvent(input$subsetVars, {
    req(cleanData(), input$subsetVars)
    
    data <- cleanData()
    data <- data[, input$subsetVars, drop = FALSE] # drop=FALSE to keep it as a data frame if only one column is selected
    cleanData(data)
  })
  
  # Filtering Data
  observeEvent(input$applyFilter, {
    req(cleanData(), input$filterVar)
    
    data <- cleanData()
    var_name <- input$filterVar
    var <- data[[var_name]]
    
    if (is.numeric(var)) {
      filter_expr <- input$numericFilter
      tryCatch({
        # Parse the filter expression
        parts <- strsplit(filter_expr, " ")[[1]]
        if (length(parts) == 2) {
          operator <- parts[1]
          value <- as.numeric(parts[2])
          
          # Apply dplyr filter based on operator
          data <- switch(operator,
                         ">=" = filter(data, !!sym(var_name) >= value),
                         ">" = filter(data, !!sym(var_name) > value),
                         "<=" = filter(data, !!sym(var_name) <= value),
                         "<" = filter(data, !!sym(var_name) < value),
                         "==" = filter(data, !!sym(var_name) == value),
                         "!=" = filter(data, !!sym(var_name) != value),
                         {
                           showNotification("Invalid numeric filter operator.", type = "error")
                           return()
                         }
          )
          cleanData(data)
        } else {
          showNotification("Invalid numeric filter format. Use e.g., '> 10' or '<= 5'.", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error in numeric filter:", e$message), type = "error")
      })
    } else if (is.character(var) || is.factor(var)) {
      selected_categories <- input$categoricalFilters
      data <- data[var %in% selected_categories, , drop = FALSE]
      cleanData(data)
    }
  })
  
  
  # Categorical Data Cleaning
  observeEvent(input$applyCategoricalClean, {
    req(cleanData(), input$categoricalCleanVar, input$categoricalCleanOptions)
    
    data <- cleanData()
    var_name <- input$categoricalCleanVar
    var <- data[[var_name]]
    
    if ("lower" %in% input$categoricalCleanOptions) {
      var <- tolower(var)
    }
    if ("trim" %in% input$categoricalCleanOptions) {
      var <- trimws(var)
    }
    if ("fuzzy" %in% input$categoricalCleanOptions) {
      # Apply user-selected fuzzy matches
      fuzzy_matches <- input$fuzzyMatchChoice
      if (!is.null(fuzzy_matches)) {
        for (match in fuzzy_matches){
          original_value = unlist(strsplit(match, " -> "))[1]
          new_value = unlist(strsplit(match, " -> "))[2]
          var[var == original_value] = new_value
        }
      }
    }
    
    data[[var_name]] <- var
    cleanData(data)
  })
  
  # Fuzzy Matching Output
  output$fuzzyMatches <- renderPrint({
    req(cleanData(), input$categoricalCleanVar, "fuzzy" %in% input$categoricalCleanOptions)
    
    var <- cleanData()[[input$categoricalCleanVar]]
    unique_values <- unique(var)
    
    if (length(unique_values) <= 1) {
      cat("No unique values to perform fuzzy matching on\n")
      return()
    }
    
    # Simple fuzzy matching example (using stringdist)
    matches <- list()
    for (i in seq_along(unique_values)) {
      for (j in seq_along(unique_values)) {
        if (i != j) {
          dist <- stringdist(unique_values[i], unique_values[j], method = "jw") # Jaro-Winkler distance
          if (dist < 0.2) { # Adjust threshold as needed
            matches[[length(matches) + 1]] <- paste(unique_values[i], "->", unique_values[j])
          }
        }
      }
    }
    
    if (length(matches) > 0) {
      cat("Potential fuzzy matches:\n")
      print(unlist(matches))
      
      # Update input choices with potential matches
      updateSelectInput(session, "fuzzyMatchChoice", choices = unlist(matches), selected = NULL)
    } else {
      cat("No potential fuzzy matches found\n")
    }
  })
}
