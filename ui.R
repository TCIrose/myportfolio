library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(plotly)
library(bslib)
library(waiter)
library(readxl)    # For Excel files
library(haven)     # For SAS, SPSS, Stata files

ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(
    title = span(tagList(
      icon("rocket"), 
      span("Tony Irose", style = "font-weight: 300; font-size: 22px"),
      span(" | Data Analyst", style = "font-weight: 300; font-size: 14px")
    )),
    titleWidth = 300,
    dropdownMenuOutput("notificationMenu")
  ),
  
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("About Me", tabName = "about", icon = icon("user-astronaut")),
      menuItem("Skills & Education", tabName = "skills", icon = icon("brain")),
      
      menuItem("Projects", icon = icon("project-diagram"),
               menuSubItem("Obesity & Education", tabName = "obesity_education"),
               menuSubItem("Demand Forecasting", tabName = "demand_forecast"),
               menuSubItem("Airline Reviews", tabName = "airline_reviews")
      ),
      menuItem("Interactive Projects", icon = icon("laptop-code"),
               menuSubItem("Data Explorer", tabName = "data_explorer"),
               menuSubItem("Data Cleaning", tabName = "data_cleaning") # the data cleaner
      ),
      
      menuItem("Blog", tabName = "blog", icon = icon("blog"),
               menuSubItem("Data Science Tips", tabName = "b1"),
               menuSubItem("Career Insights", tabName = "b2"),
               menuSubItem("Tech Trends", tabName = "b3")
      ),
      
      hr(),
      div(style = "text-align: center;",
          actionBttn("contactBtn", "Contact Me", 
                     style = "gradient", color = "primary", size = "sm"),
          br(), br(),
          actionLink("linkedinBtn", 
                     icon("linkedin", class = "fa-2x"),
                     style = "color: #0077B5; margin: 0 5px;",
                     onclick = "window.open('http://www.linkedin.com/in/tony-irose-628060129', '_blank')"),
          actionLink("githubBtn", 
                     icon("github", class = "fa-2x"),
                     style = "color: #333; margin: 0 5px;",
                     onclick = "window.open('https://github.com/TCIrose', '_blank')"),
          actionLink("emailBtn", 
                     icon("envelope", class = "fa-2x"),
                     style = "color: #D44638; margin: 0 5px;",
                     onclick = "window.open('mailto:irosetony@gmail.com', '_blank')")
      )
    )
  ),
  
  dashboardBody(
    use_waiter(),
    waiter_show_on_load(html = spin_orbit()),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      tags$style(HTML("
                      /* Custom CSS */
                      .main-header .logo {
                      font-family: 'Arial', sans-serif;
                      font-weight: bold;
                      font-size: 20px;
                      }
                      
                      .skin-black .main-sidebar {
                      background-color: #2c3e50;
                      }
                      
                      .content-wrapper {
                      background-color: #f9f9f9;
                      }
                      
                      .box {
                      border-radius: 8px;
                      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                      border-top: none;
                      }
                      
                      .box-header {
                      border-radius: 8px 8px 0 0;
                      background-color: #ffffff;
                      border-bottom: 1px solid #eaeaea;
                      }
                      
                      .box-title {
                      font-weight: 600;
                      color: #2c3e50;
                      }
                      
                      /* Skill bars */
                      .skill-container {
                      width: 100%;
                      background-color: #e0e0e0;
                      border-radius: 5px;
                      margin-bottom: 15px;
                      }
                      
                      .skill-bar {
                      height: 20px;
                      border-radius: 5px;
                      background-color: #3498db;
                      text-align: center;
                      color: white;
                      font-weight: bold;
                      line-height: 20px;
                      }
                      
                      /* Timeline */
                      .timeline {
                      position: relative;
                      max-width: 100%;
                      margin: 0 auto;
                      }
                      
                      .timeline::after {
                      content: '';
                      position: absolute;
                      width: 2px;
                      background-color: #3498db;
                      top: 0;
                      bottom: 0;
                      left: 50%;
                      margin-left: -1px;
                      }
                      
                      .timeline-item {
                      padding: 10px 40px;
                      position: relative;
                      background-color: inherit;
                      width: 50%;
                      }
                      
                      .timeline-item::after {
                      content: '';
                      position: absolute;
                      width: 20px;
                      height: 20px;
                      background-color: white;
                      border: 4px solid #3498db;
                      border-radius: 50%;
                      top: 15px;
                      z-index: 1;
                      }
                      
                      .left {
                      left: 0;
                      }
                      
                      .right {
                      left: 50%;
                      }
                      
                      .left::after {
                      right: -12px;
                      }
                      
                      .right::after {
                      left: -12px;
                      }
                      
                      .timeline-content {
                      padding: 20px;
                      background-color: white;
                      border-radius: 8px;
                      box-shadow: 0 2px 5px rgba(0,0,0,0.1);
                      }
                      
                      /* Hover effects */
                      .box:hover {
                      transform: translateY(-5px);
                      transition: transform 0.3s ease;
                      box-shadow: 0 6px 12px rgba(0,0,0,0.15);
                      }
                      
                      /* Profile image */
                      .profile-img {
                      width: 150px;
                      height: 150px;
                      border-radius: 50%;
                      object-fit: cover;
                      border: 5px solid #3498db;
                      display: block;
                      margin: 0 auto;
                      box-shadow: 0 4px 8px rgba(0,0,0,0.2);
                      }
                      
                      /* Project cards */
                      .project-card {
                      border-radius: 8px;
                      overflow: hidden;
                      box-shadow: 0 4px 8px rgba(0,0,0,0.1);
                      transition: all 0.3s ease;
                      margin-bottom: 20px;
                      background: white;
                      }
                      
                      .project-card:hover {
                      transform: translateY(-5px);
                      box-shadow: 0 8px 16px rgba(0,0,0,0.2);
                      }
                      
                      .project-img {
                      width: 100%;
                      height: 180px;
                      object-fit: cover;
                      }
                      
                      .project-content {
                      padding: 15px;
                      }
                      
                      /* Tags */
                      .tag {
                      display: inline-block;
                      background-color: #3498db;
                      color: white;
                      padding: 2px 8px;
                      border-radius: 4px;
                      font-size: 12px;
                      margin-right: 5px;
                      margin-bottom: 5px;
                      }
                      
                      /* Data Explorer specific styles */
                      .alert-danger {
                      color: #721c24;
                      background-color: #f8d7da;
                      border-color: #f5c6cb;
                      padding: 10px;
                      border-radius: 4px;
                      margin-top: 10px;
                      }
                      
                      /* Make data tables responsive */
                      .dataTables_wrapper {
                      overflow-x: auto;
                      }
                      "))
      ),
    
    tabItems(
      # Home Section
      tabItem(
        tabName = "home",
        fluidRow(
          column(
            width = 12,
            align = "center",
            div(
              style = "background: linear-gradient(135deg, #3498db, #2c3e50); color: white; padding: 40px; border-radius: 8px; margin-bottom: 30px;",
              h1("Welcome to My Data Journey", style = "font-weight: 300;"),
              h3("Transforming Data into Actionable Insights", style = "font-weight: 300;"),
              br(),
              actionBttn("exploreBtn", "Explore My Work", 
                         style = "gradient", color = "primary", size = "lg")
            )
          )
        ),
        fluidRow(
          box(
            title = "Quick Stats", width = 4, status = "primary", solidHeader = TRUE,
            div(
              style = "text-align: center;",
              h2("3+", style = "color: #3498db;"),
              p("Years of Experience"),
              br(),
              h2("5+", style = "color: #3498db;"),
              p("Individual Projects Completed"),
              br(),
              h2("5+", style = "color: #3498db;"),
              p("Programming Languages")
            )
          ),
          box(
            title = "Featured Project", width = 4, status = "warning", solidHeader = TRUE,
            div(
              class = "project-card",
              tags$img(src = "logistic.png", class = "project-img"),
              div(
                class = "project-content",
                h4("Obesity & Education Analysis"),
                p("Exploring the relationship between education levels and obesity rates using logistic regression."),
                div(
                  span(class = "tag", "R"),
                  span(class = "tag", "Statistical modelling"),
                  span(class = "tag", "Data Visualization")
                ),
                br(),
                actionLink("viewProject1", "View Project →", style = "color: #3498db;")
              )
            )
          ),
          box(
            title = "Latest Blog Post", width = 4, status = "success", solidHeader = TRUE,
            div(
              class = "project-card",
              tags$img(src = "data_cleaning.jpeg", class = "project-img"),
              div(
                class = "project-content",
                h4("5 Data Cleaning Tricks You Should Know"),
                p("Learn some lesser-known techniques to clean messy data efficiently."),
                br(),
                actionLink("viewBlog1", "Read Post →", style = "color: #3498db;")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "My Tech Stack", width = 12,
            fluidRow(
              column(3, align = "center",
                     div(style = "margin-bottom: 20px;",
                         icon("python", class = "fa-4x", style = "color: #3776AB;"),
                         h4("Python"))
              ),
              column(3, align = "center",
                     div(style = "margin-bottom: 20px;",
                         icon("r-project", class = "fa-4x", style = "color: #276DC3;"),
                         h4("R"))
              ),
              column(3, align = "center",
                     div(style = "margin-bottom: 20px;",
                         icon("database", class = "fa-4x", style = "color: #F29111;"),
                         h4("SQL"))
              ),
              column(3, align = "center",
                     div(style = "margin-bottom: 20px;",
                         icon("java", class = "fa-4x", style = "color: #F29111;"),
                         h4("Java"))
              ),
              column(3, align = "center",
                     div(style = "margin-bottom: 20px;",
                         icon("js", class = "fa-4x", style = "color: #4BC0C0;"),
                         h4("Javascript"))
              )
            )
          )
        )
      ),
      
      # About Me Section
      tabItem(
        tabName = "about",
        fluidRow(
          box(
            title = "About Me", width = 12,
            fluidRow(
              column(
                width = 4, align = "center",
                tags$img(src = "me.jpeg", class = "profile-img"),
                br(), br(),
                h3("Tony Irose"),
                p("Data Analyst | GIS Specialist"),
                hr(),
                h4("Contact Info"),
                p(icon("envelope"), " irosetony@gmail.com"),
                p(icon("map-marker-alt"), " Nairobi, Kenya"),
                div(
                  style = "margin-top: 20px;",
                  actionBttn("downloadCV", "Download CV", 
                             style = "gradient", color = "primary", size = "sm")
                )
              ),
              column(
                width = 8,
                h3("Professional Summary"),
                p("I am a highly analytical and results-driven data professional with expertise in machine learning, natural language processing (NLP), and predictive modeling. I love using data insights to optimize business strategies and improve efficiency."),
                p("With experience in both the fashion retail and geospatial industries, I bring a unique perspective to data analysis problems."),
                hr(),
                h3("Career Timeline"),
                div(
                  class = "timeline",
                  div(
                    class = "timeline-item left",
                    div(
                      class = "timeline-content",
                      h4("2025 - Present"),
                      h5("Data Analyst @ Vazi Couture"),
                      p("Created and automated interactive dashboards for business performance monitoring and implemented machine learning methods to identify consumer insights.")
                    )
                  ),
                  div(
                    class = "timeline-item right",
                    div(
                      class = "timeline-content",
                      h4("2024"),
                      h5("GIS Specialist @ GICAT Merchants"),
                      p("Assisted in mapping amd visualization, and automating spatial data cleaning and outlier detection.")
                    )
                  ),
                  div(
                    class = "timeline-item left",
                    div(
                      class = "timeline-content",
                      h4("2022 - 2023"),
                      h5("Data Analyst @ Vazi Couture"),
                      p("Implemented demand forecasting models, sentiment analysis, visualized outcomes and automated data cleaning processes.")
                    )
                  ),
                  div(
                    class = "timeline-item right",
                    div(
                      class = "timeline-content",
                      h4("2017 - 2022"),
                      h5("Bachelor's Degree in Geospatial Information Science"),
                      p("Jomo Kenyatta University of Agriculture and Technology")
                    )
                  )
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Hobbies & Interests", width = 12,
            column(
              width = 6,
              h4(icon("book"), " Reading"),
              p("Interested in philosophy, especially physicalism and the qualia problem. Currently reading 'The Conscious Mind' by David Chalmers."),
              hr(),
              h4(icon("gamepad"), " Gaming"),
              p("Enjoy strategy games that challenge problem-solving skills. Favorite games include Civilization VI and XCOM 2.")
            ),
            column(
              width = 6,
              h4(icon("film"), " Movies & Series"),
              p("Love philosophy & physics-inspired sci-fi like Interstellar, Dark Matter, and Ex Machina. Always looking for thought-provoking content."),
              hr(),
              h4(icon("code"), " Side Projects"),
              p("When not working, I build small automation tools and explore new programming languages. Recently learning Rust for systems programming.")
            )
          )
        )
      ),
      
      # Skills & Education Section
      tabItem(
        tabName = "skills",
        fluidRow(
          box(
            title = "Technical Skills", width = 12,
            h4("Programming Languages"),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 60%;", "Python (60%)")
            ),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 75%;", "R (75%)")
            ),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 50%;", "SQL (50%)")
            ),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 50%;", "Java (50%)")
            ),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 50%;", "Javascript (50%)")
            ),
            br(),
            h4("Data Visualization"),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 50%;", "Power BI (50%)")
            ),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 60%;", "ggplot2 (60%)")
            ),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 50%;", "Tableau (50%)")
            ),
            br(),
            h4("Machine Learning"),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 60%;", "Regression (60%)")
            ),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 25%;", "Neural Networks (25%)")
            ),
            div(
              class = "skill-container",
              div(class = "skill-bar", style = "width: 30%;", "NLP (30%)")
            )
          )
        ),
        fluidRow(
          box(
            title = "Education", width = 6,
            div(
              class = "project-card",
              div(
                class = "project-content",
                h4("Bachelor's Degree in Geospatial Information Science"),
                p("Jomo Kenyatta University of Agriculture and Technology"),
                p("2017 - 2022"),
                hr(),
                h5("Key Courses:"),
                tags$ul(
                  tags$li("Advanced Spatial Modelling & Analysis"),
                  tags$li("Geographic Information Systems"),
                  tags$li("Remote Sensing"),
                  tags$li("Programming for Geospatial Applications"),
                  tags$li("Spatial visualization (Cartography)"),
                  tags$li("Geostatistics"),
                  tags$li("Database Management")
                )
              )
            )
          ),
          box(
            title = "Soft Skills", width = 6,
            div(
              class = "project-card",
              div(
                class = "project-content",
                h4("Professional Strengths"),
                div(
                  style = "display: flex; flex-wrap: wrap;",
                  div(style = "width: 50%; padding: 5px;",
                      h5(icon("lightbulb"), " Problem-Solving"),
                      p("Strong analytical thinking for tackling complex data challenges.")
                  ),
                  div(style = "width: 50%; padding: 5px;",
                      h5(icon("comments"), " Communication"),
                      p("Ability to explain technical concepts clearly to non-technical stakeholders.")
                  ),
                  div(style = "width: 50%; padding: 5px;",
                      h5(icon("random"), " Adaptability"),
                      p("Quick to learn new technologies and programming languages.")
                  ),
                  div(style = "width: 50%; padding: 5px;",
                      h5(icon("users"), " Collaboration"),
                      p("Work well in teams to deliver data-driven solutions.")
                  )
                )
              )
            )
          )
        ),# I have commented this out: i dont have any certification
        # fluidRow(
        #   box(
        #     title = "Certifications", width = 12,
        #     div(
        #       style = "display: flex; flex-wrap: wrap;",
        #       div(
        #         style = "width: 33%; padding: 10px;",
        #         class = "project-card",
        #         div(
        #           class = "project-content",
        #           h4("Data Science Specialization"),
        #           p("Coursera - Johns Hopkins University"),
        #           p("2021"),
        #           actionLink("viewCert1", "View Certificate →", style = "color: #3498db;")
        #         )
        #       ),
        #       div(
        #         style = "width: 33%; padding: 10px;",
        #         class = "project-card",
        #         div(
        #           class = "project-content",
        #           h4("Machine Learning A-Z"),
        #           p("Udemy"),
        #           p("2022"),
        #           actionLink("viewCert2", "View Certificate →", style = "color: #3498db;")
        #         )
        #       ),
        #       div(
        #         style = "width: 33%; padding: 10px;",
        #         class = "project-card",
        #         div(
        #           class = "project-content",
        #           h4("Power BI Masterclass"),
        #           p("DataCamp"),
        #           p("2023"),
        #           actionLink("viewCert3", "View Certificate →", style = "color: #3498db;")
        #         )
        #       )
        #     )
        #   )
        # )
      ),
      
      # Projects Sections
      tabItem(
        tabName = "obesity_education",
        fluidRow(
          box(
            title = "The Impact of Education on Obesity", width = 12,
            tags$iframe(src = "logistic_regression.html", 
                        width = "100%", height = "600px", 
                        frameborder = 0)
          )
        )
      ),
      
      tabItem(
        tabName = "demand_forecast",
        fluidRow(
          box(
            title = "Demand Forecasting using RNN", width = 12,
            tags$iframe(src = "demand_forecast.html", 
                        width = "100%", height = "600px", 
                        frameborder = 0)
          )
        )
      ),
      
      tabItem(
        tabName = "airline_reviews",
        fluidRow(
          box(
            title = "Airline Reviews Analysis", width = 12,
            tags$iframe(src = "airline_reviews_analysis.pdf", 
                        width = "100%", height = "600px", 
                        frameborder = 0)
          )
        )
      ),
      # Interactive projects section
      tabItem(
        tabName = "data_explorer",
        fluidRow(
          box(
            title = "Data Explorer", width = 12, status = "primary", solidHeader = TRUE,
            h4("Upload your dataset to explore its structure and contents"),
            
            # File upload section
            fileInput("fileUpload", "Choose Data File",
                      multiple = FALSE,
                      accept = c(".csv", ".xls", ".xlsx", ".xpt", ".sas7bdat", ".dat", ".txt", ".rds")),
            
            # Data loading button
            conditionalPanel(
              condition = "output.fileUploaded",
              actionButton("loadData", "Load Data", class = "btn-primary")
            ),
            
            # Data exploration section
            conditionalPanel(
              condition = "output.dataLoaded",
              tabBox(
                width = 12,
                tabPanel(
                  "Data Structure",
                  h4("Dataset Dimensions:"),
                  verbatimTextOutput("dataDims"),
                  h4("Variable Types:"),
                  DTOutput("varTypes")
                ),
                tabPanel(
                  "Variable Explorer",
                  selectInput("varSelect", "Select Variable:", choices = NULL),
                  radioButtons("treatAs", "Treat variable as:",
                               choices = c("Automatic" = "auto",
                                           "Numeric" = "numeric",
                                           "Categorical" = "categorical"),
                               inline = TRUE),
                  h4("Variable Summary:"),
                  verbatimTextOutput("varSummary"),
                  h4("Value Distribution:"),
                  plotlyOutput("varPlot"),
                  
                  # Cleaning options for this specific variable
                  h4("Data Cleaning Options for this Variable"),
                  checkboxInput("removeNAVar", "Remove NA values for this variable", FALSE),
                  conditionalPanel(
                    condition = "input.treatAs == 'numeric' || (input.treatAs == 'auto' && output.isVarNumeric)",
                    checkboxInput("removeOutliersVar", "Remove outliers for this variable", FALSE),
                    conditionalPanel(
                      condition = "input.removeOutliersVar",
                      sliderInput("outlierThresholdVar", "Outlier threshold (IQR multiplier):",
                                  min = 1, max = 5, value = 1.5, step = 0.1)
                    )
                  ),
                  actionButton("cleanVar", "Apply Cleaning to This Variable", 
                               class = "btn-warning")
                ),
                tabPanel(
                  "Advanced Visualization",
                  fluidRow(
                    column(4,
                           selectInput("visX", "X-axis Variable:", choices = NULL),
                           selectInput("visY", "Y-axis Variable (for scatter/box):", 
                                       choices = c("None" = "none")),
                           selectInput("plotType", "Plot Type:",
                                       choices = c("Histogram" = "hist",
                                                   "Box Plot" = "box",
                                                   "Bar Chart" = "bar",
                                                   "Scatter Plot" = "scatter",
                                                   "Violin Plot" = "violin",
                                                   "Line Chart" = "line"))
                    ),
                    column(4,
                           selectInput("group1", "Group By (Primary):", 
                                       choices = c("None" = "none")),
                           selectInput("group2", "Group By (Secondary - optional):", 
                                       choices = c("None" = "none")),
                           radioButtons("summaryFunc", "Summary Function:",
                                        choices = c("Mean" = "mean",
                                                    "Median" = "median",
                                                    "Sum" = "sum",
                                                    "Count" = "count"),
                                        selected = "mean",
                                        inline = TRUE)
                    ),
                    column(4,
                           checkboxInput("showPoints", "Show individual points", FALSE),
                           conditionalPanel(
                             condition = "input.plotType == 'scatter' || input.plotType == 'violin'",
                             checkboxInput("addTrend", "Add trend line", FALSE)
                           ),
                           br(),
                           actionButton("generatePlot", "Generate Plot", 
                                        class = "btn-primary")
                    )
                  ),
                  plotlyOutput("advancedPlot"),
                  h4("Plot Data Summary"),
                  verbatimTextOutput("plotSummary")
                ),
                tabPanel(
                  "Data Preview",
                  DTOutput("dataPreview")
                )
              )
            ),
            
            # Error messages
            conditionalPanel(
              condition = "output.loadError",
              div(class = "alert alert-danger",
                  textOutput("errorMessage"))
            )
          )
        )
      ),
      # Data Cleaning Tab
      tabItem(
        tabName = "data_cleaning",
        fluidRow(
          box(
            title = "Data Cleaning", width = 12,
            h4("Clean and preprocess your data"),
            
            # File upload section
            fileInput("cleanFileUpload", "Choose Data File",
                      multiple = FALSE,
                      accept = c(".csv", ".xls", ".xlsx", ".xpt", ".sas7bdat", ".dat", ".txt", ".rds")),
            actionButton("cleanLoadData", "Load Data"),
            
            
            # Subsetting Data
            h4("Subsetting Data"),
            fluidRow(
              column(12, pickerInput("subsetVars", "Select Columns to Keep:", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE))),
              column(12, actionButton("applySubset", "Apply Subset")) # Add the apply button
              
            ),
            hr(),
            
            # Convert Data Types
            h4("Convert Data Types"),
            fluidRow(
              column(6, selectInput("convertVar", "Select Variable:", choices = NULL)),
              column(6, radioButtons("convertTo", "Convert to:",
                                     choices = c("Categorical" = "categorical", "Continuous" = "continuous"),
                                     inline = TRUE)),
              column(12, actionButton("applyConvert", "Apply Conversion"))
            ),
            hr(),
            
            # Handle Duplicates
            h4("Handle Duplicates"),
            fluidRow(
              column(12, actionButton("removeDuplicates", "Remove Duplicate Rows"))
            ),
            hr(),
            
            # Handle Missing Values
            h4("Handle Missing Values"),
            fluidRow(
              column(6, selectInput("missingVar", "Select Continuous Variable:", choices = NULL)),
              column(6, radioButtons("missingAction", "Action:",
                                     choices = c("Remove Rows" = "remove",
                                                 "Replace with Mean" = "mean",
                                                 "Replace with Median" = "median",
                                                 "Replace with Mode" = "mode"),
                                     inline = TRUE)),
              column(12, actionButton("applyMissing", "Apply Action"))
            ),
            hr(),
            
            # Outlier Detection & Removal
            h4("Outlier Detection & Removal"),
            fluidRow(
              column(6, selectInput("outlierVar", "Select Variable(s):", choices = NULL, multiple = FALSE)),
              column(6, radioButtons("outlierMethod", "Outlier Removal Method:",
                                     choices = c("IQR" = "iqr", "Z-Score" = "zscore"),
                                     inline = TRUE)),
              conditionalPanel(
                condition = "input.outlierMethod == 'iqr'",
                column(6, sliderInput("iqrDegree", "IQR Multiplier:", min = 1, max = 5, value = 1.5, step = 0.1))
              ),
              conditionalPanel(
                condition = "input.outlierMethod == 'zscore'",
                column(6, numericInput("zscoreThreshold", "Z-Score Threshold:", value = 3, min = 1))
              ),
              column(12, plotOutput("outlierPlot")), # Visualization
              column(12, actionButton("applyOutlier", "Apply Outlier Removal"))
            ),
            hr(),
            
            # Filtering Data
            h4("Filtering Data"),
            fluidRow(
              column(6, selectInput("filterVar", "Select Variable:", choices = NULL)),
              conditionalPanel(
                condition = "output.isFilterVarNumeric", # Corrected condition
                column(6, textInput("numericFilter", "Numeric Filter (e.g., '> 10')"))
              ),
              conditionalPanel(
                condition = "output.isFilterVarCategorical", # Corrected condition
                column(6, checkboxGroupInput("categoricalFilters", "Select Categories:", choices = NULL))
              ),
              column(12, actionButton("applyFilter", "Apply Filter"))
            ),
            hr(),
            
            # Categorical Data Cleaning
            h4("Categorical Data Cleaning"),
            fluidRow(
              column(6, selectInput("categoricalCleanVar", "Select Textual Variable:", choices = NULL)),
              column(6, checkboxGroupInput("categoricalCleanOptions", "Cleaning Options:",
                                           choices = c("Convert to Lowercase" = "lower",
                                                       "Trim Whitespace" = "trim",
                                                       "Apply Fuzzy Matching" = "fuzzy"))
              ),
              conditionalPanel(
                condition = "input.categoricalCleanOptions.includes('fuzzy')",
                column(12, verbatimTextOutput("fuzzyMatches")), # Display potential matches
                column(12, selectInput("fuzzyMatchChoice", "Choose Match for Selected Value", choices = NULL, multiple = TRUE)) # User selects their preferred matches
              ),
              column(12, actionButton("applyCategoricalClean", "Apply Cleaning"))
            ),
            hr(),
            
            # Download data Button Section
            h4("Download Data"),
            fluidRow(
              column(12, downloadButton("downloadData", "Download Cleaned Data"))
            )
          )
        )
      ),
      
      # Blog Sections
      tabItem(
        tabName = "b1",
        fluidRow(
          box(
            title = "5 Data Cleaning Tricks You Should Know", width = 12,
            div(
              style = "padding: 20px;",
              h3("Making Data Cleaning Less Painful"),
              p("Data cleaning is often the most time-consuming part of any data analysis project. Here are five tricks that have saved me countless hours:"),
              h4("1. The Power of Regular Expressions"),
              p("Regular expressions can seem intimidating, but they're incredibly powerful for pattern matching and text cleaning. For example:"),
              pre("import re\n# Extract all email addresses from text\nemails = re.findall(r'[\\w\\.-]+@[\\w\\.-]+', text)"),
              h4("2. Automated Outlier Detection"),
              p("Instead of manually inspecting for outliers, use statistical methods:"),
              pre("# Using z-scores for outlier detection\nfrom scipy import stats\nz_scores = stats.zscore(data)\nabs_z_scores = np.abs(z_scores)\noutliers = (abs_z_scores > 3).all(axis=1)"),
              h4("3. Fuzzy Matching for Messy Categorical Data"),
              p("When dealing with inconsistent category names (e.g., 'New York', 'NY', 'New York City'), fuzzy matching can help:"),
              pre("from fuzzywuzzy import process\nmatches = process.extract('New York', ['NY', 'New York City', 'Boston'], limit=2)"),
              h4("4. Parallel Processing for Large Datasets"),
              p("Speed up operations on large datasets using multiprocessing:"),
              pre("from multiprocessing import Pool\n\ndef clean_chunk(chunk):\n    # cleaning operations\n    return cleaned_chunk\n\nwith Pool(4) as p:\n    results = p.map(clean_chunk, df_splits)"),
              h4("5. Automated Data Validation Reports"),
              p("Create validation functions that generate reports on data quality issues:"),
              pre("def validate_data(df):\n    report = {}\n    report['missing_values'] = df.isnull().sum()\n    report['duplicates'] = df.duplicated().sum()\n    # Add more checks\n    return pd.DataFrame(report)"),
              hr(),
              p("What are your favorite data cleaning tricks? Share in the comments below!"),
              textInput("comment", "Add a comment:"),
              actionButton("submitComment", "Submit", class = "btn-primary")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "b2",
        fluidRow(
          box(
            title = "Transitioning from Academia to Industry Data Science", width = 12,
            div(
              style = "padding: 20px;",
              h3("Lessons from My Career Transition"),
              p("Moving from an academic background in Geospatial Science to industry data science presented several challenges and learning opportunities. Here are my key takeaways:"),
              h4("1. The Importance of Business Context"),
              p("In academia, the focus is often on methodological rigor. In industry, the emphasis shifts to solving business problems. Understanding the business context is crucial for creating impactful models."),
              h4("2. Communication is Key"),
              p("You'll need to explain technical concepts to non-technical stakeholders. Developing clear visualization and storytelling skills is as important as technical skills."),
              h4("3. The 80/20 Rule of Model Building"),
              p("In industry, a 'good enough' model deployed now is often more valuable than a perfect model deployed too late. Focus on quick iterations and incremental improvements."),
              h4("4. Continuous Learning"),
              p("The field evolves rapidly. Dedicate time each week to learning new techniques, tools, and best practices."),
              h4("5. Building a Portfolio"),
              p("Side projects and a strong portfolio helped me demonstrate my skills more effectively than my academic credentials alone."),
              hr(),
              plotlyOutput("careerTransitionPlot"),
              hr(),
              p("What challenges did you face in your career transition? Share your experience below!"),
              textInput("comment2", "Add a comment:"),
              actionButton("submitComment2", "Submit", class = "btn-primary")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "b3",
        fluidRow(
          box(
            title = "Emerging Trends in Geospatial Data Science", width = 12,
            div(
              style = "padding: 20px;",
              h3("Where Location Intelligence is Heading"),
              p("The intersection of geospatial science and data science is creating exciting new opportunities. Here are three trends to watch:"),
              h4("1. Real-time Geospatial Analytics"),
              p("With IoT devices and 5G networks, we can now process and analyze location data in real-time, enabling applications like:"),
              tags$ul(
                tags$li("Dynamic traffic routing"),
                tags$li("Emergency response optimization"),
                tags$li("Precision agriculture monitoring")
              ),
              h4("2. AI-powered Satellite Imagery Analysis"),
              p("Deep learning models can now extract insights from satellite imagery at scale:"),
              tags$ul(
                tags$li("Automated land use classification"),
                tags$li("Deforestation monitoring"),
                tags$li("Urban growth prediction")
              ),
              h4("3. Spatial Data Science in Public Health"),
              p("The pandemic accelerated the use of spatial analysis in epidemiology, with applications like:"),
              tags$ul(
                tags$li("Disease spread modeling"),
                tags$li("Vaccine distribution optimization"),
                tags$li("Health facility accessibility analysis")
              ),
              hr(),
              p("Which of these trends do you find most promising? Let me know your thoughts!"),
              selectInput("trendOpinion", "Most Promising Trend:",
                          choices = c("Real-time Analytics", 
                                      "Satellite Imagery AI", 
                                      "Public Health Applications")),
              actionButton("submitTrend", "Vote", class = "btn-primary"),
              textOutput("trendResults")
            )
          )
        )
      )
    )
      )
      )
