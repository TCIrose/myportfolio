library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(rmarkdown)

#Define the User interface
ui <- dashboardPage(
  dashboardHeader(title = "Tony Irose's Portfolio"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("About Me", tabName = "about", icon = icon("user")),
      menuItem("Skills & Education", tabName = "skills", icon = icon("graduation-cap")),
      
      menuItem("Projects", icon = icon("folder"),
               menuSubItem("Obesity & Education", tabName = "obesity_education"),
               menuSubItem("Demand Forecasting", tabName = "demand_forecast"),
               menuSubItem("Airline Reviews", tabName = "airline_reviews")
      ),
      
      menuItem("Blog", tabName = "blog", icon = icon("book"),
               menuSubItem("Blog 1", tabName = "b1"),
               menuSubItem("Blog 2", tabName = "b2"),
               menuSubItem("Blog 3", tabName = "b3")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home Section
      tabItem(tabName = "home",
              fluidRow(
                box(title = "Welcome!", width = 12, 
                    "Welcome to my portfolio! I am Tony Irose, a data analyst specializing in Python and R.")
              )
      ),
      
      # About Me Section
      tabItem(tabName = "about",
              fluidRow(
                box(title = "About Me", width = 12,
                    tags$p("I am highly analytical and results-driven with expertise in machine learning, natural language processing (NLP), and predictive modeling."),
                    tags$p("I love using data insights to optimize business strategies and improve efficiency."),
                    tags$br(),
                    tags$strong("📧 Contact: "), "irosetony@gmail.com",
                    tags$br(),
                    tags$strong("🔗 LinkedIn: "), tags$a(href="http://www.linkedin.com/in/tony-irose-628060129", "Visit my LinkedIn", target="_blank"),
                    tags$br(),
                    tags$strong("📍 Location: "), "Nairobi, Kenya"
                )
              ),
              fluidRow(
                box(title = "Hobbies & Interests", width = 12,
                    tags$ul(
                      tags$li("💻 **Coding** - I love learning new programming languages and creating simple automation scripts."),
                      tags$li("📖 **Reading** - Interested in philosophy, especially physicalism and the qualia problem."),
                      tags$li("🎬 **Movies & Series** - Enjoys philosophy & physics-inspired sci-fi like *Interstellar*, *Dark Matter*, and *Ex Machina*."),
                      tags$li("🎮 **Gaming** - Likes playing strategy games occasionally.")
                    )
                )
              )
      ),
      
      # Skills & Education Section
      tabItem(tabName = "skills",
              fluidRow(
                box(title = "Education", width = 12,
                    tags$p("🎓 Bachelor's Degree in Geospatial Information Science"),
                    tags$p("Jomo Kenyatta University of Agriculture and Technology (2017 - 2022)")
                ),
                box(title = "Technical Skills", width = 12,
                    tags$ul(
                      tags$li("Programming: Python, R"),
                      tags$li("Data Visualization: Power BI, ggplot2, Matplotlib, Seaborn, Tableau"),
                      tags$li("Machine Learning: Multiple Linear Regression, Neural Networks"),
                      tags$li("Database Management: SQL, MongoDB")
                    )
                ),
                box(title = "Soft Skills", width = 12,
                    tags$ul(
                      tags$li("Problem-Solving - Strong analytical thinking for tackling complex data challenges."),
                      tags$li("Communication - Ability to explain technical concepts clearly."),
                      tags$li("Adaptability - Quick to learn new technologies and programming languages."),
                      tags$li("Collaboration - Work well in teams to deliver data-driven solutions.")
                    )
                ),
                box(title = "Industries I've Worked In", width = 12,
                    tags$ul(
                      tags$li("👕 **Fashion & Retail** - Data Analyst at Vazi Couture (2022 - Present)"),
                      tags$li("🗺 **Geospatial & Construction** - GIS Specialist at GICAT Merchants (2024)")
                    )
                )
              )
      ),
      
      # Logistic Regression Project
      tabItem(tabName = "obesity_education",
              fluidRow(
                box(title = "The Impact of Education on Obesity", width = 12,
                    tags$iframe(src = "logistic_regression.html", 
                                width = "100%", height = "600px", 
                                frameborder = 0))
              )
      ),
      
      # Demand Forecast
      tabItem(tabName = "demand_forecast",
              fluidRow(
                box(title = "Demand Forecasting using RNN", width = 12,
                    tags$iframe(src = "demand_forecast.html", 
                                width = "100%", height = "600px", 
                                frameborder = 0))
                
              )
      ),
      # Airline Reviews Analysis Project
      tabItem(tabName = "airline_reviews",
              fluidRow(
                box(title = "Airline Reviews Analysis", width = 12,
                    tags$iframe(src = "airline_reviews_analysis.pdf", 
                                width = "100%", height = "600px", 
                                frameborder = 0))
              )
      ),
      
      # Blog Section
      tabItem(tabName = "b1",
              fluidRow(
                box(title = "First blog", width = 12, 
                    "This is where my first blog post will appear.")
              )
      ),
      tabItem(tabName = "b2",
              fluidRow(
                box(title = "Second blog", width = 12, 
                    "This is where my second blog post will appear.")
              )
      ),
      tabItem(tabName = "b3",
              fluidRow(
                box(title = "Third blog", width = 12, 
                    "This is where my third blog post will appear.")
              )
      )
    )
  )
)

# Define server logic 

server <- function(input, output) {}


# Run the application 
shinyApp(ui = ui, server = server)

