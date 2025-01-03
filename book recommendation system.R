library(shiny)
library(dplyr)
library(stringr)
library(googleAuthR)

# Load data
books <- read.csv("C:\\Users\\91701\\Downloads\\Books_with_Price_with_Genre.csv", stringsAsFactors = FALSE)

books <- books %>%
  filter(!is.na(Book.Title), !is.na(Book.Author), !is.na(Price), !is.na(Genre)) %>%
  select(ISBN, Book.Title, Book.Author, Publisher, Year.Of.Publication, Publication.Decade, Book.Age, Price, Genre)

recommend_books <- function(max_price, genre, n = 5) {
  
  recommended_books <- books %>%
    filter(Price <= max_price, str_detect(Genre, regex(genre, ignore_case = TRUE))) %>%
    arrange(desc(Price)) %>%  
    head(n) %>%
    select(Book.Title, Book.Author, Publisher, Year.Of.Publication, Publication.Decade, Book.Age, Price, Genre)
  
  if (nrow(recommended_books) == 0) {
    recommended_books <- data.frame(
      Book.Title = "No recommendations found",
      Book.Author = "",
      Publisher = "",
      Year.Of.Publication = NA,
      Publication.Decade = NA,
      Book.Age = NA,
      Price = NA,
      Genre = ""
    )
  }
  
  return(recommended_books)
}

# UI and Server with Google authentication
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "body { background-color: #f0f8ff; font-family: 'Arial', sans-serif; }"
    ))
  ),
  
  uiOutput("loginUI"),
  uiOutput("mainContent")
)

server <- function(input, output, session) {
  # Google authentication setup
  options(googleAuthR.scopes.selected = "email")
  googleAuthR::gar_set_client("C:\\Users\\91701\\Downloads\\client_secret_614389998645-8nr9uvo5drhl49h6ini5bbmrk8u8rvcf.apps.googleusercontent.com.json") # Replace with your JSON credentials file path
  
  user <- reactiveVal(NULL)
  
  # UI for login/logout
  output$loginUI <- renderUI({
    if (is.null(user())) {
      googleAuthR::googleAuthUI("login_button")
    } else {
      div(
        h4(paste("Logged in as:", user()$email)),
        actionButton("logout_button", "Log Out", class = "btn-back")
      )
    }
  })
  
  observeEvent(input$login_button, {
    user(googleAuthR::gar_shiny_getAuth(input, session))
  })
  
  observeEvent(input$logout_button, {
    googleAuthR::gar_shiny_revokeAuth()
    user(NULL)
  })
  
  # Render welcome page after login
  output$mainContent <- renderUI({
    req(user()) # Ensure user is logged in
    
    div(class = "welcome-page",
        h1("Welcome to the Book Recommendation System"),
        div(class = "about-us-section",
            p("Our book recommendation system is designed to help you explore and discover the perfect book for your preferences."),
            p("From timeless classics to modern-day fantasy, our system offers a diverse range of genres to cater to all readers."),
            p("Simply set your budget and choose your favorite genre, and our app will suggest the top books that match your criteria."),
            p("Whether you are a lover of fiction, non-fiction, or self-improvement, our system provides recommendations tailored to your taste."),
            p("Experience a user-friendly interface that allows you to filter books by genre and price effortlessly."),
            p("Stay informed with key details about each book, such as author, publication year, and publisher information."),
            p("Enjoy a seamless experience with visually appealing themes and interactive elements that enhance your journey."),
            p("Our platform is continuously updated to ensure you have access to the latest and most popular titles available."),
            p("Connect with the world of books and explore hidden gems that match your unique interests."),
            p("We believe reading is for everyone, and our mission is to make finding your next book easier and more enjoyable."),
            p("Get started now and let us guide you to the perfect book that suits your mood and preferences.")),
        br(),
        actionButton("goButton", "Let's Go", class = "btn-go"),
        div(class = "shape shape-1"),
        div(class = "shape shape-2"),
        div(class = "shape shape-3")
    )
  })
  
  observeEvent(input$goButton, {
    output$mainContent <- renderUI({
      sidebarLayout(
        sidebarPanel(
          div(class = "sidebar",
              numericInput("maxPrice", "Enter the Price (Max):", value = 20, min = 0),
              textInput("genre", "Enter the Genre:", placeholder = "e.g., Fiction, Fantasy"),
              numericInput("numRecommendations", "Number of Recommendations:", value = 5, min = 1),
              actionButton("getRecommendations", "Get Recommendations", class = "action-button")
          ),
          br(),
          actionButton("backButton", "Back to Welcome Page", class = "btn-back")  # Back button added here
        ),
        mainPanel(
          div(class = "main-panel",
              h3("Recommended Books"),
              tableOutput("recommendationsTable")
          )
        )
      )
    })
  })
  
  observeEvent(input$backButton, {
    output$mainContent <- renderUI({
      div(class = "welcome-page",
          h1("Welcome to the Book Recommendation System"),
          div(class = "about-us-section",
              p("Our book recommendation system is designed to help you explore and discover the perfect book for your preferences."),
              p("From timeless classics to modern-day fantasy, our system offers a diverse range of genres to cater to all readers."),
              p("Simply set your budget and choose your favorite genre, and our app will suggest the top books that match your criteria."),
              p("Whether you are a lover of fiction, non-fiction, or self-improvement, our system provides recommendations tailored to your taste."),
              p("Experience a user-friendly interface that allows you to filter books by genre and price effortlessly."),
              p("Stay informed with key details about each book, such as author, publication year, and publisher information."),
              p("Enjoy a seamless experience with visually appealing themes and interactive elements that enhance your journey."),
              p("Our platform is continuously updated to ensure you have access to the latest and most popular titles available."),
              p("Connect with the world of books and explore hidden gems that match your unique interests."),
              p("We believe reading is for everyone, and our mission is to make finding your next book easier and more enjoyable."),
              p("Get started now and let us guide you to the perfect book that suits your mood and preferences.")),
          br(),
          actionButton("goButton", "Let's Go", class = "btn-go"),
          div(class = "shape shape-1"),
          div(class = "shape shape-2"),
          div(class = "shape shape-3")
      )
    })
  })
  
  recommendations <- eventReactive(input$getRecommendations, {
    req(input$maxPrice, input$genre)
    recommend_books(input$maxPrice, input$genre, input$numRecommendations)
  })
  
  output$recommendationsTable <- renderTable({
    recommendations()
  })dzfaffdsfv
}

shinyApp(ui = ui, server = server)
