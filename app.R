# Load necessary libraries
library(shiny)    # For creating the web app
library(class)    # For kNN implementation
library(tm)       # For text preprocessing
library(shinythemes)

# Load and preprocess the data
data <- read.csv('email_classification.csv')

# Preprocess the text data
corpus <- Corpus(VectorSource(data$email))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Create Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99)  # Keep only the most frequent terms

# Convert the Document-Term Matrix to a data frame
dtm_data <- as.data.frame(as.matrix(dtm))
dtm_data$label <- data$label

# Split data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(dtm_data), 0.8 * nrow(dtm_data))
train_data <- dtm_data[train_index, ]
test_data <- dtm_data[-train_index, ]

# Define the kNN model (We use k=3 for this example)
k <- 3

# Define UI for the Shiny app
library(shiny)
library(shinythemes)

library(shiny)
library(shinythemes)

library(shiny)
library(shinythemes)

ui <- navbarPage(
  theme = shinytheme("cosmo"),
  title = "Email Spam Classifier",
  
  # Home Tab
  tabPanel(
    "Home",
    fluidPage(
      tags$head(
        tags$style(HTML("
          .sidebar { background-color: #f2f2f2; } 
          .main { background-color: #e6e6e6; }
          .content { height: 100%; }
          .btn-primary { background-color: #007bff; border-color: #007bff; }
        "))
      ),
      titlePanel("Email Spam Classifier"),
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          textInput("email", "Enter an email text:", "", 
                    placeholder = "Type your email content here..."),
          actionButton("predict", "Predict", class = "btn-primary"),
          br(),
          br(),
          p("Submit your email text and click 'Predict' to classify it as spam or not spam.")
        ),
        mainPanel(
          class = "main",
          div(
            class = "content",
            h3("Prediction Result"),
            h4("This is a machine learning model prediction. Human crosscheck for strange predictions is advisable."),
            verbatimTextOutput("result"),
            style = "padding: 15px;"
          )
        )
      )
    )
  ),
  
  # About Tab
  tabPanel(
    "About",
    fluidPage(
      tags$head(
        tags$style(HTML("
          .main { background-color: #e6e6e6; }
        "))
      ),
      titlePanel("About This App"),
      mainPanel(
        class = "main",
        div(
          class = "content",
          h4("Overview"),
          p("This Shiny app uses a k-Nearest Neighbors (kNN) algorithm to classify emails as spam or not spam."),
          p("The app demonstrates text preprocessing techniques and the application of the kNN algorithm for classification."),
          p("Created using R and Shiny."),
          br(),
          h4("Key Features"),
          tags$ul(
            tags$li("Simple and interactive interface"),
            tags$li("Real-time spam classification"),
            tags$li("Based on kNN algorithm")
          ),
          style = "padding: 15px;"
        )
      )
    )
  ),
  
  # Contact Tab
  tabPanel(
    "Contact",
    fluidPage(
      tags$head(
        tags$style(HTML("
          .main { background-color: #e6e6e6; }
        "))
      ),
      titlePanel("Developer Information"),
      mainPanel(
        class = "main",
        div(
          class = "content",
          h4("Get in Touch and let's build"),
          p("www.krishnaprakash.in"),
          tags$ul(
            tags$li("Email: krishnaprakash997@gmail.com"),
            tags$li("https://github.com/KPKR1997")
          ),
          br(),
          p("Thanks for your interest!"),
          style = "padding: 15px;"
        )
      )
    )
  )
)





# Define server logic for the Shiny app
server <- function(input, output) {
  observeEvent(input$predict, {
    email_input <- input$email
    email_corpus <- Corpus(VectorSource(email_input))
    email_corpus <- tm_map(email_corpus, content_transformer(tolower))
    email_corpus <- tm_map(email_corpus, removePunctuation)
    email_corpus <- tm_map(email_corpus, removeNumbers)
    email_corpus <- tm_map(email_corpus, removeWords, stopwords("english"))
    email_corpus <- tm_map(email_corpus, stripWhitespace)
    email_dtm <- DocumentTermMatrix(email_corpus, list(dictionary = Terms(dtm)))
    email_df <- as.data.frame(as.matrix(email_dtm))
    
    # Ensure email_df has the same columns as the training data
    missing_cols <- setdiff(colnames(train_data), colnames(email_df))
    email_df[missing_cols] <- 0
    email_df <- email_df[, colnames(train_data)[-ncol(train_data)]]
    
    
    # Use kNN to predict the label
    prediction <- knn(train = train_data[, -ncol(train_data)], test = email_df, cl = train_data$label, k = k)
    output$result <- renderText({
      paste("The email seems to be", ifelse(prediction == "spam", "Spam. Better not click any links associated with this email.", "Not Spam. No need to worry much!"))
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
