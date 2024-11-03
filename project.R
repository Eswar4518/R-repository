library(dplyr)
library(ggplot2)
getwd()
setwd("D:\\Dataset")
Pro<-read.csv("D:/Dataset/Spotify Most Streamed Songs.csv")
View(Pro)

summary(Pro)
mean(Pro$apple_playlists)
median(Pro$released_year)
dim(Pro)
glimpse(Pro)

qqnorm(Pro$apple_playlists)
qqline(Pro$released_day)

# Distribution of releaesd_day
# Histogram

ggplot(Pro, aes(x = released_day)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "black") +
  labs(title = "Histogram of songs as per the date", x = "Days of Month", y = "Number of songs")

# Scatter plot 

ggplot(Pro, aes(x =valence , y = instrumentalness,color="green")) +
  geom_point() +
  labs(title = "Scatter Plot of instrumental vs valence", x = "valence ", y = "Instrumental")


# Box plot of sepal length by species

ggplot(Pro, aes(x = , y = instrumentalness)) +
  geom_boxplot(color = "green") +
  labs(title = "Box Plot of Pro", x = "Valence", y = "instrumenralness")

ggplot(Pro, aes(x = , y = dance_ability)) +
  geom_boxplot(color = "blue") +
  labs(title = "Boxplot of Danceability",x="X-Axis",Y="Y-Axis")

ggplot(Pro, aes(x = "", y = liveness)) +
  geom_boxplot() +
  labs(title = "Boxplot of liveliness")

install.packages("shiny")

library(shiny)
library(ggplot2)
library(dplyr)

# Load the dataset
Pro <- read.csv("D:/Dataset/Spotify Most Streamed Songs.csv")

# Define UI
library(shiny)
library(dplyr)

# Load the dataset
Pro <- read.csv("D:/Dataset/Spotify Most Streamed Songs.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Spotify Most Streamed Songs Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Summary Statistics"),
      verbatimTextOutput("summary"),
      h4("Mean of Apple Playlists:"),
      verbatimTextOutput("mean"),
      h4("Median of Released Year:"),
      verbatimTextOutput("median"),
      h4("Dimensions of the Dataset:"),
      verbatimTextOutput("dimensions"),
      h4("Glimpse of the Dataset:"),
      verbatimTextOutput("glimpse")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Histogram", plotOutput("histogram")),
        tabPanel("Scatter Plot", plotOutput("scatterPlot")),
        tabPanel("Box Plots",
                 plotOutput("boxPlot1"),
                 plotOutput("boxPlot2"),
                 plotOutput("boxPlot3"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$summary <- renderPrint({
    summary(Pro)
  })
  
  output$mean <- renderPrint({
    mean_value <- mean(Pro$apple_playlists, na.rm = TRUE)
    paste( mean_value)
  })
  
  output$median <- renderPrint({
    median_value <- median(Pro$released_year, na.rm = TRUE)
    paste( median_value)
  })
  
  output$dimensions <- renderPrint({
    dim(Pro)
  })
  
  output$glimpse <- renderPrint({
    str(Pro)
  })

  output$histogram <- renderPlot({
    hist(Pro$released_day, breaks = seq(0, 31, by = 1),binsize=0.3, 
         col = "purple", border = "black",
         main = "Histogram of Songs by Release Day",
         xlab = "Days of Month", ylab = "Number of Songs")
  })
  
  output$scatterPlot <- renderPlot({
    plot(Pro$valence, Pro$instrumentalness, 
         col = "yellow", pch = 19,
         main = "Scatter Plot of Instrumental vs Valence",
         xlab = "Valence", ylab = "Instrumentalness")
  })
  
  output$boxPlot1 <- renderPlot({
    boxplot(instrumentalness ~ valence, data = Pro,
            col = "green",
            main = "Box Plot of Instrumentalness by Valence",
            xlab = "Valence", ylab = "Instrumentalness")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



