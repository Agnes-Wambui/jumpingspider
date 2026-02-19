library(shiny)
library(e1071)
library(caTools)
library(ggplot2)
library(dplyr)
library(reshape2)
library(caret)
library(pROC)

svmModel <- readRDS("final_svm_model.rds")

ui <-  fluidPage(
  titlePanel("Support Vector Machine Model Performance"),
  
  sliderInput("size", "Sample Size:",
                min = 100, max = 1000, value = 100
    ),
  
  plotOutput("confusionMatrix")
  
)
 
server <- function(input, output) {
  conf_matrix_DF<-reactive({
    req(input$size)
    df <- read.csv("cholera_dt.csv")
    sim_df <- df[sample(nrow(df), input$size , TRUE),]
    sim_df$predictions<-predict(svmModel, newdata = sim_df)
    conf_matrix <- table(sim_df$cholera.outbreak, sim_df$predictions)
    conf_matrix_melted <- as.data.frame(conf_matrix)
    colnames(conf_matrix_melted) <- c("Actual", "Predicted", "Count")
    return(conf_matrix_melted)
    
  })
  
  output$confusionMatrix <- renderPlot({
    ggplot(conf_matrix_DF(), aes(x = Actual, y = Predicted, fill = Count)) +
      geom_tile() +
      geom_text(aes(label = Count), color = "black", size = 6) +  # Add text labels
      scale_fill_gradient(low = "white", high = "blue") +
      labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
      theme_minimal()
    
  })
  
}
 
shinyApp(ui = ui, server = server)