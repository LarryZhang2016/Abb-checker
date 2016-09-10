## abb-checker-app.R ##
library(stringr)
library(tidyr)
library(dplyr)
library(shiny)

server <- function(input, output) {
  # You can access the value of the widget with input$text, e.g.
   
    output$value <- renderText(
      {my_text <- toupper(input$text)

       origin_df <- read.table(file="./abbreviations.txt",  row.names=NULL, sep="\n") 
      
       n <- dim(origin_df)[1]
       first_part <- rep("", n)
       second_part <- rep("", n)
      
       for(i in 1:n)
       {first_part[i] <- unlist(strsplit(as.character(origin_df[i,]), split = ":"))[1]
        second_part[i] <- unlist(strsplit(as.character(origin_df[i,]), split = ":"))[2]
       }
      
       new_df <- 
        data.frame(abb = toupper(first_part), desc = str_trim(second_part, side = "left")) %>%
        distinct(abb)
       
       the_re <- ifelse(my_text %in% new_df$abb, 
                        as.character(new_df[which(new_df$abb==my_text), 2]),
                        "Not found")
       
       return(the_re)
      }
      )
  
}

ui <- fluidPage(
  # Copy the line below to make a text input box
  textInput("text", 
            label = h3("MBIE Abbreviation checker: Please enter an abbreviation (e.g. MBIE) below"), 
            value = ""),
  #actionButton(inputId = "go", label = "Check"),
  # hr(),
  fluidRow(column(10, verbatimTextOutput("value")))
)

shinyApp(ui = ui, server = server)


