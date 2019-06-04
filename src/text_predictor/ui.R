#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme=shinytheme("united"),

    # Application title
    titlePanel("Text Prediction using a Katz Backoff Algorithm"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("text", label="Write text", value="i am so", width=NULL),
            submitButton(text="Submit", icon=NULL, width=NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h1(textOutput("top.str")),
            verbatimTextOutput("top"),
            h1(textOutput("all.str")),
            dataTableOutput("predictions")
        )
    )
))
