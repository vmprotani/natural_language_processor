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
shinyUI(fluidPage(theme=shinytheme("slate"),

    # Application title
    titlePanel("NLP Text Predictor"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput("text", label="Write text", value="i am so", width=NULL),
            submitButton(text="Submit", icon=NULL, width=NULL)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Predictions",
                         h2(textOutput("top.str")),
                         verbatimTextOutput("top"),
                         h2(textOutput("all.str")),
                         dataTableOutput("predictions")
                ),
                tabPanel("Documentation",
                         h2("Summary"),
                         p("This application uses a Katz Backoff model to predict user text from previous input. It utilizes datasets of ngrams of sizes 1 through 4 words."),
                         p("The output of the backoff model consists of predicted words, their respective probabilities, and the number of words in the ngrams where the prediction is found. For example, if the user types"),
                         verbatimTextOutput("sample.input"),
                         p("the top prediction will show, and the entries in the top 25 predictions will look like"),
                         verbatimTextOutput("sample.output"),
                         p("which means the word \"day\" has a roughly 28% probability of being the next word, and it was found in the 4grams data. Note that since this is a \"stupid\" Katz Backoff implementation, these probabilities will not add up perfectly to 1."),
                         h2("How to Use"),
                         p("The left panel has a text box where you type text. Once you finish your text, click the submit button. On the main panel, the top prediction will appear separately from the top 25 predictions."))
            )
        )
    )
))
