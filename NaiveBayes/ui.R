# Define UI for caret package laboratory
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Shiny Caret: Naive Bayes"),
    sidebarLayout(
        sidebarPanel(
            br(),
            sliderInput("trainpercent",  "Fraction of data for training",
                        value = 0.75, step = 0.1, min = 0, max = 1.00),
                     ),
            mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Prediction",              verbatimTextOutput("pred")),
                        tabPanel("ConfusionMatrix",  verbatimTextOutput("confusion")),
                        tabPanel("AUC & ROC",        plotOutput("ROC")) #,
                  #      tabPanel("AUC",              verbatimTextOutput("AUC"))
            )
        )
    )
))
