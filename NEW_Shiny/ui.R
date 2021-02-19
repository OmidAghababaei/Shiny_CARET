# Define UI for caret package laboratory
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Shiny for Caret"),
    
    # Sidebar with controls.  Note the use of the br()
    # element to introduce extra vertical spacing
    sidebarLayout(
        sidebarPanel(
            h4("Preprocessing"),
            checkboxInput("centerscale", "Center and Scale Variables", TRUE),
            br(),
            sliderInput("trainpercent",
                        "Fraction of data for training",
                        value = 0.75, step = 0.05, min = 0.50, max = 1.00),
            br(),
            numericInput("randomseed", "Random Seed", 12345, min=100, max=1000000),
            HTML("<hr>"),
            radioButtons("method", h4("Caret Model"),
                         c("Linear Discriminant Analysis"          = "lda",
                           "Classification and Regression Trees" = "rpart",
                           "Bagged CART (treebag)"                      = "treebag",
                           "Stochastic Gradient Boosting"         = "gbm",
                           "Random Forest"                        = "rf",
                           "Support Vector Machines with Polynomial Kernel"  = "svmPoly"
                         ))
        ),
        
        # Tab panels:
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Fit",              verbatimTextOutput("fit")),
                        tabPanel("ConfusionMatrix",  verbatimTextOutput("confusion")),
                        tabPanel("Plot",          plotOutput("dotplot"))
            )
        )
    )
))

###############################
library(AppliedPredictiveModeling)
library(caret)
library(shiny)
library(e1071)
library(pROC)

###########################################################################
### caret processing helper functions

# Get segmentationOriginal data and preprocess a bit by removing
# near-zero variance and/or high correlating variables.  This reduction
# also helps speed up processing by the selected machine learning method.
setupData <- function(trainpercent, randomseed)
{
    # Make sure computations can be reproducible.
    set.seed(randomseed)
    
    # Cell segmenation data
    data(segmentationOriginal)
    
    # Use Case to divide into raw training and test and final Test sets, as
    # defined in original data.
    # Drop "Cell" ID and Case from original data.frame.
    rawTrain  <- segmentationOriginal[segmentationOriginal$Case == "Train",][,-1:-2]
    finalTest <- segmentationOriginal[segmentationOriginal$Case == "Test",][,-1:-2]
    
    # Remove near-zero variance variables from rawTrain.
    # Use freqCut=2 to get rid of mostly skewed variables with few unqiue values.
    nzv <- nearZeroVar(rawTrain, freqCut=2, saveMetrics=TRUE)
    count.nzv <- sum(nzv$nzv)
    count.nzv
    if (count.nzv > 0)
    {
        rawTrain  <- rawTrain[, !nzv$nzv]
        finalTest <- finalTest[, !nzv$nzv]
    }
    
    # Remove variables with high correlation
    HIGH.CORRELATION.CUTOFF <- 0.90
    cor.matrix <- cor(rawTrain[,-1])
    cor.high   <- findCorrelation(cor.matrix, HIGH.CORRELATION.CUTOFF)
    
    high.corr.remove <- row.names(cor.matrix)[cor.high]
    high.corr.remove
    
    rawTrain <- rawTrain[,  -cor.high]
    finalTest <- finalTest[, -cor.high]
    
    # Partition raw training data into a training and validation set.
    inTrainSet <- createDataPartition(y=rawTrain$Class, p=trainpercent, list=FALSE)
    training <- rawTrain[inTrainSet,]
    
    validation <- rawTrain[-inTrainSet,]
    invisible( list(training=training, validation=validation, finalTest=finalTest) )
}

# Train learning model.  Apply to out-of-sample test data to compute
# confusion matrix and related data.
generic.fit <- function(method, trainingData, validationData, centerscale)
{
    preprocess.methods <- NULL
    if (centerscale) preprocess.methods = c("center", "scale")
    
    fit <- train(Class ~ ., data = trainingData,
                 preProcess=preprocess.methods, method=method)
    OutOfSample  <- predict(fit, newdata=validationData)
    confusion <- confusionMatrix(validationData$Class, OutOfSample)
    
    invisible( list(fit=fit, confusion=confusion) )
}

# Setup data and use specified caret learning method.
responseRoutine <- function(method, centerscale, trainpercent, randomseed)
{
    d <- setupData(trainpercent, randomseed)
    fit <- generic.fit(method, d$training, d$validation, centerscale)
    invisible(fit)
}

###########################################################################
### Server logic
shinyServer(function(input, output) {
    
    # Reactive expression called whenever inputs change.
    data <- reactive({
        responseRoutine(input$method, input$centerscale, input$trainpercent, input$randomseed)
    })
    
    # Fill-in the tabs with output from caret
    
    output$fit <- renderPrint({
        data()$fit
    })
    
    output$confusion <- renderPrint({
        data()$confusion
    })
    
    output$dotplot <- renderPlot({
        dotPlot(varImp(data()$fit), main="Dotplot of variable importance values")
    })
    
    
})