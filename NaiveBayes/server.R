################################################## Load libraries & data
library(caret)
library(kernlab) 
library(ROCR) 
library(e1071)
data(spam)
options(digits = 4)
###########################################################################
setupData <- function(trainpercent){

    inTrain <- createDataPartition(y=spam$type, p=trainpercent, list=FALSE)
    training <- spam[inTrain,]
    validation <- spam[-inTrain,]
    
   # modelFit <- train(type ~.,data=training, method=method)
   # predictions <- predict(modelFit,newdata=validation)
   # confusion<- confusionMatrix(predictions,validation$type, positive = "spam")
    
    naive_spam <- naiveBayes(type ~., data=training)  
    
    pred_class <- predict(object = naive_spam,  newdata = validation,  type = "class")        
    
    confusion<- confusionMatrix(data=pred_class, reference = validation$type, positive = 'spam')
    
    pred_prob<- predict(naive_spam, validation, type= "raw")
    data_roc <- data.frame(pred_prob = pred_prob[,'spam'],
                           actual_label = ifelse(validation$type == 'spam', 1, 0))
    
    invisible( list(pred=head(pred_class,10), confusion=confusion, data_roc=data_roc) )
}

##########################################################

### Server logic
shinyServer(function(input, output){
    
    
    # Reactive expression called whenever inputs change.
    data <- reactive({
        setupData( input$trainpercent)
                   })
    
    # Fill-in the tabs with output from caret
    
    output$pred <- renderPrint({
        data()$pred
                        })
    
    output$confusion <- renderPrint({
        data()$confusion
                             })
    
    output$ROC <- renderPlot({
      sms_auc <- performance(roc_curve, measure = "auc")
      roc_curve <- prediction(predictions = data_roc$pred_prob, labels = data_roc$actual_label)
       plot(performance(roc_curve, "tpr", "fpr"),col="red")
        abline(0, 1, lty = 2)
        title(paste("AUC =",sms_auc@y.values))
    })
    
    # output$AUC <- renderPrint({
    #    roc_curve <- prediction(predictions = data_roc$pred_prob, labels = data_roc$actual_label)
    #     sms_auc <- performance(roc_curve, measure = "auc")
    #     sms_auc@y.values
    #          })
    
})


