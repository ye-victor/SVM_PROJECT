library(shiny)
library(pROC)
library(class)
library(randomForest)
library(caret)
library(ineq)
library(shinyjs)
library(lift)
library(DMwR)
library(e1071)


appCSS <- "
#loading-content {
  position: absolute;
  background: #FFFFFF;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #000000;
}
"




ui <- fluidPage(
  
  
  useShinyjs(),
  inlineCSS(appCSS),
  
  # Loading message
  div(
    id = "loading-content",
    h2("Initialization of the application, please wait...")
  ),
  
  hidden(
    div(
      id = "app-content",
  
  
    tags$style("body {padding-top: 70px;}"),
    navbarPage("SVM Project",position="fixed-top",
               inverse=TRUE,
               tabPanel(p(icon("database"),"Introduction"),tabsetPanel(tabPanel("English", includeHTML("Introductionen.html")),
                                                 tabPanel("Français",includeHTML("Introduction.html")))),
                 tabPanel(p(icon("wrench"),"Use of the system"),tabsetPanel(tabPanel("English", includeHTML("utilisationen.html")),
                                                                      tabPanel("Français",includeHTML("utilisation.html")))),
               tabPanel(p(icon("check"),"Measuring tools"), tabsetPanel(tabPanel("English", includeHTML("explainen.html")),
                                                                        tabPanel("Français",includeHTML("explain.html"))) ),
               tabPanel(p(icon("calculator"),"Case study"),
                          sidebarLayout(
                    sidebarPanel(selectInput(inputId='kernel', label='Choose a kernel to apply to the SVM',
                                             choices=c('Linear','Radial','Polynomial','Sigmoid'), multiple = FALSE, selected='Radial'),
                                 conditionalPanel("input.kernel == 'Radial'",
                                                  sliderInput("costrad", "The C constant of the regularization term in the Lagrange formulation", min = 1, max = 10, value = 6, step=1),
                                                  sliderInput("gammrad", "Hyperparameter Gamma", min = 0.1, max = 1.5, value =0.1, step=0.1)
                                                  ),
                                 conditionalPanel("input.kernel == 'Polynomial'",
                                                  sliderInput("costpoly","The C constant of the regularization term in the Lagrange formulation", min = 1, max = 10, value = 5, step=1),
                                                  sliderInput("cpoly","Hyperparameter c", min = 0, max = 1, value=0, step=0.1),
                                                  sliderInput("ppoly","Hyperparameter p (degree)", min = 1, max = 10, value=3, step=1)
                                                  ),
                                 conditionalPanel("input.kernel == 'Sigmoid'",
                                                  sliderInput("teta1sigmoid", "Hyperparameter Têta1 (Gamma)", min = 0.1, max = 1.5, value=0.1, step=0.1),
                                                  sliderInput("teta2sigmoid", "Hyperparameter Têta2 (Coef0)", min = 0, max = 1, value=0, step=0.1)
                                                  ),
                                 em("Note : the default values (kernel='Radial', C=6, Gamma=0.1) are the optimal values found from the tune.svm function.")
                                 
                                 
                                 
                                 
                                 )
                                 
                                 
                                 
                                 ,
                    mainPanel(plotOutput("ROCSVM"),plotOutput("CONFSVM"),includeHTML("logo.html")))),
               
                 tabPanel(p(icon("balance-scale"),"SVM vs. other models"),
                          h2("Comparison of the different models"),
                          splitLayout(plotOutput("ROCCOMP"),plotOutput("GAINCOMP")),
                          sidebarLayout(
                            sidebarPanel(radioButtons("compare", label="Model(s) to be compared", choices=c("Logistic regression","KNN","Random forest","All"), selected="All")),
                            mainPanel(tableOutput("mesure"))
                          ),
                          br(),
                          h2("Conclusion"),
                          tabsetPanel(tabPanel("English", includeHTML("conclusionen.html")),
                                      tabPanel("Français",includeHTML("conclusion.html")))

                                      
                          ),
               tabPanel(p(icon("info"),"About"),
                        downloadButton("downloadData", "Download PDF associated to this App"),
                        tabsetPanel(tabPanel("English", includeHTML("abouten.html")),
                                                               tabPanel("Français",includeHTML("about.html"))))
               
               ) ) )  )






server <- function(input, output, session) {
  
  #creditcard <- read.csv("/Users/Victor/Desktop/creditcard.csv")
  creditcard <- readRDS(file=url("https://raw.githubusercontent.com/VictorYeGitHub/SVM_PROJECT/master/creditcard.rds"))
  creditcard$Class <- factor(creditcard$Class, levels=c("0","1"))
  
  set.seed(2019)
  creditcard <- creditcard[sample(nrow(creditcard)),]
  samp = sample(1:nrow(creditcard),nrow(creditcard)*0.7)
  train = creditcard[samp,]
  testSplit = creditcard[-samp,] 
  
  fraudData= train[train$Class==1,]
  NoFraudData=train[train$Class==0,]
  NoFraudData=NoFraudData[1:800,]
  trainSplit=rbind(NoFraudData,fraudData)
  
  
  svm.model <- svm(Class~., data=trainSplit, kernel="radial", cost=10, gamma=0.1, probability=TRUE)
  svm.predict <- predict(svm.model,newdata=testSplit,type="class",probability=TRUE)

  
  log.model <- glm(Class~.,trainSplit,family="binomial")
  log.predict <- predict(log.model,testSplit,type="response")
  fitted.results <- ifelse(log.predict > 0.5,1,0)
  
  knn.model = knn(trainSplit[,-31],testSplit[,-31],trainSplit$Class,k=13)
  
  
  rf.model <- randomForest(Class~.,trainSplit , ntree=180, importance = TRUE)
  rf.pred <- predict(rf.model, testSplit)

  
  
  cost_model = function(predicted.classes, true.classes, amounts, fixedcost) {
    cost = sum(true.classes*(1 - predicted.classes)*amounts +
                 predicted.classes*fixedcost)
    return(cost)
  }
  
  numsvm=as.numeric(svm.predict)
  numsvm[numsvm == 1] <- 0
  numsvm[numsvm == 2] <- 1
  
  numfit=as.numeric(fitted.results)
  
  numknn=as.numeric(knn.model)
  numknn[numknn == 1] <- 0
  numknn[numknn == 2] <- 1
  
  numrf=as.numeric(rf.pred)
  numrf[numrf == 1] <- 0
  numrf[numrf == 2] <- 1
  
  numtest=as.numeric(testSplit$Class)
  numtest[numtest==1] <- 0
  numtest[numtest==2] <- 1
  
  costsvm=cost_model(numsvm,numtest, testSplit$Amount, fixedcost=10)
  costfit=cost_model(numfit,numtest, testSplit$Amount, fixedcost=10)
  costknn=cost_model(numknn,numtest, testSplit$Amount, fixedcost=10)
  costrf=cost_model(numrf,numtest, testSplit$Amount, fixedcost=10)
  
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
  
x <- reactive({input$kernel})

output$ROCSVM <- renderPlot ({ 
  if (x()=="Radial"){
    withProgress(message = 'Calcul with new values in progress...', value = 0,style = getShinyOption("progress.style",
                                                                                                     default = "old"), {
                                                                                                       for (i in 1:100) {
                                                                                                         incProgress(1/100)
                                                                                                         Sys.sleep(0.15)
                                                                                                       }
                                                                                                     })
  par(pty= "s")
  svm.model <- svm(Class ~ ., data=trainSplit, kernel="radial", cost=input$costrad, gamma=input$gammrad)
  svm.predict <- predict(svm.model,testSplit)
roc(testSplit$Class,as.numeric(svm.predict),plot=T,main="SVM with radial kernel",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=30, print.auc.x=25)
 } 
  if (x()=="Linear"){
    withProgress(message = 'Calcul with new values in progress...',value = 0,style = getShinyOption("progress.style",
                                                                                                    default = "old"), {
                                                                                                      for (i in 1:100) {
                                                                                                        incProgress(1/100)
                                                                                                        Sys.sleep(0.08)
                                                                                                      }
                                                                                                    })
    par(pty= "s")
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="linear", cost=5)
    svm.predict <- predict(svm.model,testSplit)
    roc(testSplit$Class,as.numeric(svm.predict),plot=T,main="SVM with linear kernel",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=30, print.auc.x=25)
  }
  if (x()=="Polynomial"){
    withProgress(message = 'Calcul with new values in progress...',value = 0,style = getShinyOption("progress.style",
                                                                                                    default = "old"), {
                                                                                                      for (i in 1:100) {
                                                                                                        incProgress(1/100)
                                                                                                        Sys.sleep(0.08)
                                                                                                      }
                                                                                                    })
    par(pty= "s")
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="polynomial", degree=input$ppoly,coef0=input$cpoly, cost=input$costpoly)
    svm.predict <- predict(svm.model,testSplit)
    roc(testSplit$Class,as.numeric(svm.predict),plot=T,main="SVM with polynomial kernel",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=30, print.auc.x=25)
  }
  if (x()=="Sigmoid"){
    withProgress(message = 'Calcul with new values in progress...',value = 0,style = getShinyOption("progress.style",
                                                                                                    default = "old"), {
                                                                                                      for (i in 1:100) {
                                                                                                        incProgress(1/100)
                                                                                                        Sys.sleep(0.08)
                                                                                                      }
                                                                                                    })
    par(pty= "s")
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="sigmoid", gamma=input$teta1sigmoid, coef0=input$teta2sigmoid)
    svm.predict <- predict(svm.model,testSplit)
    roc(testSplit$Class,as.numeric(svm.predict),plot=T,main="SVM with sigmoid kernel",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=30, print.auc.x=25)
  }
})  


output$CONFSVM <- renderPlot ({ 
  
  if (x()=="Radial"){
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="radial", cost=input$costrad, gamma=input$gammrad)
    svm.predict <- predict(svm.model,testSplit)
  }
  
  if (x()=="Linear"){
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="linear", cost=5)
    svm.predict <- predict(svm.model,testSplit)
  }
  
  if (x()=="Polynomial"){
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="polynomial", degree=input$ppoly,coef0=input$cpoly, cost=input$costpoly)
    svm.predict <- predict(svm.model,testSplit)
  }
  
  if (x()=="Sigmoid"){
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="sigmoid", gamma=input$teta1sigmoid, coef0=input$teta2sigmoid)
    svm.predict <- predict(svm.model,testSplit)
  }
  
  numsvm=as.numeric(svm.predict)
  numsvm[numsvm == 1] <- 0
  numsvm[numsvm == 2] <- 1
  
  costsvm=cost_model(numsvm,numtest, testSplit$Amount, fixedcost=10)
  
  svm.auc <- auc(roc(testSplit$Class,as.numeric(svm.predict)))
  svm.gini <- 2*svm.auc-1
  
  cm1 <- confusionMatrix(svm.predict,testSplit$Class)
  
  par(mar=c(2,2,2,2))
  draw_confusion_matrix <- function(cmtrx,auc,gini,loss) {
    total <- sum(cmtrx$table)
    
    layout(matrix(c(1,1,2)))
    par(mar=c(2,2,2,2))
    plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
    title('CONFUSION MATRIX')
    
    classes = colnames(cmtrx$table)
    rect(150, 430, 240, 370, col="green")
    text(195, 435, classes[1], cex=1.2)
    rect(250, 430, 340, 370, col="red")
    text(295, 435, classes[2], cex=1.2)
    text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
    text(245, 450, 'Actual', cex=1.3, font=2)
    rect(150, 305, 240, 365, col="red")
    rect(250, 305, 340, 365, col="green")
    text(140, 400, classes[1], cex=1.2, srt=90)
    text(140, 335, classes[2], cex=1.2, srt=90)
    
    text(195, 400,cm1$table[1,1], cex=1.6, font=2, col='white')
    text(195, 335,cm1$table[1,2], cex=1.6, font=2, col='white')
    text(295, 400,cm1$table[2,1], cex=1.6, font=2, col='white')
    text(295, 335,cm1$table[2,2], cex=1.6, font=2, col='white')
    
    plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
    
    text(10, 85, names(cmtrx$byClass[1]), cex=1.2, font=2)
    text(10, 70, round(as.numeric(cmtrx$byClass[1]), 3), cex=1.2)
    text(36, 85, names(cmtrx$byClass[2]), cex=1.2, font=2)
    text(36, 70, round(as.numeric(cmtrx$byClass[2]), 3), cex=1.2)
    text(62, 85, names(cmtrx$byClass[5]), cex=1.2, font=2)
    text(62, 70, round(as.numeric(cmtrx$byClass[5]), 3), cex=1.2)
    text(90, 85, names(cmtrx$byClass[7]), cex=1.2, font=2)
    text(90, 70, round(as.numeric(cmtrx$byClass[7]), 3), cex=1.2)
    
    text(10, 35, names(cmtrx$overall[1]), cex=1.2, font=2)
    text(10, 20, round(as.numeric(cmtrx$overall[1]), 3), cex=1.2)
    text(36, 36, "AUC", cex=1.2, font=2)
    text(36, 20, round(as.numeric(auc), 3), cex=1.2)
    text(62, 36, "Gini", cex=1.2, font=2)
    text(62, 20, round(as.numeric(gini), 3), cex=1.2)
    text(90, 36, "Loss", cex=1.2, font=2)
    text(90, 20, round(as.numeric(loss), 3), cex=1.2)
  }
  draw_confusion_matrix(cm1,svm.auc,svm.gini,costsvm)
  
})


y <- reactive({input$compare})

  output$ROCCOMP <- renderPlot ({
    withProgress(message = 'Loading...',
                 detail = 'Computing data and creating plots', value = 0, {
                   for (i in seq(from=1, to=100, by=4)) {
                     incProgress(4/100, detail=paste(i,"%"))
                     Sys.sleep(0.08)
                   }
                 })
    par(pty= "s")
    roc(testSplit$Class,as.numeric(svm.predict),main="ROC curve",plot=T,legacy.axes=TRUE,percent=TRUE,xlab="1 - Specificity",ylab="Sensitivity",col="#377eb8",lwd=4)
    legend("bottomright",legend=c("SVM","Logistic regression","KNN","Random forest"),col=c("#377eb8","#4daf4a","#850606","#EE82EE"),lwd=4)
    
    if (y()=="Logistic regression"){
    plot.roc(testSplit$Class,as.numeric(fitted.results),percent=T,col="#4daf4a",lwd=2,add=TRUE)
    }
    
    if (y()=="KNN"){
    plot.roc(testSplit$Class,as.numeric(knn.model),percent=T,col="#850606",lwd=2,add=TRUE)
    }
    if (y()=="Random forest"){
    plot.roc(testSplit$Class,as.numeric(rf.pred),percent=T,col="#EE82EE",lwd=2,add=TRUE)
    }
    
    if (y()=="All"){
      plot.roc(testSplit$Class,as.numeric(fitted.results),percent=T,col="#4daf4a",lwd=2,add=TRUE)
      plot.roc(testSplit$Class,as.numeric(knn.model),percent=T,col="#850606",lwd=2,add=TRUE)
      plot.roc(testSplit$Class,as.numeric(rf.pred),percent=T,col="#EE82EE",lwd=2,add=TRUE)
    }

  })
  

  
output$GAINCOMP <- renderPlot ({

  if (y()=="Logistic regression"){
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(svm.predict,as.numeric(testSplit$Class),cumulative=T,col="#377eb8", main="SVM Gain Chart vs. other models",ylim=c(1,1.020),xlim=c(1,10),lwd=4)
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(fitted.results,as.numeric(testSplit$Class),cumulative=T,col="#4daf4a", axes=FALSE ,ylim=c(1,1.020),xlim=c(1,10),lwd=2)
    
  }
  if (y()=="KNN"){
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(svm.predict,as.numeric(testSplit$Class),cumulative=T,col="#377eb8", main="SVM Gain Chart vs. other models",ylim=c(1,1.020),xlim=c(1,10),lwd=4)
    
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(knn.model,as.numeric(testSplit$Class),cumulative=T,col="#850606", axes=FALSE ,ylim=c(1,1.020),xlim=c(1,10),lwd=2)
    
  }
  if (y()=="Random forest"){
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(svm.predict,as.numeric(testSplit$Class),cumulative=T,col="#377eb8", main="SVM Gain Chart vs. other models",ylim=c(1,1.020),xlim=c(1,10),lwd=4)
    
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(rf.pred,as.numeric(testSplit$Class),cumulative=T, col="#EE82EE", axes=FALSE ,ylim=c(1,1.020),xlim=c(1,10),lwd=2)
    
  }
  if (y()=="All"){
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(svm.predict,as.numeric(testSplit$Class),cumulative=T,col="#377eb8", main="SVM Gain Chart vs. other models",ylim=c(1,1.020),xlim=c(1,10),lwd=4)
    
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(fitted.results,as.numeric(testSplit$Class),cumulative=T,col="#4daf4a", axes=FALSE ,ylim=c(1,1.020),xlim=c(1,10),lwd=2)
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(knn.model,as.numeric(testSplit$Class),cumulative=T,col="#850606", axes=FALSE ,ylim=c(1,1.020),xlim=c(1,10),lwd=2)
    par(fig=c(0,1,0,1), new=TRUE)
    plotLift(rf.pred,as.numeric(testSplit$Class),cumulative=T, col="#EE82EE", axes=FALSE ,ylim=c(1,1.020),xlim=c(1,10),lwd=2)
    
  }
})  
  

output$mesure <- renderTable({
  cm1 <- confusionMatrix(svm.predict,testSplit$Class)
  cm2 <- confusionMatrix(as.factor(fitted.results),testSplit$Class)
  cm3 <- confusionMatrix(knn.model,testSplit$Class)
  cm4 <- confusionMatrix(rf.pred,testSplit$Class)
  
  svm.auc <- auc(roc(testSplit$Class,as.numeric(svm.predict)))
  glm.auc <- auc(roc(testSplit$Class,as.numeric(fitted.results)))
  knn.auc <- auc(roc(testSplit$Class,as.numeric(knn.model)))
  rf.auc <- auc(roc(testSplit$Class,as.numeric(rf.pred)))
  
  svm.gini <- 2*svm.auc-1
  glm.gini <- 2*glm.auc-1
  knn.gini <- 2*knn.auc-1
  rf.gini <- 2*rf.auc-1


  if (y()=="Logistic regression"){
  q=cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(svm.gini,digits=4),Loss=costsvm)
  s=cbind("Logistic regression",round(t(cm2$byClass[c(1,2,5,11)]),digits=4),round(glm.gini,digits=4),costfit)
  rbind(q,s)
  }

  else if (y()=="KNN"){
  q=cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(svm.gini,digits=4),Loss=costsvm)
  s=cbind("KNN",round(t(cm3$byClass[c(1,2,5,11)]),digits=4),round(knn.gini,digits=4),costknn)
  rbind(q,s)
  }
  
  else if (y()=="Random forest"){
  q=cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(svm.gini,digits=4),Loss=costsvm)
  s=cbind("Random forest",round(t(cm4$byClass[c(1,2,5,11)]),digits=4),round(rf.gini,digits=4),costrf)
  rbind(q,s)
  }
  
  else if (y()=="All"){

    q=cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(svm.gini,digits=4),Loss=costsvm)
    k=cbind("Logistic regression",round(t(cm2$byClass[c(1,2,5,11)]),digits=4),round(glm.gini,digits=4),costfit)
    l=cbind("KNN",round(t(cm3$byClass[c(1,2,5,11)]),digits=4),round(knn.gini,digits=4),costknn)
    m=cbind("Random forest",round(t(cm4$byClass[c(1,2,5,11)]),digits=4),round(rf.gini,digits=4),costrf)
    rbind(q,k,l,m)
  }
})

output$downloadData <- downloadHandler(

  filename <- function() {
    paste("test", "pdf", sep=".")
  },
  
  content <- function(file) {
    file.copy("test.pdf", file)
  },
  contentType = "application/pdf"
)

}

# Run the application 
shinyApp(ui = ui, server = server)
