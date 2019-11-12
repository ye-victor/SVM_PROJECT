library(shiny)
library(DMwR)
library(e1071)
library(pROC)
library(class)
library(randomForest)
library(caret)
library(ineq)
library(shinyjs)



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
                 tabPanel(p(icon("calculator"),"Case study"),
                          sidebarLayout(
                    sidebarPanel(selectInput(inputId='kernel', label='Choose a kernel to apply to the SVM',
                                             choices=c('Linear','Radial','Polynomial','Sigmoid'), multiple = FALSE, selected='Radial'),
                                 conditionalPanel("input.kernel == 'Radial'",
                                                  sliderInput("costrad", "The C constant of the regularization term in the Lagrange formulation", min = 1, max = 10, value = 10, step=1),
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
                                 em("Note : the default values (kernel='Radial', C=10, Gamma=0.1) are the optimal values found from the tune.svm function.")
                                 
                                 
                                 
                                 
                                 )
                                 
                                 
                                 
                                 ,
                    mainPanel(plotOutput("ROCSVM"),tableOutput("ROCmesure"),includeHTML("logo.html")))),
                 tabPanel(p(icon("check"),"SVM vs. other models"),
                          tabsetPanel(tabPanel("English", includeHTML("explainen.html")),
                                      tabPanel("Français",includeHTML("explain.html"))),
                          h2("Comparison of the different models"),
                          splitLayout(plotOutput("ROCCOMP"),
                                         plotOutput("LORCOMP")),
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
  
  fraudData= train[train$Class=="1",]
  NoFraudData=train[train$Class=="0",]
  NoFraudData=NoFraudData[1:1180,]
  trainSplit=rbind(NoFraudData,fraudData)
  
  svm.model <- svm(Class ~ ., data=trainSplit, kernel="radial", cost=10, gamma=0.1)
  svm.predict <- predict(svm.model,testSplit)
  
  log.model <- glm(Class~.,trainSplit,family="binomial")
  log.predict <- predict(log.model,testSplit,type="response")
  fitted.results <- ifelse(log.predict > 0.5,1,0)
  
  knn.model = knn(trainSplit[,-31],testSplit[,-31],trainSplit$Class,k=17)
  
  rf.model <- randomForest(Class~.,trainSplit , mtry=1, importance = TRUE)
  rf.pred <- predict(rf.model, testSplit)
  
  
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

output$ROCmesure <- renderTable ({

  if (x()=="Linear"){
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="linear", cost=5)
    svm.predict <- predict(svm.model,testSplit)
    cm1 <- confusionMatrix(svm.predict,testSplit$Class)
    cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(ineq(svm.predict,type="Gini"),digits=4))
    
    
  }
  
  if (x()=="Radial"){
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="radial", cost=input$costrad, gamma=input$gammrad)
    svm.predict <- predict(svm.model,testSplit)
    cm1 <- confusionMatrix(svm.predict,testSplit$Class)
    cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(ineq(svm.predict,type="Gini"),digits=4))
    
  }
  
  if (x()=="Polynomial"){
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="polynomial", degree=input$ppoly,coef0=input$cpoly, cost=input$costpoly)
    svm.predict <- predict(svm.model,testSplit)
    cm1 <- confusionMatrix(svm.predict,testSplit$Class)
    cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(ineq(svm.predict,type="Gini"),digits=4))
    
  }
  
  if (x()=="Sigmoid"){
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="sigmoid", gamma=input$teta1sigmoid, coef0=input$teta2sigmoid)
    svm.predict <- predict(svm.model,testSplit)
    cm1 <- confusionMatrix(svm.predict,testSplit$Class)
    cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(ineq(svm.predict,type="Gini"),digits=4))
    
  }  
  
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
    plot.roc(testSplit$Class,as.numeric(fitted.results),percent=T,col="#4daf4a",lwd=4,add=TRUE)
    }
    
    if (y()=="KNN"){
    plot.roc(testSplit$Class,as.numeric(knn.model),percent=T,col="#850606",lwd=4,add=TRUE)
    }
    if (y()=="Random forest"){
    plot.roc(testSplit$Class,as.numeric(rf.pred),percent=T,col="#EE82EE",lwd=4,add=TRUE)
    }
    
    if (y()=="All"){
      plot.roc(testSplit$Class,as.numeric(fitted.results),percent=T,col="#4daf4a",lwd=4,add=TRUE)
      plot.roc(testSplit$Class,as.numeric(knn.model),percent=T,col="#850606",lwd=4,add=TRUE)
      plot.roc(testSplit$Class,as.numeric(rf.pred),percent=T,col="#EE82EE",lwd=4,add=TRUE)
    }

  })
  

  
output$LORCOMP <- renderPlot ({
  par(pty= "s")
  plot(Lc(svm.predict),main="Lorenz curve",col="#377eb8",lwd=4, xlab="Cumulative percentage of the population", ylab="Cumulative percentage of the variable Class")
  legend("topleft",legend=c("SVM","Logistic regression","KNN","Random forest"),col=c("#377eb8","#4daf4a","#850606","#EE82EE"),lwd=4)
  
  if (y()=="Logistic regression"){
  lines(Lc(fitted.results),col="#4daf4a",lwd=4)
  }
  if (y()=="KNN"){
  lines(Lc(knn.model),col="#850606",lwd=4)
  }
  if (y()=="Random forest"){
  lines(Lc(rf.pred),col="#EE82EE",lwd=4) 
  }
  if (y()=="All"){
    lines(Lc(fitted.results),col="#4daf4a",lwd=4)
    lines(Lc(knn.model),col="#850606",lwd=4)
    lines(Lc(rf.pred),col="#EE82EE",lwd=4) 
  }
})  
  

output$mesure <- renderTable({
  cm1 <- confusionMatrix(svm.predict,testSplit$Class)
  cm2 <- confusionMatrix(as.factor(fitted.results),testSplit$Class)
  cm3 <- confusionMatrix(knn.model,testSplit$Class)
  cm4 <- confusionMatrix(rf.pred,testSplit$Class)

  
  if (y()=="Logistic regression"){
  q=cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(ineq(svm.predict,type="Gini"),digits=4))
  s=cbind("Logistic regression",round(t(cm2$byClass[c(1,2,5,11)]),digits=4),round(ineq(fitted.results,type="Gini"),digits=4))
  rbind(q,s)
  }

  else if (y()=="KNN"){
  q=cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(ineq(svm.predict,type="Gini"),digits=4))
  s=cbind("KNN",round(t(cm3$byClass[c(1,2,5,11)]),digits=4),round(ineq(knn.model,type="Gini"),digits=4))
  rbind(q,s)
  }
  
  else if (y()=="Random forest"){
  q=cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(ineq(svm.predict,type="Gini"),digits=4))
  s=cbind("Random forest",round(t(cm4$byClass[c(1,2,5,11)]),digits=4),round(ineq(rf.pred,type="Gini"),digits=4))
  rbind(q,s)
  }
  
  else if (y()=="All"){
    q=cbind(Method="SVM",round(t(cm1$byClass[c(1,2,5,11)]),digits=4),Gini=round(ineq(svm.predict,type="Gini"),digits=4))
    k=cbind("Logistic regression",round(t(cm2$byClass[c(1,2,5,11)]),digits=4),round(ineq(fitted.results,type="Gini"),digits=4))
    l=cbind("KNN",round(t(cm3$byClass[c(1,2,5,11)]),digits=4),round(ineq(knn.model,type="Gini"),digits=4))
    m=cbind("Random forest",round(t(cm4$byClass[c(1,2,5,11)]),digits=4),round(ineq(rf.pred,type="Gini"),digits=4))
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
