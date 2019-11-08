library(shiny)
library(DMwR)
library(e1071)
library(pROC)
library(class)
library(randomForest)
library(caret)
library(ineq)




#creditcard <- read.csv("/Users/Victor/Desktop/creditcard.csv")
creditcard <- readRDS(file=url("https://raw.githubusercontent.com/VictorYeGitHub/SVM_PROJECT/master/creditcard.rds"))
creditcard$Class <- factor(creditcard$Class, levels=c("0","1"))

set.seed(12345)
creditcard <- creditcard[sample(nrow(creditcard)),]
samp = sample(1:nrow(creditcard),nrow(creditcard)*0.7)
train = creditcard[samp,]
testSplit = creditcard[-samp,] 
trainSplit <- SMOTE(Class ~ ., data  = creditcard, perc.over = 500, perc.under = 285, k=5)

svm.model <- svm(Class ~ ., data=trainSplit, kernel="radial", cost=5, gamma=0.3)
svm.predict <- predict(svm.model,testSplit)

log.model <- glm(Class~.,trainSplit,family="binomial")
log.predict <- predict(log.model,testSplit,type="response")
fitted.results <- ifelse(log.predict > 0.5,1,0)

knn.model = knn(trainSplit[,-31],testSplit[,-31],trainSplit$Class,k=5)

rf.model <- randomForest(Class~.,trainSplit , mtry=4, importance = TRUE)
rf.pred <- predict(rf.model, testSplit)

ui <- fluidPage(
    tags$style("body {padding-top: 70px;}"),
    navbarPage("SVM Project",position="fixed-top",
               tabPanel(p(icon("database"),"Introduction"),tabsetPanel(tabPanel("English", includeHTML("Introductionen.html")),
                                                 tabPanel("Français",includeHTML("Introduction.html")))),
                 tabPanel(p(icon("wrench"),"Use of the system"),tabsetPanel(tabPanel("English", includeHTML("utilisationen.html")),
                                                                      tabPanel("Français",includeHTML("utilisation.html")))),
                 tabPanel(p(icon("calculator"),"Case study"),
                          sidebarLayout(
                    sidebarPanel(selectInput(inputId='kernel', label='Choose a kernel to apply to the SVM',
                                             choices=c('Linear','Radial','Polynomial','Sigmoid'), multiple = FALSE, selected='Radial'),
                                 conditionalPanel("input.kernel == 'Radial'",
                                                  sliderInput("costrad", "The C constant of the regularization term in the Lagrange formulation", min = 1, max = 10, value = 5, step=1),
                                                  sliderInput("gammrad", "Hyperparameter Gamma", min = 0.1, max = 1, value =0.3, step=0.1)
                                                  ),
                                 conditionalPanel("input.kernel == 'Polynomial'",
                                                  sliderInput("costpoly","The C constant of the regularization term in the Lagrange formulation", min = 1, max = 10, value = 5, step=1),
                                                  sliderInput("cpoly","Hyperparameter c", min = 0, max = 1, value=0, step=0.1),
                                                  sliderInput("ppoly","Hyperparameter p (degree)", min = 1, max = 10, value=3, step=1)
                                                  ),
                                 conditionalPanel("input.kernel == 'Sigmoid'",
                                                  sliderInput("teta1sigmoid", "Hyperparameter Têta1 (Gamma)", min = 0.1, max = 1, value=0.1, step=0.1),
                                                  sliderInput("teta2sigmoid", "Hyperparameter Têta2 (Coef0)", min = 0, max = 1, value=0, step=0.1)
                                                  ),
                                 em("Note : the default values (kernel='Radial', C=5, Gamma=0.3) are the optimal values found from the tune.svm function.")
                                 
                                 
                                 
                                 
                                 )
                                 
                                 
                                 
                                 ,
                    mainPanel(plotOutput("ROCSVM"),includeHTML("logo.html")))),
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
               tabPanel(p(icon("info"),"About"),tabsetPanel(tabPanel("English", includeHTML("abouten.html")),
                                                               tabPanel("Français",includeHTML("about.html"))))
               
               ) )






server <- function(input, output, session) {

x <- reactive({input$kernel})

output$ROCSVM <- renderPlot ({ 
  if (x()=="Radial"){
  par(pty= "s")
  svm.model <- svm(Class ~ ., data=trainSplit, kernel="radial", cost=input$costrad, gamma=input$gammrad)
  svm.predict <- predict(svm.model,testSplit)
roc(svm.predict,as.numeric(testSplit$Class),plot=T,main="SVM with radial kernel",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=30, print.auc.x=25)
 } 
  if (x()=="Linear"){
    par(pty= "s")
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="linear", cost=5)
    svm.predict <- predict(svm.model,testSplit)
    roc(svm.predict,as.numeric(testSplit$Class),plot=T,main="SVM with linear kernel",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=30, print.auc.x=25)
  }
  if (x()=="Polynomial"){
    par(pty= "s")
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="polynomial", degree=input$ppoly,coef0=input$cpoly, cost=input$costpoly)
    svm.predict <- predict(svm.model,testSplit)
    roc(svm.predict,as.numeric(testSplit$Class),plot=T,main="SVM with polynomial kernel",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=30, print.auc.x=25)
  }
  if (x()=="Sigmoid"){
    par(pty= "s")
    svm.model <- svm(Class ~ ., data=trainSplit, kernel="sigmoid", gamma=input$teta1sigmoid, coef0=input$teta2sigmoid)
    svm.predict <- predict(svm.model,testSplit)
    roc(svm.predict,as.numeric(testSplit$Class),plot=T,main="SVM with sigmoid kernel",legacy.axes=TRUE,percent=TRUE,xlab="False Positive Percentage",ylab="True Positive Percentage",col="#377eb8",lwd=4,print.auc=T,print.auc.y=30, print.auc.x=25)
  }
})  


y <- reactive({input$compare})

  output$ROCCOMP <- renderPlot ({
    par(pty= "s")
    a <- as.numeric(testSplit$Class)
    roc(svm.predict,a,main="ROC curve",plot=T,legacy.axes=TRUE,percent=TRUE,xlab="1 - Specificity",ylab="Sensitivity",col="#377eb8",lwd=4)
    legend("bottomright",legend=c("SVM","Logistic regression","KNN","Random forest"),col=c("#377eb8","#4daf4a","#850606","#EE82EE"),lwd=4)
    
    if (y()=="Logistic regression"){
    plot.roc(fitted.results,a,percent=T,col="#4daf4a",lwd=4,add=TRUE)
    }
    
    if (y()=="KNN"){
    plot.roc(knn.model,a,percent=T,col="#850606",lwd=4,add=TRUE)
    }
    if (y()=="Random forest"){
    plot.roc(rf.pred,a,percent=T,col="#EE82EE",lwd=4,add=TRUE)
    }
    
    if (y()=="All"){
      plot.roc(fitted.results,a,percent=T,col="#4daf4a",lwd=4,add=TRUE)
      plot.roc(knn.model,a,percent=T,col="#850606",lwd=4,add=TRUE)
      plot.roc(rf.pred,a,percent=T,col="#EE82EE",lwd=4,add=TRUE)
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



}

# Run the application 
shinyApp(ui = ui, server = server)
