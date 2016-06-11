# import libraries
library(MASS)
library(class)
library(ROCR)
library(kknn)   # knn
library(e1071)  # SVM
library(rpart)  # Tree
library(tree)
library(klaR)   # NB
library(C50)
library(pls)
library(caret)  # plsda
library(class)
library(mda)
library(nnet)   # multinom
library(Rmisc)  # plot
library(ggplot2)
library(mvtnorm)
library(pROC)
library(verification)
library(randomForest)
library(matrixStats)
source("/Users/ensheng/OneDrive/stat500/functions/getData.R")
source("/Users/ensheng/OneDrive/stat500/functions/getName.R")

for (id in c(504)) {
  # 504 wine.dat
  # http://sci2s.ugr.es/keel/dataset.php?cod=1077
  # count(realData[,1]==1)=59
  # count(realData[,1]==2)=71
  # count(realData[,1]==3)=48
  # IR = 71/48 = 1.5
  readFile<-read.csv("/Users/ensheng/OneDrive/stat500/Data/wine.dat",header = F)
  realData<-readFile[,c(ncol(readFile),1:(ncol(readFile)-1))]
  realData0<-realData[realData[,1]==1,][,-1]
  realData1<-realData[realData[,1]==3,][,-1]
  realData2<-realData[realData[,1]==2,][,-1]
  
  # set up the image layout and the processing time
  par(mfrow=c(3,1))
  ptm<-proc.time()
  
  # create the output file
  fileNum<-504
  fileName<-getName(fileNum)
  outputFileA<-paste('/Users/ensheng/OneDrive/stat500/results/nsim3/output_',toString(fileNum),sep="")
  outputFileB<-paste(outputFileA,'_',sep="")
  outputFileC<-paste(outputFileB,fileName,sep="")
  outputFile<-paste(outputFileC,'.txt',sep="")
  outputLoc<-paste(outputFile,sep="")
  sink(outputLoc,split=TRUE)
  
  # get the dataset
  print(fileNum)
  realData<-getData(fileNum)
  
  N<-ncol(realData)-1                       # number of features
  kValue<-10
  
  # c("knn","ld","log","svm","dtree","ptree","neural","nb","C50","fda","pls","mda")
  
  # output summary table
  output.table<-matrix(data=NA,nrow=36,ncol=12)
  w<-1
  
  for (models in c("knn","ld","log","svm","dtree","ptree","neural","nb","C50","fda","pls","mda")) {
    cat("\n", models, "\n")
    
    sigma.noise<-c(0.1,0.5,1.0)             # test for 0, 0.1, 0.5, 1.0
    nsim<-3                                # 100, simulation repeat times (1, 50, 100, 500)
    noisy.repl<-c(1)                     # c(1:10) replications of the rare parts (y=1)
    noisy.train<-c(10,20,40,60,80,100)      # c(10,20,40,60,80,100) replications of the training data sets
    nnrepl<-max(noisy.repl)                 # number of rows; maximum number in the noisy.repl; j
    nntrain<-max(noisy.train)               # number of columns; maximum number in the noisy.train; i
    e<-0.00000001                           # for KL distance
    roc.diff.ci<-c()                        # for the plot
    roc.diff.mean<-c()                      # for the plot
    kl.diff.ci<-c()
    kl.diff.mean<-c()
    eu.diff.ci<-c()                            
    eu.diff.mean<-c()  
    
    for (k in sigma.noise) {
      print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
      cat("model =", models, "; sigma.noise =", k, "\n")
      
      # evaluation criteria
      roc.multi<-list()
      rocdiff.multi<-list()
      roc.sum<-matrix(0, nrow=nnrepl, ncol=nntrain) # save roc results for each nsim; same as roc.ave in previous versions
      rocdiff.sum<-matrix(0, nrow=nnrepl, ncol=nntrain)
      rocMean<-0
      
      kl.multi<-list()
      kldiff.multi<-list()
      kl.sum<-matrix(0, nrow=nnrepl, ncol=nntrain) # save kl results for each nsim; same as kl.ave in previous versions
      kldiff.sum<-matrix(0, nrow=nnrepl, ncol=nntrain)
      klMean<-0
      
      eu.multi<-list()
      eudiff.multi<-list()
      eu.sum<-matrix(0, nrow=nnrepl, ncol=nntrain) # save eu results for each nsim; same as eu.ave in previous versions
      eudiff.sum<-matrix(0, nrow=nnrepl, ncol=nntrain)
      euMean<-0
      
      for (t in 1:nsim) {
        
        ############ Real Data Set ############
        
        ## split into training/testing data sets
        smp_size<-floor(0.5 * nrow(realData0))
        train_ind<-sample(seq_len(nrow(realData0)), size = smp_size)
        train0<-realData0[train_ind, ]
        test0<-realData0[-train_ind, ]
        
        smp_size<-floor(0.5 * nrow(realData1))
        train_ind<-sample(seq_len(nrow(realData1)), size = smp_size)
        train1<-realData1[train_ind, ]
        test1<-realData1[-train_ind, ]
        
        smp_size<-floor(0.5 * nrow(realData2))
        train_ind<-sample(seq_len(nrow(realData2)), size = smp_size)
        train2<-realData2[train_ind, ]
        test2<-realData2[-train_ind, ]
        
        #
        train.X<-rbind(train0,train1,train2)
        test.X<-rbind(test0,test1,test2)
        train.y<-c(rep(0,nrow(train0)),rep(1,nrow(train1)),rep(2,nrow(train2)))
        test.y<-c(rep(0,nrow(test0)),rep(1,nrow(test1)),rep(2,nrow(test2)))
        factor.y<-as.factor(train.y)
        df.train<-as.data.frame(cbind(train.y,train.X))
        df.test<-as.data.frame(cbind(test.y,test.X))
        #
        n0<-nrow(train0)
        n1<-nrow(train1)
        n2<-nrow(train2)
        train.size<-nrow(train.X)  # size of the training data set
        
        ############################################### 
        ############# Original Assessmet ##############
        ############################################### 
        
        if (models=="knn") {
          ############ KKNN ############
          y.fit<-kknn(as.factor(df.train$train.y)~., df.train, df.test, k=kValue)
          y.prob<-y.fit$"prob"
        } else if (models=="ld") {
          ############ Linear Discriminant ############
          y.fit<-lda(train.y~., data=df.train)
          y.prob<-predict(y.fit,df.test)$posterior
        } else if (models=="log") {
          ############ Logistic Regression ############
          y.fit<-multinom(factor.y~., data=df.train[,-1], trace=FALSE)
          y.prob<-predict(y.fit,df.test,type="probs")
        } else if (models=="svm") {
          ############ SVM ############
          y.fit<-svm(factor.y~., data=df.train[,-1], probability=TRUE)
          y.pred<-predict(y.fit,newdata=df.test,probability=TRUE)
          y.prob<-attr(y.pred,"probabilities")
        } else if (models=="dtree") {
          ############ Decision Tree ############
          y.fit<-tree(factor.y~., data=df.train[,-1])
          y.prob<-predict(y.fit, df.test, type="vector")
        } else if (models=="ptree") {
          ############ Prune Tree ############
          y.auto<-rpart(factor.y~., data=df.train[,-1])
          y.fit<-prune(y.auto, cp=0.1)
          y.prob<-predict(y.fit, df.test)
        } else if (models=="forest") {
          ############ Random Forest ############
          y.fit<-randomForest(factor.y~.,data=df.train[,-1],sampsize=train.size)
          y.prob<-predict(y.fit,newdata=df.test,type="prob")
        } else if (models=="neural") {
          ############ Neural Network ############
          y.fit<-nnet(factor.y~., data=df.train[,-1],size=2,decay = 5e-4, maxit = 200, trace=FALSE)
          y.prob<-predict(y.fit, df.test,type="raw")
        } else if (models=="nb") {
          ############ Naive Bayes ############
          y.fit<-naiveBayes(factor.y~.,data=df.train[,-1])
          y.prob=predict(y.fit,df.test,type = "raw")
        } else if (models=="C50") {
          ############ C50 ############
          y.fit<-C5.0(factor.y~., data=df.train[,-1],rules=FALSE)
          y.prob<-predict(y.fit, df.test, type = "prob")
        } else if (models=="fda") {
          ############ FDA ############
          y.fit<-fda(factor.y~., data=df.train[,-1])
          y.prob<-predict(y.fit, df.test,type = "posterior")
        } else if (models=="pls") {
          ############ PLS ############
          y.fit<-plsda(df.train[,-1],factor.y)
          y.prob<-predict(y.fit,df.test[,-1],type = "prob")[,1:(max(df.test[,1])+1),]
        } else if (models=="mda") {
          ############ MDA ############
          y.fit<-mda(factor.y~., data=df.train[,-1])
          y.prob<-predict(y.fit,df.test,type = "posterior")
        } else {
          stop("Wrong model type!")
          quit("no")  # not working?
        }
        
        # evaluation
        # roc
        roc0<-multiclass.roc(test.y, y.prob[,1])$auc
        # kl
        log.prob<-log(1/(y.prob+e))
        log.matrix<-cbind(df.test[,1],log.prob)
        kl.list<-list()
        for (p in c(0:max(df.test[,1]))) {
          kl.list[[p+1]]<-sum(log.matrix[log.matrix[,1] == p,][,(p+2)])
        }
        kl0<-Reduce("+",kl.list)/nrow(log.matrix)
        # eu
        minus.prob<-(1-y.prob)^2
        minus.matrix<-cbind(df.test[,1],minus.prob)
        eu.list<-list()
        for (p in c(0:max(df.test[,1]))) {
          eu.list[[p+1]]<-sum(minus.matrix[minus.matrix[,1] == p,][,(p+2)])
        }
        eu0<-sqrt(Reduce("+",eu.list)/nrow(log.matrix))
        
        rocMean<-rocMean+roc0
        klMean<-klMean+kl0
        euMean<-euMean+eu0
        
        ################################################ 
        ######## Vibration in Training Data Set ########
        ################################################ 
        
        # store the results of roc areas for each pair of training and validation data sets
        yprob.single<-matrix(0, nrow=nnrepl, ncol=nntrain)
        roc.table<-matrix(0, nrow=nnrepl, ncol=nntrain)
        roc.diff<-matrix(0, nrow=nnrepl, ncol=nntrain)
        
        kl.table<-matrix(0, nrow=nnrepl, ncol=nntrain)
        kl.diff<-matrix(0, nrow=nnrepl, ncol=nntrain)
        
        eu.table<-matrix(0, nrow=nnrepl, ncol=nntrain)
        eu.diff<-matrix(0, nrow=nnrepl, ncol=nntrain)
        ############################
        for (j in 1:nnrepl) {
          rare.size<-j*n1
          total.size<-rare.size+n0+n2 # size of the vibrated training data set: (n0+j*n1)
          train1.star<-train1[rep(seq_len(nrow(train1)), j), ] # duplicate the rare part
          varDiag<-diag(colVars(as.matrix(train1.star)))  # sample variance diagonal (sigma q)
          trainVib.y<-c(rep(0,n0),rep(1,rare.size),rep(2,n2))
          factoryVib.y<-as.factor(trainVib.y)
          yhat<-0
          for (i in 1:nntrain) {
            # add noise to each rare part in the training data set
            noise<-mvrnorm(rare.size, rep(0,N), k*varDiag, empirical = TRUE) # epsilson diag(N)
            train1.vib<-train1.star+noise # vibrate the rare part
            train1.anti<-train1.star-noise # add anti-noise
            
            # generate the training data set with j rare parts (y=1)
            trainVib.X<-rbind(train0,train1.vib,train2) # training data set after vibrating the rare part
            trainVib<-as.data.frame(cbind(trainVib.y, trainVib.X))
            trainAnti.X<-rbind(train0,train1.anti,train2)
            trainAnti<-as.data.frame(cbind(trainVib.y, trainAnti.X))
            
            # models
            if (models=="knn") {
              ############ kknn ############ 
              y.pred_noise<-kknn(as.factor(trainVib$trainVib.y)~., trainVib, df.test, k=kValue)
              y.prob_noise<-y.pred_noise$"prob"
              
              y.pred_anti<-kknn(as.factor(trainAnti$trainVib.y)~., trainAnti, df.test, k=kValue)
              y.prob_anti<-y.pred_anti$"prob"
            }  else if (models=="ld") {
              ############ lda ############
              lda.fit<-lda(trainVib.y~., data=trainVib[,-1])
              y.prob_noise<-predict(lda.fit,df.test)$posterior
              
              lda.fit<-lda(trainVib.y~., data=trainAnti[,-1])
              y.prob_anti<-predict(lda.fit,df.test)$posterior
            } else if (models=="log") {
              ############ Logistic Regression ############
              y.fit<-multinom(factoryVib.y~., data=trainVib[,-1], trace=FALSE)
              y.prob_noise<-predict(y.fit,df.test,type="probs")
              
              y.fit<-multinom(factoryVib.y~., data=trainAnti[,-1], trace=FALSE)
              y.prob_anti<-predict(y.fit,df.test,type="probs")
            } else if (models=="svm") {
              ############ SVM ############
              y.fit<-svm(factoryVib.y~., data=trainVib[,-1], probability=TRUE)
              y.pred<-predict(y.fit,newdata=df.test,probability=TRUE)
              y.prob_noise<-attr(y.pred,"probabilities")
              
              y.fit<-svm(factoryVib.y~., data=trainAnti[,-1], probability=TRUE)
              y.pred<-predict(y.fit,newdata=df.test,probability=TRUE)
              y.prob_anti<-attr(y.pred,"probabilities")
            } else if (models=="dtree") {
              ############ Decision Tree ############ 
              y.fit<-tree(factoryVib.y~., data=trainVib[,-1])
              y.prob_noise<-predict(y.fit, df.test, type="vector")
              
              y.fit<-tree(factoryVib.y~., data=trainAnti[,-1])
              y.prob_anti<-predict(y.fit, df.test, type="vector")
            } else if (models=="ptree") {
              ############ Prune Tree ############
              y.auto<-rpart(factoryVib.y~., data=trainVib[,-1])
              y.fit<-prune(y.auto, cp=0.1)
              y.prob_noise<-predict(y.fit, df.test)
              
              y.auto<-rpart(factoryVib.y~., data=trainAnti[,-1])
              y.fit<-prune(y.auto, cp=0.1)
              y.prob_anti<-predict(y.fit, df.test)
            } else if (models=="forest") {
              ############ Random Forest ############
              y.fit<-randomForest(factoryVib.y~., data=trainVib[,-1],sampsize=train.size)
              y.prob_noise<-predict(y.fit,newdata=df.test,type="prob")
              
              y.fit<-randomForest(factoryVib.y~., data=trainAnti[,-1],sampsize=train.size)
              y.prob_anti<-predict(y.fit,newdata=df.test,type="prob")
            } else if (models=="neural") {
              ############ Neural Network ############
              y.fit<-nnet(factoryVib.y~., data=trainVib[,-1],size=2,decay = 5e-4, maxit = 200, trace=FALSE)
              y.prob_noise<-predict(y.fit, df.test,type="raw")
              
              y.fit<-nnet(factoryVib.y~., data=trainAnti[,-1],size=2,decay = 5e-4, maxit = 200, trace=FALSE)
              y.prob_anti<-predict(y.fit, df.test,type="raw")
            } else if (models=="nb") {
              ############ Naive Bayes ############ 
              y.fit<-naiveBayes(factoryVib.y~.,data=trainVib[,-1])
              y.prob_noise<-predict(y.fit,df.test,type = "raw")
              
              y.fit<-naiveBayes(factoryVib.y~.,data=trainAnti[,-1])
              y.prob_anti<-predict(y.fit,df.test,type = "raw")
            } else if (models=="C50") {
              ############ C50 ############
              y.fit<-C5.0(factoryVib.y~., data=trainVib[,-1],rules=TRUE)
              y.prob_noise<-predict(y.fit, df.test, type = "prob")
              
              y.fit<-C5.0(factoryVib.y~., data=trainAnti[,-1],rules=TRUE)
              y.prob_anti<-predict(y.fit, df.test, type = "prob")
            } else if (models=="fda") {
              ############ FDA ############
              y.fit<-fda(factoryVib.y~., data=trainVib[,-1])
              y.prob_noise<-predict(y.fit, df.test, type = "posterior")
              
              y.fit<-fda(factoryVib.y~., data=trainAnti[,-1])
              y.prob_anti<-predict(y.fit, df.test, type = "posterior")
            } else if (models=="pls") {
              ############ PLS ############
              y.fit<-plsda(trainVib[,-1],factoryVib.y)
              y.prob_noise<-predict(y.fit, df.test[,-1], type = "prob")[,1:(max(df.test[,1])+1),]
              
              y.fit<-plsda(trainAnti[,-1],factoryVib.y)
              y.prob_anti<-predict(y.fit, df.test[,-1], type = "prob")[,1:(max(df.test[,1])+1),]
            } else if (models=="mda") {
              ############ MDA ############
              y.fit<-mda(factoryVib.y~., data=trainVib[,-1])
              y.prob_noise<-predict(y.fit, df.test, type = "posterior")
              
              y.fit<-mda(factoryVib.y~., data=trainAnti[,-1])
              y.prob_anti<-predict(y.fit, df.test, type = "posterior")
            } else {
              stop("Wrong model type! Please use lower cases")
            }
            
            # prediction probabilities after two-size vibration
            yhat<-yhat+((y.prob_noise+y.prob_anti)/2)   # accumulative yhat
            
            # assessment
            y.prob<-yhat/i
            roc.table[j,i]<-multiclass.roc(test.y, y.prob[,1])$auc       # final roc table, same size as roc.summary
            roc.diff[j,i]<-roc.table[j,i]-roc0
            
            #kl
            log.prob<-log(1/(y.prob+e))
            log.matrix<-cbind(df.test[,1],log.prob)
            kl.list<-list()
            # p - categories for df.test[,1]
            for (p in c(0:max(df.test[,1]))) {
              kl.list[[p+1]]<-sum(log.matrix[minus.matrix[,1] == p,][,(p+2)])
            }
            kl.table[j,i]<-Reduce("+",kl.list)/nrow(log.matrix)
            kl.diff[j,i]<-kl.table[j,i]-kl0
            
            #eu
            minus.prob<-(1-y.prob)^2
            minus.matrix<-cbind(df.test[,1],minus.prob)
            eu.list<-list()
            for (p in c(0:max(df.test[,1]))) {
              eu.list[[p+1]]<-sum(minus.matrix[minus.matrix[,1] == p,][,(p+2)])
            }
            eu.table[j,i]<-sqrt(Reduce("+",eu.list)/nrow(log.matrix))      # final eu table, same size as eu.summary
            eu.diff[j,i]<-eu.table[j,i]-eu0
          }
          plot(roc.table[j,])
          abline(h = roc0)
          
          plot(kl.table[j,])
          abline(h = kl0)
          
          plot(eu.table[j,])
          abline(h = eu0)
        }
        roc.multi[[t]]<-roc.table
        roc.sum<-roc.sum+roc.table
        rocdiff.multi[[t]]<-roc.diff
        rocdiff.sum<-rocdiff.sum+roc.diff
        
        kl.multi[[t]]<-kl.table
        kl.sum<-kl.sum+kl.table
        kldiff.multi[[t]]<-kl.diff
        kldiff.sum<-kldiff.sum+kl.diff
        
        eu.multi[[t]]<-eu.table
        eu.sum<-eu.sum+eu.table
        eudiff.multi[[t]]<-eu.diff
        eudiff.sum<-eudiff.sum+eu.diff
      }
      
      cat("\nOriginal ROC Mean =", rocMean/nsim, "\n")
      
      # roc final result
      roc.final<-(roc.sum/nsim)[noisy.repl,noisy.train]
      print ("roc results: ")
      print (roc.final)
      
      # roc difference
      rocdiff.final<-(rocdiff.sum/nsim)[noisy.repl,noisy.train]
      print ("roc difference: ")
      print (rocdiff.final)
      
      # plot roc difference (noisy.repl=2, nosiy.train=10)
      roc.ci.table<-c()
      for (t in 1:nsim) {
        roc.ci.table<-append(roc.ci.table,rocdiff.multi[[t]][nnrepl,nntrain])
      }
      roc.diff.ci<-append(roc.diff.ci,(qnorm(0.975)*sd(roc.ci.table)/sqrt(nsim)))    # roc difference ci
      roc.diff.mean<-append(roc.diff.mean, (rocdiff.sum/nsim)[nnrepl,nntrain])     # roc difference mean
      
      
      ###############################
      cat("\nOriginal KL Mean =", klMean/nsim, "\n")
      
      # kl final result
      kl.final<-(kl.sum/nsim)[noisy.repl,noisy.train]
      print ("kl results: ")
      print (kl.final)
      
      # kl difference
      kldiff.final<-(kldiff.sum/nsim)[noisy.repl,noisy.train]
      print ("kl difference: ")
      print (kldiff.final)
      
      # plot kl difference (noisy.repl=2, nosiy.train=10)
      kl.ci.table<-c()
      for (t in 1:nsim) {
        kl.ci.table<-append(kl.ci.table,kldiff.multi[[t]][nnrepl,nntrain])
      }
      kl.diff.ci<-append(kl.diff.ci,(qnorm(0.975)*sd(kl.ci.table)/sqrt(nsim)))    # kl difference ci
      kl.diff.mean<-append(kl.diff.mean, (kldiff.sum/nsim)[nnrepl,nntrain])     # kl difference mean
      
      ###############################
      cat("\nOriginal EU Mean =", euMean/nsim, "\n")
      
      # eu final result
      eu.final<-(eu.sum/nsim)[noisy.repl,noisy.train]
      print ("eu results: ")
      print (eu.final)
      
      # eu difference
      eudiff.final<-(eudiff.sum/nsim)[noisy.repl,noisy.train]
      print ("eu difference: ")
      print (eudiff.final)
      
      # plot eu difference (noisy.repl=2, nosiy.train=10)
      eu.ci.table<-c()
      for (t in 1:nsim) {
        eu.ci.table<-append(eu.ci.table,eudiff.multi[[t]][nnrepl,nntrain])
      }
      eu.diff.ci<-append(eu.diff.ci,(qnorm(0.975)*sd(eu.ci.table)/sqrt(nsim)))    # eu difference ci
      eu.diff.mean<-append(eu.diff.mean, (eudiff.sum/nsim)[nnrepl,nntrain])     # eu difference mean
      
      # output table
      output.table[w,1]<-rocMean/nsim
      output.table[w,2]<-roc.final[length(roc.final)]
      output.table[w,3]<-rocdiff.final[length(rocdiff.final)]
      output.table[w,4]<-roc.diff.ci[length(roc.diff.ci)]
      output.table[w,5]<-klMean/nsim
      output.table[w,6]<-kl.final[length(kl.final)]
      output.table[w,7]<-kldiff.final[length(kldiff.final)]
      output.table[w,8]<-kl.diff.ci[length(kl.diff.ci)]
      output.table[w,9]<-euMean/nsim
      output.table[w,10]<-eu.final[length(eu.final)]
      output.table[w,11]<-eudiff.final[length(eudiff.final)]
      output.table[w,12]<-eu.diff.ci[length(eu.diff.ci)]
      w<-w+1
    }
    
    # plot roc difference among sigma.noise=(0.1,0.5,1.0)
    cat("\n")
    print("***********************")
    print("ROC diff mean:")
    print(roc.diff.mean)
    print("ROC diff CI")
    print(roc.diff.ci)
    
    roc.plot<-matrix(0, nrow = 3, ncol = 3)
    colnames(roc.plot)<-c("noise","mean","sd")
    roc.plot[,1]<-c(0.1,0.5,1.0)
    roc.plot[1,2:3]<-c(roc.diff.mean[1],roc.diff.ci[1])
    roc.plot[2,2:3]<-c(roc.diff.mean[2],roc.diff.ci[2])
    roc.plot[3,2:3]<-c(roc.diff.mean[3],roc.diff.ci[3])
    roc.plot<-data.frame(noise=c(0.1,0.5,1.0),
                         mean=roc.plot[,2],
                         sd=roc.plot[,3])
    p<-ggplot(roc.plot, aes(x=noise, y=mean), colour=mean) +
      geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
      geom_line() +
      geom_point() +
      xlab("noise") +
      ylab("roc.diff") +
      geom_hline(yintercept = 0)
    
    if (models=="knn") {
      roc.p1<-p+ggtitle("KNN ROC diff")
    } else if (models=="ld") {
      roc.p2<-p+ggtitle("LD ROC diff")
    } else if (models=="log") {
      roc.p3<-p+ggtitle("LOG ROC diff")
    } else if (models=="svm") {
      roc.p4<-p+ggtitle("SVM ROC diff")
    } else if (models=="dtree") {
      roc.p5<-p+ggtitle("Dtree ROC diff")
    } else if (models=="ptree") {
      roc.p6<-p+ggtitle("Ptree ROC diff")
    } else if (models=="forest") {
      roc.p7<-p+ggtitle("Forest ROC diff")
    } else if (models=="neural") {
      roc.p8<-p+ggtitle("Neural ROC diff")
    } else if (models=="nb") {
      roc.p9<-p+ggtitle("NBayes ROC diff")
    } else if (models=="C50") {
      roc.p10<-p+ggtitle("C5.0 ROC diff")
    } else if (models=="fda") {
      roc.p11<-p+ggtitle("FDA ROC diff")
    } else if (models=="pls") {
      roc.p12<-p+ggtitle("PLSDA ROC diff")
    } else if (models=="mda") {
      roc.p13<-p+ggtitle("MDA ROC diff")
    } else {
      stop("Wrong model type!!!")
    }
    
    
    ##############################################
    # plot kl difference among sigma.noise=(0.1,0.5,1.0)
    print("***********************")
    print("KL diff mean:")
    print(kl.diff.mean)
    print("KL diff CI")
    print(kl.diff.ci)
    
    kl.plot<-matrix(0, nrow = 3, ncol = 3)
    colnames(kl.plot)<-c("noise","mean","sd")
    kl.plot[,1]<-c(0.1,0.5,1.0)
    kl.plot[1,2:3]<-c(kl.diff.mean[1],kl.diff.ci[1])
    kl.plot[2,2:3]<-c(kl.diff.mean[2],kl.diff.ci[2])
    kl.plot[3,2:3]<-c(kl.diff.mean[3],kl.diff.ci[3])
    kl.plot<-data.frame(noise=c(0.1,0.5,1.0),
                        mean=kl.plot[,2],
                        sd=kl.plot[,3])
    p<-ggplot(kl.plot, aes(x=noise, y=mean), colour=mean) +
      geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
      geom_line() +
      geom_point() +
      xlab("noise") +
      ylab("kl.diff") +
      geom_hline(yintercept = 0)
    
    if (models=="knn") {
      kl.p1<-p+ggtitle("KNN KL diff")
    } else if (models=="ld") {
      kl.p2<-p+ggtitle("LD KL diff")
    } else if (models=="log") {
      kl.p3<-p+ggtitle("LOG KL diff")
    } else if (models=="svm") {
      kl.p4<-p+ggtitle("SVM KL diff")
    } else if (models=="dtree") {
      kl.p5<-p+ggtitle("Dtree KL diff")
    } else if (models=="ptree") {
      kl.p6<-p+ggtitle("Ptree KL diff")
    } else if (models=="forest") {
      kl.p7<-p+ggtitle("Forest KL diff")
    } else if (models=="neural") {
      kl.p8<-p+ggtitle("Neural KL diff")
    } else if (models=="nb") {
      kl.p9<-p+ggtitle("NBayes KL diff")
    } else if (models=="C50") {
      kl.p10<-p+ggtitle("C5.0 KL diff")
    } else if (models=="fda") {
      kl.p11<-p+ggtitle("FDA KL diff")
    } else if (models=="pls") {
      kl.p12<-p+ggtitle("PLSDA KL diff")
    } else if (models=="mda") {
      kl.p13<-p+ggtitle("MDA KL diff")
    } else {
      stop("Wrong model type!!!")
    }
    
    
    ##############################################
    # plot eu difference among sigma.noise=(0.1,0.5,1.0)
    print("***********************")
    print("EU diff mean:")
    print(eu.diff.mean)
    print("EU diff CI")
    print(eu.diff.ci)
    
    eu.plot<-matrix(0, nrow = 3, ncol = 3)
    colnames(eu.plot)<-c("noise","mean","sd")
    eu.plot[,1]<-c(0.1,0.5,1.0)
    eu.plot[1,2:3]<-c(eu.diff.mean[1],eu.diff.ci[1])
    eu.plot[2,2:3]<-c(eu.diff.mean[2],eu.diff.ci[2])
    eu.plot[3,2:3]<-c(eu.diff.mean[3],eu.diff.ci[3])
    eu.plot<-data.frame(noise=c(0.1,0.5,1.0),
                        mean=eu.plot[,2],
                        sd=eu.plot[,3])
    p<-ggplot(eu.plot, aes(x=noise, y=mean), colour=mean) +
      geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
      geom_line() +
      geom_point() +
      xlab("noise") +
      ylab("eu.diff") +
      geom_hline(yintercept = 0)
    
    if (models=="knn") {
      eu.p1<-p+ggtitle("KNN RMSE diff")
    } else if (models=="ld") {
      eu.p2<-p+ggtitle("LD RMSE diff")
    } else if (models=="log") {
      eu.p3<-p+ggtitle("LOG RMSE diff")
    } else if (models=="svm") {
      eu.p4<-p+ggtitle("SVM RMSE diff")
    } else if (models=="dtree") {
      eu.p5<-p+ggtitle("Dtree RMSE diff")
    } else if (models=="ptree") {
      eu.p6<-p+ggtitle("Ptree RMSE diff")
    } else if (models=="forest") {
      eu.p7<-p+ggtitle("Forest RMSE diff")
    } else if (models=="neural") {
      eu.p8<-p+ggtitle("Neural RMSE diff")
    } else if (models=="nb") {
      eu.p9<-p+ggtitle("NBayes RMSE diff")
    } else if (models=="C50") {
      eu.p10<-p+ggtitle("C5.0 RMSE diff")
    } else if (models=="fda") {
      eu.p11<-p+ggtitle("FDA RMSE diff")
    } else if (models=="pls") {
      eu.p12<-p+ggtitle("PLSDA RMSE diff")
    } else if (models=="mda") {
      eu.p13<-p+ggtitle("MDA RMSE diff")
    } else {
      stop("Wrong model type!!!")
    }
  }
  
  # save the images
  imageROC1<-paste(outputFileC,"_roc",sep="")
  imageROC<-paste(imageROC1,".png",sep="")
  png(imageROC, width=18, height=8, units="in", res=300)
  multiplot(roc.p1, roc.p2, roc.p3, roc.p4, roc.p5, roc.p6, roc.p8, roc.p9, roc.p10, roc.p11, roc.p12, roc.p13, cols=6)
  # multiplot(roc.p1, roc.p3, roc.p4, roc.p6, roc.p8, roc.p9, roc.p10, roc.p12, cols=4)
  dev.off()
  dev.off()
  
  imageKL1<-paste(outputFileC,"_kl",sep="")
  imageKL<-paste(imageKL1,".png",sep="")
  png(imageKL, width=18, height=8, units="in", res=300)
  multiplot(kl.p1, kl.p2, kl.p3, kl.p4, kl.p5, kl.p6, kl.p8, kl.p9, kl.p10, kl.p11, kl.p12, kl.p13, cols=6)
  # multiplot(kl.p1, kl.p3, kl.p4, kl.p6, kl.p8, kl.p9, kl.p10, kl.p12, cols=4)
  dev.off()
  
  imageRMSE1<-paste(outputFileC,"_rmse",sep="")
  imageRMSE<-paste(imageRMSE1,".png",sep="")
  png(imageRMSE, width=18, height=8, units="in", res=300)
  multiplot(eu.p1, eu.p2, eu.p3, eu.p4, eu.p5, eu.p6, eu.p8, eu.p9, eu.p10, eu.p11, eu.p12, eu.p13, cols=6)
  # multiplot(eu.p1, eu.p3, eu.p4, eu.p6, eu.p8, eu.p9, eu.p10, eu.p12, cols=4)
  dev.off()
  
  # output the final big table
  rownames(output.table)<-rep(c("0.1","0.5","1.0"),12)
  colnames(output.table)<-c("ROC0","ROC100","ROCdiff","ROCdiffCI","KL0","KL100","KLdiff","KLdiffCI","RMSE0","RMSE100","RMSEdiff","RMSEdiffCI")
  print(output.table)
  
  # close the output file
  sink()
  
  # running time
  proc.time() - ptm
  
}