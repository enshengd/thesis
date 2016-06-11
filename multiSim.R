# import libraries
library(MASS)
library(class)
library(ROCR)
library(kknn)   # kknn
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

par(mfrow=c(3,1))
ptm<-proc.time()

# c("knn","ld","log","svm","dtree","ptree","neural","nb","C50","fda","pls","mda")

# output summary table
output.table<-matrix(data=NA,nrow=36,ncol=12)
w<-1

for (models in c("knn","ld","log","svm","dtree","ptree","nb","C50","fda","pls","mda","neural")) {
  print (models)
  
  N<-2                                    # number of varialbes (x)
  sigma.noise<-c(0.1,0.5,1.0)             # test for 0, 0.1, 0.5, 1.0
  nsim<-100                               # 3, 100, simulation repeat times (1, 50, 100, 500)
  noisy.repl<-c(1)                        # c(1,3,5,7,9,10), c(1:10) replications of the rare parts (y=1)
  noisy.train<-c(10,20,40,60,80,100)      # c(1:10), c(10,20,40,60,80,100) replications of the training data set
  nnrepl<-max(noisy.repl)                 # number of rows; maximum number in the noisy.repl; j
  nntrain<-max(noisy.train)               # number of columns; maximum number in the noisy.train; i
  n0<-200                                 # number of "0"s in the training data set
  n1<-20                                  # number of "1"s in the training data set
  n2<-180
  train.size<-220                         # size of the training data set
  e<-0.00000001                           # for KL distance
  roc.diff.ci<-c()                        # for the plot
  roc.diff.mean<-c()                      # for the plot
  kl.diff.ci<-c()
  kl.diff.mean<-c()
  rmse.diff.ci<-c()                            
  rmse.diff.mean<-c()                         
  kValue<-10                              # k for knn (1,10)
  
  for (k in sigma.noise) {
    cat("\nmodel =", models, "; sigma.noise =", k, "\n")
    
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
    
    rmse.multi<-list()
    rmsediff.multi<-list()
    rmse.sum<-matrix(0, nrow=nnrepl, ncol=nntrain) # save rmse results for each nsim; same as rmse.ave in previous versions
    rmsediff.sum<-matrix(0, nrow=nnrepl, ncol=nntrain)
    rmseMean<-0
    
    for (t in 1:nsim) {
      ############ Simulated data sets (N variables) ############
      # training data set
      sigma0<-diag(N)
      sigma1<-diag(N)
      sigma1[lower.tri(sigma1)]<-0.5
      sigma1[upper.tri(sigma1)]<-0.5
      train0<-mvrnorm(n0, rep(0,N), sigma0)
      train1<-mvrnorm(n1, rep(1,N), sigma1)
      train2<-mvrnorm(n2, rep(2,N), sigma0)
      # test data set
      test0<-mvrnorm(n0, rep(0,N), sigma0)
      test1<-mvrnorm(n1, rep(1,N), sigma1)
      test2<-mvrnorm(n2, rep(2,N), sigma0)
      # data sets summary
      train.X<-rbind(train0,train1,train2)
      test.X<-rbind(test0,test1,test2)
      train.y<-c(rep(0,n0),rep(1,n1),rep(2,n2))
      test.y<-c(rep(0,n0),rep(1,n1),rep(2,n2))
      factor.y<-as.factor(train.y)
      df.train<-as.data.frame(cbind(train.y,train.X))
      df.test<-as.data.frame(cbind(test.y,test.X))
      
      ###############################################
      ############# Original Assessmet ##############
      ###############################################
      
      if (models=="knn") {
        ############ KKNN ############
        y.fit<-kknn(factor.y~., df.train, df.test, k=kValue)
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
      kl.list<-list()      # p - categories for df.test[,1]
      for (p in c(0:max(df.test[,1]))) {
        kl.list[[p+1]]<-sum(log.matrix[log.matrix[,1] == p,][,(p+2)])
      }
      kl0<-Reduce("+",kl.list)/nrow(log.matrix)
      # rmse
      minus.prob<-(1-y.prob)^2
      minus.matrix<-cbind(df.test[,1],minus.prob)
      rmse.list<-list()
      for (p in c(0:max(df.test[,1]))) {
        rmse.list[[p+1]]<-sum(minus.matrix[minus.matrix[,1] == p,][,(p+2)])
      }
      rmse0<-sqrt(Reduce("+",rmse.list)/nrow(log.matrix))
      
      rocMean<-rocMean+roc0
      klMean<-klMean+kl0
      rmseMean<-rmseMean+rmse0
      # print (roc0)
      
      ################################################ 
      ######## Vibration in Training Data Set ########
      ################################################ 
      
      # store the results of roc areas for each pair of training and validation data sets
      yprob.single<-matrix(0, nrow=nnrepl, ncol=nntrain)
      roc.table<-matrix(0, nrow=nnrepl, ncol=nntrain)
      roc.diff<-matrix(0, nrow=nnrepl, ncol=nntrain)
      
      kl.table<-matrix(0, nrow=nnrepl, ncol=nntrain)
      kl.diff<-matrix(0, nrow=nnrepl, ncol=nntrain)
      
      rmse.table<-matrix(0, nrow=nnrepl, ncol=nntrain)
      rmse.diff<-matrix(0, nrow=nnrepl, ncol=nntrain)
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
          noise<-mvrnorm(rare.size, rep(0,N), k*diag(N), empirical = TRUE) # epsilson
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
            
            y.pred_anti<-kknn(as.factor(trainVib$trainVib.y)~., trainAnti, df.test, k=kValue)
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
          for (p in c(0:max(df.test[,1]))) {
            kl.list[[p+1]]<-sum(log.matrix[log.matrix[,1] == p,][,(p+2)])
          }
          kl.table[j,i]<-Reduce("+",kl.list)/nrow(log.matrix)
          kl.diff[j,i]<-kl.table[j,i]-kl0
          
          #rmse
          minus.prob<-(1-y.prob)^2
          minus.matrix<-cbind(df.test[,1],minus.prob)
          rmse.list<-list()
          for (p in c(0:max(df.test[,1]))) {
            rmse.list[[p+1]]<-sum(minus.matrix[minus.matrix[,1] == p,][,(p+2)])
            # rmse.list[[p+1]]<-sqrt(sum(minus.matrix[minus.matrix[,1] == p,][,(p+2)])/nrow(minus.matrix[minus.matrix[,1] == p,]))
          }
          rmse.table[j,i]<-sqrt(Reduce("+",rmse.list)/nrow(log.matrix))      # final rmse table, same size as rmse.summary
          rmse.diff[j,i]<-rmse.table[j,i]-rmse0
        }
        plot(roc.table[j,])
        abline(h = roc0)
        
        plot(kl.table[j,])
        abline(h = kl0)
        
        plot(rmse.table[j,])
        abline(h = rmse0)
      }
      roc.multi[[t]]<-roc.table
      roc.sum<-roc.sum+roc.table
      rocdiff.multi[[t]]<-roc.diff
      rocdiff.sum<-rocdiff.sum+roc.diff
      
      kl.multi[[t]]<-kl.table
      kl.sum<-kl.sum+kl.table
      kldiff.multi[[t]]<-kl.diff
      kldiff.sum<-kldiff.sum+kl.diff
      
      rmse.multi[[t]]<-rmse.table
      rmse.sum<-rmse.sum+rmse.table
      rmsediff.multi[[t]]<-rmse.diff
      rmsediff.sum<-rmsediff.sum+rmse.diff
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
    cat("\nOriginal rmse Mean =", rmseMean/nsim, "\n")
    
    # rmse final result
    rmse.final<-(rmse.sum/nsim)[noisy.repl,noisy.train]
    print ("rmse results: ")
    print (rmse.final)
    
    # rmse difference
    rmsediff.final<-(rmsediff.sum/nsim)[noisy.repl,noisy.train]
    print ("rmse difference: ")
    print (rmsediff.final)
    
    # plot rmse difference (noisy.repl=2, nosiy.train=10)
    rmse.ci.table<-c()
    for (t in 1:nsim) {
      rmse.ci.table<-append(rmse.ci.table,rmsediff.multi[[t]][nnrepl,nntrain])
    }
    rmse.diff.ci<-append(rmse.diff.ci,(qnorm(0.975)*sd(rmse.ci.table)/sqrt(nsim)))    # rmse difference ci
    rmse.diff.mean<-append(rmse.diff.mean, (rmsediff.sum/nsim)[nnrepl,nntrain])     # rmse difference mean
    
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
  # plot rmse difference among sigma.noise=(0.1,0.5,1.0)
  print("***********************")
  print("rmse diff mean:")
  print(rmse.diff.mean)
  print("rmse diff CI")
  print(rmse.diff.ci)
  
  rmse.plot<-matrix(0, nrow = 3, ncol = 3)
  colnames(rmse.plot)<-c("noise","mean","sd")
  rmse.plot[,1]<-c(0.1,0.5,1.0)
  rmse.plot[1,2:3]<-c(rmse.diff.mean[1],rmse.diff.ci[1])
  rmse.plot[2,2:3]<-c(rmse.diff.mean[2],rmse.diff.ci[2])
  rmse.plot[3,2:3]<-c(rmse.diff.mean[3],rmse.diff.ci[3])
  rmse.plot<-data.frame(noise=c(0.1,0.5,1.0),
                      mean=rmse.plot[,2],
                      sd=rmse.plot[,3])
  p<-ggplot(rmse.plot, aes(x=noise, y=mean), colour=mean) +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.1) +
    geom_line() +
    geom_point() +
    xlab("noise") +
    ylab("rmse.diff") +
    geom_hline(yintercept = 0)
  
  if (models=="knn") {
    rmse.p1<-p+ggtitle("KNN RMSE diff")
  } else if (models=="ld") {
    rmse.p2<-p+ggtitle("LD RMSE diff")
  } else if (models=="log") {
    rmse.p3<-p+ggtitle("LOG RMSE diff")
  } else if (models=="svm") {
    rmse.p4<-p+ggtitle("SVM RMSE diff")
  } else if (models=="dtree") {
    rmse.p5<-p+ggtitle("Dtree RMSE diff")
  } else if (models=="ptree") {
    rmse.p6<-p+ggtitle("Ptree RMSE diff")
  } else if (models=="forest") {
    rmse.p7<-p+ggtitle("Forest RMSE diff")
  } else if (models=="neural") {
    rmse.p8<-p+ggtitle("Neural RMSE diff")
  } else if (models=="nb") {
    rmse.p9<-p+ggtitle("NBayes RMSE diff")
  } else if (models=="C50") {
    rmse.p10<-p+ggtitle("C5.0 RMSE diff")
  } else if (models=="fda") {
    rmse.p11<-p+ggtitle("FDA RMSE diff")
  } else if (models=="pls") {
    rmse.p12<-p+ggtitle("PLSDA RMSE diff")
  } else if (models=="mda") {
    rmse.p13<-p+ggtitle("MDA RMSE diff")
  } else {
    stop("Wrong model type!!!")
  }
}

multiplot(roc.p1, roc.p2, roc.p3, roc.p4, roc.p5, roc.p6, roc.p8, roc.p9, roc.p10, roc.p11, roc.p12, roc.p13, cols=6)
multiplot(kl.p1, kl.p2, kl.p3, kl.p4, kl.p5, kl.p6, kl.p8, kl.p9, kl.p10, kl.p11, kl.p12, kl.p13, cols=6)
multiplot(rmse.p1, rmse.p2, rmse.p3, rmse.p4, rmse.p5, rmse.p6, rmse.p8, rmse.p9, rmse.p10, rmse.p11, rmse.p12, rmse.p13, cols=6)

rownames(output.table)<-rep(c("0.1","0.5","1.0"),12)
colnames(output.table)<-c("ROC0","ROC100","ROCdiff","ROCdiffCI","KL0","KL100","KLdiff","KLdiffCI","RMSE0","RMSE100","RMSEdiff","RMSEdiffCI")
output.table

# running time
proc.time() - ptm
