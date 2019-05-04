
CVgeneric <- function(classifier, features, labels, K, loss){
  labels[labels == -1] <- 0
  train = split_cond(cbind(labels,features))[[1]]
  val = split_cond(cbind(labels,features))[[3]]
  t.train = rbind(train, val)
  id <- sample(1:len(t.train), size = len(t.train)*(K-1)/K)
  cv_train = t.train[id]
  cv_val = t.train[-id]
  
  model <- train(labels ~ NDAI + CORR + AF, data = cv_train, method = classifier)
  y_hat <- predict(model, cv_val[,-1])
  y.pred = rep(0, length(log.pred))
  y.pred[y_hat > 0.5] = 1
  return(mean(y.pred != labels))
}


train_block$label[train_block$label==-1] <- 0 #set cloud free label to 0
test_block$label[test_block$label==-1] <- 0
val_block$label[val_block$label==-1] <- 0

t.train = rbind(train_block, val_block)

modelList = list()
folds <- createFolds(t.train$label, k = 10)
foldsAccuracy<-list()
for (j in 1:10){
  
  train_set <- t.train[-unlist(folds[j]),]
  val_set <- t.train[unlist(folds[j]),]
  y_train_set <- train_set$label[-unlist(folds[j])]
  # y_val_set <- val_set$label[unlist(folds[j])]
  # currFormula <- as.formula(paste("log(SalePrice+1)", "~", paste(Xnames[1:i], collapse = "+"), sep = ""))
  # model <-  qda(label ~ AF + CORR + NDAI, data = train_set)
  # pred <- predict(model, val_set)
  # foldsAccuracy[j] = mean(pred$class == y_val_set)
  
  qda.fit.1 <- lda(label ~ AF + CORR + NDAI, data = t.train)
  qda.pred.1 <- predict(qda.fit.1, val_set)
  foldsAccuracy[j] = mean(qda.pred.1$class == val_set$label)
}
unlist(foldsAccuracy)

modelList = list()
folds <- createFolds(t.train$label, k = 10)
foldsAccuracy<-list()
for (j in 1:10){
  
  train_set <- t.train[-unlist(folds[j]),]
  val_set <- t.train[unlist(folds[j]),]
  y_train_set <- train_set$label[-unlist(folds[j])]
  # y_val_set <- val_set$label[unlist(folds[j])]
  # currFormula <- as.formula(paste("log(SalePrice+1)", "~", paste(Xnames[1:i], collapse = "+"), sep = ""))
  # model <-  qda(label ~ AF + CORR + NDAI, data = train_set)
  # pred <- predict(model, val_set)
  # foldsAccuracy[j] = mean(pred$class == y_val_set)
  
  qda.fit.1 <- qda(label ~ AF + CORR + NDAI, data = t.train)
  qda.pred.1 <- predict(qda.fit.1, val_set)
  foldsAccuracy[j] = mean(qda.pred.1$class == val_set$label)
}
unlist(foldsAccuracy)


modelList = list()
folds <- createFolds(t.train$label, k = 10)
foldsAccuracy<-list()
for (j in 1:10){
  
  train_set <- t.train[-unlist(folds[j]),]
  val_set <- t.train[unlist(folds[j]),]
  y_train_set <- train_set$label[-unlist(folds[j])]
  y_val_set <- val_set$label[unlist(folds[j])]
  
  log.Mod = glm(label ~ AF + CORR + NDAI, data = train_set, family = binomial(link = "logit"))
  log.pred <- predict(log.Mod, val_set, type="response")
  
  optCutOff <- optimalCutoff(val_set$label, log.pred)[1]
  
  #check accuracy
  glm.pred = rep(0, length(log.pred))
  glm.pred[log.pred > optCutOff] = 1
  foldsAccuracy[j] = mean(glm.pred == val_set$label)
}
unlist(foldsAccuracy)






modelList = list()
folds <- createFolds(t.train$label, k = 10)
foldsAccuracy<-list()
for (j in 1:10){
  
  train_set <- t.train[-unlist(folds[j]),]
  val_set <- t.train[unlist(folds[j]),]
  y_train_set <- train_set$label[-unlist(folds[j])]
  y_val_set <- val_set$label[unlist(folds[j])]
  
  knn.pred.1 <- knn(train_set, val_set, cl = train_set$label, k = 100, prob= TRUE)
  foldsAccuracy[j] = mean(knn.pred.1 == val_set$label)
  
}

unlist(foldsAccuracy)

