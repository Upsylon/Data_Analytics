```{r, cache=TRUE, results='hide'}

acc.nnet.cv <- matrix(nrow=30,ncol=10)
for (j in 1:10){
  index.train1 <- sample(1:nrow(cred), size=nrow(cred)*0.7, replace=FALSE)
  cred.train1 <- cred[index.train1,]
  cred.test1 <- cred[-index.train1,]
  nnet.model = list()
  for (i in 11:30) {
    nnet.model[[i]] = nnet(cred.train$response~.,
                           data = cred.train, size=i, maxit=200, decay = 1)
  }
  pred.nnet=list()
  for (i in 11:30){
    pred.nnet[i] <- predict(nnet.model[i], cred.test, type="class")
    
  }# making the predictions
  tab.nnet=list()
  for (i in 11:30){
    tab.nnet[[i]] <- table(Reality=cred.test$response,
                           Predicted=unlist(pred.nnet[i]))
  }
  acc.nnet = list()
  for (i in 11:30) {
    acc.nnet[i] <-
      sum(ifelse(cred.test$response == unlist(pred.nnet[i]), 1, 0), na.rm = TRUE) /
      length(cred.test$response)
    
    acc.nnet.cv[i,j] <- acc.nnet[[i]]}
}

```

On utilise à chaque itération des train et test sets différents.

```{r}
mean.acc.nnet.cv <- numeric(30)
sd.acc.nnet.cv <- numeric(30)
for(i in 11:30){mean.acc.nnet.cv[i] <- mean(acc.nnet.cv[i,])
sd.acc.nnet.cv[i] <- sd(acc.nnet.cv[i,])}
mean.acc.nnet.cv[-c(1:10)] # average mean for the models with i = 11,..,30 nodes
accu <- data.frame(Average=mean.acc.nnet.cv[-c(1:10)], Neurones = c(11:30))
```

The

```{r, comment=NA}
# number of neurones leading to the model with the highest accuracy
which.max(mean.acc.nnet.cv)
mean.acc.nnet.cv[which.max(mean.acc.nnet.cv)] # the accuracy
```

The maximum accuracy  is equal to `r mean.acc.nnet.cv[which.max(mean.acc.nnet.cv)]`.
On the following plot, we can see at a glance the accuracies and the model that we are going to select.

```{r, echo=FALSE}
accu %>% ggplot() +
  geom_point(aes(x = Neurones, y = Average)) +
  geom_hline(yintercept = mean.acc.nnet.cv[which.max(mean.acc.nnet.cv)], col =
               'red') +
  scale_x_continuous(breaks = c(11:30)) + scale_y_continuous(breaks = round(c(
    seq(min(mean.acc.nnet.cv[mean.acc.nnet.cv != 0]), max(mean.acc.nnet.cv),
        by = (max(mean.acc.nnet.cv) - min(mean.acc.nnet.cv[mean.acc.nnet.cv != 0]))/4
    )), 3)) + ylab("Average accuracy") +
  ggtitle("Average accuracy according to the number of neurones in our network") +
  my_theme()
```

We can directly see which that the model with `r which.max(mean.acc.nnet.cv)` neurones presents the highest accuracy.

Obviously, this choice is subject to discussion and other methods could be preferred. At the end, the differences resulting from the use of our different models are really low, meaning that they have all quite similar predictive capabilities.

We can therefore continue to explore further the model with `r which.max(mean.acc.nnet.cv)` neurones that we retained.

#### TESTING DIFFERENT DECAYS

Note, on a considéré plus de decay, mais pour plus de "computational efficicency", on va en display seulement certains...

```{r, cache = TRUE, results='hide'}
acc.nnet.cv2 <- matrix(nrow=length(seq(2^-8,2,2^-3)), ncol=10) # we considered initially extreme small values like seq(0,2^-5,2^-8), seq(0,2^-4,2^-10) intermediate and high and the range that we propose here best fits the data.
j <- 1
for (j in 1:10){
  index.train2 <- sample(1:nrow(cred), size=nrow(cred)*0.7, replace=FALSE)
  cred.train2 <- cred[index.train2,]
  cred.test2 <- cred[-index.train2,]
  nnet.model2 = list()
  pred.nnet2=list()
  tab.nnet2=list()
  acc.nnet2 = list()
  k <- 1
  for (i in seq(2^-8,2,2^-3)) {
    nnet.model2[[k]] = nnet(cred.train$response~.,
                            data = cred.train, size=which.max(mean.acc.nnet.cv),
                            maxit=200, decay = i)
    pred.nnet2[[k]] <- predict(nnet.model2[k], cred.test, type="class") #  making the predictions
    tab.nnet2[[k]] <- table(Reality=cred.test$response,
                            Predicted=unlist(pred.nnet2[k]))
    acc.nnet2[[k]] <- sum(ifelse(cred.test$response
                                 == unlist(pred.nnet2[k]), 1, 0), na.rm = TRUE) /
      length(cred.test$response)
    acc.nnet.cv2[k,j] <- acc.nnet2[[k]]
    k <- k+1
  }
  j <- j+1
}
```


```{r}
mean.acc.nnet.cv2 <- numeric(16)
sd.acc.nnet.cv2 <- numeric(16)
for(i in 1:16){mean.acc.nnet.cv2[i] <- mean(acc.nnet.cv2[i,])
sd.acc.nnet.cv2[i] <- sd(acc.nnet.cv2[i,])}
mean.acc.nnet.cv2 # average mean for the models with i = 11,..,30 nodes
accu2 <- data.frame(Average=mean.acc.nnet.cv2, Decay = seq(2^-8,2,2^-3))
```


```{r, comment=NA}
which.max(mean.acc.nnet.cv2) # neurones leading to the model with the highest accuracy
mean.acc.nnet.cv2[which.max(mean.acc.nnet.cv2)] # the accuracy
round(seq(2^-8,2,2^-3)[which.max(mean.acc.nnet.cv2)],1) # the retained decay
```

```{r}
# scaling except last row (except the response variable)
scaled.cred <- scale(cred[,-length(cred)]) %>% 
  data.frame(response=cred$response)

scaled.cred.train <- scale(cred.train[,-length(cred.train)]) %>% data.frame(response=cred.train$response)
scaled.cred.test <- scale(cred.test[,-length(cred.test)]) %>% 
  data.frame(response=cred.test$response)
```

```{r}
# Creation of 20 KNN: 10 with i={1,..,10} and k = 2 and 10 with i={1,..,10} and k = 3 
knn.model = list()
k <- 0
for (j in 2:3) {
  for (i in 1:20) {
    knn.model[[20 * k + i]] <- kknn(response ~ ., 
                                    k = i, 
                                    distance = j, 
                                    train = scaled.cred.train, 
                                    test=scaled.cred.test)
  }
  k <- k+1
}

# making the predictions on the testing set
pred.knn=list()
for (i in 1:length(knn.model)){
  pred.knn[[i]] = table(Prediction=knn.model[[i]]$fitted.value, 
                        Actual=cred.test$response)
}

# calculating the accuracy
acc.knn = list()
for (i in 1:length(knn.model)) {
  acc.knn[i] <- sum(diag(pred.knn[[i]]))/250
}
```


