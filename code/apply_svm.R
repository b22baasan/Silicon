# author: bbaasan
# Date: May 17, 2022
# version:
# Purpose: applying svm to model

library(tidyverse)
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
#library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots



x <- scale(updated_main[,2:17], center = TRUE, scale = TRUE) 
y <- updated_main$type



dat1 <- data.frame(x=x, y = as.factor(y))
dat1 %>% str()



updated_main$fab <- updated_main$type == 'fab'
updated_main$foundry <- updated_main$type == 'foundry'

updated_main.train.idx <- sample(x = nrow(updated_main), 
                                 size = nrow(updated_main)*0.5)
updated_main_train <- updated_main[updated_main.train.idx, ]
updated_main_valid <- updated_main[-updated_main.train.idx, ]

names(updated_main_train)

library(neuralnet)
trade.net <- neuralnet(fab+foundry ~ 
                      w_qty + w_val + w_dgre + w_betw + w_eigen, 
                       data=updated_main_train, hidden=c(10,10), rep = 5, 
                       err.fct = "ce", linear.output = F, lifesign = "minimal", 
                       stepmax = 1000000, threshold = 0.001)

plot(trade.net, rep="best")

trade.prediction <- compute(trade.net, updated_main_valid[, c(-1, -18:-20)])
idx <- apply(trade.prediction$net.result, 1, which.max)
predicted <- c('fabs', 'foundry')[idx]
table(predicted, updated_main_valid$type)

help(neuralnet)

trade.net$err.fct
trade.net$result.matrix


wafers = updated_main[, 16:18]
x2 <- scale(wafers[, 1:2], center = TRUE, scale = TRUE)
y2 <- wafers$type

dot <- data.frame(x=x2, y=as.factor(y2))

plot(dot[, 1:2], col = dot$y)

dot %>% 
  ggplot(aes(x=x.w_betw, y = x.w_eigen, color = y))+
  geom_point(size=2)+
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")+
  ggtitle('Wafers Point Estimation: Betweenness vs. Eigenvalue')

svmfit2 <- svm(y ~ ., data= dot, kernel = 'linear', cost = 10)
plot(svmfit2, dot)


kernfit <- ksvm(x2, y2, type = "C-svc", kernel = 'vanilladot')
plot(kernfit, data = x2)

train <- base::sample(1:nrow(dot), 25, replace = FALSE)
svmfit3 <- svm(y~., data = dot[train,], kernel = "radial", gamma = 1, cost = 1)
# plot classifier
plot(svmfit3, dot)

# sample(1:nrow(dot),25)

# sample training data and fit model
train <- base::sample(1:nrow(dot),25, replace = FALSE)
svmfit4 <- svm( y2 ~ ., data = dot[train,], kernel = "radial", gamma = 1, cost = 1)
# plot classifier
plot(svmfit4, dot)


# Fit radial-based SVM in kernlab
kernfit <- ksvm(x2[train,],y[train], type = "C-svc", kernel = 'rbfdot', C = 1, scaled = c())
# Plot training data
plot(kernfit, data = x2[train,])


