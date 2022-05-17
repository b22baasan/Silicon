# author: bbaasan
# Date: May 17, 2022
# version:
# Purpose: applying svm to model

library(tidyverse)
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
#library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots



x <- scale(updated_2020[,16:17], center = TRUE, scale = TRUE) 
y <- updated_2020$type



dat <- data.frame(x=x, y = as.factor(y))
dat %>% str()

# sample training data and fit model
train <- base::sample(1:nrow(dat),25, replace = FALSE)
svmfit <- svm(y ~ ., data = dat[train,], 
                kernel = "radial", gamma = 1, cost = 1)
# plot classifier
plot(svmfit, dat)


# Fit radial-based SVM in kernlab
kernfit <- ksvm(x[train,], y[train], type = "C-svc", kernel = 'rbfdot', 
                C = 1, scaled = c())
# Plot training data
plot(kernfit, data = x[train,])



