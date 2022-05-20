# author: bbaasan
# Date: May 17, 2022
# version:
# Purpose: applying svm to model

library(tidyverse)
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
#library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots



x <- scale(updated2020[,16:17], center = TRUE, scale = TRUE) 
y <- updated2020$type



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


#####################################################################
# include USA
x1 <- scale(updated_2020_USA[,16:17], center = TRUE, scale = TRUE) 
y1 <- updated_2020_USA$type



dat <- data.frame(x=x1, y1 = as.factor(y1))
dat %>% str()

# sample training data and fit model
train <- base::sample(1:nrow(dat),25, replace = FALSE)
svmfit <- svm(y1 ~ ., data = dat[train,], 
              kernel = "radial", gamma = 1, cost = 1)
# plot classifier
plot(svmfit, dat)


# Fit radial-based SVM in kernlab
kernfit <- ksvm(x1[train,], y1[train], type = "C-svc", kernel = 'rbfdot', 
                C = 1, scaled = c())
# Plot training data
plot(kernfit, data = x1[train,], 
     main = "USA included in the Fabs")

updated_2020_USA %>% 
  select(CountryCode, w_eigen) %>% 
  arrange(desc(w_eigen))

updated_2020_USA %>% 
  select(CountryCode, w_qty) %>% 
  group_by(CountryCode == ) %>% 
  summarise(ttl = sum(w_qty)) %>% 
  arrange(desc(ttl))
