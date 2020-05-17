library(mdatools)
library(caret)
library(pls)
library(DiscriMiner)
library(muma)
library(ropls)
library(mix)
library(mixOmics)
library(ggplot2)
library(rgl)
data <- read.csv("~/GitHub/Rice_authenticity_ICP_new/grand.csv")
# trainx <- grand[, 2:31]
# trainy <- grand$lv
# set.seed(123)
# cv_10_folds <- createMultiFolds(trainy, k = 10, times = 10)
# table(trainy)
data$lv <- as.factor(data$lv)
df <- data
df$lv <- gsub("GG", "Dongjinxi", df$lv)
df$lv <- gsub("JS", "Jinshanqiao", df$lv)
df$lv <- gsub("PJ-1", "Panjin-Yanfeng", df$lv)
df$lv <- gsub("PJ-2", "Panjin-Liaoxing", df$lv)
df$lv <- gsub("SY", "Sheyang", df$lv)
df$lv <- gsub("WC", "Wuchang", df$lv)
data <- df
train_ind <- createDataPartition(data$lv, p=0.8, list = FALSE)

train <- data[train_ind, ]
test <- data[-train_ind, ]
#Random generate 15 mtry values with tuneLength = 30

plsda_result<- plsda(train[2:31], train$lv, ncomp = 10, mode = "classic")

perf.plsda <- perf(plsda_result, validation = "Mfold", folds = 5, 
                   progressBar = FALSE, auc = TRUE, nrepeat = 10) 
# perf.plsda.srbct$error.rate  # error rates
plot(perf.plsda, col = color.mixo(1:3), sd = TRUE, legend.position = "horizontal") 


# plsda_result_eig <- {plsda_result$explained_variance$X}[1:2]
# sample_site <- data.frame(plsda_result$variates)[1:2]
# sample_site$names <- rownames(sample_site)
# names(sample_site)[1:2] <- c('plsda1', 'plsda2')
# sample_site$names<- train$lv
plotIndiv(plsda_result, ind.names = FALSE, style = 'ggplot2',ellipse = TRUE, title = "PLS-DA plot", legend = TRUE,size.legend = 18,
          size.xlabel = 15, size.ylabel = 15,
          X.label = "Component 1 (28%)", Y.label = "Component 2 (22%)", point.lwd = 1.5, legend.title = NULL)
plotIndiv(plsda_result, ind.names = FALSE, style = '3d',ellipse = TRUE, title = "PLS-DA plot", legend = TRUE)

set.seed(5) # for reproducibility here
# Creation of a randomised set of sample
train_ind <- createDataPartition(data$lv, p=0.8, list = FALSE)

train <- data[train_ind, ]
test <- data[-train_ind, ]
# 
# # 1/3 of the data will compose the test set
# test <- which(samp == 1) 
# # rest will compose the training set
# train <- setdiff(1:nrow(X), test) 
plsda <- opls(x = train[2:31], y = train$lv, predI = 7)

## For PLS-DA, train the model
plsda.train <- plsda(train[2:31], train$lv, ncomp = 10)
# then predict
plotIndiv(plsda.train, ind.names = FALSE, style = 'ggplot2',ellipse = TRUE, title = "PLS-DA plot", legend = TRUE,
          X.label = "Component 1 (28%)", Y.label = "Component 2 (21%)",point.lwd = 1.5, legend.title = NULL)
test.predict <- predict(plsda.train, test[2:31], dist = "max.dist")
# store prediction for the 4th component
prediction <- test.predict$class$max.dist[,4] 
# calculate the error rate of the model
confusion.mat = get.confusion_matrix(truth = test$lv, predicted = prediction)

get.BER(confusion.mat)
cf <- as.data.frame(confusion.mat)
write.csv()
# plsda_plot <- ggplot(sample_site, aes(plsda1, plsda2)) +
#   geom_point(size = 1.5, alpha = 0.6) + 
#   stat_ellipse(show.legend = FALSE) +    #?????? 95% ????????????
#   scale_color_manual(values = c('#1D7ACC', '#F67433', '#00815F')) +
#   theme(panel.grid = element_line(color = 'grey50'), panel.background = element_rect(color = 'black', fill = 'transparent')) + 
#   theme(legend.title = element_blank(), legend.key = element_rect(fill = 'transparent')) +
#   labs(x = paste('PLS-DA axis1 ( explained variance ', round(100 * plsda_result_eig[1], 2), '% )', sep = ''), y = paste('PLS-DA axis2 ( explained variance ', round(100 * plsda_result_eig[2], 2), '% )', sep = ''))
# # plsda <- opls(train[2:31], train$lv)
# # par(mfrow = c(1,1))
# # plot(plsda, which = 4)
# plsda_plot


 set.seed(1) # for reproducibility here
 # Creation of a randomised set of sample
 X <- data[2:31]
 Y<- data$lv
 samp <- sample(1:4, nrow(X), replace = TRUE) 
 # 1/3 of the data will compose the test set
 test <- which(samp == 1) 
 # rest will compose the training set
 train <- setdiff(1:nrow(X), test) 
 ## For PLS-DA, train the model
 plsda.train <- plsda(X[train, ], Y[train], ncomp = 7)
 plsda <- opls(X[train, ],Y[train], predI = 7)
 # then predict
 plotIndiv(plsda.train, ind.names = FALSE, style = 'ggplot2',ellipse = TRUE, title = "PLS-DA plot", legend = TRUE,
           X.label = "Component 1 (29%)", Y.label = "Component 2 (22%)", point.lwd = 1.5, legend.title = NULL)
 test.predict <- predict(plsda.train, X[test, ], dist = "max.dist")
 # store prediction for the 4th component
 prediction <- test.predict$class$max.dist[,4] 
 # calculate the error rate of the model
 confusion.mat = get.confusion_matrix(truth = Y[test], predicted = prediction)
 get.BER(confusion.mat)
 confusion.mat <- as.data.frame(confusion.mat)
write.csv(confusion.mat,"~/GitHub/Rice_authenticity_ICP_new/confusion_matrix.csv") 
