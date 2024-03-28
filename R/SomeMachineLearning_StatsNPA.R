setwd("D:\Documents\R") 
getwd() 
list.files()

install.packages("readxl") 
library(readxl) 
mydata <- read_excel("NNPP2020_Data.xlsx", sheet = NULL, col_names=T) 
View(mydata)

# Pricipal Components Analysis for variable selection and Stepwise of top variables
## packages needed to calculate linear models stepwise models and the coefficients of partial determination
#install.packages("MASS")
#install.packages("relaimpo")
library(MASS)
library(relaimpo)

## Basic Scatterplot Matrix
pairs(~SPAD+GA+GGA+CSI+Hue+Saturation+Lightness+a+b+NGRDI+TGI, data=mydata, main="Scatterplot Matrix of  My Data")
##try creating a more specifc model if it comes out with too many variables
### Better scatterplot matrix
install.packages("car")

library(car)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(~SPAD+GA+GGA+CSI+Hue+Saturation+Lightness++a+b+NGRDI+TGI, data=mydata, lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Scatterplot Matrix of  My Data")
#
#AND ALSO
#
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.001) txt2 <- paste("p= ", "<0.001", sep = "")
  text(0.5, 0.4, txt2)
}
#
twolines = function(x,y) {
  points(x,y,pch=".")
  abline(line(x,y),col="red")
  abline(lsfit(x,y),col="blue")
}
#
pairs(~SPAD+GA+GGA+CSI+Hue+Saturation+Lightness++a+b+NGRDI+TGI, data=mydata, lower.panel=twolines, upper.panel=panel.cor, 
      main="Scatterplot Matrix of  My Data")
pairs(~SPAD+GA+GGA+CSI+Hue, data=mydata, lower.panel=twolines, upper.panel=panel.cor, 
      main="Scatterplot Matrix of  My Data")

#PCA
# entering raw data and extracting PCs 
# from the correlation matrix 
#Subset to only dependent variables for PCA testing...
mypcadata <- mydata[c(4,10:21)]
names(mypcadata)
### run a basic correlation matrix PCA on the data
fit1 <- princomp(mypcadata, cor=FALSE)
### print variance accounted for
summary(fit1)  
### pc loadings
loadings(fit1)  
### scree plot
plot(fit1,type="lines") 
### the principal components
#fit1$scores 
## the classic PCA graphic
biplot(fit1)

###PCA REGRESSION - PCR
require(pls)
set.seed (1000)
pcr_model <- pcr(SPAD~GA+GGA+CSI+Hue, data = mypcadata, scale = TRUE, validation = "CV")
summary(pcr_model)
# Plot the root mean squared error
validationplot(pcr_model)
# Plot the cross validation MSE
validationplot(pcr_model, val.type="MSEP")
# Plot the R2
validationplot(pcr_model, val.type = "R2")
predplot(pcr_model)

#STEPWISE LINEAR REGRESSION MODELS
#start with a smaller model to begin with, then stepwise it
##Stepwise and Coefficients of Partial Determination for conteo and breedpix data
#create whole model
fitmydata <- lm(SPAD~GA+GGA+CSI+Hue ,data=mydata)
summary(fitmydata)

library(MASS)

#stepwise the whole model based on the AIC criteria
stepwisemydata <- stepAIC(fitmydata, direction="both")
summary(stepwisemydata)
predplot(stepwisemydata)
anova(stepwisemydata)

#After all this, pick and make what you think is the best model
fitmydatabest <- lm(SPAD~GA+GGA+CSI+Hue ,data=mydata)
summary(fitmydatabest)
predplot(fitmydatabest)

#R calculate the coefficients of partial determination
library(relaimpo)
calc.relimp(fitmydatabest)

# Calculate Relative Importance for Each Predictor
calc.relimp(fitmydatabest,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(fitmydatabest, b = 100, type = c("lmg","last", "first", "pratt"), rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

#############RandomForest classification of the different MLN scores from the RGB results
## Install MASS, ROCR and randomForest.
#install.packages("MASS")
#install.packages("ROCR")
#install.packages("randomForest")
## (included in software packages or downloadable via CRAN mirror within R)
## You can load each of the packages now or at the start of each section:
library(ROCR)
library(MASS)
library(randomForest)

#Subset to only dependent variables for RF
mydataRF <- mydata[c(4,10:21)]
names(mydataRF)

## With the raw data now prepared we can proceed.
## Split the input data into two random subset for testing and training
## 60% of the sample size
smp_size <- floor(0.60 * nrow(mydataRF))

##Set the seed to make your partition reproductible and define the split list
set.seed(123)
train_ind <- sample(seq_len(nrow(mydataRF)), size = smp_size)
mydataRF.train <- mydataRF[train_ind, ]
mydataRF.test <- mydataRF[-train_ind, ]

##Check the splits!
str(mydataRF.train)
str(mydataRF.test)
str(mydataRF)

## No NA values !!! (randomForest really hates those)
## Check the header names
names(mydataRF.train)
names(mydataRF.test)

##Proceed with the randomForest model at different sampling quanitites.
RF1 = randomForest(as.factor(SPAD) ~ ., data=mydataRF.train, ntree=1000, 
                           importance=TRUE)
RF2 = randomForest(as.factor(SPAD) ~ ., data=mydataPlotRF.train, ntree=5000, 
                           importance=TRUE)
RF3 = randomForest(as.factor(SPAD) ~ ., data=mydataPlotRF.train, ntree=10000, 
                           importance=TRUE)
print(RF1)
print(RF2)
print(RF3)

plot(RF1, log="y")
varImpPlot(RF1)

set.seed(1)
RF1 <- randomForest(SPAD ~ ., data=mydataRF, proximity=TRUE,
                           keep.forest=FALSE)
MDSplot(RF1, mydataRF$SPAD)

getTree(randomForest(mydataRF[,-5], mydataRF[,5], ntree=10), 3, labelVar=TRUE)

#Rpart regression trees
##install.packages("rpart")
# Regression Tree Example
library(rpart)
# grow tree with all possible variables
##PlotMLN.fit <- rpart(PlotMLN~., 
##             method="anova", data=mydataPlotRF)
##Only with select variables
PlotMLN.fit <- rpart(PlotMLN~greenveg+chlorosis+necrosis+NGRDI+TGI, 
                     method="anova", data=mydataPlotRF)

printcp(PlotMLN.fit) # display the results 
plotcp(PlotMLN.fit) # visualize cross-validation results 
summary(PlotMLN.fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(PlotMLN.fit) # visualize cross-validation results  	

# plot tree 
plot(PlotMLN.fit, uniform=TRUE, branch = 0.3, compress = TRUE,
     main="Regression Tree for PlotMLN ")
text(PlotMLN.fit, use.n=TRUE, all=TRUE, cex=.8)

printcp(PlotMLN.fit) # display the results 
plotcp(PlotMLN.fit) # visualize cross-validation results 
summary(PlotMLN.fit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(PlotMLN.pfit) # visualize cross-validation results 

rss <- sum((mydataPlotRF.test$PlotMLN - predict(PlotMLN.fit, newdata = mydataPlotRF.test))^2) 
tss <- sum((mydataPlotRF.test$PlotMLN-mean(mydataPlotRF.test$PlotMLN))^2) 
r2.fit <- 1-rss/tss
print(r2.fit)

# create attractive postcript plot of tree 
#post(PlotMLN.fit, file = "D:/PlotMLN.tree2.ps", 
#     title = "Regression Tree for PlotMLN")

PlotMLN.fit$cptable[which.min(PlotMLN.fit$cptable[,"xerror"]),"CP"]

# prune the tree 
PlotMLN.pfit<- prune(PlotMLN.fit, cp=PlotMLN.fit$cptable[which.min(PlotMLN.fit$cptable[,"xerror"]),"CP"]) # from cptable   

##PlotMLN.pfit<- prune(PlotMLN.fit, cp=0.012368)

# plot the pruned tree 
plot(PlotMLN.pfit, uniform=TRUE, branch = 0.3, compress = TRUE,
     main="Pruned Regression Tree for PlotMLN")
text(PlotMLN.pfit, use.n=TRUE, all=TRUE, cex=.8)

printcp(PlotMLN.pfit) # display the results 
plotcp(PlotMLN.pfit) # visualize cross-validation results 
summary(PlotMLN.pfit) # detailed summary of splits

# create additional plots 
par(mfrow=c(1,2)) # two plots on one page 
rsq.rpart(PlotMLN.pfit) # visualize cross-validation results  

rss <- sum((mydataPlotRF.test$PlotMLN - predict(PlotMLN.pfit, newdata = mydataPlotRF.test))^2) 
tss <- sum((mydataPlotRF.test$PlotMLN-mean(mydataPlotRF.test$PlotMLN))^2) 
r2.pfit <- 1-rss/tss
print(r2.pfit)

##save this graph elsewhere
#post(PlotMLN.pfit, file = "D:/PlotMLN.ptree2.ps", 
#     title = "Pruned Regression Tree for PlotMLN")

###Conditional inference trees
##install.packages("party")
# Conditional Inference Tree for Kyphosis
library(party)
PlotMLN.cfit <- ctree(PlotMLN~., 
                      data=mydataPlotRF)
plot(PlotMLN.cfit, main="Conditional Inference Tree for PlotMLN")

##PlotMLN.cfit <- ctree(PlotMLN~greenveg+chlorosis+necrosis+Hue+NGRDI+TGI, 
##                      data=mydataPlotRF)
plot(PlotMLN.cfit, main="Conditional Inference Tree for PlotMLN")
summary(PlotMLN.cfit)
PlotMLN.cfit

nodes(PlotMLN.cfit, 4)

### distribution of responses in the terminal nodes
plot(mydataPlotRF$PlotMLN ~ as.factor(where(PlotMLN.cfit)))
### get all terminal nodes from the tree
nodes(PlotMLN.cfit, unique(where(PlotMLN.cfit)))
### extract weights and compute predictions
pmean <- sapply(weights(PlotMLN.cfit), function(w) weighted.mean(mydataPlotRF$PlotMLN, w))
#pmean
plot(mydataPlotRF$PlotMLN, pmean, col = "red")

table(predict(PlotMLN.cfit), mydataPlotRF$PlotMLN)

### estimated class probabilities, a list
tr <- treeresponse(PlotMLN.cfit, newdata = mydataPlotRF.test)

rss <- sum((mydataPlotRF.test$PlotMLN - predict(PlotMLN.cfit, newdata = mydataPlotRF.test))^2) 
tss <- sum((mydataPlotRF.test$PlotMLN-mean(mydataPlotRF.test$PlotMLN))^2) 
r2.cfit <- 1-rss/tss
print(r2.cfit)

# simpler version of plot
plot(PlotMLN.cfit, type="simple",           # no terminal plots
     inner_panel=node_inner(PlotMLN.cfit,
                            abbreviate = TRUE,            # short variable names
                            pval = FALSE,                 # no p-values
                            id = FALSE),                  # no id of node
     terminal_panel=node_terminal(PlotMLN.cfit, 
                                  abbreviate = TRUE,
                                  digits = 3,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = FALSE)
)

### ordinal regression
PlotMLNct <- ctree(PlotMLN~greenveg+chlorosis+necrosis+NGRDI+TGI, 
                   data=mydataPlotRF) 
plot(PlotMLNct, main="Ordinal Regression Decision Tree for PlotMLN")

### estimated class probabilities
treeresponse(PlotMLNct, newdata = mydataPlotRF.test)

#Table of prediction errors
table(predict(PlotMLN.cfit), mydataPlotRF$PlotMLN)

# Estimated class probabilities
tr.pred = predict(PlotMLN.cfit, newdata=mydataPlotRF.test, type="prob")
rss <- sum((mydataPlotRF.test$PlotMLN - predict(PlotMLN.cfit, newdata = mydataPlotRF.test))^2) 
tss <- sum((mydataPlotRF.test$PlotMLN-mean(mydataPlotRF.test$PlotMLN))^2) 
r2.cfit <- 1-rss/tss
print(r2.cfit)
