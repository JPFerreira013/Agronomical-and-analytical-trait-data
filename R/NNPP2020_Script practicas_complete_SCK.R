### Introduction to R: Statistics basics and data visualization
### Adrian Gracia Romero, PhD Student - a.graciaromero@ub.edu
### Master de Agrobiologia Ambiental, Nuevas Perspectivas - 2020

# Before starting: Working directory --------------------------------------

#  The working directory is the folder where the data will be 
#  saved. 
#  We can choose our working directory using the function setwd()
setwd("D:/Documents/R")
#  For checking if the working directory set is the correct use 
#  the function getwd().
getwd()
#  If you want to check what is inside the working directory, 
#  use the function list.files().
list.files()

# Import the data from an excel document -----------------------------------

#  Load excel file with the package readxl and the function 
#  read_excel(). 
#  https://cran.project.org/web/packages/readxl/readxl.pdf
#  "Path" to the xls/xlsx file. "Sheet" to read. 
#  Either a string (the nameof a sheet), or an integer 
#  (the position of the sheet). Ignored if the sheet is 
#  specified via range. If neither argument specifies the 
#  sheet, defaults to the first sheet. col_names TRUE to use 
#  the first row as column names, FALSE to get default names, 
#  or a character vector giving a name for each column. If user
#  provides col_types as a vector, col_names can have one entry
#  per column, i.e. have the same length as col_types, or one 
#  entry per unskipped column.
install.packages("readxl") 
library(readxl) 
mydata <- read_excel("NNPP2020_Data+isotopes.xlsx", sheet = NULL, col_names=T)
#  Use View() to visualize the data
View(mydata)

# Data manipulation -------------------------------------------------------

# The interpretation of a factor depends on both the codes 
# and the "levels" attribute. Be careful only to compare factors
# with the same set of levels (in the same order). In particular,
# as.numeric() applied to a factor is meaningless, and may 
# happen by implicit coercion. To transform a column to 
# categorical values use as.factor().

# The function str() display the internal structure of an 
# R object
str()
# If you need to correct the interpretation of a factor,
# you have to options:
# (1) Create a vector with the name of the columns, and then 
# use apply() and factor to change the interpretation
factors <- c("Plot", "Columna", "Variedad")
mydata[factors] <- lapply(mydata[factors], factor)
# or (2) use as_factor() and rewrite again the column inside the 
# data
#mydata$"name of the column" <- as_factor(mydata$"Plot", "Columna", "Variedad")

# The packages dplyr and tidyr are useful to manipulate easily
# the data. Use filter() to filter the data into groups, 
# select() to select columns, subset() to create a new data 
# using only some columns.
# https://cran.r-project.org/web/packages/dplyr/dplyr.pdf
# https://cran.r-project.org/web/packages/tidyr/tidyr.pdf
install.packages("dplyr") 
library(dplyr) 
install.packages("tidyr") 
library(tidyr) 
#mydata.filter <- filter(mydata, FACTOR == "Level of the factor") 
mydata.subset <- subset(mydata, , c(1:21, 34, 66:72))
View(mydata.subset)
str(mydata.subset)

# Basic statistics --------------------------------------------------------

# For the calculation of the main statistics basics we can use 
# the function summarySE(), and we will need the packages 
# lattice, plyr and Rmisc. Using the argument mesurevar we 
# will indicate the parameter that we want to study and using 
# groupvars() we will indicate tha grouping variable.
# Gives count, mean, standard deviation, standard error 
# of the mean, and confidence interval 
# https://cran.r-project.org/web/packages/Rmisc/Rmisc.pdf 
install.packages("Rmisc") 
install.packages("lattice") 
# if it doesnt work, close and open the programe again
library(lattice) 
library(plyr) 
library(Rmisc)
summarySE(mydata.subset, measurevar="Relative Chlorophyll", 
          groupvars=c("Variedad"),
          na.rm = T)

# Another option is using the function describeBy() 
# inside the package "psych". Report basic summary statistics 
# by a grouping variable 
# https://cran.r-project.org/web/packages/psych/psych.pdf 
install.packages("psych") 
library(psych) 
describeBy(mydata.subset, group = mydata.subset$Variedad)


# ANOVA -------------------------------------------------------------------

# The ANOVA anlysis can be made using the function anova().
anova(lm(SPAD_420 ~ Variedad, mydata.subset))


# Correlations ------------------------------------------------------------

# Para el cálculo de una correlación entre dos parámetros podemos utilizar la función cor.test().
cor.test(mydata$SPAD, mydata$GA)


# Data visualization with "ggplot2" ---------------------------------------

# One of the most useful packages for data visualization 
# is ggplot2. All ggplot2 plots begin with a call to ggplot(),
# supplying default data and aesthethic mappings, specified 
# by aes(). You then add layers, scales, coords and facets 
# with +. To save a plot to disk, use ggsave().
# https://ggplot2.tidyverse.org/reference/


mydata.bp <- read_excel("./NNPP2020_Data+isotopes.xlsx", 
                        sheet = "barplotdata", col_names=T)

mydata.sol <- read_excel("./NNPP2020_Data+isotopes.xlsx", 
                        sheet = "soluble", col_names=T)

library(ggplot2) 

#Barplot
ggplot(mydata.bp, aes(x=Variedad, y=Chl, 
                      fill=Variedad), width=1) +
  geom_col(position=position_dodge()) + theme_classic() +
  geom_errorbar(aes(ymin=Chl-Chl.se, ymax=Chl+Chl.se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("") + 
  xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 40)) +
  scale_fill_brewer(palette = "Pastel1") +
  annotate("text", x=2, y=35, size=4, label= "Level of significance") +
  annotate("text", x=2, y=32, size=4, label= " ") +
  theme(
    legend.position = c(.2, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6)) +
  labs(fill = "Chl x Variedad")
ggsave("NNPP2020_fig_Chl.jpg", width = 22, height = 15, units = "cm")

##soluble
#Barplot
ggplot(mydata.bp, aes(x=Variedad, y=d13C_hojaDM, 
                      fill=Variedad), width=1) +
  geom_col(position=position_dodge()) + theme_classic() +
  geom_errorbar(aes(ymin=d13C_hojaDM-d13C_hojaDM.se, ymax=d13C_hojaDM+d13C_hojaDM.se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  ylab("") + 
  xlab("") +
  scale_y_continuous(expand = c(0, 0), limits = c(-40, 0)) +
  scale_fill_brewer(palette = "Pastel1") +
  annotate("text", x=2, y=35, size=4, label= "Level of significance") +
  annotate("text", x=2, y=32, size=4, label= " ") +
  theme(
    legend.position = c(.2, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "left",
    legend.margin = margin(6, 6, 6, 6)) +
  labs(fill = "d13C_hojaDM x Variedad")

## Correlation
ggplot(data=mydata, 
       aes(x=Chl, y=SPAD,
           color = Variedad,
           group= Variedad)) + geom_point() + 
  scale_x_continuous(expand = c(0, 0), limits = c(20, 30)) +
  scale_y_continuous(expand = c(0, 0), limits = c(30, 50)) +
  theme_bw() + geom_smooth(method = "lm", se = FALSE, lwd=1, 
                           formula = y ~ x, color = "Black") +
  labs(y = "", 
       x = "") + 
  theme(legend.position="none")

## PCA
install.packages("ggfortify") 
library(ggfortify)
df.fact <- subset(mydata, , c(Plot:Variedad))
df.num <- subset(mydata, , -c(Plot:Variedad, angle_direction))

pca <- prcomp(na.omit(df.num), scale. = TRUE)

autoplot(pca, data = mydata, colour = 'Variedad') +
  theme_bw()

autoplot(pca, data = mydata, colour = 'Variedad',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3,
         frame = TRUE, frame.type = 'norm') +
  theme_bw()

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
