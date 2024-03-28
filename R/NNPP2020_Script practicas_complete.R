### Introduction to R: Statistics basics and data visualization
### Adrian Gracia Romero, PhD Student - a.graciaromero@ub.edu
### Master de Agrobiologia Ambiental, Nuevas Perspectivas - 2020

# Before starting: Working directory --------------------------------------

#  The working directory is the folder where the data will be 
#  saved. 
#  We can choose our working directory using the function setwd()
setwd("C:/Users/joaop/OneDrive/Documentos/Mestrado Espanha/Agrobiologia - UB/Nuevas perspectivas/R/tabela pigmentos")
#  For checking if the working directory set is the correct use 
#  the function getwd().
getwd()
#  If you want to check what is inside the working directory, 
#  use the function list.files().
list.files()

# Import the data from an excel document NNPP2020_Data+isotopes.xlsx-----------------------------------

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
mydata <- read_excel("tabela pigmentos.xlsx", sheet = NULL, col_names=T)
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
str(mydata)
# If you need to correct the interpretation of a factor,
# you have to options:
# (1) Create a vector with the name of the columns, and then 
# use apply() and factor to change the interpretation
factors <- c("SPAD","Chl","Flav", "Anth")
mydata[factors] <- lapply(mydata[factors], factor)
# or (2) use as_factor() and rewrite again the column inside the 
# data
mydata$"SPAD" <- as_factor(mydata$"SPAD")

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
mydata.filter <- filter(mydata, FACTOR == "Level of the factor") 
mydata.subset <- subset(mydata, , c("SPAD":"Anth"))
mydata.columns 
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
summarySE(mydata, measurevar="NDVI", 
          groupvars=c("Parcela"),
          na.rm = T)

# Another option is using the function describeBy() 
# inside the package "psych". Report basic summary statistics 
# by a grouping variable 
# https://cran.r-project.org/web/packages/psych/psych.pdf 
install.packages("psych") 
library(psych) 
describeBy(mydata, group = mydata$Treatment)


# ANOVA -------------------------------------------------------------------

# The ANOVA anlysis can be made using the function anova().
anova(lm(MESURE ~ FACTOR, mydata))


# Correlations ------------------------------------------------------------

# Para el cálculo de una correlación entre dos parámetros podemos utilizar la función cor.test().
cor.test(mydata$MEASURE1, mydata.A.I$MEASURE2)


# Data visualization with "ggplot2" ---------------------------------------

# One of the most useful packages for data visualization 
# is ggplot2. All ggplot2 plots begin with a call to ggplot(),
# supplying default data and aesthethic mappings, specified 
# by aes(). You then add layers, scales, coords and facets 
# with +. To save a plot to disk, use ggsave().
# https://ggplot2.tidyverse.org/reference/

