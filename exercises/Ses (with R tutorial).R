###########################
#### EXERCISE SESSION 1 ###
###########################

# TUTORIALS ON R:

# ALWAYS USE AN R SCRIPT!

# RUN
# a <- 1, press 'ctrl' and 'enter' or use the button RUN.
a <- 1

# SYNTAX:
# +-*/^ ==><>=<= ()[]{} ""'' TRUE FALSE
# <-: FOR ASSIGNMENTS, PRESS 'ALT' AND '-'
# ?: ?ANYTHINGYOUDON'TKNOW, e.g. ?read.table
# $: FOR SELECTING COLUMNS, ATTRIBUTES
#    e.g.: A$age gives the 'age' column of 'A'.
#          model$coefficients gives the 'coefficients' attributes of 'model'.

# VECTORS:
# a <- c(), a <- c(1,2,3)
# VECTOR OF A GIVEN LENTGTH: a <- rep(0, 100) 
# MERGE VECTORS: C <- c(A,B) (FOR MATRIX, cbind or rbind)

# MATRIX AND DATAFRAME:
# USE matrix(), data.frame() TO CREATE
# SELECT ENTRY: A[row, column]
# SELECT COLUMN: A[,column], INDEX = a number or vector (or a condition)
# SELECT ROW: A[row,]

# MAGIC METHODS:
# SUPPOSE a IS A VECTOR AND WE WANT TO CHOOSE THE ENTRIES FROM a
# S.T. THEY ARE LESS THAN 0:
# b <- a[a<0]
# OR, b <- a[which(condition)], ?which
# SAME APPLIED TO MATRIX AND DATAFRAME

# USEFUL FUNCTIONS LEARNED ALONG THE COURSE:
# ALWAYS REMEMBER (WHERE TO FIND) FREQUENTLY USED FUNCTIONS!



# EXERCISE 0

# importing BLOOD.DAT dataset 'path'
blood.df <- read.table(file=file.choose(), header=TRUE, sep=",")
#blood.df <- read.table(file=file.choose(), header=FALSE, sep=",")
#blood.df <- read.table(file=file.choose(), header=TRUE, sep="\t") # \t means 'TAB'

# EXERCISE 1

# a
beta.df <- read.table(file=file.choose(), header=TRUE, sep="\t")
beta.df <- read.table(file=file.choose(), header=TRUE)

names(beta.df)

# EXERCISE 2
# To view a DAT file you could use text

# a
chick.df <- read.table(file=file.choose(), sep="\t")

# b
names(chick.df)

# c
summary(chick.df$weight)
var(chick.df$weight)
sd(chick.df$weight)

# d
View(chick.df)
summary(chick.df$chicken)
# This is nonsense, as these numbers are simply labels

# e
names(chick.df)[1] <- "No."
names(chick.df)
# Use as.factor
chick.df$No. <- as.factor(chick.df$No.)
chick.df$No.

# f
?by
by(chick.df$weight, chick.df$feed, summary)
by(chick.df$weight, chick.df$feed, var)
by(chick.df$weight, chick.df$feed, sd)

# g
table(chick.df$feed)

# EXERCISE 3

# a
monica.df <- read.table(file=file.choose(), header=TRUE, sep=";")

# b
by(monica.df$age, monica.df$sex, summary)
by(monica.df$age, monica.df$sex, var)
by(monica.df$age, monica.df$sex, sd)

# c
par(mfrow = c(1,1 ))
boxplot(monica.df$age)
boxplot(monica.df$age, ylab = 'age')
# d
par(mfrow = c(1,2))
boxplot(age~sex, data = monica.df) # better
by(monica.df$age, monica.df$sex, boxplot) 

# f
# HINTS:

# 1: apply density function to monica.df$age with condition 
#    monica.df$sex=='f' and 'm'
# 2: plot

# OR: page 7 of chapter 2 + conditions

# 1: USE 'by' and 'density' FUNCTION to compute the density of age 
#    given sex='m' and 'f'. Save the results to some variable den
# 2: USE den[[1]] for data on 'f', and den[[2]] for data on 'm'
# 3: plot

monica.den <- by(monica.df$age, monica.df$sex, density) # calculate densities
par(mfrow=c(1,2)) # put two graphs next to each other in one row, two columns
plot(monica.den[[1]], main="Females")
plot(monica.den[[2]], main="Males")
# NOTE: Females is the first density:

den.m <- density(monica.df$age[monica.df$sex == 'm'])
den.f <- density(monica.df$age[monica.df$sex == 'f'])
plot(den.f, main="Females")


# alternative with histogram and lines
par(mfrow=c(1,2))
hist(monica.df$age[which(monica.df$sex == 'm')], probability = TRUE, main = "Males", breaks = 36, ylim = c(0,0.08), xlab = "Age")
lines(density(monica.df$age[which(monica.df$sex == 'm')]))
hist(monica.df$age[which(monica.df$sex == 'f')], probability = TRUE, main = "Females", breaks = 36, ylim = c(0,0.08), xlab = "Age")
lines(density(monica.df$age[which(monica.df$sex == 'f')]))
