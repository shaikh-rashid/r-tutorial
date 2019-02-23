#!/usr/bin/env R

# first row contains variable names
install.packages("readxl")
library(readxl)
excel <- read_excel("~/Downloads/R Tutorial/r-sample-data.20190222.xlsx", 1, skip = 5)

# view data
View(excel)
summary(excel)
excel
class(excel)

# to do the same with text data
install.packages("readr")
library(readr)
mtext  <- read_delim("~/Downloads/R Tutorial/r-sample-data.20190222.txt", "\t", skip = 5)
View(mtext)
summary(mtext)

# visualize the data in plot
boxplot(excel) # error
boxplot(excel[ , 3:89 ])
boxplot(excel[ , 3:ncol(excel)])
boxplot(excel[ , -1:2]) # error
boxplot(excel[ , -(1:2)])

# use colours to plot
mdata = excel[ , -(1:2)]
boxplot(mdata, col = 'lightblue', las = 2)

# create a boxplot function
my_boxplot = function(x) {
    boxplot(mdata, col = rainbow(100), las = x)
}

my_boxplot(2)
my_boxplot(4)
my_boxplot(x = 2) # passes value to local scope
my_boxplot(x <- 2) # passes value and assign to global scope

# assignments are dangerous
x = 5
if(x <- 1) {
    print("Weird")
} else {
    print("OK")
}

x = 5
if(x < -1) {
    print("Weird")
} else {
    print("OK")
}

# sequence operations
x = 1:10
x = seq(from=1, to=10, by=1)
x = c(1,2,3,4,5,6,7,8,9,10)
sqrt(x)

# square the sequence
sq = x * x
sq
x^2

# mean/median/mode
mean(x)
median(x)

# apply functions
x=1:10
sapply(x, sqrt)
sapply(x, function(i) sqrt(i))

# finding correlation of dataset
mm = data.matrix(mdata)
class(mm)

cor(mm[,1] , mm[,2])
cor(mm[,1] , mm[,50])
cor(mm[,50] , mm[,51])

plot(mm[,1] , mm[,2])
plot(mm[,1] , mm[,50])
plot(mm[,50] , mm[,51])

correlations = sapply(2:ncol(mm) , function(k) cor(mm[,1] , mm[,k]))
pos = which.max(correlations)
plot(mm[,1] , mm[,pos])

# create heatmaps
heatmap(cor(mm), col = rainbow(100))

install.packages("pheatmap")
library("pheatmap")
pheatmap(cor(mm))

# working with collections
colnames(mm)
grep('c', colnames(mm))
grepl('c', colnames(mm))
control = grep('^c.+', colnames(mm), value = TRUE)
disease = grep('^d.+', colnames(mm), value = TRUE)
unknown = grep('^u.+', colnames(mm), value = TRUE)

tt = t.test(mm[1, control] , mm[1 , disease])
sapply(t.test(mm[2, control] , mm[2 , disease])
