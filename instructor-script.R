##################################################
# R Programming and data Analysis Boot Camp
# 2019-Feb-22
#
# The sample data was extracted from a DNA methylation dataset at: 
# https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE52588


##################################################
# SESSION 3
##################################################

# Import Excel data. 
# WARNING: Your path may be different

library(readxl)
mtable <- read_excel("~/Desktop/r-course/r-sample-data.20190222.xlsx", skip = 5)

# Alternatively, choose the file interactively each time
# mtable <- read_excel(file.choose(), skip = 5)

##################################################

# examine class and content of the data
summary(mtable)
mtable
class(mtable)

# force tibble into the old-style data frame if you wish

mdf = data.frame(mtable)
summary(mdf)
mdf
class(mdf)

##################################################
# try boxplot options 

boxplot(mdf) # ERROR: first 2 columns are not numeric

# boxplot after removing the 1st and 2nd columns

boxplot(mdf[ , 3:89])
boxplot(mdf[ , 3:ncol(mdf)])
boxplot(mdf[ , -1:2]) # ERROR: invalid idexing
boxplot(mdf[ , -(1:2)])

mdata = mdf[ , -(1:2)]
boxplot(mdata)

##################################################
# Other visualization options
boxplot(mdata, col = 'grey')

boxplot(mdata, col = 'grey', las = 0)
boxplot(mdata, col = 'grey', las = 1)
boxplot(mdata, col = 'grey', las = 2)
boxplot(mdata, col = 'grey', las = 3)

?boxplot # Help page

# create a function

my_boxplot = function(x) {
    boxplot(mdata, col = 'grey', las = x)
}

my_boxplot(2)
my_boxplot(3)

##################################################
# Issues with variable scope and assignment operator "<-" 

x=5 # global variable x, UNrelated to the parameter x in my_boxplot

my_boxplot(x=2) # here x is the parameter of the function my_boxplot
my_boxplot(x<-2) # WARNING: global variable x is overridden with a new value

# In the previous line, the assignment operator "<-" was interpreted 
# in the global scope, which may be completely unintended and a source of BUGS.
# In essence, the value '2' was first assigned to the global variable x; 
# then the result of this operation was passed as a parameter to the function.

##################################################
# Another example of BUGS with "<-" if spaces are not used properly: 
# The intended behaviour is to check if x is less than -1, as in:  "x < -1" 
# The actual behaviour is a BUG assigning the value 1 to x, as in: "x <- 1". 

x=5
if(x<-1) { 
    print("Weird")
} else {
    print("OK")
}


##################################################
# SESSION 4
##################################################

# collection of numbers

x = 1:10 # sequence of consecutive integers
x = seq(from=1, to=10, by=4) # same but using sequence function
x = c(1,2,3,4,5,6,7,8,9,10)  # same but using concatenation function
x = c(1,2,37,4,NA,6,73,8,9,100) # can concatenate any non-sequential values

# applying an operation to every element of a vector

x=1:10
sqrt(x) # sqrt of every element - dump on the console screen
x^2     # square of every element - dump on the console screen
y = x^2 # square of every element - save in a new vector y
x = x^2 # square of every element - save in the same vector x, overriding it

mean(x) # computes a single value, not element-by-element 

# generic way of applying an operation to every element of a vector

x=1:10
sqrt(x)                         # use a pre-existing function sqrt
sapply( x, sqrt)                # use a pre-existing function sqrt
sapply( x, function(k) sqrt(k)) # use a pre-existing function sqrt
sapply( x, function(k) { sqrt(k) + 1 - log10(k) } ) # define a new function

# define a new function separately, give it a name, then use it by name
heavy_computation = function(k) { 
    sqrt(k) + 1 - log10(k) 
}
sapply( x, heavy_computation ) 

# Assuming some functions are defined in a file 'my_functions.R', load them: 
#source('my_functions.R')

##################################################
# force your data into a matrix object

mm = data.matrix(mdata)
class(mm)

# Pearson correlations between pairs of matrix columns (or samples)

cor( mm[,1]  , mm[,2])
cor( mm[,1]  , mm[,50] )
cor( mm[,50] , mm[,51] )

# Show scatter plots between pairs of matrix columns (or samples)

plot( mm[,1]  , mm[,2] )
plot( mm[,1]  , mm[,50] )
plot( mm[,50] , mm[,51] )

# Find correlations of column #1 to all other columns of the matrix

sapply( 2:87 , function(k) cor( mm[,1]  , mm[,k]) ) 

corr_to_col1 = function(k) { cor( mm[,1]  , mm[,k]) } 
sapply( 2:87 , corr_to_col1) # first give a name to the function, then use it.

##################################################
# Matrix of all pairwise correlations between all columns of mm

corr_mm = cor(mm)
heatmap( corr_mm ) # show a basic heatmap

# WARNING: Need to install the package 'pheatmap' first 
library('pheatmap')
pheatmap( corr_mm ) # show a pretty heatmap

##################################################
# Operations of the column labels

colnames(mm) # show all column names
grep('c', colnames(mm))  # indices of the control columns
grepl('c', colnames(mm)) # logical values whether the column are controls
grep('c', colnames(mm), value = TRUE) # actual names of the control columns

cols_control = grep('c', colnames(mm), value = TRUE) # control sample names
cols_disease = grep('d', colnames(mm), value = TRUE) # disease sample names

##################################################
# Testing the difference between control vs disease in gene #1 and gene #2

t.test ( mm[ 1, cols_control] , mm[ 1, cols_disease])
tt = t.test ( mm[ 2, cols_control] , mm[ 2, cols_disease])
summary(tt)
class(tt)
str(tt)

tt$p.value

# Testing the difference between control vs disease in all genes at once.

# Use a vector of row indices, extracting one index at a time. 
# For each index 'k', apply a t-test as before.
sapply(1:132 , function(k) t.test ( mm[k, cols_control] , mm[k, cols_disease]) $ p.value)

# This function extracts the actual rows themselves, rather than their indices.
# For each row (which is a vector of numbers), apply a t-test.
apply(mm , 1, function(row) t.test ( row[cols_control] , row[cols_disease]) $ p.value)

##################################################
# END OF SCRIPT
##################################################






