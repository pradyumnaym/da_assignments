### Understanding Functions in R

# setting a seed to make a random sample reproducible, choose any number you want
set.seed(65)

# with argument name, exact argument order
runif(n = 9, min = 3, max = 6)

set.seed(65)

# without argument name, exact argument order
runif(9, 3, 6)

set.seed(65)

# with argument name, mixed argument order
runif(min = 3, max = 6, n = 9)

set.seed(65)

# without argument name, mixed argument order
runif(3, 6, 9) # this means n=3, max=9

set.seed(65)

# using only the first argument
runif(3)

set.seed(65)

# using arguments 1 and 3
runif(3,,4)







### First Coding Steps

# R as a calculator
4 + 4                        + 5

# Space does not matter

# Operators: + - / * ^

# Using several operators and round brackets
(5-3)^3

# Creating objects
x <- c(4,5,6)

c(4, 5.2, 76) -> y # arrows work both directions
y

x = c(4, 5, 6)

assign("x", c(4.2, 1, 5)) # function assign

# c for concatenate

x = c(4,5,6)

x # display in console

x = c(4,5,6); x # semicolon indicates a new row of code

# See which objects are already created
ls()

objects()

# Removing an object
rm("x")

# Working with vectors
x = c(y, 5, y)
x

# Sum and roots
sum(x)

sqrt(x)

# Values at a given position within the vector
x[1]

newobject <- x < 5; newobject

x # Comparison against original values

## Types of brackets

# () round brackets as the standard

# [] box brackets for index positions

# {} curled brackets for functions and loops





### Functions: seq, paste, rep

?seq # starting with the seq function

seq() # just using the default settings

# simple sequence from 3 to 5
seq(3, 5)

seq(from = 3, to = 5)

# using length
seq(from = 3, length = 3)

# step manipulations
seq(from = 3, length = 3, by = 0.5)

seq(from = 3, by = 0.5, length = 3) # changed order

?paste # next function

paste(10:14)

class(paste(1:4)) # checking the class

paste("xyz", 1:10)

cc<- c(2,5,7,"test", 4.5)

paste("xyz", c(2,5,7,"test", 4.5)) # random vector with numbers and characters

paste("xyz", 1:10, sep = "") # modifying the seperator

?rep # next function

rep(c(3,4,5), 3)

rep(1:10, times = 3)

x = c(1,2,3) # creating x

rep(x, each = 3) # using each

rep(x, each = 3, times = 3) # combining arguments

## Working with index positions

x = 4:20 # our data

which(x >= 10) # double equal sign, logical operation

x[3]





### Exercise

# 1 . Create the object "myobject" and assign the values 1:10
# in at least 3 different ways
myobject <- 1:10
myobject
myobject <- c(1,2,3,4,5,6,7,8,9,10)
myobject
myobject <- seq(1,10)
myobject
# 2. Get the sum of your object
sum(myobject)
# 3. Create the following vector by using the paste function
"R is great 4 and I will love it"
"R is great 7 and I will love it"
"R is great 45 and I will love it"

paste("R is great",c(4,7,45),"and I will love it")

# 4. vector of 1,2,3 : repeat the vector to get 11 x 1, 10 x 2, and 10 x 3
rep(c(1,2,3),length=31)
# 5. What is the value of this vector on position 7

## Solutions

# 1 . Create the object "myobject" and assign the values 1:10
# in at least 3 different ways

myobject <- (1:10)

myobject = (1:10)

(1:10) -> myobject

assign("myobject", 1:10)

# 2. Get the sum of your object

sum (myobject)

# 3. Create the following vector by using the paste function
"R is great 4 and I will love it"
"R is great 7 and I will love it"
"R is great 45 and I will love it"

paste ("R is great", c(4,7,45), "and I will love it")

# 4. vector of 1,2,3 : repeat the vector to get 11 x 1, 10 x 2, and 10 x 3

x = rep(1:3, length = 31); x

# 5. What is the value of this vector on position 7

x[7]





### Functions in R


# Brief description: R functions are OBJECTS
# They do calculations for you
# R function structure: name <- function (argument) {statements}
# The arguments specify the components to be used in the function

myfirstfn <- function(x) {x+x}
 myfirstfn(10)

# stepwise working functions

mysecondfn <- function(t,z) {
  value = z*3
  value = value *t
  print(value)}
t= 5
z= 9
mysecondfn(t,z)

## Loops - loops and functions are a crucial part in programming
# FOR loops allow a certain operation to be repeated a fixed nr of times
# This is opposed to the While loop where the rep nr is not prefixed
# The syntax looks like this: for (name in vector) {commands}

for (i in 1:15) {print (i)}
for (z in 1:15) {print (z)}

# variable name does not matter although you will see i quite often
# Can be used for quite complex calculations
# Example calculation of primes with the Eratosthenes method (the oldest known systematic method)

PrimVec = function(n){
  # to start from 2
  if (n>=2) {
    # to further specify the sequence we want to work with
    s = seq(2,n)
    # p will be the container for our primes,
    # numbers will be moved from s to p step by step if they meet the
    criteria
    p = c()
    # we start the loop
    for (i in seq(2,n)){
      
      # we use any to check that i (of this loop round) is still in s, multiples of iwill be removed
      if(any(s==i)){
        # we store i if it meets our criteria in p together with the previous p
        p = c(p,i)
        # to search for numbers with a remainder at modulus division
        s = c(s[(s%%i) != 0],i)
      }}
    return(p) }
  # to specify the output if n < 2 (optional)
  else{
    stop("Input at least 2")
  }}
PrimVec(100)



### Working with data.frames

?airmiles

head(airmiles) # first 6 rows

tail(airmiles) # last 6 rows

summary(mtcars)

plot(mtcars) # simple xy plot function of R Base

hist(airmiles) # histogram

head(mtcars)

sum(mtcars$wt)

attach(mtcars) # attach to R session environment

sum(wt) # now R knows which data.frame to use since it is attached

detach(mtcars) # remove it from environment

sum(wt) # error message since mtcars it not attached any more

mtcars[2,6]

mtcars[c(2,5,8),6]

?lynx

### Graphs in R Base

# 3 main data viz systems:
# ggplot2, lattice and R Base

# simple scatterplot

x=5:7 # 3 data points, integers
y=8:10

# default plot output here is a scatterplot
plot(x,y)

# data is a time series, default here is a line plot
plot(lynx)

# title, color, title color, title magnification
plot(lynx, main="Lynx Trappings", col="red",
     col.main=52, cex.main=1.5)

# label names
plot(lynx, ylab="Lynx Trappings", xlab="")

# label orientation
plot(lynx, ylab="Lynx Trappings", xlab="", las=2)

# las - 0:3, scale orientation

# changing the session paramter, 2*2 plot matrix
par(mfrow=c(2,2), col.axis="red")

plot(1:8, las=0, xlab="xlab", ylab="ylab", main="LAS = 0")
plot(1:8, las=1, xlab="xlab", ylab="ylab", main="LAS = 1")
plot(1:8, las=2, xlab="xlab", ylab="ylab", main="LAS = 2")
plot(1:8, las=3, xlab="xlab", ylab="ylab", main="LAS = 3")

# color manipulation

colors()

# point symbol types

?pch

x=2:4

plot(x, pch="c") # using letters as point symbols

plot(x, pch=13) # symbol nr 13

# Line Types

par(mfrow=c(1,1), col.axis="black") # setting parameters back to default

library(plotrix) # add on package for "ablineclip", install if not yet available
?ablineclip
plot(1:7, ylab="", main="Line Types lty 0:6", xlab="lty 0:6") # test plot

ablineclip(v=1, lty=1, col="sienna2", lwd=2) # solid (default)
ablineclip(v=2, lty=2, col="sienna2", lwd=2) # dashed
ablineclip(v=3, lty=3, col="sienna2", lwd=2) # dotted
ablineclip(v=4, lty=4, col="sienna2", lwd=2) # dotdash
ablineclip(v=5, lty=5, col="sienna2", lwd=2) # longdash
ablineclip(v=6, lty=6, col="sienna2", lwd=5) # twodash, thicker for comparison
ablineclip(v=7, lty=0, col="sienna2", lwd=2) # blank

# plot types of R Base plot

? plot

# by using "type" we can specify which kind of plot we want

plot(lynx) # plot for time series data

plot(lynx, type="p", main="Type p") # points (default)
plot(lynx, type="l", main="Type l") # lines (default for time series)
plot(lynx, type="b", main="Type b") # points connected by lines
plot(lynx, type="b", main="Type c") # lines only of b
plot(lynx, type="o", main="Type o") # points overlaid by lines
plot(lynx, type="h", main="Type h") # high density
plot(lynx, type="s", main="Type s") # steps
plot(lynx, type="n", main="Type n") # no plot

# Example: advanced line plot with R Base

par(mar=c(4,3,3,3), col.axis="darkgreen") # change of plot margins

plot(cars$speed, type="s", col="red", bty="n", xlab="Cars ID", ylab="")
text(8, 14, "Speed in mph", cex=0.85, col="red") # adding the explanatory text to plot 1

par(new=T) # allows 2 in 1 plot

plot(cars$dist, type="s", bty="n", ann=F, axes=F, col="darkblue")
axis(side=4, col = "darkblue") # y axis for plot 2
text(37, 18, "Stopping distance in ft", cex=0.85, col="darkblue") # explanations to plot 2
title(main="Speed and Stopping\n Distances of Cars") # main title

#??? graphical parameters

?par

par()





### Graphs Exercise

# 1. get familiar with "rivers" - how many observations?
# 2. plot rivers against its index (hint: number of observation on x)
# 3. add: header (red), label names
# 4. change the point symbol and point color

## Solution

?rivers # 141 observations

x = 1:141

y = rivers

plot(x,y, col = "green", pch = 20,
     main = "Lengths of\nMajor N. American Rivers",
     col.main ="red", xlab = "",
     ylab = "length in miles")