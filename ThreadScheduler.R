#This program attempts to get the plot of sucessful runs over number of tries
library(dplyr)
library(RSQLite)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(scales)
library(mixtools)


# There are 4  tests  A, B, C, and D
# Further more A can run in Master or slave modes( M or S)
# 4 instances of each tests can be run at any time
# For all parctical purposes, the total pool of tests can be thought of as
# MSSS
# BBBB
# CCCC
# DDDD
# There are 4 threads to schedule in a iteration
# Stress test is performed by running 4096 iterations of 4 tests selected out of 16

# t0,t1, t2 and t3 are the threads which are used for scheduling
 
# Only if M is scheduled in t0, S can run sucessfully
# e.g MSSS runs successfully -> q3

#     MSSX runs successfully
      # MSXS runs scuccessfully ->q2
      # MXSS runs successfully

      # MSXX runs successfully
      # MXSX runs successfully ->q1
      # MXXS runs successfully

      # MXXX runs successfully ->q0

      # XXXX runs succesfully ->qx

# Probability 
q3 <- (1/13)*(4/16) * (3/15) * (2/14)

q2 <- (2/13) * 3 * (4/16) * (3 /15)* (12/14)

q1 <- (3/13) * 3 * (4/16) * (12/15) * (11/14)

q0 <- (4/13) * (12/16) * (11/15) * (10/14)

qx <- (12/16) * (11/15) * (10/14) * (9/13)

# total probability of a sucessful run in 1 try 

q <- q3 + q2 + q1 + q0 + qx

print(paste("Probability of succesful sheduling is ",q))
# probability of failure in 1st try

 1-q
 
 # Probility of failure in  2 trys

q * ( 1-q)

# Probability of failure in 3 trys

q*q *( 1-q)

# probability of failure in 4096th try 
q^4095 * ( 1-q)

# Probability of success in 4096 tries
#1 - probability of 
P <- q^4096

# Probability of failure
F <- 1-P

# create a vector of 
tries <- c(seq(1,20, by = 1))

# Define a function 
# gives sucess probability in xth tries

sched_success <- function (x, q) { q^(x)}
sched_fail    <- function (x, q) { 1 -  (q^(x))}

# print y 
y<- sched_success(tries,q)
y

# Data Layer
n <-(ggplot)

# Data + Aesthetics Mapping 
n <-ggplot(data_frame(tries),aes(x= tries, y = sapply(tries, sched_success,q)))

# Data + aes + Geometries 
n <- n + geom_jitter(aes( color = "success")) 

n <- n + geom_jitter(aes( x = tries, y = sapply(tries, sched_fail,q),color = "fail"))

# Data + Aesthetic Mapping +  Geom + Facets
#n <- n + facet_grid(. ~ Department.Title ) 

#Data + Aesthetics + Geoms + Facets + Statistics
# ... add a linear regression model here

#Data + Aesthetics + Geoms + Facets + Statistics + Co-ordinates
#n <- n + scale_y_discrete(limits=c("00000","120000"), breaks=seq(00000,120000,10000))
n <- n + xlab("Number of tries")
n <- n + ylab("Probability of Success or Failure ")

#Data + Aesthetics + Geoms + Facets + Statistics + Co-ordinates + Theme 
#n <- n + theme(axis.text.x = element_text(angle=305))

print(n)

