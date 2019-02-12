###Chapter 6 notes
library(tidyverse)
library(rethinking)

sppnames <- c( "afarensis","africanus","habilis","boisei", "rudolfensis","ergaster","sapiens") 
brainvolcc <- c( 438 , 452 , 612, 521, 752, 871, 1350 ) 
masskg <- c( 37.0 , 35.5 , 34.5 , 41.5 , 55.5 , 61.0 , 53.5 ) 
d <- data.frame( species=sppnames , brain=brainvolcc , mass=masskg )

#calculation of entropy
p <- c( 0.3 , 0.7 ) 
-sum( p*log(p) )
