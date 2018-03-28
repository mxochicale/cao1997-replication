###############################################################################	
#
#  Henon Attractor
# 
#
#
#
#
# Written by Miguel P Xochicale [http://mxochicale.github.io]
# email:@gmail.com
# please email me directly if you see any errors or have any questions
#
###############################################################################	
	# OUTLINE:





#################
# Start the clock!
start.time <- Sys.time()


################################################################################
# (0) Loading Functions and Libraries and Setting up digits
library(data.table) # for manipulating data
library(ggplot2) # for plotting 
library(RColorBrewer)




################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()





#reference
#http://www.stat.cmu.edu/~cshalizi/462/lectures/03/03.R

henon.map = function(x,a,b){
  x.new=x
  x.new[1,] = 1 - a*x[1,]*x[1,] + x[2,]
  x.new[2,] = b*x[1,]
  x=x.new
  return(x.new)
}


henon.map.ts = function(timesteps,x,a,b) {
  x.ts = matrix(nrow=2,ncol=timesteps)
  x.ts[,1] = x[,1]
  for (t in (2:timesteps)) {
    x = henon.map(x,a,b)
    x.ts[,t] = x
  }
  return(x.ts)
}






############################################
###### E1000

N <- 1000

#x <- matrix(nrow=2,ncol=1)
x <- matrix(c(.0,.0),ncol=1)


henon <- as.data.table( t( henon.map.ts(N,x,1.4,0.3))  )
henon [,n:= 0:(.N-1),]
setcolorder(henon, c(3,1,2))
names(henon) <- c('n', 'x', 'y')



################################################################################
## CAO's Algorithm
##

setwd('../../functions')
source('cao97_functions.R')

maxdim <- 10
tau <- 1

timeseries <- henon$x


E<- as.data.table(cao97sub(timeseries,maxdim,tau ) )
func <-function(x) {list( tau )}
E[,c("tau"):=func(), ]
E[,dim:=seq(.N)]
setcolorder(E, c(3,4,1:2))
   
names(E) <- gsub("V1", "E1_1000", names(E))
names(E) <- gsub("V2", "E2_1000", names(E))

#func <-function(x) {list( N )}
#E[,c("datapoints"):=func(), ]
#

E1000<-E
E<-NULL



############################################
###### E10000

N <- 10000

#x <- matrix(nrow=2,ncol=1)
x <- matrix(c(.0,.0),ncol=1)


henon <- as.data.table( t( henon.map.ts(N,x,1.4,0.3))  )
henon [,n:= 0:(.N-1),]
setcolorder(henon, c(3,1,2))
names(henon) <- c('n', 'x', 'y')



################################################################################
## CAO's Algorithm
##



timeseries <- henon$x


E<- as.data.table(cao97sub(timeseries,maxdim,tau ) )


  
names(E) <- gsub("V1", "E1_10000", names(E))
names(E) <- gsub("V2", "E2_10000", names(E))



E10000<-E
E<-NULL


E <- cbind(E1000, E10000)




###########################
#### Ploting 
e <- plotE1E2tau(E)






#################
# Stop the clock!
end.time <- Sys.time()
end.time - start.time



################################################################################
setwd(r_scripts_path) ## go back to the r-script source path
