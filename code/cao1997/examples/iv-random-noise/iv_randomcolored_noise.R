###############################################################################	
#
#  Random colored noise
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



################################################################################
# (0) Loading Functions and Libraries and Setting up digits
library(data.table) # for manipulating data
library(ggplot2) # for plotting 
library(RColorBrewer)

################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()




# simulate Gaussian White Noise process 
#set.seed(123)
#y = rnorm(60, mean=0.01, sd=0.05)

randomcolorednoise.map = function(X){
  X.new=X
  
  X.new[1,] = 0.95*X[1,] + rnorm(1, mean=0.01, sd=0.05)

  X=X.new
  return(X.new)  
}

x <- matrix(c(0),ncol=1) ## initial conditions

randomcolorednoise.map.ts = function(timesteps,x) {
    x.ts = matrix(nrow=1,ncol=timesteps)
     x.ts[,1] = x[,1]
    for (t in (2:timesteps)) {
      x = randomcolorednoise.map(x)
      x.ts[,t] = x
   }
    return(x.ts)
  }
 

N <- 10000
noise<- as.data.table(  t( randomcolorednoise.map.ts(N,x)   )  ) 



noise [,n:= 0:(.N-1),]
setcolorder(noise, c(2,1))
names(noise) <- c('n', 'x')



################################################################################
### plot time series
p <- ggplot(noise, aes(x=n)) +
	geom_line(aes(y=x,col='x'),lwd = 1,alpha=0.8)+
	theme_bw(20)

#geom_path(aes(x,y,alpha=n))
# p



################################################################################
## CAO's Algorithm
##

setwd('../../functions')
source('cao97_functions.R')

maxdim <- 31
maxtau <- 10
timeseries <- noise$x


E <- data.table()
for (tau_i in 1:maxtau){
	message('* tau: ', tau_i)
	Et<- as.data.table(cao97sub(timeseries,maxdim,tau_i) )
    func <-function(x) {list( tau_i )}
    Et[,c("tau"):=func(), ]
    Et[,dim:=seq(.N)]
    setcolorder(Et, c(3,4,1:2))
    E <- rbind(E, Et )
}

names(E) <- gsub("V1", "E1", names(E))
names(E) <- gsub("V2", "E2", names(E))




################################################################################
setwd(r_scripts_path) ## go back to the r-script source path
