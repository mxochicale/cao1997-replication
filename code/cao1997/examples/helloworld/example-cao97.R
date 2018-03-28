#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
# FileName:    example-cao97.R
# Description:
#
#
############
# How to use
#
# source('~/cao97_functions.R')
#
#
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Miguel P. Xochicale [http://mxochicale.github.io]
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



################################################################################
# (0) Loading Functions and Libraries and Setting up digits
library(data.table) # for manipulating data
library(ggplot2) # for plotting 
library(deSolve)
library(RColorBrewer)





################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()





################################################################################
# lorenz function
lorenz <- function(t, state, parameters){
          with(as.list( c(state,parameters)),
              {
              #rate of change
              dx <- sigma*(y-x)
              dy <- rho*x - x*z - y
              dz <- x*y - beta*z

              # return the rate of change
              list( c(dx, dy, dz) )
              }
          )# end with(as.list...
}


################################################################################
# lorenz time-series

#define controlling parameters
# rho     - scaled rayleigh number.
# sigma   - prandtl number.
# beta   - geometry ascpet ratio.
parameters <- c(rho=28, sigma= 10, beta=8/3)

#define initial state
state <- c(x=1, y=1, z=1)
# yini <- c(x = 0.01, y = 0.001, z = 0.001)
# state <- c(x=20, y=41, z=20)


# define integrations times
# times <- seq(0,100, by=0.001)
times <- seq(0,150, by=0.1)
# timeserie <- seq(0, 150, by = 0.1)

#perform the integration and assign it to variable 'out'
out <- ode(y=state, times= times, func=lorenz, parms=parameters)
maxlength <- dim(out)[1]


# lorenz time series as data.table object
lts <- as.data.table(out)
func <-function(x) {list("lorenz")}
lts[,c("type"):=func(), ]
lts[,n:=seq(.N)]
setcolorder(lts, c(5,6,1:4))



################################################################################
### (4.1) windowing data
###
windowframe = 1000:maxlength;
lts <- lts[,.SD[windowframe],by=.(type)];


################################################################################
### plot time series
p <- ggplot(lts) +
   geom_line(aes(x=n,y=x,col='x'),lwd = 1,alpha=0.8)+
   geom_line(aes(x=n,y=y,col='y'),lwd = 1,alpha=0.8)+
   geom_line(aes(x=n,y=z,col='z'),lwd = 1,alpha=0.8)+
   facet_wrap(~type, scales = 'free', nrow = 4)+
   theme_bw(20)




################################################################################
## CAO's Algorithm
##

setwd('../../functions')
source('cao97_functions.R')

maxdim <- 31
maxtau <- 4


E <- data.table()
for (tau_i in 1:maxtau){
	message('tau:', tau_i)
    	Et<- as.data.table(cao97sub(lts$x,maxdim,tau_i) )
    	func <-function(x) {list( tau_i )}
    	Et[,c("tau"):=func(), ]
    	Et[,dim:=seq(.N)]
    	setcolorder(Et, c(3,4,1:2))
    	E <- rbind(E, Et )
}

names(E) <- gsub("V1", "E1", names(E))
names(E) <- gsub("V2", "E2", names(E))



################################################################################
### Plot E values

e1 <- plotE1taus(E)
e2 <- plotE2taus(E)





################################################################################
setwd(r_scripts_path) ## go back to the r-script source path
