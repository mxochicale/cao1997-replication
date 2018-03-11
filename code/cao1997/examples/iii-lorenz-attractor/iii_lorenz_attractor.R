###############################################################################	
#
#  Lorenz Attractor
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
library(deSolve)





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


# Parameters for the solver
parameters <- c(s = 10, r = 28, b = 8/3)

##
# In initial state
yini <- c(X = 0.01, Y = 0.001, Z = 0.001)


################################################################################
# lorenz time-series

#define controlling parameters
# rho     - scaled rayleigh number.
# sigma   - prandtl number.
# beta   - geometry ascpet ratio.
parameters <- c(rho=28, sigma= 10, beta=8/3)

###define initial state
#initial_state <- c(x=1, y=1, z=1)
initial_state <- c(x = 0.01, y = 0.001, z = 0.001)


# define integrations times
# times <- seq(0,100, by=0.001)
times <- seq(0,150, by=0.1)
# timeserie <- seq(0, 150, by = 0.1)

#perform the integration and assign it to variable 'out'
out <- ode(y=initial_state, times= times, func=lorenz, parms=parameters)
maxlength <- dim(out)[1]


# lorenz time series as data.table object
lorenz <- as.data.table(out)
func <-function(x) {list("lorenz")}
lorenz[,c("type"):=func(), ]
lorenz[,n:=0:(.N-1),]
setcolorder(lorenz, c(5,6,1:4))



################################################################################
### (4.1) windowing data
###
windowframe = 1000:maxlength;
lorenz <- lorenz[,.SD[windowframe],by=.(type)];


################################################################################
### plot time series
p <- ggplot(lorenz, aes(x=n)) +
   geom_line(aes(y=x,col='x'),lwd = 1,alpha=0.8)+
   geom_line(aes(y=y,col='y'),lwd = 1,alpha=0.8)+
   geom_line(aes(y=z,col='z'),lwd = 1,alpha=0.8)+
   facet_wrap(~type, scales = 'free', nrow = 4)+
   theme_bw(20)

# dev.new(xpos=0,ypos=0,width=18, height=6)
# p



################################################################################
setwd(r_scripts_path) ## go back to the r-script source path

