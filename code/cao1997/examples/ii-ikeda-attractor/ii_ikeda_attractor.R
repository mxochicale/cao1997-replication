##############################################################################
#
#  Ikeda Attractor
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

################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()




ikeda.map = function(X,p,u,k,alpha){
  X.new=X
  
  
  X.new[1,] = p + (u * ( X[1,]*cos(X[3,]) - X[2,]*sin(X[3,]) )) ## x
  X.new[2,] = u * ( X[1,]*sin(X[3,]) - X[2,]*cos(X[3,]) ) ##y
  
  X.new[3,] = (k) - ((alpha) / (1 + X[1,]*X[1,] + X[2,]*X[2,]))   ##t

  X=X.new
  return(X.new)  
}



x <- matrix(c(0,0,0),ncol=1) ## initial conditions

 ikeda.map.ts = function(timesteps,x,p,u,k,alpha) {
   x.ts = matrix(nrow=3,ncol=timesteps)
   x.ts[,1] = x[,1]
   for (t in (2:timesteps)) {
     x = ikeda.map(x,p,u,k,alpha)
     x.ts[,t] = x
  }
   return(x.ts)
 }


N <- 1000
ikeda<-as.data.table(  t(  ikeda.map.ts(N,x,1.0,0.9,0.4,6.0)  )) 
ikeda [,n:= 0:(.N-1),]
setcolorder(ikeda, c(4,1,2,3))
names(ikeda) <- c('n', 'x', 'y', 'z')




################################################################################
### Plot time series
p <- ggplot(ikeda, aes(x=n)) +
	geom_line(aes(y=x, col='x'),lwd = 1,alpha=0.8)+
	geom_line(aes(y=y, col='y'),lwd = 1,alpha=0.8)+
	geom_line(aes(y=z, col='z'),lwd = 1,alpha=0.8)+
	theme_bw(20)







################################################################################
setwd(r_scripts_path) ## go back to the r-script source path

