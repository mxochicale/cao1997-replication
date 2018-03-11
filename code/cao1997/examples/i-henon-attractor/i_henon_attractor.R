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



################################################################################
# (0) Loading Functions and Libraries and Setting up digits
library(data.table) # for manipulating data
library(ggplot2) # for plotting 





################################################################################
# (1) Defining paths for main_path, r_scripts_path, ..., etc.
r_scripts_path <- getwd()



N <- 1000


#reference
#http://www.stat.cmu.edu/~cshalizi/462/lectures/03/03.R

henon.map = function(x,a,b){
  x.new=x
  x.new[1,] = 1 - a*x[1,]*x[1,] + x[2,]
  x.new[2,] = b*x[1,]
  x=x.new
  return(x.new)
}

#x <- matrix(nrow=2,ncol=1)
x <- matrix(c(.0,.0),ncol=1)


henon.map.ts = function(timesteps,x,a,b) {
  x.ts = matrix(nrow=2,ncol=timesteps)
  x.ts[,1] = x[,1]
  for (t in (2:timesteps)) {
    x = henon.map(x,a,b)
    x.ts[,t] = x
  }
  return(x.ts)
}


henon <- as.data.table( t( henon.map.ts(N,x,1.4,0.3))  )
henon [,n:= 0:(.N-1),]
setcolorder(henon, c(3,1,2))
names(henon) <- c('n', 'x', 'y')




################################################################################
### plot time series
p <- ggplot(henon, aes(x=n)) +
	geom_line(aes(y=x,col='x'),lwd = 1,alpha=0.8)+
	geom_line(aes(y=y,col='y'),lwd = 1,alpha=0.8)+
	theme_bw(20)

#geom_path(aes(x,y,alpha=n))
# p




################################################################################
setwd(r_scripts_path) ## go back to the r-script source path

