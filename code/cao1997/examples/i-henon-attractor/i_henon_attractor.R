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
library(RColorBrewer)




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
## CAO's Algorithm
##

setwd('../../functions')
source('cao97_functions.R')

maxdim <- 31
maxtau <- 10
timeseries <- henon$x


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
### Plot E values
e1 <- ggplot(E, aes(x=dim)) +
    	geom_line( aes(y=E1, colour=factor(tau) ),lwd = 3,alpha=0.7)+
    	geom_point( aes(y=E1, shape=factor(tau), colour=factor(tau)  ), size=5, stroke =1 )+
    	scale_color_manual(values = colorRampPalette(brewer.pal(n = 9, name="Blues"))(2*maxtau)[(maxtau+1):(2*maxtau)]  ) +
    	scale_shape_manual(values= 1:(maxtau))+
    
	labs(x='Embedding dimension')+
    coord_cartesian(xlim = c(0, (maxdim-1) ), ylim = c(0, 2.0 ) )+
    theme(legend.position = c(0.9, 0.3) )+
    theme( axis.title.x = element_text(size = rel(2.5), angle = 0),
           axis.text.x = element_text(size = rel(2), angle = 0),
           axis.title.y = element_text(size = rel(2.5), angle = 90),
           axis.text.y = element_text(size = rel(2), angle = 90)
           )+
    theme(legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5))
          )
#e1

e2 <- ggplot(E,aes(x=dim)) +
    	geom_line( aes(y=E2, colour=factor(tau) ),lwd = 3,alpha=0.7)+
    	geom_point( aes(y=E2, shape=factor(tau), colour=factor(tau)), size=5, stroke =1 )+
    	scale_color_manual(values = colorRampPalette(brewer.pal(n = 9, name="Blues"))(2*maxtau)[(maxtau+1):(2*maxtau)]  ) +
    	scale_shape_manual(values= 1:(maxtau))+

    labs(x='Embedding dimension')+
    coord_cartesian(xlim = c(0, (maxdim-1) ), ylim = c(0, 2.0 ) )+
    theme(legend.position = c(0.9, 0.3) )+
    theme( axis.title.x = element_text(size = rel(2.5), angle = 0),
           axis.text.x = element_text(size = rel(2), angle = 0),
           axis.title.y = element_text(size = rel(2.5), angle = 90),
           axis.text.y = element_text(size = rel(2), angle = 90)
           )+
    theme(legend.title = element_text(size = rel(1.5)),
          legend.text = element_text(size = rel(1.5))
          )
#e2






################################################################################
setwd(r_scripts_path) ## go back to the r-script source path
