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
