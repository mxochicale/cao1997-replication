#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#
# FileName:    cao97_functions.R
# Description:
#
#
############
# How to use
# add the following line to your main script
# for the directory where the functions and the R scripts lives, you have to add
#
# source('~/cao97_functions.R')
#
#
#
#
#
#
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Miguel P. Xochicale [http://mxochicale.github.io]
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#-----------------------------------------------------------------------------
#--------------------  cao97 FORTRAN subrutine wraped in R  --------------------------
#build dll to wrap f function in R
#R CMD SHLIB cao97sub.f
# which produce cao97sub.so and cao97sub.o

#to replace NaN values, use
#X <- replace(X, is.nan(X), 0)
cao97sub <- function(x,maxd,tau) {
    dyn.load('cao97sub.so')
    lx = length(x)
    retdata <- .Fortran("cao97sub",
                        x = as.double(x),
                        lx = as.integer(lx),
                        maxd = as.integer(maxd),
                        tau = as.integer(tau),
                        e1 = double(maxd),
                        e2 = double(maxd)
                        )
    return(list(retdata$e1[1:maxd-1], retdata$e2[1:maxd-1]))
 }
## Example
# maxd <- 20
# tau <- 5
# E <-  cao97sub(timeseries,maxd,tau)




############################
############################
plotE1taus <- function(E) {

ylim_max <- 1.5
ggplot(E, aes(x=dim) ) +
    	geom_line(  aes(y=E1, colour=factor(tau) ),lwd = 3,alpha=0.5)+
    	geom_point( aes(y=E1, shape=factor(tau), colour=factor(tau)  ), size=5, stroke =1 )+
    	#scale_color_manual(values = colorRampPalette(brewer.pal(n = 10, name="RdBu"))(maxtau) ) +
    	scale_color_manual(values = colorRampPalette(brewer.pal(n = 9, name="Blues"))(2*maxtau)[(maxtau+1):(2*maxtau)]  ) +
    	scale_shape_manual(values= 1:(maxtau))+
    
    	coord_cartesian(xlim = c(0, (maxdim-1) ), ylim = c(0, ylim_max ) )+
	labs(x='Embedding dimension')+
	theme_bw(base_size=20)+
	theme(legend.position = c(0.9, 0.3) )

}
#usage
#e1 <- plotE1taus(E)



############################
############################
plotE2taus <- function(E) {

ylim_max <- 1.5
ggplot(E, aes(x=dim) ) +
    	geom_line(  aes(y=E2, colour=factor(tau) ),lwd = 3,alpha=0.5)+
    	geom_point( aes(y=E2, shape=factor(tau), colour=factor(tau)  ), size=5, stroke =1 )+
    	#scale_color_manual(values = colorRampPalette(brewer.pal(n = 10, name="RdBu"))(maxtau) ) +
    	scale_color_manual(values = colorRampPalette(brewer.pal(n = 9, name="Blues"))(2*maxtau)[(maxtau+1):(2*maxtau)]  ) +
    	scale_shape_manual(values= 1:(maxtau))+
    
    	coord_cartesian(xlim = c(0, (maxdim-1) ), ylim = c(0, ylim_max ) )+
	labs(x='Embedding dimension')+
	theme_bw(base_size=20)+
	theme(legend.position = c(0.9, 0.3) )

}
#usage
#e2 <- plotE2taus(E)


############################
############################
plotE1E2tau <- function(E) {

ylim_max <- 1.5


p<-ggplot(E, aes(x=dim) ) + 

geom_line(  aes(y=E1_1000, col=1 ),lwd = 3,alpha=0.5) +
geom_point( aes(y=E1_1000, shape=1, col=1 ), size=5, stroke =1 ) +
scale_shape_identity() +

geom_line(  aes(y=E1_10000, col=2 ),lwd = 3,alpha=0.5) +
geom_point( aes(y=E1_10000, shape=2, colour=2), size=5, stroke =1 ) +
#scale_shape_identity() +


geom_line(  aes(y=E2_1000, col=3 ),lwd = 3,alpha=0.5) +
geom_point( aes(y=E2_1000, shape=3, col=3 ), size=5, stroke =1 ) +
#scale_shape_identity() +

geom_line(  aes(y=E2_10000, col=4 ),lwd = 3,alpha=0.5) +
geom_point( aes(y=E2_10000, shape=4, colour=4), size=5, stroke =1 ) +
#scale_shape_identity() +


    	coord_cartesian(xlim = c(0, (maxdim-1) ), ylim = c(0, ylim_max ) )+
	labs(x='Embedding dimension', y='E1 & E2')+
	theme_bw(base_size=20)+
	theme(legend.position = c(0.9, 0.3) )




}
#usage
#e <- plotE1E2tau(E)



