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




