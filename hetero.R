library(odesolve)
#

f <- function(t,y,p){
  A <- p[['A']]
  th <- p[['th']]
  eta <- p[['eta']]
  yv <- unlist(y)
  list( yv * (-(A%*%yv) + th + eta*rnorm(length(yv)) ))
}

n <- 10
#times <- c(0, 0.01*(1:2000))
times <- c(0, 0.01*(1:10000))
eta <- 0.001

set.seed(seed1)
k <- sample(0:n,n*n,replace=T)
A <- matrix(k,n,n)

ini <- sample(1:n,n,replace=T)*0.1
th <- rep(0.3*10,n)

set.seed(seed2)
params <- list(A=A,
               th=th,
               eta=eta
               )
y<-lsoda(ini,times,f,params)

par(mfrow = c(1,1))

for(i in 2:n){
  plot(y[,i], type='l',ylim=c(0,1),col.main=i)
  lines(y[,i],col=i)
  par(new=T)
}
par(new=F)
#scatterplot3d()
