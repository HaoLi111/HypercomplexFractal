library(compiler)
enableJIT(3)

#install.packages("foreach")
#install.packages("doParallel")
#install.packages("abind")
library(abind)
library(doParallel)
library(parallel)
registerDoParallel(4)
getDoParWorkers()
setwd("D:/HypercomplexFractal")
zbase <- ybase <- xbase <- seq(from=-2,to=2,by=.2)# LOL
#http://www.bugman123.com/Hypercomplex/index.html
"%HC_power%"<-function(x,n){
  r = sqrt(sum(x^2))
  y = x[2];z=x[3];x=x[1]
  theta = n * atan(y/x);f = n*asin(z/r)
  return(r^n*c(cos(theta)*cos(f),sin(theta)*cos(f),-sin(f)))
}

escape_General_Mandelbulb = function(x,n=2,maxIter = 200){
  C = x
  for(i in 1:maxIter){
    x = fi[p]x<- x%HC_power%n + C
    if(sqrt(sum(x))>2){
      n_escape = i
      return(n_escape)
    }
  }
  return(maxIter+1)
}





n = 2


volumetric = list()
for(iz in seq_along(z)){
  z = zbase[iz]
  m<-foreach(y=ybase,.combine = cbind) %dopar% {
    v=numeric(length(ybase))
    for(x in seq_along(xbase)){
      xn = c(xbase[x],y,z)
      v[x]= escape_General_Mandelbulb(xn,n)
    }
    v
  }
  volumetric[[iz]]=m
}




volumetric=abind(volumetric,m,along = 3)
saveRDS(volumetric,"Mandelbulb.RDS")

#install.packages("plot3D")
#install.packages("misc3d")
library(plot3D)
library(misc3d)
png("Mandelbulb.png")
isosurf3D(x=xbase,y=ybase,z=zbase, colvar = volumetric, level = c(10,20,200),
             col = c("red", "blue", "yellow"),
             clab = "No. of Iterations", alpha = 0.4, plot = TRUE)
dev.off()
