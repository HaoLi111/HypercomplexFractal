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
HC_power<-function(x,n){
  r = sqrt(sum(x^2))
  if(r==0) r=+2.220446e-16
  y = x[2];z=x[3];x=x[1]
  if(x==0) x=+2.220446e-16
  #if(z==0) z=+2.220447e-16
  theta = n * atan2(y,x);f = n*asin(z/r)
  return(r^n*c(cos(theta)*cos(f),sin(theta)*cos(f),-sin(f)))
}

escape_General_Mandelbulb = function(x,n=2,maxIter = 200){

  C = x
  for(i in 1:maxIter){
    x = HC_power(x,n)+ C
    #print("work")
    if(sqrt(sum(x^2))>2){
      n_escape = i
      return(n_escape)
    }
  }

  return(maxIter+1)
}
escape_General_Mandelbulb(c(0,.1,.2))




system.time({
  
n = 2


volumetric = list()
for(iz in seq_along(zbase)){
  z = zbase[iz]
  print('Working on z = ')
  print(z)
  m<-foreach(iy=seq_along(ybase),.combine = cbind,.export =c("HC_power","escape_General_Mandelbulb")) %dopar%  {
    
    v=numeric(length(ybase))
    for(ix in seq_along(xbase)){
      xn = c(xbase[ix],ybase[iy],z)
      v[ix]= escape_General_Mandelbulb(xn,n)#+sum(xn)
    }
    v       
  }
  volumetric[[iz]]=m
}
})
#escape_General_Mandelbulb(c(.1,.1,.1))

system.time({
  volumetric=abind(volumetric,along = 3)
  saveRDS(volumetric,"Mandelbulb.RDS")
  
})


#install.packages("plot3D")
#install.packages("misc3d")
library(plot3D)
library(misc3d)
png("Mandelbulb.png")
isosurf3D(x=xbase,y=ybase,z=zbase, colvar = volumetric, level = c(10,20,200),
             col = c("red", "blue", "yellow"),
             clab = "No. of Iterations", alpha = 0.4, plot = TRUE)
dev.off()
