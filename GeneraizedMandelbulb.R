library(compiler)
enableJIT(3)

#install.packages("foreach")
#install.packages("doParallel")
#install.packages("abind")
library(abind)
library(doParallel)
library(parallel)
registerDoParallel(8)
getDoParWorkers()
setwd("D:/HypercomplexFractal")
#install.packages("plot3D")
#install.packages("misc3d")
library(plot3D)
library(misc3d)



zbase <- ybase <- xbase <- seq(from=-2,to=2,by=.02)# LOL
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
#escape_General_Mandelbulb(c(0,.1,.2))




#---------------------------------------------------------
#generalized
TIME = system.time({
  
  #dir.create("GeneralizedMbulb")
  setwd("GeneralizedMbulb")
  plist = seq(from = -5,to = 5,by = .1)
  
  
  
  
  for(pi in seq_along(plist)){

    n = plist[pi]
    if(n==0) next()
    if(file.exists(paste("Plot3",pi,".png"))) next()
    try({
      message("working on")
      message(n)
      #system.time({
        
        volumetric = list()
        for(iz in seq_along(zbase)){
          z = zbase[iz]
          print('Working on z = ')
          print(z)
          m<-foreach(iy=seq_along(ybase),.combine = cbind) %dopar%  {
            
            v=numeric(length(ybase))
            for(ix in seq_along(xbase)){
              xn = c(xbase[ix],ybase[iy],z)
              v[ix]= escape_General_Mandelbulb(xn,n,maxIter = maxIter)#+sum(xn)
            }
            v       
          }
          volumetric[[iz]]=m
        }
      #})
      #escape_General_Mandelbulb(c(.1,.1,.1))
      
      #system.time({
        volumetric=abind(volumetric,along = 3)
        saveRDS(volumetric,paste0("Mandelbulb",pi,".RDS"))
        
      #})
      
      
      #system.time({
        png(paste("Plot1",pi,".png"),width = 1600,height=900)
        isosurf3D(x=xbase,y=ybase,z=zbase, colvar = volumetric, level = maxIter,
                  col ="blue",
                  clab = paste("(No. of Iterations);z<-z^",n,"+C"),plot = TRUE)
        dev.off()
        
      #})
      #
      #system.time({
        png(paste("Plot2",pi,".png"),width = 1600,height=900)
        isosurf3D(x=xbase,y=ybase,z=zbase, colvar = volumetric, level = c(2,100,200)*5,
                  col = c("red", "blue", "yellow"),
                  clab =paste("(No. of Iterations);z<-z^",n,"+C"), lighting = TRUE,shade = .6,alpha = 0.4, plot = TRUE)
        dev.off()
        
      #})
      
      #system.time({
        png(paste("Plot3",pi,".png"),width = 1600,height=900)
        isosurf3D(x=xbase,y=ybase,z=zbase, colvar = log10(volumetric), level = log10(maxIter),
                  col ="yellow",
                  clab = "lg(No. of Iterations)", lighting = TRUE,shade = .6,alpha = .2, plot = FALSE)
        
        isosurf3D(x=xbase,y=ybase,z=zbase, colvar = log10(volumetric), level = (1:2)/3*log10(maxIter),
                  col =c("red", "blue"),
                  clab = paste("lg(No. of Iterations);z<-z^",n,"+C"), lighting = TRUE,shade = .6,alpha = .2, plot = TRUE,add = TRUE)
        dev.off()
    })
    
      }
    
  
})
message(TIME)

library(av)
#install.packages("av")
av_encode_video(paste("Plot3",seq_along(plist)[-51],".png"),"Plot3.mp4")
