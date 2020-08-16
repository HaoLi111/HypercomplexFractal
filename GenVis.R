
library(compiler);enableJIT(3)
library(misc3d);library(plot3D)
setwd("D:/HypercomplexFractal/GeneralizedMbulb")
maxIter=1024
zbase <- ybase <- xbase <- seq(from=-2,to=2,by=.02)
plist = seq(from = -5,to = 5,by = .1)
for(pi in seq_along(plist)){
  volumetric = as.matrix(readRDS(paste0("Mandelbulb",pi,".RDS")))
  dim(volumetric) = c(length(xbase),length(ybase),length(zbase))
  #ds = readRDS(paste0("Mandelbulb",pi,".RDS"))
  #for(xi in 1:201){
  #  for(yi in 1:201){
  #    
  #   for(zi in 1:201){
  # #     if(volumetric[xi,yi,zi]!=ds[xi,yi,zi]) print("STOP!!!")
   ##   }
  #  }
  #}
  
  
  paste0("Working on :: ----Mandelbulb",pi,".RDS")
  
  
  png(paste("PlotZ-",pi,".png"),width = 1600,height=900)
  
  isosurf3D(x=xbase,y=ybase,z=zbase[zbase<0], colvar = log10(volumetric[,,zbase<0]), level = log10(maxIter),
                  col ="yellow",
                  clab = "lg(No. of Iterations)", lighting = TRUE,shade = .6,alpha = .2, plot = FALSE)
        

  isosurf3D(x=xbase,y=ybase,z=zbase[zbase<0], colvar = log10(volumetric[,,zbase<0]), level = (1:2)/3*log10(maxIter),
            col =c("red", "blue"),
            clab = paste("lg(No. of Iterations);z<-z^",n,"+C"), lighting = TRUE,shade = .6,alpha = .2, plot = TRUE,add = TRUE)
  dev.off()
  
  png(paste("PlotX-",pi,".png"),width = 1600,height=900)
  
  isosurf3D(x=xbase[xbase<0],y=ybase,z=zbase, colvar = log10(volumetric[xbase<0,,]), level = log10(maxIter),
                  col ="yellow",
                  clab = "lg(No. of Iterations)", lighting = TRUE,shade = .6,alpha = .2, plot = FALSE)
        
  
  isosurf3D(x=xbase[xbase<0],y=ybase,z=zbase, colvar = log10(volumetric[xbase<0,,]), level = (1:2)/3*log10(maxIter),
            col =c("red", "blue"),
            clab = paste("lg(No. of Iterations);z<-z^",n,"+C"), lighting = TRUE,shade = .6,alpha = .2, plot = TRUE,add = TRUE)
  dev.off()
  
  png(paste("PlotY+",pi,".png"),width = 1600,height=900)
  isosurf3D(x=xbase,y=ybase[ybase>0],z=zbase, colvar = log10(volumetric[,ybase>0,]), level = log10(maxIter),
                  col ="yellow",
                  clab = "lg(No. of Iterations)", lighting = TRUE,shade = .6,alpha = .2, plot = FALSE)
        

  isosurf3D(x=xbase,y=ybase[ybase>0],z=zbase, colvar = log10(volumetric[,ybase>0,]), level = (1:2)/3*log10(maxIter),
            col =c("red", "blue"),
            clab = paste("lg(No. of Iterations);z<-z^",n,"+C"), lighting = TRUE,shade = .6,alpha = .2, plot = TRUE,add = TRUE)
  dev.off()
}



library(av)
#install.packages("av")
av_encode_video(paste("PlotZ-",pi[-51],".png"),"PlotZ-.mp4")
#install.packages("av")
av_encode_video(paste("PlotX-",pi[-51],".png"),"PlotX-.mp4")
#install.packages("av")
av_encode_video(paste("PlotY+",pi[-51],".png"),"PlotY+.mp4")



