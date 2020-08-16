volumetric  = readRDS("Mandelbulb.RDS")
library(plot3D)
library(misc3d)
dim(volumetric)
    
    
zbase <- ybase <- xbase <- seq(from=-1.8,to=1.8,by=.005)# 

system.time({
  png("Mandelbulb2.png",width = 1600,height=900)
  isosurf3D(x=xbase,y=ybase,z=zbase, colvar = volumetric, level = c(2,100,200),
            col = c("red", "blue", "yellow"),
            clab = "No. of Iterations", alpha = 0.4, plot = TRUE)
  dev.off()
  
})



for i 