library("TDAstats") 
library("neuRosim") 
library("TDA") 


hippo <- read.csv("~/hippo_mask.csv")



hippo <- hippo[order(hippo[,2]),]



hippo <- head(hippo,1386)


for(a in c(2,5,10,20)){
  
  for(b in c(2,5,10,20)){
    
    for(c in c(1,3,5,7,15)){
      
      
      design1 <- simprepTemporal(totaltime=240, onsets=seq(1,240,40),
                                 
                                 durations=20, TR=2, effectsize=b, hrf="double-gamma")
      
      region_sphere <- simprepSpatial(regions=1, coord=c(43,24,33),
                                      
                                      radius=c, form="sphere", fading=TRUE) 
      
      
      
      out <- simVOLfmri(design=design1, image=region_sphere, dim=c(60,60,60),
                        
                        SNR=a, noise="physiological")
      
      
      
      pc_sphere <- list()
      
      
      
      for(i in 1:120){
        
        mat <- matrix(nrow=1386, ncol=4)
        
        for(j in 1:1386){
          
          mat[j,] <- cbind(hippo[j,1],hippo[j,2],hippo[j,3],out[hippo[j,1],hippo[j,2],hippo[j,3],i])
          
        }
        
        
        
        pc_sphere[[i]] <- mat
        
        
        
      }
    }
  }
}
