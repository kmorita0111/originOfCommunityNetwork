#==========================================================================================
# this simulation is conducted for considering an initial transition of community assembly
# from view of network science 
#==========================================================================================

###--- parameters ---###
num <- 11
sumOfPatch <- num*num 
numOfRow <- 1:num
numOfLine <- 1:num

generation <- 100

patch <- matrix(0, num, num) 
graphics_patch <- matrix(NA, num, num) 
interaction <- matrix(0, generation, generation)

###--- working directory ---###
parePath = "~/originOfCommunityNetwork"

###--- load library ---###
library(maps)
library(fields)

###--- simulation ---###
for( t in 1:generation){
  
  # immigration 
  for( i in 1:sumOfPatch ){
    
    # number chosen ramdamly from uniform distribution from 1 to num
    nRow <- as.integer( runif(1, min = 1, max = (num+1)) ) 
    nLine <- as.integer( runif(1, min = 1, max = (num+1)) ) 
      
    if( patch[nRow,nLine] <= 0 ){
      patch[nRow,nLine] <- t
      break
    }
  }
  
  # searching extinct patches  
  extinctPatch <- which(graphics_patch > 0.5, arr.ind=TRUE)
  patch[extinctPatch[,1], extinctPatch[,2]] <- 0 
  graphics_patch[extinctPatch[,1], extinctPatch[,2]] <- NA  
  
  # searching filled patches  
  filledPatch <- which(patch > 0, arr.ind=TRUE)
  #print( filledPatch )
  numOffilledPatch <- nrow(filledPatch)
  
  for( k in 1:numOffilledPatch ){
    
    sp <- patch[filledPatch[k,1], filledPatch[k,2]] 
    sumInter <- 0
    
    # creating an intraspecific interaction
    if( interaction[sp, sp] <= 0 ){
      interaction[sp, sp] <- 1
    }
    
    # creating species interactions 
    for( i in 1:3 ){
      
      rowInter <- filledPatch[k,1]-2+i
      if( rowInter > 0 &&  rowInter <= num ){
        
        for( j in 1:3 ){
          
          lineInter <- filledPatch[k,2]-2+j
          if( lineInter > 0 &&  lineInter <= num ){
            
            anotherSp <- patch[rowInter, lineInter]
            if( anotherSp > 0 ){
              interaction[sp, anotherSp] <- 0.5
              interaction[anotherSp, sp] <- 0.5
              
              sumInter <- sumInter + 1
            }
          }
        }
      }
      
      graphics_patch[filledPatch[k,1], filledPatch[k,2]] <- sumInter/9
    } 
  }
  
  # graphics 
  png( paste( paste( parePath, paste("patch", formatC(t,width=6,flag="0"), sep="_"), sep="/result_patch/" ), "png", sep="."), 
       width=6, height=5, units = "in", res=300 
  )
  par(mar=c(5.5, 6.0, 4, 2)) # margin
  
  # originalParetto <- 
  #image.plot( graphics_patch, col= originalParetto(9) )
  
  par(ps=20)
  
  xAxis<-num-1
  yAxis<-num-1 # yMax
  colnames <- paste0("", c(seq(1, num, by=((num-1)/xAxis) )) )
  rownames <- paste0("", c(seq(1, num, by=((num-1)/yAxis) )) )
  
  #image( graphics_patch, col=rainbow(25), axes=F, 
  #       xlab="x", ylab="y", main=paste("Generation t =", formatC(t,width=6,flag="0") )
  #      ) #redblue() doesn't work on my computer.
  #axis(side=2,at=1:num,labels=rownames)
  #axis(side=1,at=1:num,labels=colnames)
  #image.plot(sample, legend.only=T)
  
  image.plot( graphics_patch, zlim=c(0,1), # xaxt="n", yaxt="n", 
              xlab="x", ylab="y", main=paste("Generation t =", formatC(t,width=6,flag="0") ) 
            )
  # filledPatch <- which(graphics_patch > 0, arr.ind=TRUE)
  for ( i in 1:nrow(filledPatch) ) {
    text( (filledPatch[i,1]-1)/(num-1), (filledPatch[i,2]-1)/(num-1), as.character(patch[filledPatch[i,1],filledPatch[i,2]]),
          col="white", font=2
    )  # This is dummy, but required for drawing axis
  }
  #axis(side=2,at=1:num,labels=rownames)
  #axis(side=1,at=1:num,labels=colnames)
  
  dev.off()
  
}
print( "simulation completed!" )
