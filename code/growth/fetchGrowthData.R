
fetchGdat2 <- function(doSpp,speciesList,datadir,distWts){

  sizeD <- read_csv( paste0(datadir,"/speciesData/",doSpp, '/', doSpp, '_genet_xy.csv' ))
  D1 <- sizeD[sizeD$allEdge==0,]
  D1$X <- 1:nrow(D1)
  
  # import neighbor data
  ringD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings.csv",sep=""))
  tmpD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings_allothers.csv",sep=""))
  ringD<-merge(ringD,tmpD)
  
  # merge D with ringD (D contains fewer rows)
  D1 <- merge(D1, ringD, by.x=c("quad","year","trackID"),by.y=c("quad","year","genetID"))
  D1 <- D1[order(D1$X),]
  rm(ringD,sizeD)
  row.names(D1) <- NULL  
  
  # calculate W's (MAKE SURE NOT TO REORDER D!)
  W <- matrix(NA, NROW(D1), length(speciesList))
  colnames(W) <- paste("W.",speciesList,sep="")

  # do big 4
  for(iSpp in 1:4){
    neighborCols=which(substr(names(D1),1,4)==speciesList[iSpp]) # pull out annulus data for the focal species 
    dist_wts <- distWts[,paste0(speciesList[iSpp])]
    C <- data.matrix(D1[,neighborCols]) #matrix of conspecific areas in the annuli 
    W[,iSpp] <- C%*%dist_wts 
  }
  
  # do allcov and allpts
  for(iSpp in 5:6){
    neighborCols=which(substr(names(D1),1,6)==speciesList[iSpp]) # pull out annulus data for the focal species 
    dist_wts <- distWts[,paste0(speciesList[iSpp])]
    C <- data.matrix(D1[,neighborCols]) #matrix of conspecific areas in the annuli 
    W[,iSpp] <- C%*%dist_wts 
  }
  
  #format
  D1 <- D1[,c("quad","year","trackID","age","allEdge", "area")]
  D1 <- cbind(D1,W)
  
  return(D1)

}



fetchGdat <- function(doSpp,speciesList,datadir,distWts){
  
  growDfile=paste(datadir,"/speciesData/",doSpp,"/growDnoNA.csv",sep="")
  growD=read.csv(file=growDfile)
  D1=growD[growD$allEdge==0,];
  D1$year <- D1$year
  D1$logarea.t0=log(D1$area.t0)
  D1$logarea.t1=log(D1$area.t1)
  D1$quad=as.character(D1$quad)
  
  # import neighbor data
  ringD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings.csv",sep=""))
  tmpD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings_allothers.csv",sep=""))
  ringD<-merge(ringD,tmpD)
  ringD$year<-ringD$year
  
  # merge D with ringD (D contains fewer rows)
  D1<-merge(D1,ringD,by.x=c("quad","year","trackID"),by.y=c("quad","year","genetID"))
  D1=D1[order(D1$X),]
  rm(ringD,growD)
  row.names(D1) <- NULL  
  
  # calculate W's (MAKE SURE NOT TO REORDER D!)
  W <- matrix(NA,NROW(D1),length(speciesList))
  colnames(W) <- paste("W.",speciesList,sep="")
  
  # do big 4
  for(iSpp in 1:4){
    neighborCols=which(substr(names(D1),1,4)==speciesList[iSpp]) # pull out annulus data for the focal species 
    dist_wts <- distWts[,paste0(speciesList[iSpp])]
    C <- data.matrix(D1[,neighborCols]) #matrix of conspecific areas in the annuli 
    W[,iSpp] <- C%*%dist_wts 
  }
  
  # do allcov and allpts
  for(iSpp in 5:6){
    neighborCols=which(substr(names(D1),1,6)==speciesList[iSpp]) # pull out annulus data for the focal species 
    dist_wts <- distWts[,paste0(speciesList[iSpp])]
    C <- data.matrix(D1[,neighborCols]) #matrix of conspecific areas in the annuli 
    W[,iSpp] <- C%*%dist_wts 
  }
  
  #format
  D1 <- D1[,c("quad","year","trackID","age","distEdgeMin","allEdge","QuadName","Grazing","Group","area.t0","logarea.t0","logarea.t1","species")]
  D1 <- cbind(D1,W)
  
  return(D1)
  
}