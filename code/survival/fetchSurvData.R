
fetchSdat <- function(doSpp,speciesList,datadir,distWts){

  infile=paste(datadir,"/speciesData/",doSpp,"/survD.csv",sep="")
  survD=read.csv(file=infile)
  D1=survD[survD$allEdge==0,];
  D1$year <- D1$year
  D1$logarea=log(D1$area)
  D1$quad=as.character(D1$quad)
  
  # import neighbor data
  ringD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings.csv",sep=""))
  tmpD <- read.csv(paste(datadir,"/speciesData/",doSpp,"/",doSpp,"_nbhood_rings_allothers.csv",sep=""))
  ringD<-merge(ringD,tmpD)
  ringD$year<-ringD$year
  
  # merge D with ringD (D contains fewer rows)
  D1<-merge(D1,ringD,all.x=T,by.x=c("quad","year","trackID"),by.y=c("quad","year","genetID"))
  D1=D1[order(D1$X),]
  rm(ringD)
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
  D1 <- D1[,c("species","quad","year","trackID","age","logarea","survives","distEdgeMin","allEdge","QuadName","Grazing","Group")]
  D1 <- cbind(D1,W)
  
  return(D1)

}