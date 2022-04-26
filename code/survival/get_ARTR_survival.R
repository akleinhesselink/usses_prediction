# PBA March 2016

# call from removal_analaysis_wrapper.r

#########################################
#  1. Import data and calculate W's
#########################################

doSpp <- "ARTR"
sppList <- c("ARTR","HECO","POSE","PSSP","allcov","allpts")
dataDir1 <- paste("lib","/driversdata/data/idaho",sep="")
dataDir2 <- paste("lib","/driversdata/data/idaho_modern",sep="")
nonCompLength.s=5 #Number of columns in SppData that are not measures of competitors 

# set up distance weights------------------------------------------------

dists <- read.csv(paste(dataDir2,"/speciesData/IdahoModDistanceWeights_noExptl.csv",sep=""));
dists$allcov <- rowMeans(dists[,1:4])  # for "other" polygons use average of big 4
dists$allpts <- dists$POSE  # set forb dist wts = smallest grass (POSE)

# import old data

D1 <- fetchSdat(doSpp=doSpp,speciesList=sppList,datadir=dataDir1,distWts=dists)
D1$Treatment <- "Control"
D1$Period <- "Historical"

# import modern data--------------------------------------------------------
D2 <- fetchSdat(doSpp=doSpp,speciesList=sppList,datadir=dataDir2,distWts=dists)
D2$Period <- "Modern"

# merge in treatment data
tmp <- read.csv(paste(dataDir2,"/quad_info.csv",sep=""))
tmp <- tmp[,c("quad","Treatment")]
D2 <- merge(D2,tmp, all.x=T)

# account for removal in baseline years
if(doSpp!="ARTR"){
  ii <- which(D2$year>=2011 & D2$Treatment=="No_shrub")
  D2$W.ARTR[ii] <- 0
}else{
  ii <- which(D2$year>=2011 & D2$Treatment=="No_grass")
  D2$W.HECO[ii] <- 0 ; D2$W.POSE[ii] <- 0 ; D2$W.PSSP[ii] <- 0
}

# combine old and modern
allD <- rbind(D1,D2)
rm(D1,D2,tmp)

# clean up dataset ----------------------------------------------
allD$year[allD$year<2000] <- allD$year[allD$year<2000] + 1900

if(doSpp=="ARTR"){
  keep <- which(is.element(allD$Treatment,c("Control","No_grass", "Irrigation", "Drought")))
}else{
  keep <- which(is.element(allD$Treatment,c("Control","No_shrub", "Irrigation", "Drought")))
}
allD <- allD[keep,]

# remove outliers 
tmp=which(allD$quad=="Q23" & allD$year==1945 & allD$trackID==67)
tmp=c(tmp,which(allD$quad=="Q12" & allD$year==1955 & allD$trackID==25))
tmp=c(tmp,which(allD$quad=="Q26" & allD$year==1945 & allD$trackID==73))
tmp=c(tmp, which(allD$Grazing == 'G' & allD$year == 1931))      # remove grazed plots in 1931 big decrease in size 
allD=allD[-tmp,]


# set up indicator variables 
allD$Treatment2 <- allD$Treatment
allD$Treatment2[allD$year>2000] <- "Modern"
allD$Treatment3 <- allD$Treatment
allD$Treatment3[allD$Treatment=="Control" & allD$year>2000] <- "ControlModern"
#allD$Treatment[ allD$year < 2012 & allD$Treatment %in% c('Drought', 'Irrigation') ] <- 'Control'  # set initial treatment to control

# ----------- use this data for prediction ------------------------------------------------------------------------------

write_csv(allD, 'data/temp_data/ARTR_survival.csv') 

# -----------------------------------------------------------------------------------------------------------------------
