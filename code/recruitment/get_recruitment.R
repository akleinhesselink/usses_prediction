# rm(list = ls()) 
# library(dplyr)
# library(tidyr)
# library(stringr)

sppList=c("ARTR","HECO","POSE","PSSP")
outfile="recruit_params_m1.csv"

dataDir1 <- paste("lib","/driversdata/data/idaho/speciesData/",sep="")
dataDir2 <- paste("lib","/driversdata/data/idaho_modern/speciesData/",sep="")
#--------------------------------------------------------

# import old data--------------------------------------------------------
Nspp=length(sppList)
for(i in 1:Nspp){
  infile1=paste(dataDir1,sppList[i],"/recArea.csv",sep="")
  tmpD=read.csv(infile1)
  tmpD=tmpD[,c("QuadName","quad","year","NRquad","totParea","Group")]
  names(tmpD)[4]=paste("R.",sppList[i],sep="")
  names(tmpD)[5]=paste("cov.",sppList[i],sep="")
  if(i==1){
    D=tmpD
  }else{
    D=merge(D,tmpD,all=T)
  }
}

D[is.na(D)]=0  # replace missing values 
D$year <- D$year+ 1900

D$Treatment <- "Control"
D$Period <- 'Historical'

# import modern data--------------------------------------------------------
Nspp=length(sppList)
for(i in 1:Nspp){
  infile1=paste(dataDir2,sppList[i],"/recArea.csv",sep="")
  tmpD=read.csv(infile1)
  tmpD=tmpD[,c("QuadName","quad","year","NRquad","totParea","Group")]
  names(tmpD)[4]=paste("R.",sppList[i],sep="")
  names(tmpD)[5]=paste("cov.",sppList[i],sep="")
  if(i==1){
    D2=tmpD
  }else{
    D2=merge(D2,tmpD,all=T)
  }
}
D2[is.na(D2)]=0  # replace missing values 
D2$Period <- 'Modern'

# merge in treatment data
tmp <- read.csv(paste(dataDir2,"/quad_info.csv",sep=""))
tmp <- tmp[,c("quad","Treatment")]
D2 <- merge(D2,tmp, all.x=T)

# combine old and new data
D=rbind(D,D2)
rm(D2)


# get rid of removal treatments
D <- subset(D,Treatment!="No_grass" & Treatment!="No_shrub")

aggregate(data = D, cov.ARTR ~ Period, 'mean')
aggregate(data = D, cov.PSSP ~ Period, 'mean')
aggregate(data = D, cov.HECO ~ Period, 'max')
aggregate(data = D, cov.POSE ~ Period, 'median')

# clean up removal treatment data
# ii <- which(D$year>=2011 & D$Treatment=="No_shrub")
# D$cov.ARTR[ii] <- 0
# D$R.ARTR[ii] <- NA # don't try to estimate ARTR recruitment in these plots
# ii <- which(D$year>=2011 & D$Treatment=="No_grass")
# D$cov.HECO[ii] <- 0 ; D$cov.POSE[ii] <- 0 ; D$cov.PSSP[ii] <- 0
# D$R.HECO[ii] <- NA ; D$R.POSE[ii] <- NA ; D$R.PSSP[ii] <- NA # don't try to estimate grass recruitment in these plots

# calculate mean cover by group and year
tmpD=subset(D,Treatment=="Control") # only use control plots
tmpD=D[,c("quad","year","Group",paste("cov.",sppList,sep=""))]
tmpD=aggregate(tmpD[,4:NCOL(tmpD)],by=list("year"=tmpD$year,
  "Group"=tmpD$Group),FUN=mean)
names(tmpD)[3:NCOL(tmpD)]=paste("Gcov.",sppList,sep="")
D=merge(D,tmpD,all.x=T)

# assign indicator variables -------------------------------------------------------------------------------- # 
D$Treatment2 <- D$Treatment
D$Treatment2[D$year>2000] <- "Modern"
D$Treatment3 <- D$Treatment
D$Treatment3[D$Treatment=="Control" & D$year>2000] <- "ControlModern"
#D$Treatment[ D$year < 2012 & D$Treatment %in% c('Drought', 'Irrigation') ] <- 'Control'  # set initial treatment to control

# assign group level zeros to smallest non-zero value 
historical_Gcov <- D[ D$Period == 'Historical', grep ( '^Gcov\\.', names(D) ) ]
min_cover <- min(historical_Gcov[ historical_Gcov > 0 ] )
D[ ,grep ( '^Gcov\\.', names(D) )][D[, grep ( '^Gcov\\.', names(D) )] == 0 ] <- min_cover

D <- D %>% gather(species, Y, R.ARTR, R.HECO, R.POSE, R.PSSP)
D$species <- str_replace(D$species, pattern = '^R.', replacement = '') 

D <- split(D, D$species )

for(i in 1:length(D)){
  write_csv(D[[i]], file.path('data', 'temp_data', paste(names(D)[i], 'recruitment.csv', sep = '_')))
}


# # set up data objects for bugs  
# y=as.matrix(D[,c(paste("R.",sppList,sep=""))])
# R.tot=rowSums(y)
# parents1=as.matrix(D[,c(paste("cov.",sppList,sep=""))])/100
# parents2=as.matrix(D[,c(paste("Gcov.",sppList,sep=""))])/100
# year=as.numeric(as.factor(D$year))
# Nyrs=length(unique(D$year))
# N=dim(D)[1]
# Nspp=length(sppList)
# Group=as.numeric(D$Group)
# Ngroups=length(unique(Group))
# # code treatment specific intercepts: 1 = old and modern control, 2 = no shrub, 3 = no grass
# TreatCode=matrix(1,length(y),Nspp)
# tmp=which(D$Treatment=="No_shrub")
# TreatCode[tmp,2:4]=2
# tmp=which(D$Treatment=="No_grass")
# TreatCode[tmp,1]=3
# test=data.frame(D$year,D$Treatment,TreatCode)
# 
# # # plots
# # pdf("recruit_data.pdf",height=6,width=8)
# # par(mfrow=c(1,2),tcl=-0.2,mgp=c(2,0.5,0),mar=c(3,3,3,1))
# # wts=c(0.6,1,0.65,0.9)
# # for(i in 1:Nspp){
# #  plot(parents1[,i],y[,i],xlab="Local parents (% cover)",ylab="Recruits",main=sppList[i],pch=1,col=year)
# #  trueparents=wts[1]*parents1[,i]+(1-wts[1])*parents2[,i]
# #  plot(trueparents,y[,i],xlab="Mixed parents (% cover)",ylab="Recruits",main=sppList[i],pch=1,col=year)
# # }
# # dev.off()
# 
# 
# # fit as negative binomial with random effects in WinBUGS
# 
# data=list("N","y","parents1","parents2",
#   "year","Nyrs","Nspp","Ngroups","Group","TreatCode")
# 
# inits=list(1)
# inits[[1]]=list(intcpt.yr=matrix(1,Nyrs,Nspp),intcpt.mu=rep(1,Nspp),intcpt.tau=rep(1,Nspp),
#   intcpt.trt=rbind(rep(NA,4),matrix(1,2,Nspp)),
#   intcpt.gr=matrix(1,Ngroups,Nspp),g.tau=rep(1,Nspp),
#   dd=matrix(0,Nspp,Nspp),theta=rep(1,Nspp)) 
# inits[[2]]=list(intcpt.yr=matrix(0,Nyrs,Nspp),intcpt.mu=rep(0,Nspp),intcpt.tau=rep(10,Nspp),
#   intcpt.trt=rbind(rep(NA,4),matrix(1,2,Nspp)),
#   intcpt.gr=matrix(0,Ngroups,Nspp),g.tau=rep(0.1,Nspp),
#   dd=matrix(0,Nspp,Nspp),theta=rep(2,Nspp))
#   
# params=c("intcpt.yr","intcpt.mu","intcpt.tau","intcpt.trt",
#   "intcpt.gr","g.tau","dd","theta","u","lambda") 
# 
# out=bugs(data,inits,params,
#   model.file="bugs-m1.txt",
#   n.chains=2,
#   n.iter=20000,
#   n.burnin=10000,
#   #n.iter=40000,
#   #n.burnin=20000,
#   n.thin=10, 
#   debug=F,DIC=T,bugs.directory="c:/WinBUGS14/")  
#    
# tmp=grep("lambda",row.names(out$summary))
# A=row.names(out$summary)[tmp]
# B=out$summary[tmp,1]
# lambda=matrix(NA,dim(y)[1],Nspp)
# C=paste(A,"<-",B,sep="")
# eval(parse(n=length(A),text=C))
# lambda[is.na(lambda)]=0
# par(mfrow=c(2,2))
# for(i in 1:Nspp){
#   plot(y[,i],lambda[,i],xlab="Obs",ylab="Pred",main=sppList[i])
# }
# par(mfrow=c(2,2))
# for(i in 1:Nspp){
#   plot(parents1[,i],lambda[,i],xlab="Obs",ylab="Pred",main=sppList[i])
# }
# 
# write.table(out$summary,outfile,row.names=T,sep=",")
