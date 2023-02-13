
library(MCMCglmm)
library(parallel)

# PLEASE note, I am setting this up to use only 80% of your machine's total 
# logical processors. You can certainly harness all of your CPUs if you'd like,
# although I advise against doing so if any of your MCMC runs take more than a 
# few minutes. It also doesn't make sense to set the number of logical 
# processors to be greater than the number of runs (chains), but more on that
# later. Anyway, treat your silicon well!
setCores<-round(detectCores()*0.8) # use detectCores() by itself if you want all CPUs

# make the cluster
cl <- makeCluster(getOption("cl.cores",setCores))

#read in pedigree file. Note that you need to change the name of the file for each year
ped <- read.delim2("../Pedigrees/maturation_pedigree_2020.txt", header=T)

ped$ANIMAL<-as.factor(ped$ANIMAL)
ped$FATHER<-as.factor(ped$FATHER)
ped$MOTHER<-as.factor(ped$MOTHER)

#read in age at maturity data
Data <- read.delim2("AgeAtMaturity_data.txt", header=T)

#subset data file so that it matches the pedigree year you are analyzing
Data<-subset(Data, YearTagged=="2020")

Data <- Data[c("Animal", "TimeToRecovery", "Domestication_Index", "DamGenID", "SireGenID", "TempBump")]
colnames(Data) <- c("animal", "TimeToRecovery", "DI", "Dam", "Sire", "temp")
Data$animal<-as.factor(Data$animal)
Data$Sire<-as.factor(Data$Sire)
Data$Dam<-as.factor(Data$Dam)
Data$temp<-as.factor(Data$temp)
Data$DI<-as.numeric(Data$DI)
Data$TimeToRecovery<-as.numeric(Data$TimeToRecovery)

#set up priors
p.var<-var(Data$TimeToRecovery,na.rm=TRUE)

prior1.3<-list(G=list(G1=list(V=matrix(p.var/3),n=1),
                      G2=list(V=matrix(p.var/3),n=1)),
               R=list(V=matrix(p.var/3),n=1))

# load the MCMCglmm package within the cluster
cl.pkg <- clusterEvalQ(cl,library(MCMCglmm)) 
# import each object that's necessary to run the function
clusterExport(cl,"prior1.3")
clusterExport(cl,"Data")
clusterExport(cl,"ped")
# use parLapply() to execute 2 runs of MCMCglmm(), each with nitt=500000
model1.3_2runs<-parLapply(cl=cl, 1:2, function(i) {
  MCMCglmm(TimeToRecovery~temp,random=~animal+Dam,
           pedigree=ped,data=Data,
           nitt=500000,thin=250,burnin=50000,
           prior=prior1.3,verbose=TRUE)}
)

stopCluster(cl)

save(model1.3_2runs, file = "DS_hert_models_maturation_parallel_2020_2runs_fixedtemp.RData")



