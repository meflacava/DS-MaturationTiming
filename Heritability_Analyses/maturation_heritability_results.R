##still looks like some autocorrelation in years 2016 and 2017, does the thinning need to be higher?
#https://github.com/tmalsburg/MCMCglmm-intro
#https://juliengamartin.github.io/wam_tuto/univariate-animal-model.html
#https://github.com/matthewwolak/wolakR

library(MCMCglmm)
library("boa")
library("coda")
library(devtools)
#install.packages("MCMCpack")
#install_github("matthewwolak/wolakR")
library(wolakR)
library(ggplot2)

setwd("~/UCDavis/FCCL/Hert_maturation/FARM_files")


#load in data that was run on HPC. We proivde results from the year 2010 as an example
load("DS_hert_models_maturation_parallel_2010_2runs_fixedtemp.RData")

#renaming model for simplicity down the line
model <- model1.3_2runs


################check if fixed effect of temp should be included in model or not

head(model[[1]]$Sol) #check how model named variable
posterior.mode(model[[1]]$Sol[,"temp1"]) #get variance
HPDinterval(model[[1]]$Sol[,"temp1"]) #if intervals cross 0 then shouldn't be included in model


#####calculate genetic variance (Va) and 95% confidence intervals
posterior.mode(model1.2[[1]]$VCV)
HPDinterval(model1.2[[1]]$VCV)

####calculate total phenotypic variance (Vp) and 95% confidence intervals
posterior.mode((model[[1]]$VCV[,"animal"]+model[[1]]$VCV[,"Dam"]+model[[1]]$VCV[,"units"]))
HPDinterval((model[[1]]$VCV[,"animal"]+model[[1]]$VCV[,"Dam"]+model[[1]]$VCV[,"units"]),0.95)




#looking at just first run
windows()
plot(model[[1]]$Sol)
windows()
plot(model[[1]]$VCV)
autocorr(model[[1]]$VCV)

#compare multiple runs plotted as diff colors
m6 <- lapply(model1.2, function(m) m$VCV)
m7 <- do.call(mcmc.list, m6)
windows()
gelman.plot(m7, auto.layout=F)
gelman.diag(m7)
windows()
plot(m7, col = c('red', 'blue')) #figure out density plots of multiple chains
summary(m7)

#check if heritability results are the same within the two chains run, and among the models with diff burnins and number of iterations
posterior.heritability.1<-model[[1]]$VCV[,"animal"]/(model[[1]]$VCV[,"animal"]+model[[1]]$VCV[,"Dam"]+model[[1]]$VCV[,"units"])
posterior.mode(posterior.heritability.1)
HPDinterval(posterior.heritability.1,0.95)

posterior.heritability.2<-model[[2]]$VCV[,"animal"]/(model[[2]]$VCV[,"animal"]+model[[2]]$VCV[,"Dam"]+model[[2]]$VCV[,"units"])
posterior.mode(posterior.heritability.2)
HPDinterval(posterior.heritability.2,0.95)


#check maternal effects
posterior.maternal.1<-model[[1]]$VCV[,"Dam"]/(model[[1]]$VCV[,"animal"]+model[[1]]$VCV[,"Dam"]+model[[1]]$VCV[,"units"])
posterior.mode(posterior.maternal.1)
HPDinterval(posterior.maternal.1,0.95)


#check maternal effects
posterior.maternal.1<-model1.2[[2]]$VCV[,"Dam"]/(model1.2[[2]]$VCV[,"animal"]+model1.2[[2]]$VCV[,"Dam"]+model1.2[[2]]$VCV[,"units"])
posterior.mode(posterior.maternal.1)
HPDinterval(posterior.maternal.1,0.95)



##############################################
#Heritability figures for manuscripts
#############################################


##Combined Vp and heritability results to plot
dat <- read.delim2("heritability_Vp_results_combined_nitt500thousand_fixedtemp.txt", header = T)


dat$Year <- as.factor(dat$Year)
dat$Effect <- as.factor(dat$Effect)
dat$Effect  <- factor(dat$Effect, levels=c("PhenotypicVariance", "Heritability"))
dat$Var <- as.numeric(dat$Var)
dat$lowerCI <- as.numeric(dat$lowerCI)
dat$upperCI <- as.numeric(dat$upperCI)


windows()
ggplot(data=dat, aes(x=Year, y=Var)) +
  geom_point(stat="identity", position=position_dodge(0.5), size=3) +
  geom_errorbar(data=dat, aes(ymin=lowerCI, ymax=upperCI), width=0, position=position_dodge(0.5)) +
  #scale_color_manual(values=c("black", "grey70", "blue", "deepskyblue1")) +
  labs(y="", x="", fill="") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        axis.text = element_text(size=14), axis.title = element_text(size=14), legend.text = element_text(size=14), axis.text.x=element_text(angle = 90),
        strip.background = element_blank(), strip.placement = "outside", strip.text = element_text(size=14),
        panel.border = element_rect(color = "black", fill=NA)) +
  facet_grid(Effect ~ ., scales = "free_y",
             switch = "y", 
             labeller = as_labeller(
               c(PhenotypicVariance = "Phenotypic Variance", Heritability = "Heritability")))


