#### Delta smelt maturation timing code ####
#Code by Melanie E.F. LaCava


#### General notes ####

##Load packages
library(vioplot)
library(RColorBrewer)
display.brewer.all()
#Color gradient generator: https://colordesigner.io/gradient-generator

##DI color gradient
#blues <- brewer.pal(6,"Blues")[-c(1,2)]
#DI 1-3 = "#9ECAE1"   
#DI 3-5 = "#6BAED6"
#DI 5-7 = "#3182BD"
#DI 7-10 = "#08519C"
blues <- c("#80afce","#5383ae","#2c598e","#08306b")
barplot(1:4,col=blues)

##Year color gradient (from https://colordesigner.io/gradient-generator)
oranges <- c("#f9b068","#f9a458","#f99748","#f98a39","#fa7c2b","#fa6d1d",
             "#fa5b0f","#d44d0d","#b0400c","#8d330a","#6b2707")
barplot(1:11,col=oranges)





### Import data and add necessary columns ####

##### Genotyped fish (age at maturity data) ####
gen <- read.csv("~/GenotypedFish.csv",
                header=T, stringsAsFactors=F)
gen <- gen[gen$YearTagged!=2011,] #exclude 2011 when fish were tagged regardless of sexual maturity

gen$DateTagged <- as.Date(gen$DateTagged)
gen$BirthDate <- as.Date(gen$BirthDate)
gen$BirthYear <- as.numeric(format(gen$BirthDate,'%Y'))
gen$JulDayRecov <- as.POSIXlt(gen$DateTagged)$yday
gen$JulWeekRecov <- as.numeric(strftime(c(gen$DateTagged),format="%V"))
gen$JulDayBorn <- as.POSIXlt(gen$BirthDate)$yday

#add DI bins
range(gen$Domestication_Index) #1-9.6255
gen$DIbin <- NA
gen$DIbin[gen$Domestication_Index>=1 & gen$Domestication_Index<=3] <- "1-3"
gen$DIbin[gen$Domestication_Index>=3 & gen$Domestication_Index<=5] <- "3-5"
gen$DIbin[gen$Domestication_Index>=5 & gen$Domestication_Index<=7] <- "5-7"
gen$DIbin[gen$Domestication_Index>=7] <- "7-10"

#add temperature regime categorical variable
mfg25 <- read.csv("~/MFG25_FirstPC.csv")
gen$TempBump <- NA
for (i in 1:nrow(gen)){
  if (gen$BirthYear[i] %in% mfg25$SpawnYear){
    if (gen$PC[i]>=mfg25$FirstPCinMFG25[mfg25$SpawnYear==gen$BirthYear[i]]){
      gen$TempBump[i] <- 1 #1 = late season fish (MFG>=25)
    } else {
      gen$TempBump[i] <- 0 #0 = regular season fish (MFG<25)
    }
  }
}

#add year 0-11 (for regression intercepts to make more sense)
gen$ZeroYearTagged <- gen$YearTagged - 2010

#separate df with regular season fish only
gen.reg <- gen[gen$TempBump==0,]



##### Ripe fish #####
ripe <- read.csv("~/RipeFemales.csv",
                 header=T, stringsAsFactors=F)
ripe$DateChecked <- as.Date(ripe$DateChecked)
ripe$YearChecked <- as.numeric(format(ripe$DateChecked,'%Y'))
ripe$BirthDate <- as.Date(ripe$BirthDate)
ripe$JulDayRipe <- as.POSIXlt(ripe$DateChecked)$yday
ripe$JulWeekRipe <- as.numeric(strftime(c(ripe$DateChecked),format="%V"))
ripe$JulBirthDate <- as.POSIXlt(ripe$BirthDate)$yday
ripe$JulBirthWeek <- as.numeric(strftime(c(ripe$BirthDate),format="%V"))

#exclude 2011
ripe <- ripe[ripe$YearChecked!=2011,]

#add DI bins
range(ripe$Domestication_Index) #1-9.6255
ripe$DIbin <- NA
ripe$DIbin[ripe$Domestication_Index>=1 & ripe$Domestication_Index<=3] <- "1-3"
ripe$DIbin[ripe$Domestication_Index>=3 & ripe$Domestication_Index<=5] <- "3-5"
ripe$DIbin[ripe$Domestication_Index>=5 & ripe$Domestication_Index<=7] <- "5-7"
ripe$DIbin[ripe$Domestication_Index>=7] <- "7-10"

#add temperature regime categorical variable
mfg25 <- read.csv("~/MFG25_FirstPC.csv")
ripe$TempBump <- NA
for (i in 1:nrow(ripe)){
  if (ripe$BY[i] %in% mfg25$SpawnYear){
    if (ripe$PC[i]>=mfg25$FirstPCinMFG25[mfg25$SpawnYear==ripe$BY[i]]){
      ripe$TempBump[i] <- 1 #1 = late season fish (MFG>=25)
    } else {
      ripe$TempBump[i] <- 0 #0 = regular season fish (MFG<25)
    }
  }
}


#separate df with regular season fish only
ripe.reg <- ripe[ripe$TempBump==0,]




##### Pedigree ####
ped <- read.csv("~/MasterPedigree.csv",
                header=T, stringsAsFactors=F)
ped$JulWeekTagged <- as.numeric(strftime(ped$DateTagged,format="%V"))

#add DI bins of parents
range(ped$Domestication_Index) #0-9.5425
ped$DIbin <- NA
ped$DIbin[ped$Domestication_Index>=1 & ped$Domestication_Index<=3] <- "0-2"
ped$DIbin[ped$Domestication_Index>=3 & ped$Domestication_Index<=5] <- "2-4"
ped$DIbin[ped$Domestication_Index>=5 & ped$Domestication_Index<=7] <- "4-6"
ped$DIbin[ped$Domestication_Index>=7] <- "6-10"
table(ped$DIbin)

#add temperature regime categorical variable
ped$JulWeekSpawn <- as.numeric(strftime(c(as.Date(ped$DateSpawned)),format="%V"))
tail(ped)
class(ped$PC)==class(mfg25$FirstPCinMFG25)
class(ped$YearSpawned)==class(mfg25$SpawnYear)
ped$TempBump <- NA
for (i in 1:nrow(ped)){
  if (ped$YearSpawned[i] %in% mfg25$SpawnYear){
    if (ped$PC[i]>=mfg25$FirstPCinMFG25[mfg25$SpawnYear==ped$YearSpawned[i]]){
      ped$TempBump[i] <- 1
    } else {
      ped$TempBump[i] <- 0
    }
  }
}

#separate df with regular season fish only
ped.reg <- ped[ped$TempBump==0,]




##### Heritability data ####

herit <- read.table("~/heritability_Vp_results_combined_nitt500thousand_fixedtemp.txt",header=T)





#### Age at maturity ~ year ####


##### 2010, 2012-2021 ####
cor(gen$TimeToRecovery,gen$ZeroYearTagged)
#0.1249304
summary(lm(TimeToRecovery~ZeroYearTagged,data=gen))
# Age at maturity = 0.20(year) + 49.59, R2=0.01561, p=1.528757e-85
# Call:
#   lm(formula = TimeToRecovery ~ ZeroYearTagged, data = gen)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -16.4483  -3.5953  -0.2712   3.6520  17.2852 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    49.59111    0.07406  669.56   <2e-16 ***
#   ZeroYearTagged  0.20468    0.01040   19.68   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.184 on 24423 degrees of freedom
# Multiple R-squared:  0.01561,	Adjusted R-squared:  0.01557 
# F-statistic: 387.2 on 1 and 24423 DF,  p-value: < 2.2e-16

summary(lm(TimeToRecovery~ZeroYearTagged,data=gen))$coefficients
# Estimate Std. Error   t value     Pr(>|t|)
# (Intercept)    49.5911129 0.07406475 669.56433 0.000000e+00
# ZeroYearTagged  0.2046849 0.01040165  19.67812 1.528757e-85


#overall median age at maturity
range(gen$TimeToRecovery) #33.14286 67.28571
median(gen$TimeToRecovery) #50.57143
sd(gen$TimeToRecovery) #5.224968

#median age at maturity by year
agg.y <- aggregate(TimeToRecovery ~ YearTagged,data=gen,FUN=median)
for (i in 1:nrow(agg.y)){
  agg.y$StDv[i] <- sd(gen$TimeToRecovery[gen$YearTagged==agg.y$YearTagged[i]])
  agg.y$n[i] <- nrow(gen[gen$YearTagged==agg.y$YearTagged[i],])
}
agg.y
#Manuscript table S2
#    YearTagged TimeToRecovery     StDv    n
# 1        2010       46.85714 7.241722 1351
# 2        2012       52.00000 6.297365 2049
# 3        2013       48.85714 5.101218 2037
# 4        2014       50.71429 4.480746 2234
# 5        2015       50.71429 3.978859 1875
# 6        2016       49.57143 4.905602 2430
# 7        2017       50.00000 5.183287 2687
# 8        2018       51.42857 5.099970 2307
# 9        2019       50.85714 4.157648 2327
# 10       2020       52.14286 5.749565 2216
# 11       2021       51.42857 3.992619 2912





##### 2012-2021 ####

#Report regression stats in manuscript to show without 2010 lessens slope, but still negative
cor(gen$TimeToRecovery[gen$YearTagged %in% 2012:2021],gen$ZeroYearTagged[gen$YearTagged %in% 2012:2021])
#0.06582267
summary(lm(TimeToRecovery~ZeroYearTagged,data=gen[gen$YearTagged %in% 2012:2021,]))
#Age at maturity = 0.12(year) + 50.31,R2=0.004333, p=1.393466e-23
# Call:
#   lm(formula = TimeToRecovery ~ ZeroYearTagged, data = gen[gen$YearTagged %in% 
#                                                              2012:2021, ])
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -14.1066  -3.5074  -0.3404   3.5724  16.7505 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    50.30515    0.08410  598.16   <2e-16 ***
#   ZeroYearTagged  0.11502    0.01148   10.02   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.01 on 23072 degrees of freedom
# Multiple R-squared:  0.004333,	Adjusted R-squared:  0.004289 
# F-statistic: 100.4 on 1 and 23072 DF,  p-value: < 2.2e-16

summary(lm(TimeToRecovery~ZeroYearTagged,data=gen[gen$YearTagged %in% 2012:2021,]))$coefficients
#                 Estimate Std. Error   t value     Pr(>|t|)
#(Intercept)    50.3051517 0.08409994 598.15916 0.000000e+00
#ZeroYearTagged  0.1150248 0.01147970  10.01984 1.393466e-23





#### Age at maturity ~ year (by DI bin) ####
#regular season fish only

#sample sizes to calc median (manuscript table S4)
table(gen.reg$YearTagged,gen.reg$DIbin)
#       1-3  3-5  5-7 7-10
# 2010 1106    0    0    0
# 2012  117 1396    0    0
# 2013   30 1427   17    0
# 2014   22  270 1235    0
# 2015   51  415 1231    0
# 2016   27  113 1264  379
# 2017   56  294  992 1079
# 2018   85  290  465 1110
# 2019   37  106  684 1272
# 2020    1  142  566 1364
# 2021   19  311  254 1688



#median age at maturity ~ year, by DI bin (manuscript figure 2b)
agg <- aggregate(TimeToRecovery ~ DIbin + YearTagged,data=gen.reg,FUN=median)
for (i in 1:nrow(agg)){
  agg$n[i] <- nrow(gen.reg[gen.reg$DIbin==agg$DIbin[i] & gen.reg$YearTagged==agg$YearTagged[i],])
}
agg <- agg[agg$n >=10,] #remove year/DI combos where there are <10 samples

plot(TimeToRecovery ~ YearTagged,data=agg[agg$DIbin=="1-3",],pch=16,cex=1.3,col=blues[1],
     ylim=c(46,56),xaxt="n",xlab="",ylab="Median age at maturity (weeks)")
axis(side=1,at=c(2010,2012:2021),labels=c(2010,2012:2021),tick=F,padj=0,cex.axis=0.9,line=-1)
lines(TimeToRecovery ~ YearTagged,data=agg[agg$DIbin=="1-3",],col=blues[1],lwd=3)
points(TimeToRecovery ~ YearTagged,data=agg[agg$DIbin=="3-5",],pch=16,col=blues[2])
lines(TimeToRecovery ~ YearTagged,data=agg[agg$DIbin=="3-5",],col=blues[2],lwd=3)
points(TimeToRecovery ~ YearTagged,data=agg[agg$DIbin=="5-7",],pch=16,col=blues[3])
lines(TimeToRecovery ~ YearTagged,data=agg[agg$DIbin=="5-7",],col=blues[3],lwd=3)
points(TimeToRecovery ~ YearTagged,data=agg[agg$DIbin=="7-10",],pch=16,col=blues[4])
lines(TimeToRecovery ~ YearTagged,data=agg[agg$DIbin=="7-10",],col=blues[4],lwd=3)
abline(h=52,lty=2)
legend("bottomright",legend=c("DI 1-3","DI 3-5","DI 5-7","DI 7-10","52 weeks"),lty=c(1,1,1,1,2),
       col=c(blues,"black"),cex=0.8,lwd=c(3,3,3,3,1))





#### Age at maturity ~ DI ####
#regular season only

cor(gen.reg$TimeToRecovery,gen.reg$Domestication_Index)
#0.08059134

#Manuscript figure 2a
plot(TimeToRecovery ~ Domestication_Index,data=gen.reg,pch=16,col="darkgray",
     xlab="Domestication index",ylab="Age at maturity (weeks)",
     ylim=c(35,max(gen.reg$TimeToRecovery)))
abline(h=52,lty=2)
abline(lm(TimeToRecovery ~ Domestication_Index,data=gen.reg),lwd=2)
legend("bottomright",legend=c("regression","52 weeks"),lty=c(1,2),lwd=c(2,1),cex=0.8)

summary(lm(TimeToRecovery ~ Domestication_Index,data=gen.reg))
summary(lm(TimeToRecovery ~ Domestication_Index,data=gen.reg))$coefficients[8]
#Age at maturity = 0.21(DI) + 50.41,R2=0.006495,p=4.641241e-30
# Call:
#   lm(formula = TimeToRecovery ~ Domestication_Index, data = gen.reg)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -14.4542  -3.6863  -0.2292   3.6453  16.1665 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)         50.41252    0.11495  438.56   <2e-16 ***
#   Domestication_Index  0.20938    0.01835   11.41   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.043 on 19913 degrees of freedom
# Multiple R-squared:  0.006495,	Adjusted R-squared:  0.006445 
# F-statistic: 130.2 on 1 and 19913 DF,  p-value: < 2.2e-16

summary(lm(TimeToRecovery ~ Domestication_Index,data=gen.reg))$coefficients
#                      Estimate Std. Error   t value     Pr(>|t|)
#(Intercept)         50.4125227 0.11495081 438.55735 0.000000e+00
#Domestication_Index  0.2093821 0.01835134  11.40963 4.641241e-30



#### Age at maturity ~ day born * DI ####

#Quick comparison of univariate/multivariate models
summary(lm(TimeToRecovery~JulDayBorn*Domestication_Index,data=gen.reg))
summary(lm(TimeToRecovery~JulDayBorn,data=gen.reg))
summary(lm(TimeToRecovery~Domestication_Index,data=gen.reg))
AIC(lm(TimeToRecovery~JulDayBorn*Domestication_Index,data=gen.reg)) #117925.6
AIC(lm(TimeToRecovery~JulDayBorn+Domestication_Index,data=gen.reg)) #118017.5
# Multivar model with interaction term performs best, proceed with this model

#Model results
summary(lm(TimeToRecovery~JulDayBorn*Domestication_Index,data=gen.reg))
# Age at maturity = -0.13(day egg fertilized) - 0.57(DI) + 0.0069(day egg fertilized)(DI) + 61.32
x<-summary(lm(TimeToRecovery~JulDayBorn*Domestication_Index,data=gen.reg))$fstatistic
pf(x[1],x[2],x[3],lower.tail=F) #p=0

# Call:
# lm(formula = TimeToRecovery ~ JulDayBorn * Domestication_Index, 
#    data = gen.reg)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.7735 -3.8749 -0.5409  3.9781 11.5172 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                    61.3196862  0.3633049 168.783   <2e-16 ***
#   JulDayBorn                     -0.1317073  0.0045598 -28.884   <2e-16 ***
#   Domestication_Index            -0.5724045  0.0545732 -10.489   <2e-16 ***
#   JulDayBorn:Domestication_Index  0.0068865  0.0007102   9.697   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.672 on 19911 degrees of freedom
# Multiple R-squared:  0.1472,	Adjusted R-squared:  0.147 
# F-statistic:  1145 on 3 and 19911 DF,  p-value: < 2.2e-16


#p values
summary(lm(TimeToRecovery~JulDayBorn*Domestication_Index,data=gen.reg))$coefficients
#                                   Estimate   Std. Error    t value      Pr(>|t|)
#(Intercept)                    61.319686182 0.3633049198 168.782978  0.000000e+00
#JulDayBorn                     -0.131707313 0.0045598268 -28.884280 9.465414e-180
#Domestication_Index            -0.572404483 0.0545732259 -10.488742  1.135346e-25
#JulDayBorn:Domestication_Index  0.006886523 0.0007101757   9.696929  3.478854e-22



#### Age at maturity ~ day born (regression for each DI bin) ####
#regular season only, all years combined (manuscript figure 3)

plot(TimeToRecovery ~ JulDayBorn, data=gen.reg,pch=16,col="darkgray",xaxt="n",
     xlab="Julian birthday",ylab="Age at maturity (weeks)")
axis(side=1,at=seq(30,130,10),labels=seq(30,130,10))
c<-1
for (i in sort(unique(gen.reg$DIbin))){
  abline(lm(TimeToRecovery ~ JulDayBorn, data=gen.reg[gen.reg$DIbin==i,]),
         col=blues[c],lwd=3)
  c<-c+1
}
abline(h=52,lty=2)
legend("topright",legend=c("DI 1-3","DI 3-5","DI 5-7","DI 7-10","52 weeks"),col=c(blues,"black"),ncol=1,
       lty=c(1,1,1,1,2),lwd=c(3,3,3,3,1),cex=0.8,inset=c(-0.0001,-0.01))





##### Number of ripe females ~ spawn week (by year) ####

#manuscript figure 4
plot(density(ripe.reg$JulWeekRipe[ripe.reg$YearChecked==2010],adjust=1.5),
     xlim=c(1,29),ylim=c(0,0.15),col=oranges[1],
     main="",xlab="Julian week ripe",ylab="Density of ripe females",lwd=4)
c<-2
for (i in sort(unique(ripe.reg$YearChecked))[-1]){
  lines(density(ripe.reg$JulWeekRipe[ripe.reg$YearChecked==i],adjust=1.5),
        col=oranges[c],lwd=4)
  c<-c+1
}
legend("topright",legend=sort(unique(ripe.reg$YearChecked)),col=oranges,lty=1,lwd=4,cex=0.9,ncol=2)






#### Offspring recovered ~ week mature + DI (years combined) ####
#Regular season fish only (since we're looking at DI/selection, so remove plasticity associated with temp regime)
#2011-2020 parents only (exclud 2009 with no week tagged info and 2010 because their offspring
# were tagged regardless of sexual maturity in 2011)

#Choose best model
AIC(lm(Recovery_WithMorts~JulWeekTagged,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,])) #24465.57
AIC(lm(Recovery_WithMorts~Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,])) #27447.74
AIC(lm(Recovery_WithMorts~JulWeekTagged+Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,])) #24325.88 lowest AIC
AIC(lm(Recovery_WithMorts~JulWeekTagged*Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,])) #24327.43

summary(lm(Recovery_WithMorts~JulWeekTagged+Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,]))
#Offspring recovered = 0.03(week mature) + 1.21(DI) + 4.34
x<-summary(lm(Recovery_WithMorts~JulWeekTagged+Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,]))$fstatistic
pf(x[1],x[2],x[3],lower.tail=F) #p=1.774061e-31 
# Call:
#   lm(formula = Recovery_WithMorts ~ JulWeekTagged + Domestication_Index, 
#      data = ped.reg[ped.reg$YearSpawned %in% 2011:2020, ])
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -14.881  -6.981  -2.509   4.036 103.160 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)          3.13069    0.78855   3.970 7.34e-05 ***
#   JulWeekTagged        0.03006    0.06055   0.497     0.62    
# Domestication_Index  1.21257    0.10081  12.029  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 10.23 on 3245 degrees of freedom
# (452 observations deleted due to missingness)
# Multiple R-squared:  0.0427,	Adjusted R-squared:  0.04211 
# F-statistic: 72.37 on 2 and 3245 DF,  p-value: < 2.2e-16
summary(lm(Recovery_WithMorts~JulWeekTagged+Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,]))$coefficients
#                      Estimate Std. Error    t value     Pr(>|t|)
#(Intercept)         4.34326479 0.70815643  6.1331996 9.651723e-10
#JulWeekTagged       0.03006308 0.06054528  0.4965387 6.195480e-01
#Domestication_Index 1.21257281 0.10080565 12.0288180 1.227374e-32




#### Fitness ~ week mature (by DI bin) ####
#regular season only
#2011-2020 parents only

#manuscript figure 5b
par(mar=c(5.1,4.1,1,6)) #c(bottom, left, top, right)
plot(Recovery_WithMorts~jitter(JulWeekTagged),data=ped.reg[ped.reg$YearSpawned %in% 2011:2020 & ped.reg$Recovery_WithMorts<80,],
     pch=16,col="gray",xaxt="n",
     xlab="Week of spawning season parents tagged as mature",
     ylab="Number of offspring recovered")
axis(side=1,at=3:16,label=F,padj=0,cex.axis=1)
axis(side=1,at=seq(4,16,2),labels=seq(4,16,2),tick=F,padj=0,cex.axis=1)
c<-1
for (i in sort(unique(ped.reg$DIbin))){
  abline(lm(Recovery_WithMorts~JulWeekTagged,data=ped.reg[ped.reg$DIbin==i & ped.reg$YearSpawned %in% 2011:2020,]),
         col=blues[c],lwd=3)
  c<-c+1
}
par(xpd=T)
legend("right",inset=c(-0.3,0),title="DI group",legend=sort(unique(ped.reg$DIbin)),
       col=blues,lty=1,lwd=4,cex=0.8,ncol=1)
par(mar=c(5.1,4.1,4.1,2.1),xpd=F) #c(bottom, left, top, right)



#### Fitness ~ age at maturity + DI (years combined) ####
#Regular season fish only (since we're looking at DI/selection, so remove plasticity associated with temp regime)
#2011-2020 parents only (exclud 2009 with no week tagged info and 2010 because their offspring
# were tagged regardless of sexual maturity in 2011)

#Choose best model
AIC(lm(Recovery_WithMorts~TimeToRecovery,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,])) #24465.06
AIC(lm(Recovery_WithMorts~Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,])) #27447.74
AIC(lm(Recovery_WithMorts~TimeToRecovery+Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,])) #24321.19
AIC(lm(Recovery_WithMorts~TimeToRecovery*Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,])) #24320.38 lowest AIC

summary(lm(Recovery_WithMorts~TimeToRecovery*Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,]))
#Offspring recovered = 0.11(age at maturity) + 3.21(DI) - 0.04(age at maturity*DI) - 0.74
x<-summary(lm(Recovery_WithMorts~TimeToRecovery*Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,]))$fstatistic
pf(x[1],x[2],x[3],lower.tail=F) #4.145694e-32
# Call:
#   lm(formula = Recovery_WithMorts ~ TimeToRecovery * Domestication_Index,
#      data = ped.reg[ped.reg$YearSpawned %in% 2011:2020, ])
#
# Residuals:
#   Min      1Q  Median      3Q     Max
# -15.193  -6.934  -2.523   4.057 104.051
#
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)                        -0.73761    6.30641  -0.117  0.90690
# TimeToRecovery                      0.10931    0.13287   0.823  0.41075
# Domestication_Index                 3.21437    1.18493   2.713  0.00671 **
#   TimeToRecovery:Domestication_Index -0.04157    0.02481  -1.675  0.09394 .
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 10.22 on 3244 degrees of freedom
# (452 observations deleted due to missingness)
# Multiple R-squared:  0.04491,    Adjusted R-squared:  0.04403
# F-statistic: 50.85 on 3 and 3244 DF,  p-value: < 2.2e-16
summary(lm(Recovery_WithMorts~TimeToRecovery*Domestication_Index,data=ped.reg[ped.reg$YearSpawned %in% 2011:2020,]))$coefficients
#                                      Estimate Std. Error    t value    Pr(>|t|)
#(Intercept)                        -0.73760858 6.30640615 -0.1169618 0.906897569
#TimeToRecovery                      0.10930778 0.13286836  0.8226773 0.410752007
#Domestication_Index                 3.21437006 1.18493025  2.7127082 0.006708814
#TimeToRecovery:Domestication_Index -0.04156638 0.02480889 -1.6754632 0.093939767




#### Fitness ~ age at maturity (by DI bin) ####
#regular season only
#2011-2020 parents only

#manuscript figure 5a
par(mar=c(5.1,4.1,1,6)) #c(bottom, left, top, right)
plot(Recovery_WithMorts~jitter(TimeToRecovery),data=ped.reg[ped.reg$YearSpawned %in% 2011:2020 & ped.reg$Recovery_WithMorts<80,],
     pch=16,col="gray",
     xlab="Age at maturity (weeks)",
     ylab="Number of offspring surviving to maturity")
c<-1
for (i in sort(unique(ped.reg$DIbin))){
  abline(lm(Recovery_WithMorts~TimeToRecovery,data=ped.reg[ped.reg$DIbin==i & ped.reg$YearSpawned %in% 2011:2020,]),
         col=blues[c],lwd=4)
  c<-c+1
}
par(xpd=T)
legend("right",inset=c(-0.3,0),title="Parent DI",legend=sort(unique(ped.reg$DIbin)),
       col=blues,lty=1,lwd=4,cex=0.8,ncol=1)
par(mar=c(5.1,4.1,4.1,2.1),xpd=F) #c(bottom, left, top, right)




#### Compound manuscript figure 1 ####

pdf("~/Fig1_3VarsOverTime.pdf")
par(mfrow=c(3,1),mar=c(1,4.5,0,1),oma=c(2,0,1,0)) #c(bottom, left, top, right)

#panel 1: violin plot, age at maturity ~ time
vioplot(TimeToRecovery ~ YearTagged,data=gen[gen$YearTagged!=2011,],
        #at=seq(1,length(table(gen$YearTagged[gen$YearTagged!=2011]))*1,by=1),
        h=1,lineCol=NA,rectCol=NA,pchMed=18,colMed="black",col="gray",
        xaxt="n",xlab="",ylab="",cex=1.5,cex.axis=1.2)
title(ylab="Age at maturity (weeks)",cex.lab=1.5)
abline(h=52,lty=2)
legend("bottomright",legend="52 weeks",lty=2, bty="n",cex=1.1)

#panel 2: scatterplot, phenotypic variance ~ time
# plot against 1:11 to avoid gap at 2011
plot(1:11,herit$Var[herit$Effect=="PhenotypicVariance"],pch=16,
     ylim=c(min(herit$lowerCI[herit$Effect=="PhenotypicVariance"]),max(herit$upperCI[herit$Effect=="PhenotypicVariance"])),
     xaxt="n",xlab="",ylab="",cex=1.5,cex.axis=1.2)
title(ylab="Phenotypic Variance",cex.lab=1.5)
arrows(1:11,herit$upperCI[herit$Effect=="PhenotypicVariance"],
       1:11,herit$lowerCI[herit$Effect=="PhenotypicVariance"],
       length=0,angle=90,code=3)

#panel 3: scatterplot, heritability ~ time
plot(1:11,herit$Var[herit$Effect=="Heritability"],pch=16,
     ylim=c(min(herit$lowerCI[herit$Effect=="Heritability"]),max(herit$upperCI[herit$Effect=="Heritability"])),
     xaxt="n",xlab="",ylab="",cex=1.5,cex.axis=1.2)
title(ylab="Heritability",cex.lab=1.5)
arrows(1:11,herit$upperCI[herit$Effect=="Heritability"],
       1:11,herit$lowerCI[herit$Effect=="Heritability"],
       length=0,angle=90,code=3)
axis(side=1,at=1:11,labels=c(2010,2012:2021),tick=F,padj=0,cex.axis=1.5)

dev.off()
#par(mfrow=c(1,1),mar=c(5,4,4,2),oma=c(0,0,0,0)) #mar=c(bottom,left,top,right)



#### Temperature regime plot ####

#manuscript figure S1
plot(1:24,c(rep(16.5,18),rep(12,6)),type="l",xaxt="n",
     xlab="",ylab="",col="#FC8D62",lwd=3,cex.axis=0.8)
lines(1:24,c(rep(16.4,14),rep(12,4),rep(16.4,4),rep(12.1,2)),
      col="#1B9E77",lwd=3)
legend("bottomleft",legend=c("regular season fish","late season fish"),
       col=c("#FC8D62","#1B9E77"),lty=1,lwd=3,bty="n",cex=0.7)
title(ylab="Temperature (C)",line=2.5,cex.lab=0.8)
title(xlab="Temperature regime over course of year life span",line=0.5,cex.lab=0.8)



#### Range of tagging dates ####

#manuscript figure S2
season <- data.frame(year=sort(unique(gen$YearTagged)))
for (i in 1:nrow(season)){
  season$start.tag[i] <- as.character(sort(gen$DateTagged[gen$YearTagged==season$year[i]])[1])
  season$end.tag[i] <- as.character(sort(gen$DateTagged[gen$YearTagged==season$year[i]],decreasing=T)[1])
  season$start.ripe[i] <- as.character(sort(ripe$DateChecked[ripe$YearChecked==(season$year[i])])[1])
  season$end.ripe[i] <- as.character(sort(ripe$DateChecked[ripe$YearChecked==(season$year[i])],decreasing=T)[1])
}
season
season$s.tag <- as.POSIXlt(as.Date(season$start.tag))$yday
season$e.tag <- as.POSIXlt(as.Date(season$end.tag))$yday
season$s.ripe <- as.POSIXlt(as.Date(season$start.ripe))$yday
season$e.ripe <- as.POSIXlt(as.Date(season$end.ripe))$yday
season

#plot recovery dates
plot(c(season$s.tag[1],season$e.tag[1]),c(season$year[1],season$year[1]),
     #type="l", lwd=2,
     pch="I",
     xlab="Range of days fish tagged as mature",ylab="",
     xlim=c(0,max(season$e.tag)),ylim=range(season$year),xaxt="n",yaxt="n")
lines(c(season$s.tag[1],season$e.tag[1]),c(season$year[1],season$year[1]))
axis(side=1,at=c(0,30,60,90,120,150),labels=c("Jan","Feb","Mar","Apr","May","June"),
     hadj=0,padj=0)
axis(side=2,at=2010:2021,labels=2010:2021,las=1,hadj=0.9)
for (i in 2:nrow(season)){
  points(c(season$s.tag[i],season$e.tag[i]),c(season$year[i],season$year[i]),pch="I")
  lines(c(season$s.tag[i],season$e.tag[i]),c(season$year[i],season$year[i]))
}



#### Age at maturity ~ year (by temp regime) ####

#manuscript figure S3
plot(TimeToRecovery~jitter(YearTagged),data=gen[gen$TempBump==0,],col=rgb(252/255,141/255,98/255,alpha=0.1),pch=16,
     xlab="",ylab="Age at maturity (weeks)",xaxt="n",ylim=c(35,70))
axis(side=1,at=c(2010,2012:2021),labels=c(2010,2012:2021),tick=F,padj=-1.5)
abline(lm(TimeToRecovery~YearTagged,data=gen[gen$TempBump==0,]),lwd=2,col=rgb(252/255,141/255,98/255))
points(TimeToRecovery~jitter(YearTagged),data=gen[gen$TempBump==1,],col=rgb(27/255,158/255,119/255,alpha=0.1),pch=16)
abline(lm(TimeToRecovery~YearTagged,data=gen[gen$TempBump==1,]),lwd=2,col=rgb(27/255,158/255,119/255))
legend("topleft",legend=c("regular season","late season"),bty="n",cex=0.9,
       pch=16,col=c(rgb(252/255,141/255,98/255),rgb(27/255,158/255,119/255)))
