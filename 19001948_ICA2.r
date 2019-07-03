################################
#     Exploratory Analysis     #
################################

#Read in data
anemia.data <- read.csv("AnemiaData.csv", header = TRUE)

#Set the unknown haemoglobin values to test.data
test.data <- anemia.data[anemia.data$Haemoglobin == -1, ]

#Set all haemoglobin -1 values to NA
anemia.data$Haemoglobin[anemia.data$Haemoglobin == -1] <- NA

#Omit the NA values so we do not need to deal with them
anemia.data <- na.omit(anemia.data)

#Based off literature, start looking at pregnant

#Create variable preghaemo to store the pregnant statistics for haemoglobin
preghaemo <- plot(anemia.data$Pregnant, anemia.data$Haemoglobin, 
     main = "Pregnant vs Haemoglobin", 
     xlab = "Pregnant", ylab = "Haemoglobin (g/dL)",
     ylim = c(0,25))

#Plot pregnant vs haemoglobin
plot(anemia.data$Pregnant, anemia.data$Haemoglobin, 
     main = "Pregnant vs Haemoglobin", 
     xlab = "Pregnant", ylab = "Haemoglobin (g/dL)",
     ylim = c(0,25))

#Clear relationship, reinforces scientific literature

#PLot density function of not pregnant women
plot(density(anemia.data$Haemoglobin[anemia.data$Pregnant == "No"]), 
     main = "Density of Not Pregnant Women")
#Comments: approximately symmetrical
abline(v=preghaemo$stats[1,1]) #minimum without outliers
abline(v=preghaemo$stats[2,1]) #25 percentile
abline(v=preghaemo$stats[3,1]) #median
abline(v=preghaemo$stats[4,1]) #75 percentile
abline(v=preghaemo$stats[5,1]) #max without outliers

#Find IQR of not pregnant women
notpregIQR <- preghaemo$stats[4,1] - preghaemo$stats[2,1]
#Find 1.5 IQR away from 25 percentile
lowernotpreg <- preghaemo$stats[2,1] - 1.5*notpregIQR
#Find 1.5 IQR away from 75 percentile
uppernotpreg <- preghaemo$stats[4,1] + 1.5*notpregIQR
#Create variable notpreg to contain the outliers(values lying outside the above ranges)
notpreg <- anemia.data[anemia.data$Haemoglobin < lowernotpreg &  anemia.data$Pregnant 
                       == "No"
                       |anemia.data$Haemoglobin >uppernotpreg & anemia.data$Pregnant 
                       == "No",]

#Plot density function of pregnant women
plot(density(anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes"]), 
     main = "Density of Pregnant Women")
#Comments: roughly symmetrical

abline(v=preghaemo$stats[1,2]) #minimum without outliers
abline(v=preghaemo$stats[2,2]) #25 percentile
abline(v=preghaemo$stats[3,2]) #median
abline(v=preghaemo$stats[4,2]) #75 percentile
abline(v=preghaemo$stats[5,2]) #max without outliers

#Find IQR of pregnant women
pregIQR <- preghaemo$stats[4,2] - preghaemo$stats[2,2]
#Find the 1.5*IQR away from 25 percentile
lowerpreg <- preghaemo$stats[2,2] - 1.5*pregIQR
#Find the 1.5*IQR away from 75 percentile
upperpreg <- preghaemo$stats[4,2] + 1.5*pregIQR
#Create a var preg to store the outliers(values outside the above ranges)
preg <- anemia.data[anemia.data$Haemoglobin < lowerpreg & anemia.data$Pregnant == "Yes"
                       | anemia.data$Haemoglobin > upperpreg & anemia.data$Pregnant 
                    == "Yes",]

#Combine the outliers of pregnant and not pregnant women together
pregnantoutlier <- rbind(preg, notpreg)

#Remove these outliers from the dataset
anemia.data <- anemia.data[-pregnantoutlier$ID,]
box(lwd=2)

dev.copy(png,"pregnant.png",width=8*72,height=6*72)
dev.off()
#Comments: Clear relationship between non-pregnant and pregnant women

#USED IN REPORT#
#Try pregnant and region
par(mfrow=c(3,3))
preg_region <- function(region){
  reg_preg <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                        anemia.data$Region == region]
  reg_npreg <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                         anemia.data$Region == region]
  boxplot(reg_preg, reg_npreg,  
          ylim = c(0,25), ylab = "Haemoglobin (g/dL)", 
          names = c("Pregnant", "Not Pregnant"),
          main = region
          )
}
lapply(unique(anemia.data$Region), FUN = preg_region)
dev.copy(png,"preg_region.png",width=8*72,height=6*72)
dev.off()

#Try pregnant and province
par(mfrow=c(2,2),mar=c(3,3,2,1), mgp=c(2,0.75,0))
preg_prov <- function(province){
  reg_preg <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                        anemia.data$Province == province]
  reg_npreg <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                         anemia.data$Province == province]
  boxplot(reg_preg, reg_npreg,  
          ylim = c(0,25), ylab = "Haemoglobin", 
          names = c("Pregnant", "Not Pregnant"),
          main = province
  )
}
lapply(unique(anemia.data$Province), FUN = preg_prov)

#Try pregnant and ethnic
par(mfrow=c(2,2),mar=c(3,3,2,1), mgp=c(2,0.75,0))
preg_eth <- function(ethnicity){
  reg_preg <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                        anemia.data$Ethnicity == ethnicity]
  reg_npreg <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                         anemia.data$Ethnicity == ethnicity]
  boxplot(reg_preg, reg_npreg,  
          ylim = c(0,25), ylab = "Haemoglobin", 
          names = c("Pregnant", "Not Pregnant"),
          main = ethnicity
  )
}
lapply(unique(anemia.data$Ethnicity), FUN = preg_eth)

#Plot sheep and haemoglobin
plot(anemia.data$Sheep, anemia.data$Haemoglobin, ylim = c(0,25),
     main = "Sheep and Haemoglobin", ylab = "Haemoglobin (g/dL)")

#Try sheep and province
par(mfrow=c(2,2),mar=c(3,3,2,1), mgp=c(2,0.75,0))
sheep_prov <- function(province){
  prov_sheep <- anemia.data$Haemoglobin[anemia.data$Sheep == "Yes" &
                                        anemia.data$Province == province]
  prov_nsheep <- anemia.data$Haemoglobin[anemia.data$Sheep == "No" &
                                         anemia.data$Province == province]
  boxplot(prov_sheep, prov_nsheep,  
          ylim = c(0,25), ylab = "Haemoglobin", 
          names = c("Sheep", "No Sheep"),
          main = province
  )
}
lapply(unique(anemia.data$Province), FUN = sheep_prov)

#Try pregnant and sheep with haemoglobin
pregsheep <- function(){
  pregsheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                              anemia.data$Sheep == "Yes"]
  pregnsheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                              anemia.data$Sheep == "No"]
  npregsheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                              anemia.data$Sheep == "Yes"]
  npregnsheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                              anemia.data$Sheep == "No"]
  boxplot(pregsheep, pregnsheep, npregsheep, npregnsheep, 
          ylim = c(0,25), ylab = "Haemoglobin (g/dL)", 
          names = c("PregSheep", "PregNSheep", "NPregSheep", "NPregNSheep"),
          main = "Pregnant and Sheep with Haemoglobin"
          )
}

par(mfrow=c(1,1))
pregsheep()

#USED IN REPORT#
#Try pregnant and recent birth with haemoglobin
pregrb <- function(){
  pregrb <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                         anemia.data$RecentBirth == "Yes"]
  pregnrb <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                          anemia.data$RecentBirth == "No"]
  npregrb <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                          anemia.data$RecentBirth == "Yes"]
  npregnrb <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                           anemia.data$RecentBirth == "No"]
  boxplot(pregrb, pregnrb, npregrb, npregnrb, 
          ylim = c(0,25), ylab = "Haemoglobin (g/dL)", 
          main = "Pregnant and Recent Birth with Haemoglobin",
          names = c("PregRecentBirth", "PregNoRecentBirth", "NPregRecentBirth", 
                    "NPregNoRecentBirth"), cex.axis = 0.7)
  box(lwd=2)
}
par(mfrow=c(1,1))
pregrb()
dev.copy(png,"preg_recentbirth.png",width=8*72,height=6*72)
dev.off()

#USED IN REPORT#
#Try graphing pregnant and rural with haemoglobin
preg_rural <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                        anemia.data$Rural == "Yes"]
preg_notrural <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                           anemia.data$Rural == "No"]
notpreg_rural <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                           anemia.data$Rural == "Yes"]
notpreg_notrural <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                              anemia.data$Rural == "No"]
boxplot(preg_rural, preg_notrural, notpreg_rural, notpreg_notrural, 
        ylim = c(0,25), ylab = "Haemoglobin (g/dL)", 
        names = c("Pregnant and Rural", "Pregnant and Not Rural", 
                  "Not Pregnant and Rural", "Not Pregnant and Not Rural"),
        main = "Pregnant and Rural Effects on Haemoglobin",
        cex.axis = 0.6)
box(lwd=2)
dev.copy(png,"preg_rural.png",width=12*72,height=8*72)
dev.off()
#Comments: Pregnant has greater effect on haemoglobin than being rural or not

#Try pregnant and region with rural
par(mfrow=c(1,1))
preg_region_rural <- function(region){
  reg_preg_rural <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                        anemia.data$Region == region &
                                          anemia.data$Rural == "Yes"]
  reg_preg_nrural <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                              anemia.data$Region == region &
                                              anemia.data$Rural == "No"]
  reg_npreg_rural <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                         anemia.data$Region == region
                                         & anemia.data$Rural == "Yes"]
  reg_npreg_nrural <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                               anemia.data$Region == region
                                             & anemia.data$Rural == "No"]
  boxplot(reg_preg_rural, reg_npreg_nrural, reg_npreg_rural, 
          reg_npreg_nrural, 
          ylim = c(0,25), ylab = "Haemoglobin", 
          names = c("PregRural", "PregNRural", "NPregRural", "NPregNRural"),
          main = region,
          xlab = "Pregnant or Rural")
}
lapply(unique(anemia.data$Region), FUN = preg_region_rural)

#Try pregnant and region with sheep
par(mfrow=c(1,1))
preg_region_sheep <- function(region){
  reg_preg_Sheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                              anemia.data$Region == region &
                                              anemia.data$Sheep == "Yes"]
  reg_preg_nSheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                               anemia.data$Region == region &
                                               anemia.data$Sheep == "No"]
  reg_npreg_Sheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                               anemia.data$Region == region
                                             & anemia.data$Sheep == "Yes"]
  reg_npreg_nSheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                                anemia.data$Region == region
                                              & anemia.data$Sheep == "No"]
  boxplot(reg_preg_Sheep, reg_npreg_nSheep, reg_npreg_Sheep, 
          reg_npreg_nSheep, 
          ylim = c(0,25), ylab = "Haemoglobin", 
          names = c("PregSheep", "PregNSheep", "NPregSheep", "NPregNSheep"),
          main = region)
}
lapply(unique(anemia.data$Region), FUN = preg_region_sheep)

#Do same as above but with province
preg_prov_sheep <- function(prov){
  prov_preg_Sheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                              anemia.data$Province == prov &
                                              anemia.data$Sheep == "Yes"]
  prov_preg_nSheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "Yes" &
                                               anemia.data$Province == prov &
                                               anemia.data$Sheep == "No"]
  prov_npreg_Sheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                               anemia.data$Province == prov
                                             & anemia.data$Sheep == "Yes"]
  prov_npreg_nSheep <- anemia.data$Haemoglobin[anemia.data$Pregnant == "No" &
                                                anemia.data$Province == prov
                                              & anemia.data$Sheep == "No"]
  boxplot(prov_preg_Sheep, prov_npreg_nSheep, prov_npreg_Sheep, 
          prov_npreg_nSheep, 
          ylim = c(0,25), ylab = "Haemoglobin", 
          names = c("PregSheep", "PregNSheep", "NPregSheep", "NPregNSheep"),
          main = prov)
}
lapply(unique(anemia.data$Province), FUN = preg_prov_sheep)

#combine cleanwater, treatedwater, electricity and toilet together to see effects
levels(anemia.data$CleanWater) <- c(0,0.1)
levels(anemia.data$TreatedWater) <- c(0,0.01)
levels(anemia.data$Electricity) <- c(0,0.001)
levels(anemia.data$Toilet) <- c(0,1)
anemia.data$Hygiene <- as.numeric(as.character(anemia.data$CleanWater)) + 
  as.numeric(as.character(anemia.data$TreatedWater)) +
  as.numeric(as.character(anemia.data$Electricity)) + 
  as.numeric(as.character(anemia.data$Toilet))

#USED IN REPORT#
#plot
par(mfrow=c(1,1))
boxplot(anemia.data$Haemoglobin[anemia.data$Hygiene == 0], 
        anemia.data$Haemoglobin[anemia.data$Hygiene == 0.001],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 0.1],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 0.101],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 0.110],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 0.111],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 1],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 1.001],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 1.1],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 1.101],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 1.11],
        anemia.data$Haemoglobin[anemia.data$Hygiene == 1.111],
        names=sort(unique(anemia.data$Hygiene)),
        ylim = c(5,20),
        xlab = "Hygiene Score",
        ylab = "Haemoglobin (g/dL)",
        main = "Hygiene vs Haemoglobin",
        cex.axis = 0.7)
dev.copy(png,"hygiene.png",width=8*72,height=6*72)
dev.off()

#comments: having toilet has very little effect, so lets create another column excluding it

anemia.data$Hygiene2 <- as.numeric(as.character(anemia.data$CleanWater)) + 
  as.numeric(as.character(anemia.data$TreatedWater)) +
  as.numeric(as.character(anemia.data$Electricity))
boxplot(anemia.data$Haemoglobin[anemia.data$Hygiene2 == 0], 
        anemia.data$Haemoglobin[anemia.data$Hygiene2 == 0.001],
        anemia.data$Haemoglobin[anemia.data$Hygiene2 == 0.1],
        anemia.data$Haemoglobin[anemia.data$Hygiene2 == 0.101],
        anemia.data$Haemoglobin[anemia.data$Hygiene2 == 0.110],
        anemia.data$Haemoglobin[anemia.data$Hygiene2 == 0.111],
        names=sort(unique(anemia.data$Hygiene2)),
        ylim = c(5,20),
        xlab = "Hygiene Score",
        ylab = "Haemoglobin",
        main = "Hygiene2 vs Haemoglobin",
        cex.axis = 0.7)

#Even worse (not as clear of an effect as with toilet), so we will keep toilet

######################################
#          Model-Building            #
######################################

#Try first model with all significant values from exploratory analysis
model1 <- lm(Haemoglobin ~ Pregnant + Rural + RecentBirth +
               Pregnant:Rural + Pregnant:RecentBirth + 
               Region + Ethnicity + Sheep + Pregnant:Sheep + Hygiene, data = anemia.data)
#Look at summary of model1
summary(model1)
#Look at diagnostics
par(mfrow=c(2,2))
plot(model1, which=1:4)
#Try again with province instead of region
model2 <- lm(Haemoglobin ~ Pregnant + Rural + RecentBirth +
               Pregnant:Rural + Pregnant:RecentBirth + 
               Province + Ethnicity + Sheep + Pregnant:Sheep + Hygiene, data = anemia.data)
summary(model2)
plot(model2, which=1:4)

#Remove interactions of rural and pregnant
model3 <- update(model2, .~. - Pregnant:Rural, data = anemia.data)
summary(model3)
plot(model3, which = 1:4)
anova(model3, model2, test = "F")
#Comments: anova test = "F" suggests simpler model is better and AIC of model3 is slightly
#lower than AIC of model2

#Now remove interaction of pregnant and sheep
model4 <- update(model3, .~. - Pregnant:Sheep, data = anemia.data)
summary(model4)
plot(model4, which = 1:4)
anova(model3, model4, test = "F")
#Similiar to previous

#Now remove hygiene and check results
model5 <- update(model4, .~. - Hygiene, data = anemia.data)
summary(model5)
plot(model5, which = 1:4)
anova(model4, model5, test = "F")
#similar to previous

#Now remove rural and compare
model6 <- update(model5, .~. - Rural, data = anemia.data)
summary(model6)
plot(model6, which = 1:4)
anova(model6, model5, test = "F")
AIC(model6)
#simpler model is better, ethnicity also doesnt look very significant

model7 <- update(model6, .~. - Ethnicity, data = anemia.data)
summary(model7)
plot(model7, which = 1:4)
AIC(model7)
anova(model6, model7, test = "F")
#although r^2 is lower, anova test suggests better without ethnicity but very low and AIC
#also only increases by 1, however ethnicity has clear impact on haemoglobin so we will 
#keep

model8 <- update(model6, .~. - Pregnant:RecentBirth, data = anemia.data)
summary(model8)
plot(model8, which = 1:4)
AIC(model8)
anova(model6, model8, test = "F")
#same as above


#pregnant, recentbirth, province, sheep, pregnant:province, 62, 3140, 3444, 3990

#these errors are with provinces with only one pregnant observation so we will cluster
model9 <- update(model6, .~. + Province:Pregnant, data = anemia.data)
summary(model9)
plot(model9, which=1:4)
table(anemia.data$Province, anemia.data$Pregnant)
#Pregnant is not significant anymore, maybe remove it?
model10 <- update(model9, .~. - Pregnant, data = anemia.data)
anova(model9, model10, test="F")
#After anova test, we need to keep pregnant

#Some provinces do not have pregnant women or 1 pregnant women
#so we will cluster and reduce number of provinces as the 
#interaction greatly improved the model
levels(anemia.data$Pregnant) <- c(0,1)
levels(anemia.data$RecentBirth) <- c(0,1)
levels(anemia.data$Sheep) <- c(0,1)

#Convert the above factors to numeric
anemia.data$Pregnant <- as.numeric(as.character(anemia.data$Pregnant))
anemia.data$RecentBirth <- as.numeric(as.character(anemia.data$RecentBirth))
anemia.data$Sheep <- as.numeric(as.character(anemia.data$Sheep))

#Do hierarchical clustering for province using continuous data
NumVars <- c(2,24,4,17) # Columns containing numeric covariates
ProvSummaries <- # Means & SDs of all numeric covariates
  aggregate(anemia.data[,NumVars], # for each ethnic group
            by=list(anemia.data$Province),
            FUN=function(x) c(Mean=mean(x), SD=sd(x)))
rownames(ProvSummaries) <- ProvSummaries[,1]
ProvSummaries <- scale(ProvSummaries[,-1]) # Standardise to mean 0 & SD 1
Distances <- dist(ProvSummaries, method = 'euclidean') # Pairwise distances
ClusTree <- hclust(Distances, method="complete") # Do the clustering
Cluster <- cutree(ClusTree, h=5.5)
par(mfrow=c(1,1))
plot(ClusTree, xlab="Province", ylab="Separation") 
abline(h=5.5, col = "red")
dev.copy(png,"cluster_province.png",width=8*72,height=6*72)
dev.off()
#doesnt look worth as we will have 5 provinces, previously we had 8 regions and model
#wasnt as good as linear model without interaction so we will drop it

#Look at AIC of the current best performing model without interaction of province and 
#pregnant
AIC(model6)
#try with region interaction
model12 <- lm(Haemoglobin ~ Pregnant + Region + RecentBirth + Region:Pregnant + Sheep +
                Ethnicity + Pregnant:RecentBirth, 
              data = anemia.data)
summary(model12)
AIC(model12)
#Not very good, we will keep the linear model

#FINAL model is model6, residuals vs fitted has constant variance, no clear structure
#QQ-plot is approxmiately linear so we will not consider GLM, scale-location also
#no clear structure and not very significant values on cook's distance to be worried about
par(mfrow=c(2,2))

#USED IN REPORT#
plot(model6, which=1:4)
dev.copy(png,"model6_diagnostics.png",width=8*72,height=6*72)
dev.off()

#Produce the prediction for the test data
haemoglobin.pred <- predict(model6, newdata=test.data, se.fit=TRUE) 
#variable to store the fitted values
haemo_pred_fit <- haemoglobin.pred$fit
#variable to store the standard errors of fitted values
haemo_pred_se <- haemoglobin.pred$se.fit
#combine them into one dataframe
haemo_pred <- cbind(haemo_pred_fit, haemo_pred_se)
#return the printed values
haemo_pred

#output them into file
#write.table(haemo_pred, "19001948_pred.dat", sep="\t", quote = FALSE, col.names = F)

