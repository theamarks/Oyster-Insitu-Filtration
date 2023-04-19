
# packages with version control
library(groundhog)
groundhog.library("sqldf", "2020-03-01")
#Read in data

#1. Gill Area Measurements.csv
#Contains information on 17 Gigas and Lorida Oysters.  
#Relevant information is physiology of the animals.

#2. Frequency of beads among individuals_equal volume_FINAL.csv
#Not the same individuals as in Gill Area Measurement
#Each row is number of particles of a given size eaten byu 
#a given animal.  Ignore 'prob' variable
#we don't remember what it does.
#Equal volume was total volume was the same among
#bead sizes.  


#3. Frequency of beads among individuals_equal concentration_FINAL.csv
#Not the same individuals as in Gill Area Measurement
#Each row is number of particles of a given size eaten byu 
#a given animal.  Ignore 'prob' variable
#we don't remember what it does.
#Equal concentration was frequency of bead sizes was the same among

#4. Particle Size Preference_equal concentration_20um bead dominating volume.csv
#Raw data for concentration?  Ask Matt why this data doesnt generate dataset 3?

data1 = read.csv("./Data/gray_data/Gill_Area_Measurements.csv",
                 h=T, check.names = F)
data2 = read.csv("./Data/gray_data/Frequency_of_beads_among_individuals_equal_volume_FINAL.csv", 
                 h=T, check.names = F)
data3 = read.csv("./Data/gray_data/Frequency_of_beads_among_individuals_equal_concentration_FINAL.csv",
                 h=T, check.names = F)

#Analysis of data1.

Results = data.frame(Variable = c("Tww (g)","Shell Height (mm)","Gill Area (um)","Area (um^2)","Dry Tissue Weight (g)"),
                     Lurida_mean = apply(data1[data1$Species=="O. lurida",3:7],2,mean),
                     Lurida_sd = apply(data1[data1$Species=="O. lurida",3:7],2,sd),
                     Lurida_n = rep(length(data1$Id[data1$Species=="O. lurida"]),5),
                     Gigas_mean = apply(data1[data1$Species=="C. gigas",3:7],2,mean),
                     Gigas_sd = apply(data1[data1$Species=="C. gigas",3:7],2,sd),
                     Gigas_n = rep(length(data1$Id[data1$Species=="C. gigas"]),5),
                     t = rep(0,5),
                     pval = rep(0,5))

Results$t = (Results$Lurida_mean - Results$Gigas_mean)/sqrt((Results$Lurida_sd^2/Results$Lurida_n) + (Results$Gigas_sd^2/Results$Gigas_n))
Results$pval = pt(-abs(Results$t),Results$Lurida_n[1] + Results$Gigas_n[1] - 2)*2

#Conclusion.  With limited data scope, can't really conclude that these species have noticibly 
#different physiology.

#Analysis of data2.

data2 = data2[data2$Level!="Total",]
data2$mass = as.numeric(data2$Level)*data2$Count
data2a = sqldf("select Species, Individual, sum(mass) as mass from data2 group by Species, Individual")


Results2 = data.frame(Species = c("O. lurida","C. gigas","Control"),
                      Mean.Freq.3 = c(sum(data2$Count[data2$Species=="O. lurida" & data2$Level == 3])/6,
                                      sum(data2$Count[data2$Species=="C. gigas" & data2$Level == 3])/6,
                                      sum(data2$Count[data2$Species=="Control" & data2$Level == 3])),									
                      Mean.Freq.6 = c(sum(data2$Count[data2$Species=="O. lurida" & data2$Level == 6])/6,
                                      sum(data2$Count[data2$Species=="C. gigas" & data2$Level == 6])/6,
                                      sum(data2$Count[data2$Species=="Control" & data2$Level == 6])),									
                      Mean.Freq.12 = c(sum(data2$Count[data2$Species=="O. lurida" & data2$Level == 10])/6,
                                       sum(data2$Count[data2$Species=="C. gigas" & data2$Level == 10])/6,
                                       sum(data2$Count[data2$Species=="Control" & data2$Level == 10])),									
                      Mean.Freq.20 = c(sum(data2$Count[data2$Species=="O. lurida" & data2$Level == 20])/6,
                                       sum(data2$Count[data2$Species=="C. gigas" & data2$Level == 20])/6,
                                       sum(data2$Count[data2$Species=="Control" & data2$Level == 20])),									
                      Mean.Mass = c(mean(data2a$mass[data2a$Species=="O. lurida"]),
                                    mean(data2a$mass[data2a$Species=="C. gigas"]),
                                    sum(data2a$mass[data2a$Species=="Control"])),
                      SD.Mass = c(sd(data2a$mass[data2a$Species=="O. lurida"]),
                                  sd(data2a$mass[data2a$Species=="C. gigas"]),
                                  sd(data2a$mass[data2a$Species=="Control"])),
                      
                      n.Mass = c(length(data2a$mass[data2a$Species=="O. lurida"]),
                                 length(data2a$mass[data2a$Species=="C. gigas"]),
                                 length(data2a$mass[data2a$Species=="Control"])))

t.num = Results2$Mean.Mass[1] - Results2$Mean.Mass[2]
t.den = sqrt((Results2$SD.Mass[1]^2/Results2$n.Mass[1]) + (Results2$SD.Mass[2]^2/Results2$n.Mass[2]))
t = t.num/t.den
pval = 2*pt(-abs(t),10)
#pval .045.  Mass consumption is different for O. Lurida and C. Gigas.

#What about pelle selectiont.  Weighted chi square test.
#Total sample size for chi-sq observed table will be number of pellets
#Row proportions however will be by overall mass (hence the weighted)
#Observed table is row total (pellets) times row proportions (weight distribution)
#I'm 90% sure this is the right way to deal with this problem.

Lurida.prop.a = Results2[1,2:5]*c(3,6,10,20)
Lurida.prop = Lurida.prop.a/sum(Lurida.prop.a)
Gigas.prop.a = Results2[2,2:5]*c(3,6,10,20)
Gigas.prop = Gigas.prop.a/sum(Gigas.prop.a)
n.Lurida = sum(data2$Count[data2$Species=="O. lurida"])
n.Gigas = sum(data2$Count[data2$Species=="C. gigas"])

observed = matrix(as.numeric(c(Lurida.prop*n.Lurida,Gigas.prop*n.Gigas)),ncol=4,byrow=T)
expected = matrix(0,nrow=2,ncol=4)
for(i in 1:2){
  for(j in 1:4){
    expected[i,j] = sum(observed[i,])*sum(observed[,j])/sum(observed)
  }
}

chi.sq = sum((observed - expected)^2/expected)
pval = 1-pchisq(chi.sq,3)
#pval is 0.  Pellet selection habits appear to be different.



#Analysis of data3.

data3 = data3[data3$Level!="Total",]
data3$mass = as.numeric(data3$Level)*data3$Count
data3a = sqldf("select Species, Individual, sum(mass) as mass from data3 group by Species, Individual")


Results3 = data.frame(Species = c("O. lurida","C. gigas","Suspension"),
                      Mean.Freq.3 = c(sum(data3$Count[data3$Species=="O. lurida" & data3$Level == 3])/6,
                                      sum(data3$Count[data3$Species=="C. gigas" & data3$Level == 3])/6,
                                      sum(data3$Count[data3$Species=="Suspension" & data3$Level == 3]))/7,									
                      Mean.Freq.6 = c(sum(data3$Count[data3$Species=="O. lurida" & data3$Level == 6])/6,
                                      sum(data3$Count[data3$Species=="C. gigas" & data3$Level == 6])/6,
                                      sum(data3$Count[data3$Species=="Suspension" & data3$Level == 6]))/7,									
                      Mean.Freq.12 = c(sum(data3$Count[data3$Species=="O. lurida" & data3$Level == 10])/6,
                                       sum(data3$Count[data3$Species=="C. gigas" & data3$Level == 10])/6,
                                       sum(data3$Count[data3$Species=="Suspension" & data3$Level == 10]))/7,									
                      Mean.Freq.20 = c(sum(data3$Count[data3$Species=="O. lurida" & data3$Level == 20])/6,
                                       sum(data3$Count[data3$Species=="C. gigas" & data3$Level == 20])/6,
                                       sum(data3$Count[data3$Species=="Suspension" & data3$Level == 20]))/7,									
                      Mean.Mass = c(mean(data3a$mass[data3a$Species=="O. lurida"]),
                                    mean(data3a$mass[data3a$Species=="C. gigas"]),
                                    sum(data3a$mass[data3a$Species=="Suspension"])),
                      SD.Mass = c(sd(data3a$mass[data3a$Species=="O. lurida"]),
                                  sd(data3a$mass[data3a$Species=="C. gigas"]),
                                  sd(data3a$mass[data3a$Species=="Suspension"])),
                      
                      n.Mass = c(length(data3a$mass[data3a$Species=="O. lurida"]),
                                 length(data3a$mass[data3a$Species=="C. gigas"]),
                                 length(data3a$mass[data3a$Species=="Suspension"])))

t.num = Results3$Mean.Mass[1] - Results3$Mean.Mass[2]
t.den = sqrt((Results3$SD.Mass[1]^2/Results3$n.Mass[1]) + (Results3$SD.Mass[2]^2/Results3$n.Mass[2]))
t = t.num/t.den
pval = 2*pt(-abs(t),10)
#pval .76.  Mass consumption is may not different for O. Lurida and C. Gigas.
#Why are they eating so much more in this second experiment?

#What about pelle selectiont.  Weighted chi square test.
#Total sample size for chi-sq observed table will be number of pellets
#Row proportions however will be by overall mass (hence the weighted)
#Observed table is row total (pellets) times row proportions (weight distribution)
#I'm 90% sure this is the right way to deal with this problem.

Lurida.prop.a = Results3[1,2:5]*c(3,6,10,20)
Lurida.prop = Lurida.prop.a/sum(Lurida.prop.a)
Gigas.prop.a = Results3[2,2:5]*c(3,6,10,20)
Gigas.prop = Gigas.prop.a/sum(Gigas.prop.a)
n.Lurida = sum(data3$Count[data3$Species=="O. lurida"])
n.Gigas = sum(data3$Count[data3$Species=="C. gigas"])

observed = matrix(as.numeric(c(Lurida.prop*n.Lurida,Gigas.prop*n.Gigas)),ncol=4,byrow=T)
expected = matrix(0,nrow=2,ncol=4)
for(i in 1:2){
  for(j in 1:4){
    expected[i,j] = sum(observed[i,])*sum(observed[,j])/sum(observed)
  }
}

chi.sq = sum((observed - expected)^2/expected)
pval = 1-pchisq(chi.sq,3)
#pval is 0.0169.  Pellet selection habits appear to be different.
