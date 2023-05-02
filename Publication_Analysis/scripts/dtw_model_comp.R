##################################################################
#####Shell length to Dry Mass comparison SD and Oregon O Lurida###
#####Created: 7/19/2021
#####Authors:  Kevin and Thea
###################################################################

#Scope.  

#We have established a relationship between dry mass and filtration.
#We'd like to see if there is a stable relationship between
#Shell length and dry mass.  Obviously just using a few sites
#doesn't prove this, it just doesn't disprove it.  Still
#could be some compelling evidence that we could proxy the relationship
#between dry mass and filtration using shell length instead of dry mass.

#Goal.

#To analyze (model) 3 separate sites (Deanza, Yaquina, SD)
#Could the same model (parametric, non-parametric?) fit all three datasets

#Data Sources.

#Two data sources.  

#1.  Marks_Gray_DTW_SL_newport_Yaquina
#2.  Langevin_SD_O_lurida_length_weight


#Data import
data_temp <- Oly_combo_DTW_SL

data_d = data_temp[data_temp$Site=="Deanza",]
data_y = data_temp[data_temp$Site=="Yaquina",]
data_s = data_temp[data_temp$Site=="San Diego",]

data_d$Site = as.character(data_d$Site)
data_y$Site = as.character(data_y$Site)
data_s$Site = as.character(data_s$Site)

#For now, drop Tidal.height and Time from SD data
data_d = data_d[,c("Site","length_mm","tissue_dry_weight_g")]
data_y = data_y[,c("Site","length_mm","tissue_dry_weight_g")]
data_s = data_s[,c("Site","length_mm","tissue_dry_weight_g")]

colnames(data_d) = c("Site","Length","Weight")
colnames(data_y) = c("Site","Length","Weight")
colnames(data_s) = c("Site","Length","Weight")

data = rbind(data_d,data_y,data_s)
plot(log(data$Length),log(data$Weight),pch="")
points(log(data$Length[data$Site=="Deanza"]),log(data$Weight[data$Site=="Deanza"]),col=2,cex=.5)
points(log(data$Length[data$Site=="Yaquina"]),log(data$Weight[data$Site=="Yaquina"]),col=3,cex=.5)
points(log(data$Length[data$Site=="San Diego"]),log(data$Weight[data$Site=="San Diego"]),col=4,cex=.5)

#Log relationship looks pretty linear.  
#Model three separate linear models, could they all have the same underlying population slope/int.

data$ln.length = log(data$Length)
data$ln.weight = log(data$Weight)

model.d = lm(ln.weight~ln.length,data=data[data$Site=="Deanza",])
model.y = lm(ln.weight~ln.length,data=data[data$Site=="Yaquina",])
model.s = lm(ln.weight~ln.length,data=data[data$Site=="San Diego",])



#Want access and store the intercepts, slops, se(intercepts),se(slopes) for all three models.
# Is this incorrectly indexed? - Thea 2023-05-02
# int.d=summary(model.d)[[4]][1,1]
# se.int.d=summary(model.d)[[4]][2,1]
# slope.d=summary(model.d)[[4]][1,2]
# se.slope.d=summary(model.d)[[4]][2,2]
# 
# int.y=summary(model.y)[[4]][1,1]
# se.int.y=summary(model.y)[[4]][2,1]
# slope.y=summary(model.y)[[4]][1,2]
# se.slope.y=summary(model.y)[[4]][2,2]
# 
# int.s=summary(model.s)[[4]][1,1]
# se.int.s=summary(model.s)[[4]][2,1]
# slope.s=summary(model.s)[[4]][1,2]
# se.slope.s=summary(model.s)[[4]][2,2]

# Is this how it should be?
int.d=summary(model.d)[[4]][1,1]
se.int.d=summary(model.d)[[4]][1,2]
slope.d=summary(model.d)[[4]][2,1]
se.slope.d=summary(model.d)[[4]][2,2]

int.y=summary(model.y)[[4]][1,1]
se.int.y=summary(model.y)[[4]][1,2]
slope.y=summary(model.y)[[4]][2,1]
se.slope.y=summary(model.y)[[4]][2,2]

int.s=summary(model.s)[[4]][1,1]
se.int.s=summary(model.s)[[4]][1,2]
slope.s=summary(model.s)[[4]][2,1]
se.slope.s=summary(model.s)[[4]][2,2]

# end of Thea's suspicion 
table.model.par = data.frame(Site=c("Deanza","Yaquina","San Diego"),
                             Intercept = c(int.d,int.y,int.s),
                             Se.Intercept = c(se.int.d,se.int.y,se.int.s),
                             Slope = c(slope.d,slope.y,slope.s),
                             Se.Slope = c(se.slope.d,se.slope.y,se.slope.s))

#Six z scores (can ignore t scores because of our healthy sample sizes...really same diff)

#z1 will be slope sd vs slope deanza
#z2 will be slope sd vs slope Yaquina
#z3 will be slope deanza vs slope Yaquina
#z4 will be intercept sd vs slope deanza
#z5 will be intercept sd vs slope Yaquina
#z6 will be intercept deanza vs slope Yaquina

z1 = (slope.s - slope.d)/sqrt(se.slope.s^2 + se.slope.d^2)
z2 = (slope.s - slope.y)/sqrt(se.slope.s^2 + se.slope.y^2)
z3 = (slope.y - slope.d)/sqrt(se.slope.y^2 + se.slope.d^2)
z4 = (int.s - slope.d)/sqrt(se.int.s^2 + se.int.d^2)
z5 = (int.s - slope.y)/sqrt(se.int.s^2 + se.int.y^2)
z6 = (int.y - slope.d)/sqrt(se.int.y^2 + se.int.d^2)

p1 = 2*pnorm(-abs(z1))
p2 = 2*pnorm(-abs(z2))
p3 = 2*pnorm(-abs(z3))
p4 = 2*pnorm(-abs(z4))
p5 = 2*pnorm(-abs(z5))
p6 = 2*pnorm(-abs(z6))

table_results = data.frame(Comparison = rep(c("SD vs. D","SD vs. Y","D vs. Y"),2),
                           Parameter = c(rep("Slope",3),rep("Intercept",3)),
                           Z = c(z1,z2,z3,z4,z5,z6),
                           Pval = c(p1,p2,p3,p4,p5,p6))


