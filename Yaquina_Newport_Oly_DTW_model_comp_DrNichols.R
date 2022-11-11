######
#setwd

#read in data

data = read.csv("Marks_Data_DTW_SL_Newport_Yaquina.csv",h=T)
head(data)

#Transform data into log.x and log.y, separate into Deanza and Yaquina

log.dw = log(data$tissue_dry_weight_g)
log.l = log(data$length_mm)

Deanza = data.frame(log.dw=log.dw[data$Site=="Deanza"],
					log.l = log.l[data$Site=="Deanza"])

Yaquina = data.frame(log.dw=log.dw[data$Site=="Yaquina"],
					log.l = log.l[data$Site=="Yaquina"])

#linear models for Deanza and Yaquina

mD = lm(log.dw~log.l,data=Deanza)
mY = lm(log.dw~log.l,data=Yaquina)

#

D.int = summary(mD)[[4]][1]
D.int.se = summary(mD)[[4]][3]
D.slope = summary(mD)[[4]][2]
D.slope.se = summary(mD)[[4]][4]


Y.int = summary(mY)[[4]][1]
Y.int.se = summary(mY)[[4]][3]
Y.slope = summary(mY)[[4]][2]
Y.slope.se = summary(mY)[[4]][4]

diff.int = D.int - Y.int
se.int = sqrt(D.int.se^2 + Y.int.se^2)
diff.slope = D.slope - Y.slope
se.slope = sqrt(D.slope.se^2 + Y.slope.se^2)

t.int = diff.int/se.int
p.val.int = pt(-abs(t.int),144)*2
t.slope = diff.slope/se.slope
p.val.slope = pt(-abs(t.slope),144)*2
p.val.int
p.val.slope


###Plots?

plot(data$length_mm,data$tissue_dry_weight_g,pch="")
points(data$length_mm[data$Site=="Deanza"],data$tissue_dry_weight_g[data$Site=="Deanza"],col=4,pch=16,cex=.7)
points(data$length_mm[data$Site!="Deanza"],data$tissue_dry_weight_g[data$Site!="Deanza"],col=3,pch=17,cex=.7)

x.seq = seq(min(data$length_mm),max(data$length_mm),by=.01)
x.log.seq = log(x.seq)
y.log.seq.D = predict(mD,newdata=data.frame(log.l=x.log.seq),interval="confidence")
y.seq.D = exp(y.log.seq.D)

y.log.seq.Y = predict(mY,newdata=data.frame(log.l=x.log.seq),interval="confidence")
y.seq.Y = exp(y.log.seq.Y)

lines(x.seq,y.seq.D[,2],lwd=.7,col=4,lty=2)
lines(x.seq,y.seq.D[,3],lwd=.7,col=4,lty=2)
lines(x.seq,y.seq.Y[,2],lwd=.7,col=3,lty=2)
lines(x.seq,y.seq.Y[,3],lwd=.7,col=3,lty=2)

