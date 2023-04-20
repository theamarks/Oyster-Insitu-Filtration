# Dry Tissue Weight to shell height model comparison of Ostrea lurida

#### start of Kevin's code ###
#find domain of shell length

#fit separate models log(weight)~log(length)

#come up with 1000 evenly separated x values over domain.  Compute y.hat for each of the three models
#add to plot.

temp = Oly_combo_DTW_SL

#find domain of shell lenght

x.seq = seq(min(temp$length_mm),max(temp$length_mm),by=0.05)

#fit separate models

x1 = temp$length_mm[temp$Site == "Deanza"]
y1 = temp$tissue_dry_weight_g[temp$Site=="Deanza"]
x2 = temp$length_mm[temp$Site == "San Diego"]
y2 = temp$tissue_dry_weight_g[temp$Site=="San Diego"]
x3 = temp$length_mm[temp$Site == "Yaquina"]
y3 = temp$tissue_dry_weight_g[temp$Site=="Yaquina"]

lx1 = log(x1)
lx2 = log(x2)
lx3 = log(x3)
ly1 = log(y1)
ly2 = log(y2)
ly3 = log(y3)

m1 = lm(ly1~lx1)
m2 = lm(ly2~lx2)
m3 = lm(ly3~lx3)

#example with confidence bounds
#y.seq1 = exp(predict(m1,newdata=data.frame(lx1=log(x.seq)),interval="confidence"))
y.seq1 = exp(predict(m1,newdata=data.frame(lx1=log(x.seq))))
y.seq2 = exp(predict(m2,newdata=data.frame(lx2=log(x.seq))))
y.seq3 = exp(predict(m3,newdata=data.frame(lx3=log(x.seq))))

temp1 = data.frame(x.seq,y.seq1)
temp2 = data.frame(x.seq,y.seq2)
temp3 = data.frame(x.seq,y.seq3)
### End of Kevin's code ####