1990-(1.960*2500/sqrt(140))


qnorm(0.95)
sqrt(140)
1960*25
Cigarttes_conjumption<-read.csv("cigarttesconsumption.csv",header=t)

pairs(Cigarttes[,2:8])

cor(Cigarttes[,2:8])

reg.model<-lm(sales~Age+HS+Income+Black+Female+Price+salce,data=Cigarttes))

pairs(Cars)

cor(Cars)

reg.model<-lm(MPG~HP+WT+VOL+SP,data = Cars)
reg.model<-lm(MPG~HP+WT+VOL+SP,data = Cars)

reg.model<-lm(VOL~SP+HP+WT,data = Cars)

>stepAIC(model.Cars)

plot(Cars)
plot(Cars)
