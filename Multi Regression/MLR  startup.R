startup<-read.csv("C:\\Users\\Ranjitha\\Downloads\\50_Startups.csv")
startup
attach(startup)
install.packages("lattice")
library(lattice)
View(startup)
library(dplyr)
library(plyr)
class(startup)

qqnorm(startup$R.D.Spend)
qqline(startup$R.D.Spend)
qqnorm(startup$Administration)
qqline(startup$Administration)
qqnorm(startup$Marketing.Spend)
qqline(satrtup$Marketing.Spend)
qqnorm(startup$Profit)
qqline(startup$Profit)

summary(startup)

plot(startup$R.D.Spend,startup$Profit)
plot(startup$Administration,startup$Profit)
plot(startup$Marketing.Spend,startup$Profit)

startup=subset(startup,select = c(R.D.Spend,Administration,Marketing.Spend,Profit))
pairs(startup)
startup
cor(startup)

model_startup<-lm(startup$Profit~startup$R.D.Spend+startup$Administration+startup$Marketing.Spend)
summary(model_startup)

model_startupA<-lm(startup$Profit~startup$Administration)
summary(model_startupA)

model_startupM<-lm(startup$Profit~startup$Marketing.Spend)
summary(model_startupM)

model_startupAM<-lm(startup$Profit~startup$Administration+startup$Marketing.Spend)
summary(model_startupAM)

panel.cor<-function(x,y,digits=2,prefix="",cex.cor)
{
  usr<- par("usr"); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r=(cor(x,y))
  txt<- format(c(r,0.123456789),digits=digits)[1]
  txt<- paste(prefix,txt,sep="")
  if(missing(cex.cor)) cex<-0.4/strwidth(txt)
  text(0.5,0.5,txt,cex=cex)
}
pairs(startup,upper.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

install.packages("corpcor")
library(corpcor)
cor(startup)
cor2pcor(cor(startup))
plot(model_startup)

model_startup1<-lm(startup$Profit~startup$R.D.startup$Spend+startup$Marketing.Spend)
summary(model_startup1)
plot(model_startup1)

r2degree <- lm(startup$Profit ~startup$R.D.Spend+I(startup$R.D.Spend*startup$R.D.Spend)+startup$Marketing.Spend+I(startup$Marketing.Spend*startup$Marketing.Spend))
summary(r2degree)

#model_startup1 is accurate as r^2 is greater than all
