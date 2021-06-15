corolla<-read.csv("C:\\Users\\Ranjitha\\Downloads\\ToyotaCorolla.csv")
corolla
attach(corolla)
install.packages("lattice")
library(lattice)

##Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight"
qqnorm(corolla$Price)
qqline(corolla$Price)
qqnorm(corolla$Age_08_04)
qqline(corolla$Age_08_04)
qqnorm(corolla$KM)
qqline(corolla$KM)
qqnorm(corolla$HP)
qqline(corolla$HP)
qqnorm(corolla$cc)
qqline(corolla$cc)
qqnorm(corolla$Doors)
qqline(corolla$Doors)
qqnorm(corolla$Gears)
qqline(corolla$Gears)
qqnorm(corolla$Quarterly_Tax)
qqline(corolla$Quarterly_Tax)
qqnorm(corolla$Weight)
qqline(corolla$Weight)

corolla<-corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
corolla
summary(corolla)

plot(corolla$Age_08_04,corolla$Price)
plot(corolla$KM,corolla$Price)
plot(corolla$HP,corolla$Price)
plot(corolla$cc,corolla$Price)
plot(corolla$Doors,corolla$Price)
plot(corolla$Gears,corolla$Price)
plot(corolla$Quarterly_Tax,corolla$Price)
plot(corolla$Weight,corolla$Price)


pairs(corolla)
cor(corolla)

model_corolla<-lm(corolla$Price~corolla$Age_08_04+corolla$KM+corolla$HP+corolla$cc+corolla$Doors+corolla$Gears+corolla$Quarterly_Tax+corolla$Weight)
summary(model_corolla)

model_corollaA<-lm(corolla$Price~corolla$cc)
summary(model_corollaA)

model_corollaB<-lm(corolla$Price~corolla$Doors)
summary(model_corollaB)

model_corollaAB<-lm(corolla$Price~corolla$cc+corolla$Doors)
summary(model_corollaAB)

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
pairs(corolla,lower.panel = panel.cor,main="Scatter plot matrix with Correlation coefficients")

install.packages("corpcor")
library(corpcor)
cor(corolla)
cor2pcor(cor(corolla))
plot(model_corolla)

model_corolla1a<-lm(corolla$Price~corolla$Age_08_04+corolla$KM+corolla$HP+corolla$Gears+corolla$Quarterly_Tax+corolla$Weight)
summary(model_corolla1a)
plot(model_corolla1a)

model_corolla1<-lm(sqrt(corolla$Price)~corolla$Age_08_04+corolla$KM+corolla$HP+corolla$Gears+corolla$Quarterly_Tax+corolla$Weight)
summary(model_corolla1)
plot(model_corolla1)


#model_corolla1 is accurate as r^2 is greater than all