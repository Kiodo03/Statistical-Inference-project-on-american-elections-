library(car)
library(rgl)
library(MASS)
library(leaps)
library(GGally)
library(ellipse)
library(faraway)
data(counties)

cor_matrix <- cor(counties[, c("age75", "white", "black", "age6574")])
print(cor_matrix)
#da qui si nota che age6574-age75 e white-black sono molto correlati, conviene togliere uno per coppia. Secondo me meglio tenere age75 perchè mette in evidenza la popolarità di Perot e la delusione dei conservatori (over 75) nella politica di Bush. Penso sia meglio tenere white perchè è la maggioranza e usare black come complementare

#democratici
dem=lm(democrat~pop.density+pop+pop.change+age6574+age75+crime+college+income+farm+white+black,data=counties)
summary(dem)
qqnorm(dem$res,col='blue')
qqline(dem$res,col='red')
vif(dem)
plot( dem$fit, dem$res, xlab = "Valori", ylab = "Residui",
      main = "Omeosch. Democratici", pch = 16 )
abline( h = 0, lwd = 3,col='blue') 
abline(h=20, lwd=1, lty=2, col='red')
abline(h=-20, lwd=1, lty=2, col='red')

dem2=lm(democrat~pop.density+pop+pop.change+age6574+age75+crime+income+farm+white+black,data=counties)
anova(dem,dem2)
dem3=lm(democrat~pop.density+pop+pop.change+age75+crime+income+farm+white+black,data=counties)
anova(dem2,dem3)
dem4=lm(democrat~pop.density+pop+pop.change+age6574+age75+crime+income+farm+white,data=counties)
anova(dem2,dem4)
H=model.matrix(dem2)
leva=hat(H)
plot( dem2$fitted.values, leva, ylab = "Leva", main = "Plot of Leverages",
      pch = 16, col = 'black' )
abline( h = 0.2, lty = 2, col = 'red' )
leverage_vals <- hatvalues(dem2)
which(leverage_vals > 2 * mean(leverage_vals))
cooks.distance=cooks.distance(dem2)
plot(dem2,which=4)
influencePlot( dem2,  main = "influential Plot")
#repubblicani
rep=lm(republican~pop.density+pop+pop.change+age6574+age75+crime+income+farm+white+black,data=counties)
summary(rep)
qqnorm(rep$res,col='green')
qqline(rep$res,col='red')
vif(rep)
plot( rep$fit, rep$res, xlab = "Valori", ylab = "Residui",
      main = "Omeosch. Repubblicani", pch = 16 )
abline( h = 0, lwd = 3,col='blue') 
abline(h=20, lwd=1, lty=2, col='red')
abline(h=-20, lwd=1, lty=2, col='red')

#Perot
per=lm(Perot~pop.density+pop+pop.change+age75+crime+income+farm+black,data=counties)
summary(per)
qqnorm(per$res,col='grey')
qqline(per$res,col='red',)
vif(per)
plot( per$fit, per$res, xlab = "Valori", ylab = "Residui",
      main = "Omeosch. Perot", pch = 16 )
abline( h = 0, lwd = 3,col='blue') 
abline(h=20, lwd=1, lty=2, col='red')
abline(h=-20, lwd=1, lty=2, col='red')

