library(BSDA)

# Chi Square
# nycC <- read.csv("C:/Users/matjo/OneDrive/Documents/Data/NYC Graduation/2020-graduation_rates-BoroughEconDisAdv.csv")

observed <- matrix(c(724516,233229,956566,	466872,
                    702647,	335833,
                     822669,	490180,
                     132525,	28049)
                     ,nrow=5)
dimnames(observed) <- list(Borough = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                           EconStatus = c("Econ Disadv","Not Econ Disadv"))

barplot(t(observed),beside=TRUE,legend=TRUE,main="Proportion of Economic Status by Borough")

chisq.test(observed, correct=F)
# End Chi Square



# Z-Test
nycG <- read.csv("C:/Users/matjo/OneDrive/Documents/Data/NYC Graduation/2020-graduation_rates-BoroughGender.csv")
nycG <- nycG[nycG$Borough != "District 79",]

zTest <- subset(nycG, select=c("Category","PerGrad"))
zTest$PerGrad <- as.numeric(zTest$PerGrad)

boxplot(zTest$PerGrad~zTest$Category, xlab = "Gender", ylab = "Graduation Rate (%)",main = "Graduation Rates for Males and Females in NYC Schools")

Male <- zTest[zTest$Category == "Male",]
MaleSD <- sd(Male$PerGrad)

Female <- zTest[zTest$Category == "Female",]
FemaleSD <- sd(Female$PerGrad)

z.test(x=Male$PerGrad,sigma.x=MaleSD,y=Female$PerGrad,sigma.y=FemaleSD,mu=mean(zTest$PerGrad))
# End ZTest


#Anova
nycB <- read.csv("C:/Users/matjo/OneDrive/Documents/Data/NYC Graduation/2020-graduation_rates-BoroughWhole.csv")

AnTest <- subset(nycB, select = c("Borough","PerGrad"))
AnTest <- AnTest[AnTest$Borough != "District 79",]
AnTest$PerGrad <- as.numeric(AnTest$PerGrad)

boxplot(AnTest$PerGrad~AnTest$Borough, main = "Graduation Rates by Borough", xlab="Borough", ylab="Graduation Rate (%)")
abline(h=mean(AnTest$PerGrad), col=2)

fit=aov(AnTest$PerGrad~factor(AnTest$Borough))
summary(fit) 
anova(fit)
TukeyHSD(fit)
#End Anova



#Correlation
nycCor <- read.csv("C:/Users/matjo/OneDrive/Documents/Data/NYC Graduation/2020-graduation_rates-School Size.csv")

CorrelationTest <- subset(nycCor, select=c("TotalCohort","PerGrad"))

CorrelationTest <- CorrelationTest[CorrelationTest$PerGrad !="s",]
CorrelationTest <- CorrelationTest[CorrelationTest$TotalCohort >= 30,]

cohortSize <- as.numeric(CorrelationTest$TotalCohort)
gradRate <- as.numeric(CorrelationTest$PerGrad)

plot(CorrelationTest,main="Graduation Rate by Cohort Size",xlab="Size of Cohort", ylab="Graduation Rate (%)")
cor(cohortSize, gradRate, method = "spearman")
#End Correlation



# Wilcoxon Sum Rank Test
sample1<- c(76.31457062,77.88748779,78.79517365,78.32942047,79.81880188,80.7161499,81.47557221,81.61648102,82.50494385,83.97058868,84.94938202,84.25367279)
sample2<- c(56.49969809,64.42806097,64.52758971,62.19593864,61.18911972,69.38690964,70.95036774,74.31491577,74.90575302,75.21414703,77.42403458,77.97825958)
wilcox.test(sample1,sample2,exact = F,correct = F,alternative = "greater")

#END Wilcoxon Sum Rank Test

#Welch 2-sample T-Test
disavantaged <- c(54.90985171,66.66265488,66.5509834,65.0697128,66.38227234,68.94467316,70.42955094,73.17104645,75.0076416,74.79967346,76.247789,78.24893951)
nondisadvantaged<-c(53.44197035,63.38415985,64.70087357,66.42395859,67.32970658,70.27947464,71.59451447,73.06860733,72.94371033,78.35296326,80.64116516,80.47876587)
t.test(disavantaged, nondisadvantaged,var.equal=F)
##End Test

##Chi Square Test
x<- matrix(c(16204,10629,31748,20355,2229,8310),nrow=3,ncol = 2)
chisq.test(x)
# End Test


