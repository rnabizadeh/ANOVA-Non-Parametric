# Read Data 
mydata=read.csv("Forana.csv", header=T,sep=",")
mydata=read.csv(file.choose(), header=T,sep=",")

mydata
names(mydata)

# Data Preparation 
ftype=factor(mydata$type)
 
# Compute Mean & Variance

tapply( mydata$removal, ftype, mean) 
tapply( mydata$removal, ftype, var) 

tapply( mydata$removal, ftype, var, na.rm=T)   #agar NA dasht az in cod estefade mikonim

# Data Visualization 
levels(ftype)= c("estrone", "Beta estradiol","Alfa-e-estradiol") 
boxplot( mydata$removal ~ ftype, col="lightblue",
xlab = "hormones",ylab = " Percent Removal", par(cex.axis=1),par(font.axis=6),par(font.lab=6))

#######################################
# Do the Test for Variance Equity 

fligner.test(mydata$removal,ftype) #p-value must be lower than 0.05 till parameteic analysis done
or
levene.test(mydata$removal, ftype)   #p-value must be greatet than 0.05 till parameteic analysis done


# Compute Median 
tapply(mydata$removal, ftype, median) # in none parameteric analysis instead of average we used median


# Perform Kruskal-wallis Test
kruskal.test(mydata$removal,ftype)


# Perform Post Hoc
#:For Kruskal-wallis Post Hoc
install.package(pgirmess)

library(pgirmess)

kruskalmc(mydata$removal,ftype)


# Test for homogeneity of variances â€“ Leveneâ€™s test and the Fligner-Killeen test
##################
install.packages("lawstat")

library(lawstat)

Levene Test shabih flignet test hast

# response variable
size <- c(25,22,28,24,26,24,22,21,23,25,26,30,25,24,21,27,28,23,25,24,20,22,24,23,22,24,20,19,21,22)

# predictor variable
location <- c(rep("ForestA",10), rep("ForestB",10), rep("ForestC",10))

# dataframe
my.dataframe <- data.frame(size,location)

****levene.test(size ~ location, my.dataframe)

## Levene's Test for Homogeneity of Variance (center = mean)
##       Df F value Pr(>F)
## group  2  0.5636 0.5757 #agar p value bozorgtat 0.05 bood varyansha ba ham barabar hastand dar ravesh fligner test in ghazie baraks ast
##       27


#######################
****fligner.test(size ~ location, my.dataframe)

## Fligner-Killeen test of homogeneity of variances
## data:  size by location
## Fligner-Killeen:med chi-squared = 0.9556, df = 2, p-value 