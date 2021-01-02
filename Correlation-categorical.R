data_mcharges_extended <- read.csv("~/R/Udemy/data_mcharges_extended.csv")
   View(data_mcharges_extended)


data_income_class <- read.csv("~/R/Udemy/data_income_class.csv")
View(data_income_class)

table(data_income_class$gender,data_income_class$income)
chisq.test(data_income_class$gender,data_income_class$income)
table(data_income_class$education,data_income_class$income)
chisq.test(data_income_class$education,data_income_class$income)
 barplot(table(data_income_class$income,data_income_class$education),
         las=1,horiz = T,legend.text = T) 
 
charges=data_mcharges_extended
male=charges[charges$sex=="male",3]
female=charges[charges$sex=="female",3]
t.test(male,female)

### Normality
shapiro.test(male )
shapiro.test(female )
shapiro.test(log(female ))
#### Homogeneity
bartlett.test(charges$charges~charges$sex)

##t-test
t.test(male,female)
### Region
table(charges$region)#### we cant use t-test
##ANOVA
##Normality test of 3 regions
east=charges[charges$region=="east",3]
nw=charges[charges$region=="northwest",3]
sw=charges[charges$region=="souththwest",3]
shapiro.test(east)
shapiro.test(nw)
shapiro.test(sw)

##Homogeneity
bartlett.test(charges$charges~charges$region)
#ANOVA
fit=aov(charges$charges~charges$region)
summary(fit)
             