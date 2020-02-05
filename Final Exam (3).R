#Question 1. 

washington <- read.csv("washington.csv")

t.test(washington$totchi, mu=2.5)

washington$aauw <- washington$ngirls

lm1 <- lm(washington$aauw~washington$ngirls)

summary(lm1)

confint(lm1)

#b

t.test (washington$totchi[party==1], washington$totchi[party==0])

#c

lm2 <- lm(washington$aauw~washington$ngirls)

lm2

summary(lm2)

confint(lm2)


#d. 

lm3 <- lm(aauw~ngirls+totchi, data=washington)

summary(lm3)

#Question 2c. 

chisq.test((matrix(nrow=2, ncol=3, c(315,998,136,512,83,169))
           
           
#Quesstion 3.

fish <- read.csv("fish_data.csv")

lm4 <- lm(fish$FHREVERS~fish$GDP90)

summary(lm4)

confint(lm4)

plot(x= fish$FHREVERS, y= fish$GDP90LGN, xlab="Freedon House Scores", ylab="Log GDP Scores")

abline(lm(fish$GDP90LGN ~ fish$FHREVERS, data = fish), col = "blue")


#Question 4. 

nsw <- read.csv("nsw_exper.csv")

lm5 <- lm(nsw$re78~nsw$nsw)

summary(lm5)

confint(lm5)

#b. 

lm5 <- lm(re78~age+educ+black+hisp+married+re74+re75+u74+u75, data = nsw)

summary(lm5)

#d.

mean(nsw$age)
mean(nsw$educ)
mean(nsw$black)
mean(nsw$hisp)
mean(nsw$married)
mean(nsw$re74)
mean(nsw$re75)
mean(nsw$u74)
mean(nsw$u75)

#e. 

nsw2 <- read.csv("nsw_psid_withtreated.csv")

lm6 <- lm(nsw2$re78~nsw2$nsw)
summary(lm6)

#f. omit
