### R code from vignette source 'using-lsmeans.rnw'

###################################################
### code chunk number 1: using-lsmeans.rnw:49-51
###################################################
options(show.signif.stars=FALSE, prompt="R> ", continue="   ", 
    useFancyQuotes=FALSE, width=100, digits=6)


###################################################
### code chunk number 2: using-lsmeans.rnw:132-135
###################################################
library("lsmeans")
oranges.lm1 <- lm(sales1 ~ price1 + price2 + day + store, data = oranges)
anova(oranges.lm1)


###################################################
### code chunk number 3: using-lsmeans.rnw:138-139
###################################################
( oranges.rg1 <- ref.grid(oranges.lm1) )


###################################################
### code chunk number 4: using-lsmeans.rnw:142-143
###################################################
summary(oranges.rg1)


###################################################
### code chunk number 5: using-lsmeans.rnw:148-149
###################################################
lsmeans(oranges.rg1, "day")   ## or lsmeans(oranges.lm1, "day")


###################################################
### code chunk number 6: using-lsmeans.rnw:152-153
###################################################
with(oranges, tapply(sales1, day, mean))


###################################################
### code chunk number 7: using-lsmeans.rnw:161-163
###################################################
lsmeans(oranges.lm1, "day", at = list(price1 = 50, 
    price2 = c(40,60), day = c("2","3","4")) )


###################################################
### code chunk number 8: using-lsmeans.rnw:168-171
###################################################
org.lsm <- lsmeans(oranges.lm1, "day", by = "price2", 
    at = list(price1 = 50, price2 = c(40,60), day = c("2","3","4")) )
org.lsm


###################################################
### code chunk number 9: using-lsmeans.rnw:174-177 (eval = FALSE)
###################################################
## lsmeans(oranges.lm1, ~ day | price, at = ... )         # Ex 1
## lsmeans(oranges.lm1, c("day","price2"), at = ... )     # Ex 2
## lsmeans(oranges.lm1, ~ day * price, at = ... )         # Ex 3


###################################################
### code chunk number 10: using-lsmeans.rnw:187-188
###################################################
str(org.lsm)


###################################################
### code chunk number 11: using-lsmeans.rnw:194-196
###################################################
( org.sum <- summary(org.lsm, infer = c(TRUE,TRUE), 
                    level = .90, adjust = "bon", by = "day") )


###################################################
### code chunk number 12: using-lsmeans.rnw:201-202
###################################################
class(org.sum)


###################################################
### code chunk number 13: using-lsmeans.rnw:206-207
###################################################
transform(org.sum, lsrubles = lsmean * 34.2)


###################################################
### code chunk number 14: using-lsmeans.rnw:215-217
###################################################
org.lsm2 <- update(org.lsm, by.vars = NULL, level = .99)
org.lsm2


###################################################
### code chunk number 15: org-plot
###################################################
plot(org.lsm, by = "price2")


###################################################
### code chunk number 16: using-lsmeans.rnw:239-240
###################################################
contrast(org.lsm, method = "eff")


###################################################
### code chunk number 17: using-lsmeans.rnw:245-247
###################################################
days.lsm <- lsmeans(oranges.rg1, "day")
contrast(days.lsm, "trt.vs.ctrl", ref = c(5,6))


###################################################
### code chunk number 18: using-lsmeans.rnw:252-253 (eval = FALSE)
###################################################
## confint(contrast(days.lsm, "trt.vs.ctrlk"))


###################################################
### code chunk number 19: using-lsmeans.rnw:261-262
###################################################
pairs(org.lsm)


###################################################
### code chunk number 20: using-lsmeans.rnw:265-266
###################################################
cld(days.lsm, alpha = .10)


###################################################
### code chunk number 21: days-cmp
###################################################
plot(days.lsm, comparisons = TRUE, alpha = .10)


###################################################
### code chunk number 22: using-lsmeans.rnw:287-290
###################################################
oranges.mlm <- lm(cbind(sales1,sales2) ~ price1 + price2 + day + store, 
                 data = oranges)
ref.grid(oranges.mlm)


###################################################
### code chunk number 23: using-lsmeans.rnw:293-295
###################################################
org.mlsm <- lsmeans(oranges.mlm, ~ day | variety, mult.name = "variety")
cld(org.mlsm, sort = FALSE)


###################################################
### code chunk number 24: using-lsmeans.rnw:303-304
###################################################
org.vardiff <- update(pairs(org.mlsm, by = "day"), by = NULL)


###################################################
### code chunk number 25: using-lsmeans.rnw:307-308
###################################################
cld(org.vardiff)


###################################################
### code chunk number 26: using-lsmeans.rnw:319-320
###################################################
confint(days.lsm, adjust = "bon")


###################################################
### code chunk number 27: using-lsmeans.rnw:324-326
###################################################
library("multcomp")
confint(as.glht(days.lsm))


###################################################
### code chunk number 28: using-lsmeans.rnw:332-334
###################################################
summary(glht(oranges.lm1, lsm("day", contr="eff")), 
        test = adjusted("free"))


###################################################
### code chunk number 29: using-lsmeans.rnw:338-339 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm)))


###################################################
### code chunk number 30: using-lsmeans.rnw:342-343 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm), by = NULL))


###################################################
### code chunk number 31: using-lsmeans.rnw:346-347 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm, by = NULL)))


###################################################
### code chunk number 32: using-lsmeans.rnw:359-364
###################################################
data("Oats", package = "nlme")
library("lme4")
Oats.lmer <- lmer(log(yield) ~ Variety*factor(nitro) + (1|Block/Variety), 
                 data = Oats)
anova(Oats.lmer)


###################################################
### code chunk number 33: oatcontr (eval = FALSE)
###################################################
## contrast(lsmeans(Oats.lmer, "nitro"), "poly")


###################################################
### code chunk number 34: using-lsmeans.rnw:371-372
###################################################
cat("NOTE: Results may be misleading due to involvement in interactions")


###################################################
### code chunk number 35: using-lsmeans.rnw:374-375
###################################################
contrast(lsmeans(Oats.lmer, "nitro"), "poly")


###################################################
### code chunk number 36: using-lsmeans.rnw:378-380
###################################################
Oats.lmer2 <- lmer(log(yield) ~ Variety + poly(nitro,2) 
                                + (1|Block/Variety),  data = Oats)


###################################################
### code chunk number 37: using-lsmeans.rnw:383-385
###################################################
Oats.lsm2 <- lsmeans(Oats.lmer2, ~ nitro | Variety, cov.reduce = FALSE)
Oats.lsm2


###################################################
### code chunk number 38: oatslmer
###################################################
lsmip(Oats.lmer, Variety ~ nitro, ylab = "Observed log(yield)")


###################################################
### code chunk number 39: oatslmer2
###################################################
lsmip(Oats.lsm2, Variety ~ nitro, ylab = "Predicted log(yield)")


###################################################
### code chunk number 40: using-lsmeans.rnw:425-426
###################################################
str(Oats.lsm2)


###################################################
### code chunk number 41: using-lsmeans.rnw:429-430
###################################################
summary(Oats.lsm2, type = "response")


###################################################
### code chunk number 42: using-lsmeans.rnw:446-448
###################################################
Oats.Vlsm = lsmeans(Oats.lmer2, "Variety")
test(Oats.Vlsm, null = log(100), type = "response")


###################################################
### code chunk number 43: using-lsmeans.rnw:460-461
###################################################
test(Oats.Vlsm, null = log(100), delta = 0.20, type = "r")


###################################################
### code chunk number 44: using-lsmeans.rnw:467-468
###################################################
test(contrast(Oats.Vlsm, "trt.vs.ctrlk"), side = ">")


###################################################
### code chunk number 45: using-lsmeans.rnw:471-472
###################################################
test(contrast(Oats.Vlsm, "trt.vs.ctrlk"), side = "nonsup", delta = .25)


###################################################
### code chunk number 46: chick-plot
###################################################
xyplot(weight~Time | Diet, groups = ~ Chick, data=ChickWeight, type="o", 
       layout=c(4,1))


###################################################
### code chunk number 47: using-lsmeans.rnw:492-494
###################################################
Chick.lmer <- lmer(weight ~ Diet * Time + (0 + Time | Chick), 
    data = ChickWeight)


###################################################
### code chunk number 48: using-lsmeans.rnw:497-498
###################################################
( Chick.lst <- lstrends (Chick.lmer, ~ Diet, var = "Time") )


###################################################
### code chunk number 49: using-lsmeans.rnw:501-502
###################################################
cld (Chick.lst)


###################################################
### code chunk number 50: using-lsmeans.rnw:513-516
###################################################
lsm.options(ref.grid = list(level = .90),
            lsmeans = list(),
            contrast = list(infer = c(TRUE,TRUE)))


###################################################
### code chunk number 51: using-lsmeans.rnw:529-530
###################################################
lsmeans(Oats.lmer2, pairwise ~ Variety)


###################################################
### code chunk number 52: using-lsmeans.rnw:534-535
###################################################
lsm.options(ref.grid = NULL, contrast = NULL)


###################################################
### code chunk number 53: using-lsmeans.rnw:545-548
###################################################
nutr.lm <- lm(gain ~ (age + group + race)^2, data = nutrition)
library("car")
Anova(nutr.lm)


###################################################
### code chunk number 54: nutr-intplot
###################################################
lsmip(nutr.lm, race ~ age | group)
lsmeans(nutr.lm, ~ group*race)


###################################################
### code chunk number 55: using-lsmeans.rnw:565-567
###################################################
nutr.lsm <- lsmeans(nutr.lm, ~ group * race, weights = "proportional",
    at = list(age = c("2","3"), race = c("Black","White")))


###################################################
### code chunk number 56: using-lsmeans.rnw:570-573
###################################################
nutr.lsm    
summary(pairs(nutr.lsm, by = "race"), by = NULL)
summary(pairs(nutr.lsm, by = "group"), by = NULL)


###################################################
### code chunk number 57: using-lsmeans.rnw:586-590
###################################################
lsmeans(nutr.lm, "race", weights = "equal")
lsmeans(nutr.lm, "race", weights = "prop")
lsmeans(nutr.lm, "race", weights = "outer")
lsmeans(nutr.lm, "race", weights = "cells")


###################################################
### code chunk number 58: using-lsmeans.rnw:599-601
###################################################
temp = lsmeans(nutr.lm, c("group","race"), weights = "prop")
lsmeans(temp, "race", weights = "prop")


###################################################
### code chunk number 59: using-lsmeans.rnw:606-607
###################################################
with(nutrition, tapply(gain, race, mean))


###################################################
### code chunk number 60: using-lsmeans.rnw:614-618
###################################################
library("mediation")
levels(framing$educ) = c("NA","Ref","< HS", "HS", "> HS","Coll +")
framing.glm = glm(cong_mesg ~ age + income + educ + emo + gender * factor(treat),
                  family = binomial, data = framing)


###################################################
### code chunk number 61: framinga
###################################################
lsmip(framing.glm, treat ~ educ | gender, type = "response")


###################################################
### code chunk number 62: framingb
###################################################
lsmip(framing.glm, treat ~ educ | gender, type = "response",
      cov.reduce = emo ~ treat*gender + age + educ + income)


###################################################
### code chunk number 63: using-lsmeans.rnw:645-647
###################################################
ref.grid(framing.glm, 
    cov.reduce = emo ~ treat*gender + age + educ + income)@grid


###################################################
### code chunk number 64: using-lsmeans.rnw:678-680 (eval = FALSE)
###################################################
## rg <- ref.grid(my.model, at = list(x1 = c(5,10,15)),
##                cov.reduce = list(x2 ~ x1,  x3 ~ x1 + x2))


###################################################
### code chunk number 65: housing-plot
###################################################
library("ordinal")
data(housing, package = "MASS")
housing.clm <- clm(Sat ~ (Infl + Type + Cont)^2,
                   data = housing, weights = Freq, link = "probit")
lsmip(housing.clm, Cont ~ Infl | Type, layout = c(4,1))


###################################################
### code chunk number 66: using-lsmeans.rnw:724-725
###################################################
test(pairs(lsmeans(housing.clm, ~ Infl | Type)), joint = TRUE)


###################################################
### code chunk number 67: using-lsmeans.rnw:728-729
###################################################
test(pairs(lsmeans(housing.clm, ~ Cont | Type)), joint = TRUE)


###################################################
### code chunk number 68: using-lsmeans.rnw:734-735
###################################################
ref.grid(housing.clm, mode = "cum.prob")


###################################################
### code chunk number 69: using-lsmeans.rnw:738-740
###################################################
lsmeans(housing.clm, ~ Infl, at = list(cut = "Medium|High"), 
        mode = "cum.prob")


###################################################
### code chunk number 70: using-lsmeans.rnw:743-745
###################################################
summary(lsmeans(housing.clm, ~ Infl, at = list(cut = "Medium|High"), 
                mode = "linear.predictor"), type = "response")


