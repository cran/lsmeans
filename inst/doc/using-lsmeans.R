### R code from vignette source 'using-lsmeans.rnw'

###################################################
### code chunk number 1: using-lsmeans.rnw:49-51
###################################################
options(show.signif.stars=FALSE, prompt="R> ", continue="   ", 
    useFancyQuotes=FALSE, width=100, digits=6)


###################################################
### code chunk number 2: using-lsmeans.rnw:134-137
###################################################
library("lsmeans")
oranges.lm1 <- lm(sales1 ~ price1 + price2 + day + store, data = oranges)
anova(oranges.lm1)


###################################################
### code chunk number 3: using-lsmeans.rnw:140-141
###################################################
( oranges.rg1 <- ref.grid(oranges.lm1) )


###################################################
### code chunk number 4: using-lsmeans.rnw:144-145
###################################################
summary(oranges.rg1)


###################################################
### code chunk number 5: using-lsmeans.rnw:150-151
###################################################
lsmeans(oranges.rg1, "day")   ## or lsmeans(oranges.lm1, "day")


###################################################
### code chunk number 6: using-lsmeans.rnw:154-155
###################################################
with(oranges, tapply(sales1, day, mean))


###################################################
### code chunk number 7: using-lsmeans.rnw:164-166
###################################################
lsmeans(oranges.lm1, "day", at = list(price1 = 50, 
    price2 = c(40,60), day = c("2","3","4")) )


###################################################
### code chunk number 8: using-lsmeans.rnw:175-178
###################################################
org.lsm <- lsmeans(oranges.lm1, "day", by = "price2", 
    at = list(price1 = 50, price2 = c(40,60), day = c("2","3","4")) )
org.lsm


###################################################
### code chunk number 9: using-lsmeans.rnw:181-184 (eval = FALSE)
###################################################
## lsmeans(oranges.lm1, ~ day | price, at = ... )         # Ex 1
## lsmeans(oranges.lm1, c("day","price2"), at = ... )     # Ex 2
## lsmeans(oranges.lm1, ~ day * price, at = ... )         # Ex 3


###################################################
### code chunk number 10: using-lsmeans.rnw:194-195
###################################################
str(org.lsm)


###################################################
### code chunk number 11: using-lsmeans.rnw:201-203
###################################################
( org.sum <- summary(org.lsm, infer = c(TRUE,TRUE), 
                    level = .90, adjust = "bon", by = "day") )


###################################################
### code chunk number 12: using-lsmeans.rnw:208-209
###################################################
class(org.sum)


###################################################
### code chunk number 13: using-lsmeans.rnw:213-214
###################################################
transform(org.sum, lsrubles = lsmean * 34.2)


###################################################
### code chunk number 14: using-lsmeans.rnw:222-224
###################################################
org.lsm2 <- update(org.lsm, by.vars = NULL, level = .99)
org.lsm2


###################################################
### code chunk number 15: org-plot
###################################################
plot(org.lsm, by = "price2")


###################################################
### code chunk number 16: using-lsmeans.rnw:246-247
###################################################
contrast(org.lsm, method = "eff")


###################################################
### code chunk number 17: using-lsmeans.rnw:252-254
###################################################
days.lsm <- lsmeans(oranges.rg1, "day")
( days_contr.lsm <- contrast(days.lsm, "trt.vs.ctrl", ref = c(5,6)) )


###################################################
### code chunk number 18: using-lsmeans.rnw:259-260 (eval = FALSE)
###################################################
## confint(contrast(days.lsm, "trt.vs.ctrlk"))


###################################################
### code chunk number 19: using-lsmeans.rnw:268-269
###################################################
pairs(org.lsm)


###################################################
### code chunk number 20: using-lsmeans.rnw:272-273
###################################################
cld(days.lsm, alpha = .10)


###################################################
### code chunk number 21: days-cmp
###################################################
plot(days.lsm, comparisons = TRUE, alpha = .10)


###################################################
### code chunk number 22: using-lsmeans.rnw:294-297
###################################################
oranges.mlm <- lm(cbind(sales1,sales2) ~ price1 + price2 + day + store, 
                 data = oranges)
ref.grid(oranges.mlm)


###################################################
### code chunk number 23: using-lsmeans.rnw:300-302
###################################################
org.mlsm <- lsmeans(oranges.mlm, ~ day | variety, mult.name = "variety")
cld(org.mlsm, sort = FALSE)


###################################################
### code chunk number 24: using-lsmeans.rnw:310-311
###################################################
org.vardiff <- update(pairs(org.mlsm, by = "day"), by = NULL)


###################################################
### code chunk number 25: using-lsmeans.rnw:314-315
###################################################
cld(org.vardiff)


###################################################
### code chunk number 26: using-lsmeans.rnw:326-328
###################################################
# Ensure we see the same results each time
set.seed(123454321)


###################################################
### code chunk number 27: using-lsmeans.rnw:330-333
###################################################
library("multcomp")
days.glht <- as.glht(days_contr.lsm)
summary(days.glht, test = adjusted("Westfall"))


###################################################
### code chunk number 28: using-lsmeans.rnw:336-338 (eval = FALSE)
###################################################
## days.glht1 <- glht(oranges.lm1, 
##                    lsm("day", contr = "trt.vs.ctrl", ref = c(5,6)))


###################################################
### code chunk number 29: using-lsmeans.rnw:342-344 (eval = FALSE)
###################################################
## summary(days_contr.lsm, adjust = "mvt")
## summary(days.glht)


###################################################
### code chunk number 30: using-lsmeans.rnw:350-351 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm)))


###################################################
### code chunk number 31: using-lsmeans.rnw:354-355 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm), by = NULL))


###################################################
### code chunk number 32: using-lsmeans.rnw:358-359 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm, by = NULL)))


###################################################
### code chunk number 33: using-lsmeans.rnw:371-376
###################################################
data("Oats", package = "nlme")
library("lme4")
Oats.lmer <- lmer(log(yield) ~ Variety*factor(nitro) + (1|Block/Variety), 
                 data = Oats)
anova(Oats.lmer)


###################################################
### code chunk number 34: oatcontr (eval = FALSE)
###################################################
## contrast(lsmeans(Oats.lmer, "nitro"), "poly")


###################################################
### code chunk number 35: using-lsmeans.rnw:383-384
###################################################
cat("NOTE: Results may be misleading due to involvement in interactions")


###################################################
### code chunk number 36: using-lsmeans.rnw:386-387
###################################################
contrast(lsmeans(Oats.lmer, "nitro"), "poly")


###################################################
### code chunk number 37: using-lsmeans.rnw:390-392
###################################################
Oats.lmer2 <- lmer(log(yield) ~ Variety + poly(nitro,2) 
                                + (1|Block/Variety),  data = Oats)


###################################################
### code chunk number 38: using-lsmeans.rnw:395-397
###################################################
Oats.lsm2 <- lsmeans(Oats.lmer2, ~ nitro | Variety, cov.reduce = FALSE)
Oats.lsm2


###################################################
### code chunk number 39: oatslmer
###################################################
lsmip(Oats.lmer, Variety ~ nitro, ylab = "Observed log(yield)")


###################################################
### code chunk number 40: oatslmer2
###################################################
lsmip(Oats.lsm2, Variety ~ nitro, ylab = "Predicted log(yield)")


###################################################
### code chunk number 41: using-lsmeans.rnw:438-439
###################################################
str(Oats.lsm2)


###################################################
### code chunk number 42: using-lsmeans.rnw:442-443
###################################################
summary(Oats.lsm2, type = "response")


###################################################
### code chunk number 43: using-lsmeans.rnw:459-461
###################################################
Oats.Vlsm = lsmeans(Oats.lmer2, "Variety")
test(Oats.Vlsm, null = log(100), type = "response")


###################################################
### code chunk number 44: using-lsmeans.rnw:473-474
###################################################
test(Oats.Vlsm, null = log(100), delta = 0.20, type = "r")


###################################################
### code chunk number 45: using-lsmeans.rnw:480-481
###################################################
test(contrast(Oats.Vlsm, "trt.vs.ctrlk"), side = ">")


###################################################
### code chunk number 46: using-lsmeans.rnw:484-485
###################################################
test(contrast(Oats.Vlsm, "trt.vs.ctrlk"), side = "nonsup", delta = .25)


###################################################
### code chunk number 47: chick-plot
###################################################
require("lattice")
xyplot(weight~Time | Diet, groups = ~ Chick, data=ChickWeight, type="o", 
       layout=c(4,1))


###################################################
### code chunk number 48: using-lsmeans.rnw:506-508
###################################################
Chick.lmer <- lmer(weight ~ Diet * Time + (0 + Time | Chick), 
    data = ChickWeight)


###################################################
### code chunk number 49: using-lsmeans.rnw:511-512
###################################################
( Chick.lst <- lstrends (Chick.lmer, ~ Diet, var = "Time") )


###################################################
### code chunk number 50: using-lsmeans.rnw:515-516
###################################################
cld (Chick.lst)


###################################################
### code chunk number 51: using-lsmeans.rnw:527-530
###################################################
lsm.options(ref.grid = list(level = .90),
            lsmeans = list(),
            contrast = list(infer = c(TRUE,TRUE)))


###################################################
### code chunk number 52: using-lsmeans.rnw:543-544
###################################################
lsmeans(Oats.lmer2, pairwise ~ Variety)


###################################################
### code chunk number 53: using-lsmeans.rnw:548-549
###################################################
lsm.options(ref.grid = NULL, contrast = NULL)


###################################################
### code chunk number 54: using-lsmeans.rnw:559-562
###################################################
nutr.lm <- lm(gain ~ (age + group + race)^2, data = nutrition)
library("car")
Anova(nutr.lm)


###################################################
### code chunk number 55: nutr-intplot
###################################################
lsmip(nutr.lm, race ~ age | group)
lsmeans(nutr.lm, ~ group*race)


###################################################
### code chunk number 56: using-lsmeans.rnw:579-581
###################################################
nutr.lsm <- lsmeans(nutr.lm, ~ group * race, weights = "proportional",
    at = list(age = c("2","3"), race = c("Black","White")))


###################################################
### code chunk number 57: using-lsmeans.rnw:584-587
###################################################
nutr.lsm    
summary(pairs(nutr.lsm, by = "race"), by = NULL)
summary(pairs(nutr.lsm, by = "group"), by = NULL)


###################################################
### code chunk number 58: using-lsmeans.rnw:600-604
###################################################
lsmeans(nutr.lm, "race", weights = "equal")
lsmeans(nutr.lm, "race", weights = "prop")
lsmeans(nutr.lm, "race", weights = "outer")
lsmeans(nutr.lm, "race", weights = "cells")


###################################################
### code chunk number 59: using-lsmeans.rnw:613-615
###################################################
temp = lsmeans(nutr.lm, c("group","race"), weights = "prop")
lsmeans(temp, "race", weights = "prop")


###################################################
### code chunk number 60: using-lsmeans.rnw:620-621
###################################################
with(nutrition, tapply(gain, race, mean))


###################################################
### code chunk number 61: using-lsmeans.rnw:628-632
###################################################
library("mediation")
levels(framing$educ) = c("NA","Ref","< HS", "HS", "> HS","Coll +")
framing.glm = glm(cong_mesg ~ age + income + educ + emo + gender * factor(treat),
                  family = binomial, data = framing)


###################################################
### code chunk number 62: framinga
###################################################
lsmip(framing.glm, treat ~ educ | gender, type = "response")


###################################################
### code chunk number 63: framingb
###################################################
lsmip(framing.glm, treat ~ educ | gender, type = "response",
      cov.reduce = emo ~ treat*gender + age + educ + income)


###################################################
### code chunk number 64: using-lsmeans.rnw:659-661
###################################################
ref.grid(framing.glm, 
    cov.reduce = emo ~ treat*gender + age + educ + income)@grid


###################################################
### code chunk number 65: using-lsmeans.rnw:692-694 (eval = FALSE)
###################################################
## rg <- ref.grid(my.model, at = list(x1 = c(5,10,15)),
##                cov.reduce = list(x2 ~ x1,  x3 ~ x1 + x2))


###################################################
### code chunk number 66: housing-plot
###################################################
library("ordinal")
data(housing, package = "MASS")
housing.clm <- clm(Sat ~ (Infl + Type + Cont)^2,
                   data = housing, weights = Freq, link = "probit")
lsmip(housing.clm, Cont ~ Infl | Type, layout = c(4,1))


###################################################
### code chunk number 67: using-lsmeans.rnw:738-739
###################################################
test(pairs(lsmeans(housing.clm, ~ Infl | Type)), joint = TRUE)


###################################################
### code chunk number 68: using-lsmeans.rnw:742-743
###################################################
test(pairs(lsmeans(housing.clm, ~ Cont | Type)), joint = TRUE)


###################################################
### code chunk number 69: using-lsmeans.rnw:748-749
###################################################
ref.grid(housing.clm, mode = "cum.prob")


###################################################
### code chunk number 70: using-lsmeans.rnw:752-754
###################################################
lsmeans(housing.clm, ~ Infl, at = list(cut = "Medium|High"), 
        mode = "cum.prob")


###################################################
### code chunk number 71: using-lsmeans.rnw:757-759
###################################################
summary(lsmeans(housing.clm, ~ Infl, at = list(cut = "Medium|High"), 
                mode = "linear.predictor"), type = "response")


