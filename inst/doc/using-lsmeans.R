### R code from vignette source 'using-lsmeans.rnw'

###################################################
### code chunk number 1: using-lsmeans.rnw:36-38
###################################################
options(show.signif.stars=FALSE, prompt="R> ", continue="   ", 
    useFancyQuotes=FALSE, width=100, digits=6)


###################################################
### code chunk number 2: using-lsmeans.rnw:121-124
###################################################
library("lsmeans")
oranges.lm1 <- lm(sales1 ~ price1 + price2 + day + store, data = oranges)
anova(oranges.lm1)


###################################################
### code chunk number 3: using-lsmeans.rnw:127-128
###################################################
( oranges.rg1 <- ref.grid(oranges.lm1) )


###################################################
### code chunk number 4: using-lsmeans.rnw:131-132
###################################################
summary(oranges.rg1)


###################################################
### code chunk number 5: using-lsmeans.rnw:137-138
###################################################
lsmeans(oranges.rg1, "day")   ## or lsmeans(oranges.lm1, "day")


###################################################
### code chunk number 6: using-lsmeans.rnw:141-142
###################################################
with(oranges, tapply(sales1, day, mean))


###################################################
### code chunk number 7: using-lsmeans.rnw:150-152
###################################################
lsmeans(oranges.lm1, "day", at = list(price1 = 50, 
    price2 = c(40,60), day = c("2","3","4")) )


###################################################
### code chunk number 8: using-lsmeans.rnw:157-160
###################################################
org.lsm <- lsmeans(oranges.lm1, "day", by = "price2", 
    at = list(price1 = 50, price2 = c(40,60), day = c("2","3","4")) )
org.lsm


###################################################
### code chunk number 9: using-lsmeans.rnw:163-166 (eval = FALSE)
###################################################
## lsmeans(oranges.lm1, ~ day | price, at = ... )         # Ex 1
## lsmeans(oranges.lm1, c("day","price2"), at = ... )     # Ex 2
## lsmeans(oranges.lm1, ~ day * price, at = ... )         # Ex 3


###################################################
### code chunk number 10: using-lsmeans.rnw:172-173
###################################################
str(org.lsm)


###################################################
### code chunk number 11: using-lsmeans.rnw:178-180
###################################################
( org.sum <- summary(org.lsm, infer = c(TRUE,TRUE), 
                    level = .90, adjust = "bon", by = "day") )


###################################################
### code chunk number 12: using-lsmeans.rnw:185-186
###################################################
class(org.sum)


###################################################
### code chunk number 13: using-lsmeans.rnw:190-191
###################################################
transform(org.sum, lsrubles = lsmean * 34.2)


###################################################
### code chunk number 14: using-lsmeans.rnw:197-199
###################################################
org.lsm2 <- update(org.lsm, by.vars = NULL, level = .99)
org.lsm2


###################################################
### code chunk number 15: using-lsmeans.rnw:205-206
###################################################
contrast(org.lsm, method = "eff")


###################################################
### code chunk number 16: using-lsmeans.rnw:211-213
###################################################
days.lsm <- lsmeans(oranges.rg1, "day")
contrast(days.lsm, "trt.vs.ctrl", ref = c(5,6))


###################################################
### code chunk number 17: using-lsmeans.rnw:218-219 (eval = FALSE)
###################################################
## confint(contrast(days.lsm, "trt.vs.ctrlk"))


###################################################
### code chunk number 18: using-lsmeans.rnw:227-228
###################################################
pairs(org.lsm)


###################################################
### code chunk number 19: using-lsmeans.rnw:231-232
###################################################
cld(days.lsm, alpha = .10)


###################################################
### code chunk number 20: using-lsmeans.rnw:239-242
###################################################
oranges.mlm <- lm(cbind(sales1,sales2) ~ price1 + price2 + day + store, 
                 data = oranges)
ref.grid(oranges.mlm)


###################################################
### code chunk number 21: using-lsmeans.rnw:245-247
###################################################
org.mlsm <- lsmeans(oranges.mlm, ~ day | variety, mult.name = "variety")
cld(org.mlsm, sort = FALSE)


###################################################
### code chunk number 22: using-lsmeans.rnw:252-253
###################################################
org.vardiff <- update(pairs(org.mlsm, by = "day"), by = NULL)


###################################################
### code chunk number 23: using-lsmeans.rnw:256-257
###################################################
cld(org.vardiff)


###################################################
### code chunk number 24: using-lsmeans.rnw:265-266
###################################################
confint(days.lsm, adjust = "bon")


###################################################
### code chunk number 25: using-lsmeans.rnw:270-272
###################################################
library("multcomp")
confint(as.glht(days.lsm))


###################################################
### code chunk number 26: using-lsmeans.rnw:278-280
###################################################
summary(glht(oranges.lm1, lsm("day", contr="eff")), 
        test = adjusted("free"))


###################################################
### code chunk number 27: using-lsmeans.rnw:284-285 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm)))


###################################################
### code chunk number 28: using-lsmeans.rnw:288-289 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm), by = NULL))


###################################################
### code chunk number 29: using-lsmeans.rnw:292-293 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm, by = NULL)))


###################################################
### code chunk number 30: using-lsmeans.rnw:302-307
###################################################
data("Oats", package = "nlme")
library("lme4")
Oats.lmer <- lmer(log(yield) ~ Variety*factor(nitro) + (1|Block/Variety), 
                 data = Oats)
anova(Oats.lmer)


###################################################
### code chunk number 31: oatcontr (eval = FALSE)
###################################################
## contrast(lsmeans(Oats.lmer, "nitro"), "poly")


###################################################
### code chunk number 32: using-lsmeans.rnw:314-315
###################################################
cat("NOTE: Results may be misleading due to involvement in interactions")


###################################################
### code chunk number 33: using-lsmeans.rnw:317-318
###################################################
contrast(lsmeans(Oats.lmer, "nitro"), "poly")


###################################################
### code chunk number 34: using-lsmeans.rnw:321-323
###################################################
Oats.lmer2 <- lmer(log(yield) ~ Variety + poly(nitro,2) 
                                + (1|Block/Variety),  data = Oats)


###################################################
### code chunk number 35: using-lsmeans.rnw:326-328
###################################################
Oats.lsm2 <- lsmeans(Oats.lmer2, ~ nitro | Variety, cov.reduce = FALSE)
Oats.lsm2


###################################################
### code chunk number 36: oatslmer
###################################################
lsmip(Oats.lmer, Variety ~ nitro, ylab = "Observed log(yield)")


###################################################
### code chunk number 37: oatslmer2
###################################################
lsmip(Oats.lsm2, Variety ~ nitro, ylab = "Predicted log(yield)")


###################################################
### code chunk number 38: using-lsmeans.rnw:365-366
###################################################
str(Oats.lsm2)


###################################################
### code chunk number 39: using-lsmeans.rnw:369-370
###################################################
summary(Oats.lsm2, type = "response")


###################################################
### code chunk number 40: chick-plot
###################################################
xyplot(weight~Time | Diet, groups = ~ Chick, data=ChickWeight, type="o", 
       layout=c(4,1))


###################################################
### code chunk number 41: using-lsmeans.rnw:388-390
###################################################
Chick.lmer <- lmer(weight ~ Diet * Time + (0 + Time | Chick), 
    data = ChickWeight)


###################################################
### code chunk number 42: using-lsmeans.rnw:393-394
###################################################
( Chick.lst <- lstrends (Chick.lmer, ~ Diet, var = "Time") )


###################################################
### code chunk number 43: using-lsmeans.rnw:397-398
###################################################
cld (Chick.lst)


###################################################
### code chunk number 44: using-lsmeans.rnw:406-409
###################################################
lsm.options(ref.grid = list(level = .90),
            lsmeans = list(),
            contrast = list(infer = c(TRUE,TRUE)))


###################################################
### code chunk number 45: using-lsmeans.rnw:419-420
###################################################
lsmeans(Oats.lmer2, pairwise ~ Variety)


###################################################
### code chunk number 46: using-lsmeans.rnw:424-425
###################################################
lsm.options(ref.grid = NULL, contrast = NULL)


###################################################
### code chunk number 47: using-lsmeans.rnw:432-435
###################################################
nutr.lm <- lm(gain ~ (age + group + race)^2, data = nutrition)
library("car")
Anova(nutr.lm)


###################################################
### code chunk number 48: nutr-intplot
###################################################
lsmip(nutr.lm, race ~ age | group)
lsmeans(nutr.lm, ~ group*race)


###################################################
### code chunk number 49: using-lsmeans.rnw:452-454
###################################################
nutr.lsm <- lsmeans(nutr.lm, ~ group * race, weights = "proportional",
    at = list(age = c("2","3"), race = c("Black","White")))


###################################################
### code chunk number 50: using-lsmeans.rnw:457-460
###################################################
nutr.lsm    
summary(pairs(nutr.lsm, by = "race"), by = NULL)
summary(pairs(nutr.lsm, by = "group"), by = NULL)


###################################################
### code chunk number 51: using-lsmeans.rnw:473-477
###################################################
lsmeans(nutr.lm, "race", weights = "equal")
lsmeans(nutr.lm, "race", weights = "prop")
lsmeans(nutr.lm, "race", weights = "outer")
lsmeans(nutr.lm, "race", weights = "cells")


###################################################
### code chunk number 52: using-lsmeans.rnw:484-486
###################################################
temp = lsmeans(nutr.lm, c("group","race"), weights = "prop")
lsmeans(temp, "race", weights = "prop")


###################################################
### code chunk number 53: using-lsmeans.rnw:491-492
###################################################
with(nutrition, tapply(gain, race, mean))


###################################################
### code chunk number 54: using-lsmeans.rnw:497-499
###################################################
feedlot.lm <- lm(swt ~ ewt + herd * diet, data = feedlot)
Anova(feedlot.lm)


###################################################
### code chunk number 55: using-lsmeans.rnw:502-503
###################################################
feedlot.add <- update(feedlot.lm, . ~ . - herd:diet)


###################################################
### code chunk number 56: using-lsmeans.rnw:506-507
###################################################
cld(lsmeans(feedlot.add, "herd"))


###################################################
### code chunk number 57: using-lsmeans.rnw:511-512
###################################################
cld(lsmeans(feedlot.add, "herd", cov.reduce = ewt ~ herd))


###################################################
### code chunk number 58: using-lsmeans.rnw:517-519 (eval = FALSE)
###################################################
## rg <- ref.grid(my.model, at = list(x1 = c(5,10,15)),
##                cov.reduce = list(x2 ~ x1,  x3 ~ x1 + x2))


###################################################
### code chunk number 59: using-lsmeans.rnw:541-546
###################################################
library("MASS")
housing.plr <- polr(Sat ~ Infl + Type + Cont,
                   data = housing, weights = Freq)
ref.grid(housing.plr)
housing.lsm <- lsmeans(housing.plr, ~ Infl | cut)


###################################################
### code chunk number 60: using-lsmeans.rnw:549-551
###################################################
summary(housing.lsm, type = "response")
summary(pairs(housing.lsm), type = "response") [1:3, ]


