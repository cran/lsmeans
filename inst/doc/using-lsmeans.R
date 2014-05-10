### R code from vignette source 'using-lsmeans.rnw'

###################################################
### code chunk number 1: using-lsmeans.rnw:41-42
###################################################
options(show.signif.stars=FALSE, prompt="R> ", continue="   ", width=100)


###################################################
### code chunk number 2: using-lsmeans.rnw:75-78
###################################################
library(lsmeans)
oranges.lm1 = lm(sales1 ~ price1 + price2 + day + store, data = oranges)
anova(oranges.lm1)


###################################################
### code chunk number 3: using-lsmeans.rnw:81-82
###################################################
( oranges.rg1 = ref.grid(oranges.lm1) )


###################################################
### code chunk number 4: using-lsmeans.rnw:85-86
###################################################
summary(oranges.rg1)


###################################################
### code chunk number 5: using-lsmeans.rnw:91-92
###################################################
lsmeans(oranges.rg1, "day")   ## or lsmeans(oranges.lm1, "day")


###################################################
### code chunk number 6: using-lsmeans.rnw:95-96
###################################################
with(oranges, tapply(sales1, day, mean))


###################################################
### code chunk number 7: using-lsmeans.rnw:104-106
###################################################
lsmeans(oranges.lm1, "day", at = list(price1 = 50, 
    price2 = c(40,60), day = c("2","3","4")) )


###################################################
### code chunk number 8: using-lsmeans.rnw:111-114
###################################################
org.lsm = lsmeans(oranges.lm1, "day", by ="price2", 
    at = list(price1 = 50, price2 = c(40,60), day = c("2","3","4")) )
org.lsm


###################################################
### code chunk number 9: using-lsmeans.rnw:117-120 (eval = FALSE)
###################################################
## lsmeans(oranges.lm1, ~ day | price, at = ... )         # Ex 1
## lsmeans(oranges.lm1, c("day","price2"), at = ... )     # Ex 2
## lsmeans(oranges.lm1, ~ day * price, at = ... )         # Ex 3


###################################################
### code chunk number 10: using-lsmeans.rnw:126-127
###################################################
str(org.lsm)


###################################################
### code chunk number 11: using-lsmeans.rnw:132-134
###################################################
( org.sum = summary(org.lsm, infer=c(TRUE,TRUE), 
                    level=.90, adjust="bon", by = "day") )


###################################################
### code chunk number 12: using-lsmeans.rnw:139-140
###################################################
class(org.sum)


###################################################
### code chunk number 13: using-lsmeans.rnw:143-144
###################################################
cbind(org.sum[, 1:2], lsrubles = org.sum$lsmean * 35.7) # as of April 22, 2014


###################################################
### code chunk number 14: using-lsmeans.rnw:149-151
###################################################
org.lsm2 = update(org.lsm, by.vars = NULL, level = .99)
org.lsm2


###################################################
### code chunk number 15: using-lsmeans.rnw:157-158
###################################################
contrast(org.lsm, "eff")


###################################################
### code chunk number 16: using-lsmeans.rnw:163-165
###################################################
days.lsm = lsmeans(oranges.rg1, "day")
contrast(days.lsm, "trt.vs.ctrl", ref = c(5,6))


###################################################
### code chunk number 17: using-lsmeans.rnw:170-171 (eval = FALSE)
###################################################
## confint(contrast(days.lsm, "trt.vs.ctrlk"))


###################################################
### code chunk number 18: using-lsmeans.rnw:179-180
###################################################
pairs(org.lsm)


###################################################
### code chunk number 19: using-lsmeans.rnw:183-184
###################################################
cld(days.lsm, alpha = .10)


###################################################
### code chunk number 20: using-lsmeans.rnw:191-194
###################################################
oranges.mlm = lm(cbind(sales1,sales2) ~ price1 + price2 + day + store, 
                 data = oranges)
ref.grid(oranges.mlm)


###################################################
### code chunk number 21: using-lsmeans.rnw:197-199
###################################################
org.mlsm = lsmeans(oranges.mlm, ~ day | variety, mult.name="variety")
cld(org.mlsm, sort = FALSE)


###################################################
### code chunk number 22: using-lsmeans.rnw:204-205
###################################################
org.vardiff = update(pairs(org.mlsm, by = "day"), by = NULL)


###################################################
### code chunk number 23: using-lsmeans.rnw:208-209
###################################################
cld(org.vardiff)


###################################################
### code chunk number 24: using-lsmeans.rnw:217-218
###################################################
confint(days.lsm, adjust = "bon")


###################################################
### code chunk number 25: using-lsmeans.rnw:222-224
###################################################
library(multcomp)
confint(as.glht(days.lsm))


###################################################
### code chunk number 26: using-lsmeans.rnw:230-231
###################################################
summary(glht(oranges.lm1, lsm("day", contr="eff")), test = adjusted("free"))


###################################################
### code chunk number 27: using-lsmeans.rnw:235-236 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm)))


###################################################
### code chunk number 28: using-lsmeans.rnw:239-240 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm), by = NULL))


###################################################
### code chunk number 29: using-lsmeans.rnw:243-244 (eval = FALSE)
###################################################
## summary(as.glht(pairs(org.lsm, by = NULL)))


###################################################
### code chunk number 30: using-lsmeans.rnw:253-258
###################################################
data("Oats", package = "nlme")
library("lme4")
Oats.lmer = lmer(log(yield) ~ Variety*factor(nitro) + (1|Block/Variety), 
                 data = Oats)
anova(Oats.lmer)


###################################################
### code chunk number 31: oatcontr (eval = FALSE)
###################################################
## contrast(lsmeans(Oats.lmer, "nitro"), "poly")


###################################################
### code chunk number 32: using-lsmeans.rnw:265-266
###################################################
cat("NOTE: Results may be misleading due to involvement in interactions")


###################################################
### code chunk number 33: using-lsmeans.rnw:268-269
###################################################
contrast(lsmeans(Oats.lmer, "nitro"), "poly")


###################################################
### code chunk number 34: using-lsmeans.rnw:272-274
###################################################
Oats.lmer2 = lmer(log(yield) ~ Variety + poly(nitro,2) + (1|Block/Variety),
                  data = Oats)


###################################################
### code chunk number 35: using-lsmeans.rnw:277-279
###################################################
Oats.lsm2 = lsmeans(Oats.lmer2, ~ nitro | Variety, cov.reduce = FALSE)
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
### code chunk number 38: using-lsmeans.rnw:314-315
###################################################
str(Oats.lsm2)


###################################################
### code chunk number 39: using-lsmeans.rnw:318-319
###################################################
summary(Oats.lsm2, type = "response")


###################################################
### code chunk number 40: chick-plot
###################################################
xyplot(weight~Time | Diet, groups = ~ Chick, data=ChickWeight, type="o", 
       layout=c(4,1))


###################################################
### code chunk number 41: using-lsmeans.rnw:339-341
###################################################
Chick.lmer <- lmer(weight ~ Diet * Time + (0 + Time | Chick), 
    data = ChickWeight)


###################################################
### code chunk number 42: using-lsmeans.rnw:344-346
###################################################
( Chick.lst = lstrends (Chick.lmer, ~ Diet, var = "Time") )
cld (Chick.lst)


###################################################
### code chunk number 43: using-lsmeans.rnw:354-357
###################################################
lsm.options(ref.grid = list(level = .90),
            lsmeans = list(),
            contrast = list(infer = c(TRUE,TRUE)))


###################################################
### code chunk number 44: using-lsmeans.rnw:366-367
###################################################
lsmeans(Oats.lmer2, pairwise ~ Variety)


###################################################
### code chunk number 45: using-lsmeans.rnw:370-371
###################################################
lsm.options(NULL)


###################################################
### code chunk number 46: nutr-intplot
###################################################
nutr.lm <- lm(gain ~ (age + group + race)^2, data = nutrition)
lsmip(nutr.lm, race ~ age | group)
lsmeans(nutr.lm, ~ group*race)


###################################################
### code chunk number 47: using-lsmeans.rnw:410-415
###################################################
library(MASS)
housing.plr = polr(Sat ~ Infl + Type + Cont,
                   data = housing, weights = Freq)
ref.grid(housing.plr)
housing.lsm = lsmeans(housing.plr, "Infl", at = list(cut = "Low|Medium"))


###################################################
### code chunk number 48: using-lsmeans.rnw:418-420
###################################################
summary(housing.lsm, type="response")
summary(pairs(housing.lsm), type="response")


