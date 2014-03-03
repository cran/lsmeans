### R code from vignette source 'lsmeans-changes.rnw'

###################################################
### code chunk number 1: lsmeans-changes.rnw:39-45
###################################################
library(lsmeans)

### OLD
warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
warp.oldlsm <- .old.lsmeans(warp.lm, ~ tension | wool)
class(warp.oldlsm)


###################################################
### code chunk number 2: lsmeans-changes.rnw:47-48
###################################################
class(warp.oldlsm[[1]])


###################################################
### code chunk number 3: lsmeans-changes.rnw:51-54
###################################################
### NEW
warp.lsmobj <- lsmeans(warp.lm, ~ tension | wool)
class(warp.lsmobj)


###################################################
### code chunk number 4: lsmeans-changes.rnw:56-57
###################################################
"lsmobj"


###################################################
### code chunk number 5: lsmeans-changes.rnw:60-61
###################################################
warp.lsmobj


###################################################
### code chunk number 6: lsmeans-changes.rnw:66-68
###################################################
### OLD
warp.oldlsm[[1]]$lsmean


###################################################
### code chunk number 7: lsmeans-changes.rnw:70-73
###################################################
Try <- function(expr) tryCatch(expr, error = function(e) cat("Oops!\n"))
### NEW
Try(warp.lsmobj$lsmean)


###################################################
### code chunk number 8: lsmeans-changes.rnw:76-78
###################################################
### NEW
summary(warp.lsmobj)$lsmean


###################################################
### code chunk number 9: lsmeans-changes.rnw:81-82
###################################################
as.data.frame(summary(warp.lsmobj))


###################################################
### code chunk number 10: lsmeans-changes.rnw:86-90
###################################################
### NEW
warp.l2 <- lsmeans(warp.lm, pairwise ~ tension)
class(warp.l2)
sapply(warp.l2, class)


###################################################
### code chunk number 11: lsmeans-changes.rnw:104-105
###################################################
confint(warp.lsmobj, level = .90)


###################################################
### code chunk number 12: lsmeans-changes.rnw:108-110
###################################################
library(multcomp)
summary(as.glht(warp.lsmobj))


###################################################
### code chunk number 13: lsmeans-changes.rnw:113-114
###################################################
warp.lsmobj@linfct


###################################################
### code chunk number 14: lsmeans-changes.rnw:123-132
###################################################
library(lme4)
data(Oats, package = "nlme")
Oats.lmer <- lmer(yield ~ factor(nitro) + Variety + (1|Block/Variety), 
    data = Oats, subset = -c(1,2,3,5,8,13,21,34,55))

### OLD
.old.lsmeans(Oats.lmer, pairwise ~ Variety)
### NEW
lsmeans(Oats.lmer, pairwise ~ Variety)


###################################################
### code chunk number 15: lsmeans-changes.rnw:138-142
###################################################
### OLD
.old.lsmeans(Oats.lmer, ~ nitro, at = list(nitro = c(.1,.2,.3)))
### NEW
lsmeans(Oats.lmer, ~ nitro, at = list(nitro = c(.1,.2,.3)))


###################################################
### code chunk number 16: lsmeans-changes.rnw:147-151
###################################################
(Oats.rg <- ref.grid(Oats.lmer))
Oats.quad <- update(Oats.lmer, yield ~ Variety + poly(nitro,2) + (1|Block/Variety))
ref.grid(Oats.quad)
ref.grid(Oats.quad, at = list(nitro = c(.1,.2,.3)))


###################################################
### code chunk number 17: lsmeans-changes.rnw:156-157
###################################################
(Oats.lsm <- lsmeans(Oats.rg, "nitro", by = "Variety"))


###################################################
### code chunk number 18: lsmeans-changes.rnw:160-162
###################################################
str(Oats.lsm)
(Oats.n <- lsmeans(Oats.lsm, "nitro"))


###################################################
### code chunk number 19: lsmeans-changes.rnw:167-168
###################################################
slotNames(Oats.lsm)


###################################################
### code chunk number 20: lsmeans-changes.rnw:174-175 (eval = FALSE)
###################################################
## summary(Oats.lsm, by = "nitro")


###################################################
### code chunk number 21: lsmeans-changes.rnw:178-179
###################################################
summary(Oats.n, infer = c(TRUE,TRUE))


###################################################
### code chunk number 22: lsmeans-changes.rnw:185-186
###################################################
(warp.con <- contrast(warp.lsmobj, method = "poly"))


###################################################
### code chunk number 23: lsmeans-changes.rnw:189-190
###################################################
contrast(warp.con, "revpairwise", by = "contrast")


###################################################
### code chunk number 24: lsmeans-changes.rnw:194-195
###################################################
cld(Oats.n, sort = FALSE)


###################################################
### code chunk number 25: lsmeans-changes.rnw:199-202
###################################################
chick.lmer <- lmer(weight ~ Time * Diet + (0 + Time | Chick), data = ChickWeight)
chick.lst <- lstrends(chick.lmer, ~ Diet, var = "Time")
cld(chick.lst, Letters = "|||||")


###################################################
### code chunk number 26: lsmeans-changes.rnw:211-212
###################################################
head(MOats)


###################################################
### code chunk number 27: lsmeans-changes.rnw:215-217
###################################################
MOats.mlm <- lm(yield ~ Block + Variety, data = MOats)
(MOats.rg <- ref.grid(MOats.mlm, mult.levs = list(nitro = c(0,.2,.4,.6))))


###################################################
### code chunk number 28: lsmeans-changes.rnw:222-224
###################################################
lsmeans(MOats.rg, ~ nitro)
lsmeans(MOats.rg, ~ Variety)


###################################################
### code chunk number 29: lsmeans-changes.rnw:227-229
###################################################
MOats <- transform(MOats, avg.yield = apply(yield, 1, mean))
lsmeans(lm(avg.yield ~ Block + Variety, data = MOats), ~ Variety)


###################################################
### code chunk number 30: lsmeans-changes.rnw:235-239
###################################################
library(survival)
cgd.ph <- coxph(Surv(tstart, tstop, status) ~ treat * inherit + 
                sex + age + cluster(id), data = cgd)
(cgd.lsm <- lsmeans(cgd.ph, ~ treat | inherit))


###################################################
### code chunk number 31: lsmeans-changes.rnw:246-247
###################################################
summary(cgd.lsm, type = "response")


###################################################
### code chunk number 32: lsmeans-changes.rnw:250-253
###################################################
logwarp.rg <- ref.grid(update(warp.lm, log(breaks) ~ .))
summary(logwarp.rg)
summary(logwarp.rg, type = "response")


