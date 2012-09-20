pkgname <- "lsmeans"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('lsmeans')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("lsmeans")
### * lsmeans

flush(stderr()); flush(stdout())

### Name: lsmeans
### Title: Least-squares means
### Aliases: lsmeans print.lsm
### Keywords: models regression htest

### ** Examples

require(lsmeans)

### Covariance example (from Montgomery Design (7th ed.), p.591)
fiber = data.frame(
  machine = rep(c("A","B","C"), each=5),
  strength = c(36,41,39,42,49, 40,48,39,45,44, 35,37,42,34,32),
  diameter = c(20,25,24,25,32, 22,28,22,30,28, 21,23,26,21,15))
fiber.lm = lm(strength ~ diameter + machine, data = fiber)

# adjusted means and comparisons, treating machine C as control
lsmeans (fiber.lm, trt.vs.ctrlk ~ machine)


### Factorial experiment
warp.lm = lm(breaks ~ wool * tension, data = warpbreaks)
#-- We only need to see the wool*tension means listed once ...
print(lsmeans (warp.lm,  list(pairwise ~ wool | tension,  poly ~ tension | wool)),
    omit=3)


### Unbalanced split-plot example ###
#-- The imbalance biases the variance estimates somewhat
require(nlme)
Oats.lme = lme(yield ~ factor(nitro) + Variety, random = ~1 | Block/Variety, 
    subset = -c(1,2,3,5,8,13,21,34,55), data=Oats)
lsmeans(Oats.lme, list(poly ~ nitro, pairwise ~ Variety))

# Compare with lmer result (lsmeans provides df, adjusted SEs)
require(lme4)
Oats.lmer = lmer(yield ~ factor(nitro) + Variety + (1 | Block/Variety), 
    subset = -c(1,2,3,5,8,13,21,34,55), data=Oats)
#-- require(pbkrtest) #-- (loaded as needed by lsmeans)
lsmeans(Oats.lmer, list(poly ~ nitro, pairwise ~ Variety))

# Use glht (multcomp) to do comparisons (but does not use adjusted vcov)
#-- require(multcomp) #-- (loaded as needed by lsmeans)
lsmeans(Oats.lmer, pairwise ~ Variety, glhargs=list(df=9.5))

# Custom contrasts
lsmeans(Oats.lmer, my.own ~ Variety, 
  contr = list(my.own = list(G.vs.M = c(1,-1,0), GM.vs.V = c(.5,.5,-1))))




cleanEx()
nameEx("pairwise.lsmc")
### * pairwise.lsmc

flush(stderr()); flush(stdout())

### Name: pairwise.lsmc
### Title: Contrast families
### Aliases: pairwise.lsmc revpairwise.lsmc poly.lsmc trt.vs.ctrl.lsmc
###   trt.vs.ctrl1.lsmc trt.vs.ctrlk.lsmc
### Keywords: models regression htest

### ** Examples

### View orthogonal polynomials for 4 levels
poly.lsmc(1:4)

## Not run: 
##D ### Setting up a custom contrast function
##D helmert.lsmc = function(levs, ...) {
##D   M = as.data.frame(contr.helmert(levs))
##D   names(M) = paste(levs[-1],"vs earlier")
##D   attr(M, "desc") = "Helmert contrasts"
##D   M
##D }
##D lsmeans(Oats.lme, helmert ~ Variety)
## End(Not run)




### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
