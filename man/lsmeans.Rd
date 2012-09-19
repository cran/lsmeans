\name{lsmeans}
\alias{lsmeans}
\alias{print.lsm}

\title{Least-squares means}
\description{
Compute least-squares means for specified factors or factor combinations in a linear model,
and optionally comparisons or contrasts among them.
}
\usage{
lsmeans(object, specs, adjust=c("auto","tukey","sidak","bonferroni","none"),
  conf = .95, at, contr = list(), 
  cov.reduce = function(x, name) mean(x), 
  fac.reduce = function(coefs, lev) apply(coefs, 2, mean), 
  glhargs=NULL, ...)
  
\method{print}{lsm}(x, omit=NULL, ...)
}
\arguments{
  \item{object}{
A \code{lm}, \code{aov} (with no \code{Error} component), \code{glm}, \code{lme}, \code{gls}, \code{lmer}, or \code{glmer} object having at least one fixed factor among the predictors.
}
  \item{specs}{
A formula, or a list of formulas, specifying the desired families of least-squares means.
The right-hand side of each formula specifies the desired factor levels. The optional left-hand side specifies
what kind of comparisons or contrasts are desired. For example, \code{~ treatment} requests least-squares means for each level of \code{treatment}, and \code{pairwise ~ treatments} requests those results, plus pairwise comparisons among them.
As another example, in a three-factor model, \code{trt.vs.ctrl1 ~ A | B:C} requests least-squares means for all combinations of factors \code{A}, \code{B}, and \code{C}, as well as treatment-minus-control comparisons of \code{A} for each combination of \code{B} and \code{C}, where the first level of \code{A} is considered the control level.
}
  \item{adjust}{
Adjustment method for the p values of tests of contrasts.
\code{"auto"} uses the method returned in the \code{"adjust"} attribute of the contrast function;
\code{"tukey"} computes p values using the Studentized range distribution with the number of means in the family;
\code{"sidak"} replaces each p value by \code{1 - (1 - p)^c}, where c is the number of contrasts;
\code{"bonferroni"} multiplies each p value by the number of contrasts in the set, and bounds it by 1;
\code{"none"} makes no adjustments to thep values.
In many cases, these adjustments are only approximate, especially when the degrees of freedom vary
greatly within the family of comparisons. For more accurate adjustments, use \code{glhargs} instead.
}
  \item{conf}{
Desired confidence level for intervals. For robustness, you may specify either a fraction or a percentage; i.e., \code{.975} and \code{97.5} yield the same results.
}
  \item{at}{
An optional named list or named vector of covariate values at which predictions are computed (give only one value for each covariate). If no value is found in \code{at} for a particular covariate, then \code{cov.reduce} is called.
}
  \item{contr}{
An optional named list. Each entry is itself a list or a data.frame specifying contrast coefficients. If the left-hand side of a formula in \code{specs} matches a name in \code{contr}, then those contrasts are estimated with the specified least-squares means. An error will result if the length or one or more contrast vectors mismatches the number of levels of the factor or factor combination. Actually, it is not necessary to give contrasts; one may use this argument to estimate arbitrary linear combinations of the least-squares means.
}
  \item{cov.reduce}{
A function with arguments \code{x} and \code{name} that should return the value to use in prediction for the covariate with name \code{name} and values \code{x}. By default, the mean is used. If specified, the \code{name} argument will distinguish one covariate from another.
}
  \item{fac.reduce}{
A function of \code{coefs} and \code{lev} where \code{lev} is the level of a factor or factor combination at which a least-squares mean is calculated.
The argument \code{coefs} is a matrix whose rows correspond to the combinations of all factors in the model other than those involved in the \code{lsmeans} specification. Each row has the coefficients for the linear combination of the regression coefficients to be used in that case. By default, these rows are averaged together (mimicking SAS), but the user may override that behavior. Besides \code{lev}, \code{names(lev)} will provide the name of the factor or factor combination, and the \code{row.names} of \code{coefs} provide the levels of the extraneous factors.
}
  \item{glhargs}{
If this is a \code{list}, the object and specified contrasts are passed to the function\code{\link[multcomp]{glht}} in the \code{multcomp} package, with the contents of \code{glhargs} as additional arguments. (If you do not wish to provide additional arguments, use \code{glsargs=list()}.) If \code{glhargs} is left at \code{NULL}, or if the \code{multcomp} package is not installed, then \code{glht} is not called, and the contrast results are produced internally by \code{lsmeans}. This argument affects only the results from contrasts and not those for the lsmeans themselves. Note: If \code{glhargs} is used, the \code{adjust} argument is ignored.
}
  \item{\dots}{
Additional argument(s) passed to the contrast function; see Details.
}
\item{x}{Object of class \code{"lsm"}}
\item{omit}{Indexes of elements of \code{x} that you do not want printed.}
}
\details{
Least-squares means, popularized by SAS, are predictions from a linear model at combinations
of specified factors. SAS's documentation describes them as ``predicted population margins---that is, they estimate the marginal means over a balanced population'' (SAS Institute 2012). In generalized linear models, least-squares means are marginal linear predictions that can be transformed back to the response scale via the inverse-link function.
Unspecified factors and covariates are handled by summarizing the predictions
over those factors and variables. For example, if the fitted model has formula \code{response ~ x1 + x2 + treat}
where \code{a1} and \code{x2} are numeric and \code{treat} is a factor, the least-squares means will be the predicted response for each treatment, at some specified values of \code{x1} and \code{x2}. By default, the means of the two covariates will be used, resulting in what ANOVA textbooks oftem call the adjusted means. We may use that \code{at} argument to instead make predictions at other values of \code{x1} and \code{x2}.

Now consider the model \code{response ~ A + B + A:B}, where \code{A} and \code{B} are both factors. If we ask for least-squares means for \code{A}, then at each level of \code{A} we are faced with a different prediction for each level of \code{B}. Blind (and default) use of least-squares means would result in these predictions being averaged together with equal weight, and this may be inappropriate, especially when the interaction effect is strong. Like most statistical calculations, it is possible to use least-squares means inappropriately. The \code{fac.reduce} argument at least expands one's options in producing meaningful results in multi-factor situations. 

One other note concerning covariates: One must be careful with covariates that depend on one another. For example, if a model contains covariates \code{x} and \code{xsq} where \code{xsq = x^2}, the default behavior will make predictions at \code{x = mean(x)} and \code{xsq = mean(xsq)}, which probably isn't a valid combination (we need \code{x = mean(x)} and \code{xsq = mean(x)^2}). The inconsistency is avoided if the model specifis \code{poly(x,2)} (or even \code{x + I(x^2)}) instead of \code{x + xsq}, because then only \code{x} appears as a covariate and everything remains consistent when we substitute its mean value.

The built-in contrast methods that can be used in \code{specs} formulas are \code{pairwise}, \code{revpairwise}, \code{poly}, \code{trt.vs.ctrl}, \code{trt.vs.ctrl1}, and \code{trt.vs.ctrlk}. They are implemented as functions \code{\link{pairwise.lsmc}}, etc. having the same names with \code{.lsmc} added. Users may write additional \code{.lsmc} functions that generate custom families of contrasts. See the documentation for \code{\link{pairwise.lsmc}} for an example.

Degrees of freedom are currently not provided for \code{lme} or \code{glme} objects, or for \code{mer} objects arising from generalized linear models; in those cases, asymptotic results are printed, and this is emphasized by displaying \code{NA} for the defrees of freedom. For linear \code{mer} objects, degrees of freedom are computed using the Kenward and Roger (1997) method, provided the \code{pbkrtest} package is installed (the package is loaded if needed.) Moreover, in that case, the adjusted covariance matrix from the \code{vcovAdj()} function in the \code{pbkrtest} package is used to calculate standard errors. See Halekoh and Højsgaard (2012) and the documentation for \code{\link[pbkrtest]{KRmodcomp}} for more details. Degrees of freedom are not passed to \code{\link[multcomp]{glht}} except in the case of \code{lm} objects. 

If the model contains a matrix among its predictors, each column is averaged using the function specified in \code{cov.reduce}. There is no provision for matrices in the \code{at} argument.
}
\value{
An object of class \code{"lsm"}, which inherits from \code{"list"}. Each element of the list is either a \code{data.frame} or an object of class \code{"glht"} (see the documentation for \code{\link[multcomp]{glht}}). (The latter occur only if \code{glhargs} is non-NULL.) Each element summarizes a family of least-squares means or contrasts among them. Each \code{data.frame} contains lsmeans or contrast estimates and associated quantities; in addition, there may be a \code{mesg} attribute with character string(s) providing information on multiplicity adjustments and such. 

The \code{"lsm"} class has only one method, \code{print}, which displays \code{data.frame} elements as-is along with any \code{mesg} attributes; and the \code{\link{summary}} of any \code{glht} elements. 
}

%%%%\note{}

\references{
Halekoh, U. and Højsgaard, S. (2012),
A Kenward-Roger Approximation and parametric bootsrap methods for tests lin linear mixed models -- the R package \code{pbkrtest}, submitted. %%%%%\emph{Journal of Statistical Software}.

Kenward, M.G. and Roger, J.H. (1997),
Small sample inference for fixed effects from restricted maximum likelihood,
\emph{Biometrics}, 53, 983--997.

SAS Institute Inc. (2012), 
Online documentation; Shared concepts; LSMEANS statement,
\url{http://support.sas.com/documentation/cdl/en/statug/63962/HTML/default/viewer.htm#statug_introcom_a0000003362.htm}, accessed August 15, 2012.
}

\author{
Russell V. Lenth, The University of Iowa
}

\seealso{
For information on contrast functions, see the documentation for \code{\link{pairwise.lsmc}} and its siblings.

The package \code{multcomp} provides more comprehensive methods for multiple comparisons among predicted values. See the documentation for \code{\link[multcomp]{mcp}}.

The function \code{\link[doBy]{popMeans}} in the \code{doBy} package provides similar capabilities with a different interface.
}

\examples{
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

}
\keyword{ models }
\keyword{ regression }
\keyword{ htest }
