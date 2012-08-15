\name{lsmeans}
\alias{lsmeans}
\title{Least-squares means}
\description{
Compute least-squares means for specified factors or factor combinations in a linear model,
and optionally comparisons or contrasts among them.
}
\usage{
lsmeans(object, specs, contr = list(), at, 
  cov.reduce = function(x, name) mean(x), 
  fac.reduce = function(coefs, lev) apply(coefs, 2, mean), ...)
}
\arguments{
  \item{object}{
A \code{lm}, \code{lme}, or \code{lmer} object having at least one fixed factor among the predictors.
}
  \item{specs}{
A formula, or a list of formulas, specifying the desired families of least-squares means.
The right-hand side of each formula specifies the desired factor levels. The optional left-hand side specifies
what kind of comparisons or contrasts are desired. For example, \code{~ treatment} requests least-squares means for each level of \code{treatment}, and \code{pairwise ~ treatments} requests those results, plus pairwise comparisons among them.
As another example, in a three-factor model, \code{ctrl1 ~ A | B:C} requests least-squares means for all combinations of factors \code{A}, \code{B}, and \code{C}, as well as treatment-minus-control comparisons of \code{A} for each combination of \code{B} and \code{C}, where the first level of \code{A} is considered the control level.
}
  \item{contr}{
An optional named list. Each entry is itself a list or a data.frame specifying contrast coefficients. If the left-hand side of a formula in \code{specs} matches a name in \code{contr}, then those contrasts are estimated with the specified least-squares means. An error will result if the length or one or more contrast vectors mismatches the number of levels of the factor or factor combination. Actually, it is not necessary to give contrasts; one may use this argument to estimate arbitrary linear combinations of the least-squares means.
}
  \item{at}{
An optional named list or named vector of covariate values at which predictions are computed (give only one value for each covariate). If no value is found in \code{at} for a particular covariate, then \code{cov.reduce} is called.
}
  \item{cov.reduce}{
A function with arguments \code{x} and \code{name} that should return the value to use in prediction for the covariate with name \code{name} and values \code{x}. By default, the mean is used. If specified, the \code{name} argument will distinguish one covariate from another.
}
  \item{fac.reduce}{
A function of \code{coefs} and \code{lev} where \code{lev} is the level of a factor or factor combination at which a least-squares mean is calculated.
The argument \code{coefs} is a matrix whose rows correspond to the combinations of all factors in the model other than those involved in the \code{lsmeans} specification. Each row has the coefficients for the linear combination of the regression coefficients to be used in that case. By default, these rows are averaged together (mimicking SAS), but the user may override that behavior. Besides \code{lev}, \code{names(lev)} will provide the name of the factor or factor combination, and the \code{row.names} of \code{coefs} provide the levels of the extraneous factors.
}
  \item{\dots}{
Additional argument that is passed to the contrast function. See Details.
}
}
\details{
Least-squares means, popularized by SAS, are predictions from a linear model at combinations
of specified factors. SAS's documentation describes them as ``predicted population margins---that is, they estimate the marginal means over a balanced population'' (SAS Institute 2012).
Unspecified factors and covariates are handled by summarizing the predictions
over those factors and variables. For example, if the fitted model has formula \code{response ~ x1 + x2 + treat}
where \code{a1} and \code{x2} are numeric and \code{treat} is a factor, the least-squares means will be the predicted response for each treatment, at some specified values of \code{x1} and \code{x2}. By default, the means of the two covariates will be used, resulting in what ANOVA textbooks oftem call the adjusted means. We may use that \code{at} argument to instead make predictions at other values of \code{x1} and \code{x2}.

Now consider the model \code{response ~ A + B + A:B}, where \code{A} and \code{B} are both factors. If we ask for least-squares means for \code{A}, then at each level of \code{A} we are faced with a different prediction for each level of \code{B}. Blind (and default) use of least-squares means would result in these predictions being averaged together with equal weight, and this may be inappropriate, especially when the interaction effect is strong. Like most statistical calculations, it is possible to use least-squares means inappropriately. The \code{fac.reduce} argument at least expands one's options in producing meaningful results in multi-factor situations. 

One other note converning covariates: One must be careful with polynomials and such. If terms such as \code{x} and \code{I(x^2)} are used in the model, the default behavior will make predictions at \code{x = mean(x)} and \code{x^2 = mean(x^2)}, which isn't a valid combination (\code{mean(x)} and \code{mean(x)^2} produce a valid prediction).
Fortunely, taking advantage of R's built-in power comes to the rescue. The covariate values are summarized in the \code{model.frame}, \emph{before} expanding to a \code{model.matrix} from which predictions are made. Thus, 
if the model contains, say, \code{poly()} or \code{ns()} terms, the covariates therein are handled correctly because these are evaluated after the covariates are summarized.

The built-in contrast methods that can be used in \code{specs} formulas are \code{pairwise}, \code{revpairwise}, \code{poly}, \code{ctrl}, \code{ctrl1}, and \code{ctrlk}. They are implemented as functions \code{\link{pairwise.lsmc}}, etc. having the same names with \code{.lsmc} added. Users may write additional \code{.lsmc} functions that generate custom families of contrasts. See the documentation for \code{\link{pairwise.lsmc}} for an example.
}
\value{
A list of data.frames. Each element summarizes a family of least-squares means or contrasts among them. Each data.frame has the factor levels or appropriate contrasts labels as row names, and the columns consist of the estimates, their standard errors, and their \emph{t} ratios. No degrees of freedom of P values are provided at this time.
}
\references{
SAS Institute Inc. (2012), Online documentation; Shared concepts; LSMEANS statement,
\url{http://support.sas.com/documentation/cdl/en/statug/63962/HTML/default/viewer.htm#statug_introcom_a0000003362.htm}, accessed August 15, 2012.
}
\note{
The function \code{\link[doBy]{popMeans}} in the \code{doBy} package provides similar capabilities with a different interface.
}
\author{
Russell V. Lenth, The University of Iowa
}
\section{Warning}{
In its current version, \code{lsmeans} cannot correctly handle terms that are coerced to factors in the model formula,
e.g. \code{response ~ factor(dosage)}. One must create the factor beforehand. Perhaps a future version will work around the confusion between names in the dataset and names in the model.
}

\seealso{
\code{\link{pairwise.lsmc}}
}
\examples{
require(lsmeans)
require(nlme)

### Split-plot example ###
Oats$nitroF = ordered(Oats$nitro)  ## also creates my own local copy of this data.frame

# Main-effects model
Oats.lme = lme(yield ~ nitroF + Variety, random = ~1 | Block/Variety, data=Oats)
lsmeans(Oats.lme, list(poly ~ nitroF, pairwise ~ Variety))

# Custom contrasts
lsmeans(Oats.lme, my.own ~ Variety, 
  contr = list(my.own = list(G.vs.M = c(1,-1,0), GM.vs.V = c(.5,.5,-1))))

# Model with interaction, follow-ups at levels of other factors
OatsInt.lme = update(Oats.lme, . ~ . + nitroF:Variety)
lsmeans(OatsInt.lme, list(revpairwise ~ Variety | nitroF, poly ~ nitroF | Variety))
# The following will produce the above results for Victory only
lsmeans(OatsInt.lme, poly ~ nitroF, fac.reduce = function(coef, lev) coef[3, ])

# Add a fake covariate that has some predictive ability
set.seed(271828, "default", "default")   # for reproducibility of results
Oats$fakex = sqrt(Oats$yield) + rnorm(nrow(Oats), 5, .25)
OatsCov.lme = update(Oats.lme, . ~ . + poly(fakex,3))
lsmeans(OatsCov.lme, poly ~ nitroF)
lsmeans(OatsCov.lme, poly ~ nitroF, at = c(fakex=12)) # diff lsmeans but same poly results
}
\keyword{ models }
\keyword{ regression }
\keyword{ htest }
