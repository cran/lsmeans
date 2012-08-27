\name{empty.cells}
\alias{empty.cells}

\title{Identify empty factor combinations}
\description{This function exists primarily for use by \code{\link{lsmeans}} with \code{check.cells=TRUE}; however, it may be useful for assessing potential problems before fitting a model.}

\usage{
empty.cells(form, data)
}

\arguments{
  \item{form}{A model formula. Only the right-hand side is used.}
  \item{data}{A \code{data.frame}. Unlike in \code{\link{lm}} and related functions, it is \emph{not} optional.}
}
\details{Empty cells are defined in the \emph{context of the model formula}. For example, suppose that we have a factorial design if factors A and B, but there are no data on certain combinations of A and B. If we call \code{empty.cells} with the model \code{~ A*B} or \code{A + B + A:B}, the empty cells will be identified. However, if we specify the additive model \code{~ A + B}, no empty cells will be identified because this model can make valid predictions for all combinations of A and B. }

\value{A \code{list}. Each element is associated with a factor or combination of factors where empty cells were found, and contains a \code{data.frame} of the factor levels where they occur. If no empty cells are found, an empty \code{list} is returned.}

\author{Russell V. Lenth}

\seealso{\code{\link{lsmeans}}}

\examples{
adesign = data.frame(
    A = LETTERS[c(1,1,1, 2,2, 3,3)],
    B = letters[c(1,2,3, 2,2, 2,3)])
empty.cells(~ A * B, adesign)   # missing combinations
empty.cells(~ A + B, adesign)   # empty list
}

\keyword{ design }
\keyword{ regression }
