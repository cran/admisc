\name{combnk}

\alias{combnk}

\title{Generate all combinations of n numbers, taken k at a time}

\description{
A fast function to generate all possible combinations of n numbers, taken k at a time,
starting from the first k numbers or from a combination that contain a certain number.
}

\usage{
combnk(n, k, aloe = 0, zerobased = FALSE)
}

\arguments{
    \item{n}{A vector of mode numeric, character or factor.}
    \item{k}{Character string or a vector of character strings
                 for recoding specifications.}
    \item{aloe}{At least one value equal to this number.}
    \item{zerobased}{Logical, zero or one based.}
}

\details{
This function differs from function \bold{\code{combn}} in package \bold{\code{utils}},
covering only numerical vectors and therefore a lot faster.

When the argument \bold{\code{aloe}} is specified, the combinations will sequentially
be incremented from those which contain that number.
}


\value{
A matrix with \code{k} rows and \code{choose(n, k)} columns.
}

\author{
Adrian Dusa
}

\examples{
combnk(5, 2)

combnk(5, 2, aloe = 3)
}

\keyword{functions}
