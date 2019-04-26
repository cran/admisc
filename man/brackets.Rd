\name{Brackets}

\alias{insideBrackets}
\alias{outsideBrackets}
\alias{curlyBrackets}
\alias{roundBrackets}

\title{Extract information from a DNF}

\description{
Functions to extract information from an expression written in disjunctive normal form (DNF), written
for multi-value causal conditions. It extracts either the values within brackets, or the causal
conditions' names outside the brackets.
}

\usage{
insideBrackets(x, type = "{", invert = FALSE)
outsideBrackets(x, type = "{")
curlyBrackets(x, outside = FALSE)
roundBrackets(x, outside = FALSE)
}

\arguments{
  \item{x}{Character, a DNF expression.}
  \item{type}{Brackets type: curly, round or square}
  \item{invert}{Logical, if activated returns whatever is not within the brackets}
  \item{outside}{Logical, if activated returns the conditions' names outside the brackets}
}

\details{
Expressions written in DNF - disjunctive normal form are used in Boolean logic, signaling a
disjunction of conjunctions, or in other words a sum of products (SOP).

These expressions are useful in Qualitative Comparative Analysis, a social science methodology
that is employed in the context of searching for causal configurations that are associated
with a certain outcome.

They are also used to draw Venn diagrams with the package \bold{\code{venn}}, which draws any
kind of set intersection (conjunction) based on a custom DNF expression.

The functions \bold{\code{curlyBrackets}} and \bold{\code{roundBrackets}} are just special
cases of the functions \bold{\code{insideBrackets}} and \bold{\code{outsideBrackets}}, using
the argument \bold{\code{type}} as either \code{"\{"} or \code{"("}.

The function \bold{\code{outsideBrackets}} itself can be considered a special case of the
function \bold{\code{insideBrackets}}, when it uses the argument \bold{\code{invert = TRUE}}.

}

\author{
Adrian Dusa
}

\examples{
dnf <- "A{1} + B{2}C{0}"

insideBrackets(dnf) # 1, 2, 0

insideBrackets(dnf, invert = TRUE) # A, B, C

curlyBrackets(dnf) # 1, 2, 0

curlyBrackets(dnf, outside = TRUE) # A, B, C
}

\keyword{functions}
