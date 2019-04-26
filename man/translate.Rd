\name{translate}

\alias{translate}

\title{Translate a DNF expression}

\description{
This function interprets an expression written in a DNF (disjunctive normal form), also known
as SOP (sum of products) form, in both crisp and multivalue sets notation. The expression is 
translated into a standard (canonical) DNF matrix.

For crisp sets notation, upper case letters are considered the presence of a causal
condition, and lower case letters are considered the absence of the respective causal
condition. Tilde is recognized as a negation, even in combination with upper/lower letters.
}

\usage{
translate(expression = "", snames = "", noflevels = NULL, data = NULL, ...)
}

\arguments{
  \item{expression}{String: a QCA expression written in sum of products form.}
  \item{snames}{A string containing the sets' names, separated by commas.}
  \item{noflevels}{Numerical vector containing the number of levels for each set.}
  \item{data}{A dataset with binary cs, mv and fs data.}
  \item{...}{Other arguments, mainly for backwards compatibility.}
}

\details{
A DNF - disjunctive normal form is also known as a SOP - sum of products, or in other
words a "union of intersections", for example \bold{\code{A*D + B*c}}.

The same expression can be written in multivalue notation: \bold{\code{A{1}*D{1} + B{1}*C{0}}}.
Both types of expressions are valid, and yield the same result matrix.

For multivalue notation, causal conditions are expected and will be converted to upper case
by default. Expressions can contain multiple values to translate, separated by a comma. If B was
a multivalue causal condition, an expression could be: \bold{\code{A{1} + B{1,2}*C{0}}}.

In this example, all values in B equal to either 1 or 2 will be converted to 1, and the
rest of the (multi)values will be converted to 0.

This function automatically detects the use of tilde \dQuote{\code{~}} as a negation for a particular
causal condition. \bold{\code{~A}} does two things: it identifies the presence of causal
condition \bold{\code{A}} (because it was specified as upper case) and it recognizes that it
must be negated, because of the  tilde. It works even combined with lower case names:
\bold{\code{~a}}, which is interpreted as \bold{\code{A}}.

To negate a multivalue condition using a tilde, the total number of levels should be supplied
(see examples below), and it works even for intersections between multiple levels of the same
causal condition. For a causal condition with 3 levels (0, 1 and 2) the following expression
\bold{\code{~A{0,2}*A{1,2}}} is equivalent with \bold{\code{A{1}}}, while \bold{\code{A{0}*A{1}}}
results in the empty set.

The number of levels, as well as the set names can be automatically detected from a dataset via
the argument \bold{\code{data}}. Arguments \bold{\code{snames}} and \bold{\code{noflevels}} have
precedence over \bold{\code{data}}, when specified.

The use of the product operator \bold{\code{*}} is redundant when the set names are single
letters (for example \bold{\code{AD + Bc}}), and is also redundant for multivalue data, where 
product terms can be separated by using the curly brackets notation.

When conditions are binary and their names have multiple letters (for example
\bold{\code{AA + CC*bb}}), the use of the product operator \bold{\code{*}} is preferable but the
function manages to translate an expression even without it (\bold{\code{AA + CCbb}}) by searching
deep in the space of the conditions' names, at the cost of slowing down for a high number of causal
conditions. For this reason, an arbitrary limit of 7 causal \bold{\code{snames}} is imposed, to
write an expression.
}


\value{
A matrix containing the implicants on the rows and the set names on the columns, with the
following codes:
\tabular{rl}{
     0 \tab absence of a causal condition\cr
     1 \tab presence of a causal condition\cr
    -1 \tab causal condition was eliminated
}
The matrix was also assigned a class "translate", to avoid printing the -1 codes when signaling
a minimized condition. The mode of this matrix is character, to allow printing multiple levels
in the same cell, such as "1,2".
}

\author{
Adrian Dusa
}


\examples{
translate("A + B*C")

# same thing in multivalue notation
translate("A{1} + B{1}*C{1}")

# using upper/lower letters
translate("A + b*C")

# the negation with tilde is recognised
translate("~A + b*C")

# even in combination of upper/lower letters
translate("~A + ~b*C")

# and even for multivalue variables
# in multivalue notation, the product sign * is redundant
translate("C{1} + T{2} + T{1}V{0} + C{0}")

# negation of multivalue sets requires the number of levels
translate("~A{1} + ~B{0}*C{1}", snames = "A, B, C", noflevels = c(2, 2, 2))

# multiple values can be specified
translate("C{1} + T{1,2} + T{1}V{0} + C{0}")

# or even negated
translate("C{1} + ~T{1,2} + T{1}V{0} + C{0}", snames = "C, T, V", noflevels = c(2,3,2))

# if the expression does not contain the product sign *
# snames are required to complete the translation 
translate("AB + cD", snames = "A, B, C, D")

# otherwise snames are not required
translate("PER*FECT + str*ing")

# snames are required
translate("PERFECT + string", snames = "PER, FECT, STR, ING")

# it works even with overlapping columns
# SU overlaps with SUB in SUBER, but the result is still correct
translate("SUBER + subset", "SU, BER, SUB, SET")

# to print _all_ codes from the standard output matrix
(obj <- translate("A + b*C"))
print(obj, original = TRUE) # also prints the -1 code
}
\keyword{functions}
