# Copyright (c) 2019 - 2026, Adrian Dusa
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * The names of its contributors may NOT be used to endorse or promote
#       products derived from this software without specific prior written
#       permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#' Negate Boolean expressions
#'
#' Functions to negate a DNF/SOP expression, or to invert a SOP to a negated POS or
#' a POS to a negated SOP.
#'
#' @name invert
#' @rdname invert
#' @aliases negate
#' @aliases sopos
#' @aliases deMorgan
#' @rawRd
#' \usage{
#' invert(input, snames = "", noflevels, simplify = TRUE, ...)
#'
#' sopos(input, snames = "", noflevels)
#' }
#'
#' \arguments{
#'   \item{input}{A string representing a SOP expression, or a minimization
#'         object of class \code{"QCA_min"}.}
#'   \item{snames}{A string containing the sets' names, separated by commas.}
#'   \item{noflevels}{Numerical vector containing the number of levels for each set.}
#'   \item{simplify}{Logical, allow users to choose between the raw negation or
#'         its simplest form.}
#'   \item{...}{Other arguments (mainly for backwards compatibility).}
#' }
#'
#' \details{
#'
#' In Boolean algebra, there are two transformation rules named after the British
#' mathematician Augustus De Morgan. These rules state that:
#'
#' 1. The complement of the union of two sets is the intersection of their complements.
#'
#' 2. The complement of the intersection of two sets is the union of their complements.
#'
#' In "normal" language, these would be written as:
#'
#' 1. \code{not (A and B) = (not A) or (not B)}
#'
#' 2. \code{not (A or B) = (not A) and (not B)}
#'
#' Based on these two laws, any Boolean expression written in disjunctive normal
#' form can be transformed into its negation.
#'
#' It is also possible to negate all models and solutions from the result of a
#' Boolean minimization from function \bold{\code{\link[QCA]{minimize}()}} in
#' package \bold{\code{QCA}}. The resulting object, of class \code{"qca"}, is
#' automatically recognised by this function.
#'
#' In a SOP expression, the products should normally be split by using a star
#' \bold{\code{*}} sign, otherwise the sets' names will be considered the individual
#' letters in alphabetical order, unless they are specified via \bold{\code{snames}}.
#'
#' To negate multilevel expressions, the argument \bold{\code{noflevels}} is required.
#'
#' It is entirely possible to obtain multiple negations of a single expression, since
#' the result of the negation is passed to function \bold{\code{\link{simplify}()}}.
#'
#' Function \bold{\code{sopos}()} simply transforms an expression from a sum of
#' products (SOP) to a negated product of sums (POS), and the other way round.
#' }
#'
#' \value{
#' A character vector when the input is a SOP expresison, or a named list for
#' minimization input objects, each component containing all possible negations of
#' the model(s).
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \references{
#' Ragin, Charles C. 1987. \emph{The Comparative Method: Moving beyond Qualitative
#' and Quantitative Strategies}. Berkeley: University of California Press.
#' }
#'
#' \seealso{\code{\link[QCA]{minimize}}, \code{\link{simplify}}}
#'
#' \examples{
#'
#' # example from Ragin (1987, p.99)
#' invert(AC + B~C, simplify = FALSE)
#'
#' # the simplified, logically equivalent negation
#' invert(AC + B~C)
#'
#' # with different intersection operators
#' invert(AB*EF + ~CD*EF)
#'
#' # invert to POS
#' invert(a*b + ~c*d)
#'
#' \dontrun{
#' # using an object of class "qca" produced with minimize()
#' # from package QCA
#' library(QCA)
#' cLC <- minimize(LC, outcome = SURV)
#'
#' invert(cLC)
#'
#'
#' # parsimonious solution
#' pLC <- minimize(LC, outcome = SURV, include = "?")
#'
#' invert(pLC)
#' }
#' }
#'
#' \keyword{functions}
NULL
#' @export
`invert` <- function(input, snames = "", noflevels = NULL, simplify = TRUE, ...) {
    input <- recreate(substitute(input))
    snames <- recreate(substitute(snames))
    dots <- list(...)
    scollapse <- ifelse(
        is.element("scollapse", names(dots)),
        dots$scollapse,
        FALSE
    ) 
    if (!is.null(noflevels)) {
        if (is.character(noflevels)) {
            noflevels <- splitstr(noflevels)
            if (possibleNumeric(noflevels)) {
                noflevels <- asNumeric(noflevels)
            }
            else {
                stopError("Invalid number of levels.")
            }
        }
    }
    isol <- NULL
    minimized <- methods::is(input, "QCA_min")
    if (minimized) {
        snames <- input$tt$options$conditions
        star <- any(nchar(snames) > 1)
        if (input$options$use.letters) {
            snames <- LETTERS[seq(length(snames))]
            star <- FALSE
        }
        noflevels <- input$tt$noflevels
        if (is.element("i.sol", names(input))) {
            elengths <- unlist(lapply(input$i.sol, function(x) length(x$solution)))
            isol <- paste(rep(names(input$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
            input <- unlist(lapply(input$i.sol, function(x) {
                lapply(x$solution, paste, collapse = " + ")
            }))
        }
        else {
            input <- unlist(lapply(input$solution, paste, collapse = " + "))
        }
        if (!star) {
            input <- gsub("[*]", "", input)
        }
    }
    if (methods::is(input, "admisc_deMorgan")) {
        input <- unlist(input)
    }
    if (!is.character(input)) {
        stopError("The expression should be a character vector.")
    }
    star <- any(grepl("[*]", input))
    if (!identical(snames, "")) {
        snames <- splitstr(snames)
        if (any(nchar(snames) > 1)) {
            star <- TRUE
        }
    }
    multivalue <- any(grepl("\\[|\\]|\\{|\\}", input))
    if (multivalue) {
        start <- FALSE
        if (is.null(noflevels) | identical(snames, "")) {
            stopError(
                paste(
                    "Set names and their number of levels are required",
                    "to negate multivalue expressions."
                )
            )
        }
    }
    scollapse <- scollapse | any(nchar(snames) > 1) | multivalue | star
    collapse <- ifelse(scollapse, "*", "")
    negateit <- function(
        x, snames = "", noflevels = NULL, simplify = TRUE, collapse = "*"
    ) {
        callist <- list(expression = x)
        callist$snames <- snames
        if (!is.null(noflevels)) callist$noflevels <- noflevels
        trexp <- do.call(translate, callist)
        snames <- colnames(trexp)
        if (is.null(noflevels)) {
            noflevels <- rep(2, ncol(trexp))
        }
        snoflevels <- lapply(noflevels, function(x) seq(x) - 1)
        sr <- nrow(trexp) == 1 
        trcols <- apply(trexp, 2, function(x) any(x != "-1"))
        negated <- paste(
            apply(trexp, 1, function(x) {
                wx <- which(x != -1) 
                x <- x[wx]
                nms <- names(x)
                x <- sapply(seq_along(x), function(i) {
                    paste(
                        setdiff(snoflevels[wx][[i]], splitstr(x[i])),
                        collapse = ","
                    )
                })
                if (multivalue) {
                    return(paste(
                        ifelse(sr | length(wx) == 1, "", "("),
                        paste(
                            nms, "[", x, "]",
                            sep = "",
                            collapse = " + "
                        ),
                        ifelse(sr | length(wx) == 1, "", ")"),
                        sep = ""
                    ))
                }
                else {
                    nms[x == 0] <- paste0("~", nms[x == 0])
                    return(paste(
                        ifelse(sr | length(wx) == 1, "", "("),
                        paste(nms, collapse = " + ", sep = ""),
                        ifelse(sr | length(wx) == 1, "", ")"),
                        sep = ""))
                }
            }),
            collapse = collapse
        )
        negated <- expandBrackets(
            negated,
            snames = snames,
            noflevels = noflevels,
            scollapse = scollapse
        )
        if (simplify) {
            callist$expression <- negated
            callist$scollapse <- identical(collapse, "*")
            callist$snames <- snames[trcols]
            if (!is.null(noflevels)) {
                callist$noflevels <- noflevels[trcols]
            }
            return(unclass(do.call("simplify", callist)))
        }
        return(negated)
    }
    result <- lapply(
        input,
        negateit,
        snames = snames,
        noflevels = noflevels,
        simplify = simplify,
        collapse = collapse
    )
    if (any(unlist(lapply(result, length)) == 0)) {
        return(invisible(character(0)))
    }
    names(result) <- unname(input)
    if (!minimized) {
        attr(result, "expressions") <- input
    }
    if (!identical(snames, "")) {
        attr(result, "snames") <- snames
    }
    if (!is.null(isol)) {
        attr(result, "isol") <- isol
    }
    attr(result, "minimized") <- minimized
    return(classify(result, "admisc_deMorgan"))
}
#' @export
`deMorgan` <- function(...) {
    .Deprecated(msg = "Function deMorgan() is deprecated. Use function invert() instead.\n")
    negate(...)
}
#' @export
`negate` <- function(...) {
    invert(...)
}
