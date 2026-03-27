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

#' Numeric vectors
#'
#' Coerces objects to class "numeric", and checks if an object is numeric.
#'
#' @name asNumeric
#' @rdname numerics
#' @aliases possibleNumeric
#' @aliases wholeNumeric
#' @rawRd
#' \usage{
#' asNumeric(x, ...)
#' possibleNumeric(x, each = FALSE)
#' wholeNumeric(x, each = FALSE)
#' }
#'
#' \arguments{
#'   \item{x}{A vector of values}
#'   \item{each}{Logical, return the result for each value in the vector}
#'   \item{...}{Other arguments to be passed for class based methods}
#' }
#'
#'
#' \details{
#' Unlike the function \bold{\code{as.numeric}()} from the \bold{\pkg{base}}
#' package, the function \bold{\code{asNumeric()}} coerces to numeric without a
#' warning if any values are not numeric. All such values are considered NA missing.
#'
#' This is a generic function, with specific class methods for factors and objects
#' of class \dQuote{declared}. The usual way of coercing factors to numeric is
#' meaningless, converting the inner storage numbers. The class method of this
#' particular function coerces the levels to numeric, via the default activated
#' argument \code{levels}.
#'
#' For objects of class \dQuote{declared}, a similar argument called \code{na_values}
#' is by default activated to coerce the declared missing values to numeric.
#'
#' The function \bold{\code{possibleNumeric()}} tests if the values in a vector are
#' possibly numeric, irrespective of their storing as character or numbers. In the
#' case of factors, it tests its levels representation.
#'
#' Function \bold{\code{wholeNumeric()}} tests if numbers in a vector are whole
#' (round) numbers. Whole numbers are different from \dQuote{integer} numbers (which
#' have special memory representation), and consequently the function
#' \bold{\code{is.integer}()} tests something different, how numbers are stored in
#' memory (see the description of function \bold{\code{\link[base]{double}()}} for
#' more details).
#'
#' The function 
#' }
#'
#'
#' \seealso{
#'   \code{\link[base]{numeric}},
#'   \code{\link[base]{integer}},
#'   \code{\link[base]{double}}
#' } 
#'
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' x <- c("-.1", " 2.7 ", "B")
#' asNumeric(x) # no warning
#'
#' f <- factor(c(3, 2, "a"))
#'
#' asNumeric(f)
#'
#' asNumeric(f, levels = FALSE)
#'
#' possibleNumeric(x) # FALSE
#'
#' possibleNumeric(x, each = TRUE) # TRUE  TRUE FALSE
#'
#' possibleNumeric(c("1", 2, 3)) # TRUE
#'
#' is.integer(1) # FALSE
#'
#' # Signaling an integer in R 
#' is.integer(1L) # TRUE
#'
#' wholeNumeric(1) # TRUE
#'
#' wholeNumeric(c(1, 1.1), each = TRUE) # TRUE FALSE
#' }
#'
#'
#' \keyword{functions}
NULL
#' @export
`asNumeric` <- function(x, ...) {
    UseMethod("asNumeric")
}
#' @export
`asNumeric.declared` <- function(x, ..., na_values = TRUE) {
    na_index <- attr(x, "na_index")
    attributes(x) <- NULL
    if (isTRUE(na_values)) {
        if (!is.null(na_index)) {
            x[na_index] <- as.numeric(names(na_index))
        }
    }
    NextMethod()
}
#' @export
`asNumeric.factor` <- function(x, ..., levels = TRUE) {
    if (isTRUE(levels)) {
        return(suppressWarnings(as.numeric(levels(x)))[x])
    }
    return(as.numeric(x))
}
#' @export
`asNumeric.default` <- function(x, ...) {
    attributes(x) <- NULL
    if (is.numeric(x)) {
        return(x)
    }
    x <- gsub("\u00a0", " ", x) 
    result <- rep(NA, length(x))
    multibyte <- grepl("[^!-~ ]", x)
    result[!multibyte] <- suppressWarnings(as.numeric(x[!multibyte]))
    return(result)
}
