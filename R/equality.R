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

#' Check difference and / or (in)equality of numbers
#'
#' Check if one number is greater / lower than (or equal to) another.
#'
#' @name agtb
#' @rdname equality
#' @aliases altb
#' @aliases agteb
#' @aliases alteb
#' @aliases aeqb
#' @aliases aneqb
#' @rawRd
#' \usage{
#' agtb(a, b, bincat)
#' altb(a, b, bincat)
#' agteb(a, b, bincat)
#' alteb(a, b, bincat)
#' aeqb(a, b, bincat)
#' aneqb(a, b, bincat)
#' }
#'
#' \arguments{
#'   \item{a}{Numerical vector}
#'   \item{b}{Numerical vector}
#'   \item{bincat}{Binary categorization values, an atomic vector of length 2}
#' } 
#'
#' \details{
#' Not all numbers (especially the decimal ones) can be represented exactly in
#' floating point arithmetic, and their arithmetic may not give the normal expected
#' result.
#'
#' This set of functions check for the in(equality) between two numerical vectors a
#' and b, with the following name convention:
#'
#' \bold{\code{gt}} means \dQuote{greater than}
#'
#' \bold{\code{lt}} means a \dQuote{lower than} b
#'
#' \bold{\code{gte}} means a \dQuote{greater than or equal to} b
#'
#' \bold{\code{lte}} means a \dQuote{lower than or equal to} b
#'
#' \bold{\code{eq}} means a \dQuote{equal to} b
#'
#' \bold{\code{neq}} means a \dQuote{not equal to} b
#'
#' The argument \bold{\code{values}} is useful to replace the TRUE / FALSE values
#' with custom categories.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#'
#' \references{
#' Goldberg, David (1991) "What Every Computer Scientist Should Know About
#' Floating-point Arithmetic", ACM Computing Surveys vol.23, no.1, pp.5-48,
#' \doi{10.1145/103162.103163}
#' }
#'
#'
#' \keyword{functions}
NULL
`undeclareit` <- function(x, drop = FALSE, ...) {
    na_index <- attr(x, "na_index")
    attrx <- attributes(x)
    attributes(x) <- NULL 
    if (!is.null(na_index)) {
        x[na_index] <- names(na_index)
    }
    x <- coerceMode(x)
    attrx$na_index <- NULL
    attrx$na_values <- NULL
    attrx$na_range <- NULL
    if (isFALSE(drop)) {
        attributes (x) <- attrx
    }
    return(x)
}
#' @export
`agtb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- (a - tol) > b
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}
#' @export
`altb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- a < (b - tol)
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}
#' @export
`agteb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- (a + tol) > b
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}
#' @export
`alteb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- a < (b + tol)
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}
#' @export
`aeqb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- abs(a - b) < tol
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}
#' @export
`aneqb` <- function(a, b, bincat) {
    if (inherits(a, "declared")) a <- undeclareit(a)
    if (inherits(b, "declared")) b <- undeclareit(b)
    tol <- getOption("admisc.tol")
    result <- abs(a - b) > tol
    if (!missing(bincat)) {
        if (!is.atomic(bincat) || length(bincat) != 2) {
            stopError(
                "The argument 'bincat' should be an atomic vector of length 2"
            )
        }
        false <- !result
        result[result] <- bincat[1]
        result[false] <- bincat[2]
    }
    return(coerceMode(result))
}
