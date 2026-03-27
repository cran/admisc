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

#' Evaluate an Expression in a Data Environment
#'
#' Evaluate an R expression in an environment constructed from data.
#'
#' @name inside
#' @rdname inside
#' @aliases inside.list
#' @rawRd
#' \usage{
#' inside(data, expr, ...)
#'
#' \S3method{inside}{list}(data, expr, keepAttrs = TRUE, \dots)
#' }
#'
#' \arguments{
#'     \item{data}{Data to use for constructing an environment a \code{data frame}
#'         or a \code{list}.}
#'     \item{expr}{Expression to evaluate, often a \dQuote{compound} expression,
#'         i.e., of the form \preformatted{
#'             {
#'                 a <- somefun()
#'                 b <- otherfun()
#'                 .....
#'                 rm(unused1, temp)
#'             }
#'         }}
#'     
#'     \item{keepAttrs}{For the \code{\link{list}} method of \code{inside()},
#'         a \code{\link{logical}} specifying if the resulting list should keep
#'         the \code{\link{attributes}} from \code{data} and have its
#'         \code{\link{names}} in the same order.  Often this is unneeded as
#'         the result is a \emph{named} list anyway, and then \code{keepAttrs =
#'         FALSE} is more efficient.}
#'     \item{...}{Arguments to be passed to (future) methods.}
#' }
#'
#' \details{
#' This is a modified version of the base R function \code{within()}, with exactly
#' the same arguments and functionality but only one fundamental difference:
#' instead of returning a modified copy of the input data, this function alters the
#' data directly.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' mt <- mtcars
#' inside(mt, hwratio <- hp/wt)
#'
#' dim(mtcars)
#'
#' dim(mt)
#' }
#'
#' \keyword{functions}
NULL
#' @export
`inside` <- function(data, expr, ...) {
    UseMethod("inside")
}
#' @export
`inside.data.frame` <- function(data, expr, ...) {
    dataname <- deparse(substitute(data))
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), e)
    l <- as.list(e, all.names = TRUE)
    l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    nl <- names(l)
    del <- setdiff(names(data), nl)
    data[nl] <- l
    data[del] <- NULL
    if (exists(dataname, parent)) {
        parent[[dataname]] <- data
    }
    else {
        structure_string <- paste(capture.output(dput(data)), collapse = " ")
        eval(
            parse(text = sprintf(paste(dataname, "<- %s"), structure_string)),
            envir = parent
        )
    }
}
#' @export
`inside.list` <- function(data, expr, keepAttrs = TRUE, ...) {
    parent <- parent.frame()
    dataname <- deparse(substitute(data))
    e <- evalq(environment(), data, parent)
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr", "keepAttrs"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), e)
    if (keepAttrs) { 
        l <- as.list(e, all.names=TRUE)
        nl <- names(l)
        del <- setdiff(names(data), nl) 
        data[nl] <- l
        data[del] <- NULL
    } else { 
        data <- as.list(e, all.names=TRUE)
    }
    if (exists(dataname, parent)) {
        parent[[dataname]] <- data
    }
    else {
        structure_string <- paste(capture.output(dput(data)), collapse = " ")
        eval(
            parse(text = sprintf(paste(dataname, "<- %s"), structure_string)),
            envir = parent
        )
    }
}
