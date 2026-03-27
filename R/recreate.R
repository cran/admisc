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

#' Facilitate expression substitution
#'
#' Utility function based on \code{substitute()}, to recover an unquoted input.
#'
#' @name recreate
#' @rdname recreate
#' @rawRd
#' \usage{
#' recreate(x, snames = NULL, ...)
#' }
#'
#' \arguments{
#'   \item{x}{A substituted input.}
#'   \item{snames}{A character string containing set names.}
#'   \item{...}{Other arguments, mainly for internal use.}
#' }
#'
#' \details{
#' This function is especially useful when users have to provide lots of quoted
#' inputs, such as the name of the columns from a data frame to be considered
#' for a particular function.
#'
#' This is actually one of the main uses of the base function
#' \bold{\code{\link[base]{substitute}()}}, but here it can be employed to also
#' detect SOP (sum of products) expressions, explained for instance in function
#' \bold{\code{\link{translate}()}}.
#'
#' Such SOP expressions are usually used in contexts of sufficieny and necessity,
#' which are indicated with the usual signs \code{->} and \code{<-}. These are
#' both allowed by the R parser, indicating standard assignment. Due to the R's
#' internal parsing system, a sufficient expression using \code{->} is automatically
#' flipped to a necessity statement \code{<-} with reversed LHS to RHS, but this
#' function is able to determine what is the expression and what is the output.
#'
#' The other necessity code \code{<=} is also recognized, but the equivalent
#' sufficiency code \code{=>} is not allowed in unquoted expressions.
#' }
#'
#' \value{
#' A quoted, equivalent expression or a substituted object.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \seealso{\code{\link[base]{substitute}}, \code{\link{simplify}}}
#'
#' \examples{
#' recreate(substitute(A + ~B*C))
#'
#' foo <- function(x, ...) recreate(substitute(list(...)))
#'
#' foo(arg1 = 3, arg2 = A + ~B*C)
#'
#' df <- data.frame(A = 1, B = 2, C = 3, Y = 4)
#'
#' # substitute from the global environment
#' # the result is the builtin C() function
#' res <- recreate(substitute(C))
#'
#' is.function(res) # TRUE
#'
#' # search first within the column name space from df
#' recreate(substitute(C), colnames(df))
#' # "C"
#'
#' # necessity well recognized
#' recreate(substitute(A <- B))
#'
#' # but sufficiency is flipped
#' recreate(substitute(A -> B))
#'
#' # more complex SOP expressions are still recovered
#' recreate(substitute(A + ~B*C -> Y))
#' }
#'
#' \keyword{functions}
NULL
#' @export
`recreate` <- function(x, snames = NULL, ...) {
    if (is.null(x) | is.logical(x) | is.character(x) | is.list(x)) return(x)
    withinobj <- function(x) {
        x <- gsub("\"|[[:space:]]", "", x)
        for (i in seq(length(x))) {
            if (!grepl("<-|->", x[i])) {
                x[i] <- gsub(">|=>|-\\.>", "->", gsub("<|<=|<\\.-", "<-", x[i]))
            }
            arrows <- c("<-", "->")
            found <- sapply(arrows, grepl, x[i])
            if (sum(found) > 0) {
                if (sum(found) > 1) {
                    stopError("Ambiguous expression, more than one relation sign.")
                }
                xs <- unlist(strsplit(x[i], split = arrows[found]))
                if (length(xs) == 2) {
                    if (all(grepl("\\*|\\+", xs))) {
                        stopError("The outcome should be a single condition.")
                    }
                    if (
                        (
                            (
                                grepl("\\*|\\+", xs[2]) &
                                !grepl("\\*|\\+", xs[1])
                            ) |
                            (
                                grepl("~", ifelse(tilde1st(xs[2]), substring(xs[2], 2), xs[2])) &
                                !grepl("~", ifelse(tilde1st(xs[1]), substring(xs[1], 2), xs[1]))
                            )
                        ) &
                        which(found) == 1
                    ) {
                        x[i] <- paste(rev(xs), collapse = "->")
                    }
                }
            }
        }
        return(x)
    }
    typev <- typel <- FALSE
    callx <- identical(class(x), "call")
    dx <- deparse(x)
    if (is.character(dx) && length(dx) == 2 && dx[1] == "~") {
        dx <- paste(dx, collapse = "")
    }
    if (callx) {
        typev <- is.name(x[[1]]) & identical(as.character(x[[1]]), "c")
        typel <- is.name(x[[1]]) & identical(as.character(x[[1]]), "list")
    }
    if (callx & (typev | typel)) {
        result <- dxlist <- vector(mode = "list", length = max(1, length(x) - 1))
        if (length(x) == 1) {
            if (typev) return(NULL)
            if (typel) return(list())
        }
        if (typev) {
            if (length(snames) > 0) { 
                dx <- as.character(x)[-1]
                if (all(is.element(dx, snames))) {
                    return(dx)
                }
            }
        }
        for (i in seq(length(result))) {
            dxlist[[i]] <- dx <- deparse(x[[i + 1]])
            result[[i]] <- tryCatch(eval(x[[i + 1]], envir = parent.frame(n = 2)), error = function(e) {
                withinobj(dx)
            })
            if (length(snames) > 0) {
                if (all(is.element(dx, snames))) {
                    result[[i]] <- dx
                }
            }
        }
        classes <- unlist(lapply(result, class))
        if (length(unique(classes)) > 1) {
            for (i in seq(length(result))) {
                if (identical(classes[i], "formula") | (identical(classes[i], "function") & typev)) {
                    result[[i]] <- withinobj(dxlist[[i]])
                }
                if (identical(classes[i], "logical") & typev & nchar(dxlist[[i]] == 1)) {
                    result[[i]] <- withinobj(dxlist[[i]])
                }
                if (identical(classes[i], "list")) {
                    if (is.element("function", unlist(lapply(result[[i]], class)))) {
                        result[[i]] <- dxlist[[i]]
                    }
                }
            }
        }
        if (typev) {
            return(unlist(result))
        }
        else if (typel) {
            names(result) <- names(x[-1])
            return(result)
        }
    }
    if (length(snames) > 0 & all(!grepl("[[:punct:]]", notilde(dx)))) {
        if (all(is.element(notilde(dx), snames))) {
            return(dx)
        }
    }
    if (identical(class(x), "<-")) {
        return(withinobj(dx))
    }
    ntdx <- dx
    negated <- all(tilde1st(dx) & !grepl("\\+|\\*", dx))
    if (negated) {
        ntdx <- notilde(dx)
    }
    x <- tryCatch(
        eval(
            parse(text = ntdx),
            envir = parent.frame(n = 2)
        ),
        error = function(e) {
            withinobj(dx)
        }
    )
    if (is.numeric(x)) {
        if (negated) {
            return(1 - x)
        }
        return(x)
    }
    if (identical(class(x), "formula")) {
        return(withinobj(dx))
    }
    return(x)
}
