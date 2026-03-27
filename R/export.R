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

#' Export an object to a file or a connection
#'
#' This is a generic function, usually a wrapper to \bold{\code{\link[utils]{write.table}()}}.
#'
#' @name export
#' @rdname export
#' @rawRd
#' \usage{
#' export(what, ...)
#' }
#'
#' \arguments{
#'     \item{what}{The object to be written (matrix or dataframe)}
#'     \item{...}{Specific arguments to class functions.}
#' }
#'
#' \details{
#' The default convention for \bold{\code{\link[utils]{write.table}()}} is to add a blank column
#' name for the row names, but (despite it is a standard used for CSV files) that doesn't work
#' with all spreadsheets or other programs that attempt to import the result of
#' \bold{\code{\link[utils]{write.table}()}}.
#'
#' This function acts as if \bold{\code{\link[utils]{write.table}()}} was called, with only one
#' difference: if row names are present in the dataframe (i.e. any of them should be different
#' from the default row numbers), the final result will display a new column called
#' \bold{\code{cases}} in the first position, except the situation that another column called
#' \bold{\code{cases}} already exists in the data, when the row names will be completely ignored.
#'
#' If not otherwise specified, an argument \bold{\code{sep = ","}} is added by default.
#'
#' The argument \bold{\code{row.names}} is always set to FALSE, a new column being added anyways (if possible).
#'
#' Since this function pipes everything to \bold{\code{\link[utils]{write.table}()}}, the argument \bold{\code{file}}
#' can also be a connection open for writing, and \bold{\code{""}} indicates output to the console.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#'
#' \seealso{
#'   The \dQuote{R Data Import/Export} manual.
#'
#'   \code{\link[utils]{write.table}}
#' }
#'
#' \keyword{functions}
NULL
#' @export
export <- function (what, ...) {
    UseMethod ("export")
}
#' @export
`export.default` <- function (what, ...) {
    return(NULL)
}
#' @export
`export.data.frame` <- function(what, ...) {
    dots <- list(...)
    Call <- as.list(match.call(expand.dots = TRUE))[-1]
    caseid <- "cases"
    if (any(names(dots) == "caseid")) {
        caseid <- dots[["caseid"]]
        Call[["caseid"]] <- NULL
    }
    if (any(rownames(what) != seq(nrow(what)))) {
        if (all(colnames(what) != caseid)) {
            what <- cbind("cases" = rownames(what), what)
            names(what)[1] <- caseid
        }
    }
    Call[["x"]] <- what
    Call[["what"]] <- NULL
    if (any(names(dots) == "sep")) {
        if (dots[["sep"]] == "tab") {
            dots[["sep"]] <- "\t"
        }
        Call[["sep"]] <- dots[["sep"]]
    }
    else {
        Call[["sep"]] <- ","
    }
    if (any(names(dots) == "col.names")) {
        Call[["col.names"]] <- dots[["col.names"]]
    }
    if (any(names(dots) == "row.names")) {
        message("The argument 'row.names' is always set to FALSE, by default.")
    }
    Call[["row.names"]] <- FALSE
    do.call("write.table", Call)
}
#' @export
`export.list` <- function(what, ...) {
    dots <- list(...)
    Call <- as.list(match.call(expand.dots = TRUE))[-1]
    DDIwR <- eval(parse(text = "requireNamespace('DDIwR', quietly = TRUE)"))
    if (!DDIwR) {
        stopError("Package DDIwR needs to be installed.")
    }
    if (is.null(what$.extra)) {
        return(NULL)
    }
    names(Call)[1] <- "codeBook"
    eval(parse(text = "do.call('exportCodebook', Call)"))
}
