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

#' Inverts the values of a factor
#'
#' Provides a reversed version of the values from a factor, for instance
#'   a Likert type response scale.
#'
#' @name frev
#' @rdname frev
#' @aliases finvert
#' @rawRd
#' \usage{
#' frev(x, labels = FALSE)
#' }
#'
#' \arguments{
#'   \item{x}{A factor}
#'   \item{labels}{Logical, invert the labels as well}
#' }
#'
#' \details{
#'   The argument \code{labels} can also be used for the levels of a factor.
#' }
#'
#' \value{A factor of the same length as the original one.}
#'
#' \author{Adrian Dusa}
#'
#' \examples{
#' words <- c("ini", "mini", "miny", "moe")
#' variable <- factor(words, labels = words)
#'
#' # inverts the values, preserving the labels' order
#' frev(variable)
#'
#' # inverts both values and labels
#' frev(variable, labels = TRUE)
#'
#' }
#'
#' \keyword{misc}
NULL
#' @export
`frev` <- function(x, labels = FALSE) {
    if (!is.factor(x)) {
        stopError("The variable is not a factor.")
    }
    flist <- list(levels(x), rev(levels(x)))
    return(factor(x, levels = flist[[1 + !labels]], labels = flist[[1 + labels]]))
}
`finvert` <- function(...) {
    .Deprecated(msg = "Function finvert() is deprecated, use frev().\n")
    frev(...)
}
