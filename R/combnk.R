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

#' Generate all combinations of n numbers, taken k at a time
#'
#' A fast function to generate all possible combinations of n numbers, taken k at a time,
#' starting from the first k numbers or starting from a combination that contain a
#' certain number.
#'
#' @name combnk
#' @rdname combnk
#' @rawRd
#' \usage{
#' combnk(n, k, ogte = 0, zerobased = FALSE)
#' }
#'
#' \arguments{
#'     \item{n}{Vector of any kind, or a numerical scalar.}
#'     \item{k}{Numeric scalar.}
#'     \item{ogte}{At least one value greater than or equal to this number.}
#'     \item{zerobased}{Logical, zero or one based.}
#' }
#'
#' \details{
#' When a scalar, argument \code{n} should be numeric, otherwise when a vector its
#' length should not be less than \code{k}.
#'
#' When the argument \bold{\code{ogte}} is specified, the combinations will sequentially
#' be incremented from those which contain a certain number, or a certain position from
#' \code{n} when specified as a vector.
#' }
#'
#'
#' \value{
#' A matrix with \code{k} rows and \code{choose(n, k)} columns.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' combnk(5, 2)
#'
#' combnk(5, 2, ogte = 3)
#'
#' combnk(letters[1:5], 2)
#' }
#'
#' \keyword{functions}
NULL
#' @export
`combnk` <- function(n, k, ogte = 0, zerobased = FALSE) {
    if (!is.numeric(k)) {
        stopError("Argument k should be numeric.")
    }
    if (length(k) != 1L) {
        stopError("Argument k should be a scalar of length 1.")
    }
    if (k < 0) {
        stopError("Argument k should be positive.")
    }
    len <- length(n)
    lngt1 <- len > 1
    if (lngt1) {
        if (len < k) {
            stopError("Argument k cannot be greater than the length of n.")
        }
    }
    else {
        if (!is.numeric(n)) {
            stopError("When scalar, argument n should be numeric.")
        }
        if (n < k) {
            stopError("Argument n should be greater than or equal to k.")
        }
    }
    copyn <- n
    if (lngt1) {
        n <- len
    }
    resmat <- .Call(
        "C_ombnk",
        list(
            n = as.integer(n),
            k = as.integer(k),
            ogte = as.integer(ogte),
            zerobased = as.integer(zerobased)
        ),
        PACKAGE = "admisc"
    )
    if (lngt1) {
        resmat <- matrix(copyn[resmat], nrow = nrow(resmat))
    }
    return(resmat)
}
