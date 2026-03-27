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

#' Set matrix row or column names
#'
#' Set matrix row or column names without copying, especially useful for (very)
#' large matrices.
#'
#' @name setColnames
#' @rdname dimnames
#' @aliases dimnames
#' @aliases setRownames
#' @aliases setDimnames
#' @rawRd
#' \usage{
#' setColnames(matrix, colnames)
#' setRownames(matrix, rownames)
#' setDimnames(matrix, nameslist)
#' }
#'
#' \arguments{
#'   \item{matrix}{An R matrix}
#'   \item{colnames}{Character vector of column names}
#'   \item{rownames}{Character vector of row names}
#'   \item{nameslist}{A two-component list containing rownames and colnames}
#' }
#'
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#'
#' mat <- matrix(1:9, nrow = 3)
#' setDimnames(mat, list(LETTERS[1:3], letters[1:3]))
#' }
#'
#'
#' \keyword{functions}
NULL
#' @export
`setColnames` <- function(matrix, colnames) {
    invisible(.Call("C_setColnames", matrix, colnames))
}
#' @export
`setRownames` <- function(matrix, rownames) {
    invisible(.Call("C_setRownames", matrix, rownames))
}
#' @export
`setDimnames` <- function(matrix, nameslist) {
    invisible(.Call("C_setDimnames", matrix, nameslist))
}
