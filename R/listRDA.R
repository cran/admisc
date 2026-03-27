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

#' Load and list objects from an .rda file
#'
#' Utility functions to read the names and load the objects from an .rda file, into
#' an R list.
#'
#' @name listRDA
#' @rdname rdaFunctions
#' @aliases objRDA
#' @rawRd
#' \usage{
#' listRDA(.filename)
#'
#' objRDA(.filename)
#' }
#'
#' \arguments{
#'   \item{.filename}{The path to the file where the R object is saved.}  
#' }
#'
#' \details{
#' Files with the extension .rda are routinely created using the base function
#' \bold{\code{\link[base]{save}()}}.
#'
#' The function \bold{\code{listRDA()}} loads the object(s) from the .rda file into a list,
#' preserving the object names in the list components.
#'
#' The .rda file can naturally be loaded with the base \bold{\code{\link[base]{load}()}} function,
#' but in doing so the containing objects will overwrite any existing objects with the same names.
#'
#' The function \bold{\code{objRDA()}} returns the names of the objects from the .rda file.
#' }
#'
#' \value{
#' A list, containing the objects from the loaded .rda file.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#'
#' \keyword{functions}
NULL
#' @export
`listRDA` <- function(.filename) {
    load(.filename)
    return(as.list(environment()))
}
