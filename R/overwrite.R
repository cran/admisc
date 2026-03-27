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

#' Overwrite an object in a given environment.
#'
#' Utility function to overwrite an object, and bypass the assignment operator.
#'
#' @name overwrite
#' @rdname overwrite
#' @rawRd
#' \usage{
#' overwrite(objname, content, environment)
#' }
#'
#' \arguments{
#'     \item{objname}{Character, the name of the object to overwrite.}
#'     \item{content}{An R object}
#'     \item{environment}{The environment where to perform the overwrite procedure.}
#' }
#'
#' \details{
#' \code{assign()} is sufficient when \code{objname} is a simple object name,
#' such as \code{"bar"}. It is not sufficient when the target is an expression,
#' such as \code{"bar$A"}. A call such as \code{assign(bar$A, 1, envir =
#' parent.frame())} fails because \code{assign()} expects its first argument to
#' evaluate to a character string. If that expression is first deparsed, for
#' instance to \code{"bar$A"}, then \code{assign()} would create an object
#' literally named \code{"bar$A"} in the target environment rather than
#' replacing component \code{A} inside \code{bar}.
#'
#' This function handles both situations. For simple names, it overwrites the
#' object directly in the target environment. For expressions, it reconstructs
#' and evaluates the corresponding assignment call in that environment.
#' }
#'
#' \value{
#' This function does not return anything.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' foo <- function(object, x) {
#'     objname <- deparse(substitute(object))
#'     overwrite(objname, x, parent.frame())
#' }
#'
#'
#' bar <- 1
#' foo(bar, 2)
#'
#' bar
#' # [1] 2
#'
#' bar <- list(A = bar)
#' foo(bar$A, 3)
#'
#' bar
#' # $A
#' # [1] 3
#'
#'
#' foo_assign <- function(object, x) {
#'     objname <- deparse(substitute(object))
#'     assign(objname, x, envir = parent.frame())
#' }
#'
#' bar <- list(A = 1)
#' try(assign(bar$A, 3, envir = parent.frame()))
#'
#' bar <- 1
#' foo_assign(bar, 2)
#'
#' bar
#' # [1] 2
#'
#' bar <- list(A = 1)
#' foo_assign(bar$A, 3)
#'
#' bar
#' # $A
#' # [1] 1
#'
#' `bar$A`
#' # [1] 3
#' }
#'
#' \keyword{functions}
NULL
#' @export
`overwrite` <- function(objname, content, environment) {
    objname <- gsub("'|\"|[[:space:]]", "", objname)
    if (exists(objname, environment)) {
        environment[[objname]] <- content
    }
    else {
        structure_string <- paste(capture.output(dput(content)), collapse = " ")
        eval(
            parse(text = sprintf(paste(objname, "<- %s"), structure_string)),
            envir = environment
        )
    }
}
