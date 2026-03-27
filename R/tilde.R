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

#' Tilde operations
#'
#' Checks and changes expressions containing set negations using a tilde.
#'
#' @name hastilde
#' @rdname tilde
#' @aliases notilde
#' @aliases tilde1st
#' @rawRd
#' \usage{
#' hastilde(x)
#' notilde(x)
#' tilde1st(x)
#' }
#'
#' \arguments{
#'   \item{x}{A vector of values}
#' }
#'
#'
#' \details{
#' Boolean expressions can be negated in various ways. For binary crisp and fuzzy sets, one of
#' the most straightforward ways to invert the set membership scores is to subtract them from 1.
#' This is both possible using R vectors and also often used to signal a negation in SOP
#' (sum of products) expressions. 
#'
#' Some other times, SOP expressions can signal a set negation (also known as the absence of a 
#' causal condition) by using lower case letters, while upper case letters are used to signal
#' the presence of a causal condition. SOP expressions also use a tilde to signal a set negation,
#' immediately preceding the set name.
#'
#' This set of functions detect when and if a set present in a SOP expression contains a tilde
#' (function \bold{\code{hastilde}}), whether the entire expression begins with a tilde (function
#' \bold{\code{tilde1st}}).
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \examples{
#' hastilde("~A")
#' }
#'
#'
#' \keyword{functions}
NULL
#' @export
`tilde1st` <- function(x) {
    is.element(
        substring(
            gsub(
                paste0("[[:space:]|", "\u00a0", "]"), 
                "",
                x
            ),
            1, 1
        ),
        tildae()
    )
}
#' @export
`hastilde` <- function(x) {
    grepl(paste(tildae(), collapse = "|"), x)
}
#' @export
`notilde` <- function(x) {
    gsub(
        paste(tildae(), collapse = "|"),
        "",
        gsub(
            paste0("[[:space:]|", "\u00a0", "]"), 
            "",
            x
        )
    )
}
