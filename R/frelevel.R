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

#' Modified \code{relevel()} function
#'
#' The base function \code{relevel()} accepts a single argument "ref", which
#'   can only be a scalar and not a vector of values. \code{frelevel()} accepts
#'   more (even all) levels and reorders them.
#'
#' @name frelevel
#' @rdname frelevel
#' @rawRd
#' \usage{
#' frelevel(variable, levels)
#' }
#'
#' \arguments{
#'   \item{variable}{The categorical variable of interest}
#'   \item{levels}{One or more levels of the factor, in the desired order}
#' }
#'
#' \value{A factor of the same length as the initial one.}
#'
#' \author{Adrian Dusa}
#'
#' \seealso{\code{\link[stats]{relevel}}}
#'
#' \examples{
#' words <- c("ini", "mini", "miny", "moe")
#' variable <- factor(words, levels = words)
#'
#' # modify the order of the levels, keeping the order of the values
#' frelevel(variable, c("moe", "ini", "miny", "mini"))
#'
#' }
#'
#' \keyword{functions}
NULL
#' @export
`frelevel` <- function(variable, levels) {
    # to do: the same with havel_labelled
    if (!is.factor(variable)) {
        stopError("The input variable is not a factor.")
    }
    
    if (any(!(levels %in% levels(variable)))) {
        stopError("One or more levels do not exist in the input variable.")
    }
    
    for (i in seq_len(length(levels))) {
        variable <- relevel(variable, ref = rev(levels)[i])
    }
    
    return(variable)
}
