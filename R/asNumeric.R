# Copyright (c) 2019 - 2025, Adrian Dusa
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

`asNumeric` <- function(x, ...) {
    UseMethod("asNumeric")
}
`asNumeric.declared` <- function(x, ..., na_values = TRUE) {
    na_index <- attr(x, "na_index")
    attributes(x) <- NULL
    if (isTRUE(na_values)) {
        if (!is.null(na_index)) {
            x[na_index] <- as.numeric(names(na_index))
        }
    }
    NextMethod()
}
`asNumeric.factor` <- function(x, ..., levels = TRUE) {
    if (isTRUE(levels)) {
        return(suppressWarnings(as.numeric(levels(x)))[x])
    }
    return(as.numeric(x))
}
`asNumeric.default` <- function(x, ...) {
    attributes(x) <- NULL
    if (is.numeric(x)) {
        return(x)
    }
    x <- gsub("\u00a0", " ", x) 
    result <- rep(NA, length(x))
    multibyte <- grepl("[^!-~ ]", x)
    result[!multibyte] <- suppressWarnings(as.numeric(x[!multibyte]))
    return(result)
}
