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

`makeTag` <- function(...) {
    x <- as.character(c(...))
    x <- .Call("_tag", x, PACKAGE = "admisc")
    class(x) <- "double"
    return(x)
}
`hasTag` <- function(x, tag = NULL) {
    if (!is.double(x)) {
        return(logical(length(x)))
    }
    if (!is.null(tag) && (!is.atomic(tag) || length(tag) > 1 || is.na(tag))) {
        stopError("`tag` should be a vector of length 1.")
    }
    if (!is.null(tag)) {
        tag <- as.character(tag)
    }
    return(.Call("_has_tag", x, tag, PACKAGE = "admisc"))
}
`getTag` <- function(x) {
    if (is.double(x)) {
        x <- .Call("_get_tag", x, PACKAGE = "admisc")
        if (!any(is.na(suppressWarnings(as.numeric(na.omit(x)))))) {
            x <- as.numeric(x)
        }
        return(x)
    }
    else {
        return(rep(NA, length(x)))
    }
}
`anyTagged` <- function(x) {
    if (is.data.frame(x)) {
        i <- 1
        tagged <- FALSE
        while(!tagged & i <= ncol(x)) {
            tagged <- Recall(x[[i]])
            i <- i + 1
        }
        return(tagged)
    }
    if (is.double(x)) {
        return(.Call("_any_tagged", x, PACKAGE = "admisc"))
    }
    return(FALSE)
}
