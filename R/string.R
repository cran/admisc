# Copyright (c) 2019, Adrian Dusa
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
#     * The names of its contributors may NOT be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

`trimstr` <- function(x, what = " ", side = "both") {
    if (is.element(what, c("*", "+"))) what <- paste("\\", what, sep = "")
    what <- ifelse(what == " ", "[[:space:]]", what)
    pattern <- switch(side,
    both = paste("^", what, "+|", what, "+$", sep = ""),
    left = paste("^", what, "+", sep = ""),
    right = paste(what, "+$", sep = "")
    )
    gsub(pattern, "", x)
}
`splitstr` <- function(x) {
    if (identical(x, "")) return(x)
    y <- gsub("\\n", "", unlist(strsplit(gsub("[[:space:]]", "", x), split = ",")))
    if (any(grepl(",", x) & grepl("[{]", x))) {
        i <- 1
        while (i <= length(y)) {
            if (grepl("[{]", y[i]) & !grepl("[}]", y[i])) {
                y[i] <- paste(y[i], y[i + 1], sep = ",")
                y <- y[-(i + 1)]
            }
            i <- i + 1
        }
    }
    if (length(y) == 1) {
        y <- gsub("\\n", "", unlist(strsplit(gsub("[[:space:]]", "", y), split = ";")))
    }
    metacall <- match.call()$x
    if (metacall == "sort.by") {
        if (any(grepl("[=]", y))) {
            y <- t(as.data.frame(strsplit(y, split = "=")))
            values <- y[, 2] == TRUE
            names(values) <- y[, 1]
        }
        else {
            values <- !grepl("[+]", y)
            names(values) <- gsub("[+|-]", "", y)
        }
        return(values)
    }
    else if (metacall == "decreasing") {
        return(as.logical(y))
    }
    else if (metacall == "thresholds") {
        if (any(grepl("[=]", y))) {
            y <- t(as.data.frame(strsplit(y, split = "=")))
            values <- y[, 2]
            if (possibleNumeric(values)) {
                values <- asNumeric(values)
            }
            names(values) <- y[, 1]
        }
        else {
            if (possibleNumeric(y)) {
                values <- asNumeric(y)
            }
        }
        return(values)
    }
    else {
        if (possibleNumeric(y)) {
            y <- asNumeric(y)
        }
        return(y)
    }
}
