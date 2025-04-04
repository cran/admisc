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

`possibleNumeric` <- function(x, each = FALSE) {
    result <- rep(NA, length(x))
    nax <- is.na(x)
    if (all(nax)) {
        if (each) {
            return(result)
        }
        return(FALSE)
    }
    if (is.logical(x)) {
        if (each) {
            result <- logical(length(x))
            result[nax] <- NA
            return(result)
        }
        return(FALSE)
    }
    if (inherits(x, "haven_labelled") || inherits(x, "declared")) {
        num <- Recall(unclass(x), each = each)
        labels <- attr(x, "labels", exact = TRUE)
        if (!is.null(labels) && !each && num) {
            return(Recall(labels))
        }
        return(num)
    }
    if (is.numeric(x)) {
        if (each) {
            result[!nax] <- TRUE
            return(result)
        }
        return(TRUE)
    }
    if (is.factor(x)) {
        x <- as.character(x)
    }
    x <- gsub(
        "\u00a0", 
        " ",
        gsub(
            "\u009d", 
            "",
            x
        )
    )
    multibyte <- grepl("[^!-~ ]", x)
    if (any(multibyte)) {
        result[multibyte] <- FALSE
    }
    if (sum(nax) < length(x)) {
        eachx <- suppressWarnings(as.numeric(x[!nax & !multibyte]))
        result[!nax & !multibyte] <- !is.na(eachx)
    }
    if (each | length(x) == 1) {
        return(result)
    }
    return(all(result[!nax]))
}
