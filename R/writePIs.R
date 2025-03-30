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

`writePIs` <- function(
    impmat, mv = FALSE, collapse = "*", snames = "", curly = FALSE,
    use.labels = FALSE, categories = list(), ...
) {
    if (any(impmat > 2)) {
        mv <- TRUE
    }
    dots <- list(...)
    if (is.element("categorical", names(dots))) {
        use.labels <- dots$categorical
        dots$categorical <- NULL
    }
    if (identical(snames, "")) {
        snames <- colnames(impmat)
    }
    else {
        impmat <- t(impmat)
    }
    chars <- matrix(snames[col(impmat)], nrow = nrow(impmat))
    if (mv) {
        chars <- matrix(
            paste(
                chars,
                ifelse(curly, "{", "["),
                impmat - 1,
                ifelse(curly, "}", "]"),
                sep = ""
            ),
            nrow = nrow(impmat)
        )
        if (use.labels && length(categories) > 0) {
            fnames <- names(categories)
            for (i in seq(length(categories))) {
                values <- impmat[, fnames[i]]
                pos <- nrow(impmat) * (which(snames == fnames[i]) - 1) + 1
                pos <- seq(pos, pos + length(values) - 1)[values > 0]
                chars[pos] <- categories[[i]][values[values > 0]]
            }
        }
    }
    else {
        chars <- ifelse(impmat == 1L, paste0("~", chars), chars)
        if (use.labels && length(categories) > 0) {
            fnames <- names(categories)
            for (i in seq(length(categories))) {
                values <- impmat[, fnames[i]]
                chars[values > 0, fnames[i]] <- categories[[i]][values[values > 0]]
            }
        }
    }
    keep <- impmat > 0L
    return(
        as.vector(
            unlist(
                lapply(
                    split(chars[keep], row(chars)[keep]),
                    paste,
                    collapse = collapse
                )
            )
        )
    )
}
`writePrimeimp` <- function(...) {
    .Deprecated(msg = "Function writePrimeimp() is deprecated, use writePIs().\n")
    writePIs(...)
}
