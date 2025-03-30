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

`getInfo` <- function(data, ...) {
    dots <- list(...)
    if (is.matrix(data)) {
        data <- as.data.frame(data)
    }
    dc.code <- unique(unlist(lapply(data, function(x) {
        if (is.numeric(x) && wholeNumeric(x)) {
            return(x[x < 0])
        }
        else {
            return(as.character(x[is.element(x, c("-", "dc"))]))
        }
    })))
    if (!isTRUE(dots$no_column_info)) {
        if (length(dc.code) > 1) {
            stopError("Multiple \"don't care\" codes found.")
        }
    }
    fuzzy.cc <- logical(ncol(data))
    hastime <- logical(ncol(data))
    factor <- sapply(data, is.factor)
    declared <- sapply(data, function(x) inherits(x, "declared"))
    noflevels <- getLevels(data)
    attributes(noflevels) <- NULL
    for (i in seq(ncol(data))) {
        cc <- data[, i]
        label <- attr(cc, "label", exact = TRUE)
        labels <- attr(cc, "labels", exact = TRUE)
        if (is.factor(cc)) {
            cc <- as.character(cc)
        }
        if (length(dc.code) > 0 && any(is.element(cc, dc.code))) {
            cc[is.element(cc, dc.code)] <- -1
        }
        if (possibleNumeric(cc)) {
            cc <- asNumeric(cc)
            fuzzy.cc[i] <- any(na.omit(cc) %% 1 > 0)
            if (!fuzzy.cc[i] & !anyNA(cc)) {
                if (any(na.omit(cc) < 0)) {
                    hastime[i] <- TRUE
                    cc[cc < 0] <- max(cc) + 1 
                }
            }
            if (declared[i]) {
                attr(cc, "label") <- label
                attr(cc, "labels") <- labels
                class(cc) <- c("declared", class(cc))
            }
            data[[i]] <- cc
        }
    }
    factor <- factor & !hastime
    categories <- list()
    columns <- colnames(data)
    if (any(factor | declared)) {
        for (i in which(factor | declared)) {
            if (factor[i]) {
                categories[[columns[i]]] <- levels(data[, i])
                data[, i] <- as.numeric(data[, i]) - 1
            }
            else {
                x <- data[, i]
                labels <- attr(x, "labels", exact = TRUE)
                if (fuzzy.cc[i]) {
                    if (length(setdiff(0:1, labels) > 0)) {
                        stopError("Declared fuzzy columns should have labels for the end points.")
                    }
                } else if (length(setdiff(x, labels)) > 0) {
                    stopError("Declared columns should have labels for all values.")
                }
                categories[[columns[i]]] <- names(sort(labels))
            }
        }
    }
    return(
        list(
            data = data,
            fuzzy.cc = fuzzy.cc,
            hastime = hastime,
            factor = factor,
            declared = declared,
            categories = categories,
            dc.code = dc.code,
            noflevels = noflevels
        )
    )
}
