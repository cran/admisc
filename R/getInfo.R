# Copyright (c) 2019 - 2022, Adrian Dusa
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

`getInfo` <- function(data) {
    if (is.matrix(data)) {
        data <- as.data.frame(data)
    }
    dc.code <- unique(unlist(lapply(data, function(x) {
        if (is.numeric(x)) {
            return(x[x < 0])
        }
        else {
            return(as.character(x[is.element(x, c("-", "dc"))]))
        }
    })))
    if (length(dc.code) > 1) {
        stopError("Multiple \"don't care\" codes found.")
    }
    if (length(dc.code) > 0) {
        colnms <- colnames(data)
        data[] <- lapply(data, function(x) {
            x <- as.character(x)
            x[x == dc.code] <- -1
            if (possibleNumeric(x)) {
                x <- asNumeric(x)
            }
            return(x)
        })
        colnames(data) <- colnms
    }
    fuzzy.cc <- logical(ncol(data))
    hastime <- logical(ncol(data))
    factor <- sapply(data, is.factor)
    declared <- sapply(data, function(x) inherits(x, "declared"))
    pN <- sapply(data, possibleNumeric)
    for (i in seq(ncol(data))) {
        if (pN[i]) {
            copy.cc <- asNumeric(data[, i])
            fuzzy.cc[i] <- any(na.omit(copy.cc) %% 1 > 0)
            if (!fuzzy.cc[i] & !any(is.na(copy.cc))) {
                if (any(na.omit(copy.cc) < 0)) {
                    hastime[i] <- TRUE
                    copy.cc[copy.cc < 0] <- max(copy.cc) + 1
                }
            }
            data[, i] <- copy.cc
        }
    }
    noflevels <- getLevels(data)
    attributes(noflevels) <- NULL
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
                labels <- attr(x, "labels")
                if (is.null(labels)) {
                    stopError("Declared columns should have labels for all values.")
                }
                else {
                    if (length(labels) != noflevels[i]) {
                        stopError("All values should have declared labels.")
                    }
                }
                attributes(x) <- NULL
                data[, i] <- recode(x, paste(sort(labels), seq(noflevels[i]) - 1, sep = "=", collapse = ";"))
                categories[[columns[i]]] <- names(sort(labels))
                attr(categories, "labels") <- labels
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
