# Copyright (c) 2019 - 2023, Adrian Dusa
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
        if (length(dc.code) > 0 && is.element(dc.code, cc)) {
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
                if (min(cc) != 0 && !fuzzy.cc[i]) {
                    cc <- recode(cc, paste(sort(labels), seq(noflevels[i]) - 1, sep = "=", collapse = ";"))
                }
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
                if (is.null(labels)) {
                    stopError("Declared columns should have labels.")
                }
                else {
                    if (noflevels[i] == 2) {
                        if (length(labels) == 1) {
                            stopError("Binary crisp columns should have labels for both presence and absence.")
                        }
                    }
                    else { 
                        if (length(labels) != noflevels[i]) {
                            stopError("All multi-values should have declared labels.")
                        }
                    }
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
