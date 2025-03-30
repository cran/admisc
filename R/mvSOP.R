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

`mvSOP` <- function(
    expression = "", snames = "", data = NULL, keep.tilde = TRUE, ...
) {
    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))
    dots <- list(...)
    if (any(grepl("\\[|\\]|\\{|\\}", expression))) {
        stopError("The expression is already in multi-value notation.", ... = ...)
    }
    if (identical(snames, "")) {
        if (!is.null(data)) {
            snames <- colnames(data)
        }
    }
    else {
        snames <- splitstr(snames)
    }
    noflevels <- NULL
    oldc <- newc <- c()
    categories <- list()
    if (is.null(data)) {
        if (!is.null(dots$categories)) {
            categories <- dots$categories
        }
    }
    else {
        infodata <- getInfo(data)
        noflevels <- infodata$noflevels
        categories <- infodata$categories
    }
    checkValid(
        expression = expression,
        snames = snames,
        data = data,
        categories = categories
    )
    if (length(categories) > 0) {
        fnames <- names(categories)
        oldc <- c(paste0("~", fnames), fnames)
        newc <- c(paste0(fnames, "[0]"), paste0(fnames, "[1]"))
        for (i in seq(length(categories))) {
            values <- seq(length(categories[[i]])) - 1
            oldc <- c(oldc, categories[[i]])
            newc <- c(newc, paste0(fnames[i], "[", values, "]"))
            if (!keep.tilde) {
                oldc <- c(oldc, paste0("~", categories[[i]]))
                for (v in values) {
                    newc <- c(newc,
                        paste0(
                            fnames[i],
                            "[",
                            paste(setdiff(values, v), collapse = ","),
                            "]"
                        )
                    )
                }
            }
        }
    }
    oldc <- c(oldc, paste0("~", snames), snames)
    newc <- c(newc, paste0(snames, "[0]"), paste0(snames, "[1]"))
    expression <- replaceText(expression, oldc, newc)
    if (any(!is.element(squareBrackets(expression, outside = TRUE), snames))) {
        stopError("Unkown condition(s) in the expression.", ... = ...)
    }
    if (!is.null(noflevels)) {
        if (any(infodata$hastime)) {
            noflevels[infodata$hastime] <- noflevels[infodata$hastime] - 1
        }
        rnames <- colnames(validateNames(expression, snames = snames, data = data))
        noflevels <- noflevels[match(rnames, colnames(data))]
        if (any(noflevels > 2)) {
            stopError("Part(s) of the expression refer to multi-value data.", ... = ...)
        }
    }
    if (isTRUE(dots$translate)) {
        return(
            list(
                expression = expression,
                oldc = oldc,
                newc = newc
            )
        )
    }
    return(expression)
}
