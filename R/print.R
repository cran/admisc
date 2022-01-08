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

`print.admisc_deMorgan` <- function(x, ...) {
    prettyNums <- formatC(seq(length(x)), digits = nchar(length(x)) - 1, flag = 0)
    pM <- paste("M", prettyNums, sep = "")
    if (!is.null(isol <- attr(x, "isol"))) {
        pM <- paste(pM, isol, sep = "-")
    }
    pM <- paste(pM, ": ", sep = "")
    cat("\n")
    if (length(x) == 1 & !attr(x, "minimized")) {
        fx <- x[[1]]
        if (is.null(fx)) {
            cat("No negation possible.\n")
        }
        else {
            for (j in seq(length(fx))) {
                prettyNumsFact <- formatC(seq(length(fx)), digits = nchar(length(fx)) - 1, flag = 0)
                cat(paste("N", prettyNumsFact[j], ": ", sep = ""))
                flength <- nchar(prettyNumsFact[j]) + 1
                strvctr <- unlist(strsplit(fx[j], split = " + "))
                cat(admisc::prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
            }
            cat("\n")
        }
    }
    else {
        for (i in seq(length(x))) {
            cat(paste(pM[i], names(x)[i], sep = ""), "\n")
            fx <- x[[i]]
            if (is.null(fx)) {
                cat("No negation possible.\n")
            }
            else {
                for (j in seq(length(fx))) {
                    prettyNumsFact <- formatC(seq(length(fx)), digits = nchar(length(fx)) - 1, flag = 0)
                    cat(paste("  N", prettyNumsFact[j], ": ", sep = ""))
                    flength <- nchar(prettyNumsFact[j]) + 3
                    strvctr <- unlist(strsplit(fx[j], split = " + "))
                    cat(admisc::prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
                }
                cat("\n")
            }
        }
    }
}
`print.admisc_intersection` <- function(x, ...) {
    prettyNums <- formatC(seq(length(x)), digits = nchar(length(x)) - 1, flag = 0)
    pI <- paste("E", prettyNums, sep="")
    pO <- paste("  I", prettyNums, sep="")
    if (!is.null(isol <- attr(x, "isol"))) {
        pI <- paste(pI, isol, sep = "-")
        pO <- paste(pO, isol, sep = "-")
    }
    pI <- paste(pI, ": ", sep = "")
    pO <- paste(pO, ": ", sep = "")
    expressions <- attr(x, "expressions")
    ncharSI <- max(nchar(pI))
    for (i in seq(length(x))) {
        cat("\n", pI[i], sep = "")
        cat(admisc::prettyString(expressions[i], getOption("width") - ncharSI, ncharSI, "+"))
        cat("\n", pO[i], sep = "")
        cat(admisc::prettyString(x[i], getOption("width") - ncharSI, ncharSI, "+"))
        cat("\n")
    }
    cat("\n")
}
`print.admisc_simplify` <- function(x, ...) {
    prettyNums <- formatC(seq(length(x)), digits = nchar(length(x)) - 1, flag = 0)
    cat("\n")
    if (all(x == "")) {
        cat("S1: \"\"\n")
    }
    else {
        for (i in seq(length(x))) {
            cat(paste("S", prettyNums[i], ": ", sep = ""))
            flength <- nchar(prettyNums[i]) + 1
            strvctr <- unlist(strsplit(x[i], split = " + "))
            cat(prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n")
        }
    }
    cat("\n")
}
`print.admisc_factorize` <- function(x, ...) {
    prettyNums <- formatC(seq(length(x)), digits = nchar(length(x)) - 1, flag = 0)
    pM <- paste("M", prettyNums, sep = "")
    if (!is.null(isol <- attr(x, "isol"))) {
        pM <- paste(pM, isol, sep = "-")
    }
    pM <- paste(pM, ": ", sep = "")
    cat("\n")
    if (length(x) == 1) {
        fx <- x[[1]]
        if (is.null(fx)) {
            cat("No factorization possible.\n")
        }
        else {
            for (j in seq(length(fx))) {
                prettyNumsFact <- formatC(seq(length(fx)), digits = nchar(length(fx)) - 1, flag = 0)
                cat(paste("F", prettyNumsFact[j], ": ", sep = ""))
                flength <- nchar(prettyNumsFact[j]) + 1
                strvctr <- unlist(strsplit(fx[j], split = " + "))
                cat(admisc::prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
            }
            cat("\n")
        }
    }
    else {
        for (i in seq(length(x))) {
            cat(paste(pM[i], names(x)[i], sep = ""), "\n")
            fx <- x[[i]]
            if (is.null(fx)) {
                cat("No factorization possible.\n")
            }
            else {
                for (j in seq(length(fx))) {
                    prettyNumsFact <- formatC(seq(length(fx)), digits = nchar(length(fx)) - 1, flag = 0)
                    cat(paste("  F", prettyNumsFact[j], ": ", sep = ""))
                    flength <- nchar(prettyNumsFact[j]) + 3
                    strvctr <- unlist(strsplit(fx[j], split = " + "))
                    cat(admisc::prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
                }
                cat("\n")
            }
        }
    }
}
`print.admisc_translate` <- function(x, ...) {
    dots <- list(...)
    cat("\n")
    original <- FALSE
    y <- matrix(as.vector(x), nrow = nrow(x))
    if (is.element("original", names(dots))) {
        if (is.logical(dots$original)) {
            original <- dots$original[1]
        }
    }
    cols <- colnames(x)
    colnames(y) <- cols
    if (original) {
        minus <- any(y < 0)
        if (minus) {
            y[y >= 0] <- paste("", y[y >= 0])
            cols[nchar(cols) == 1] <- paste("", cols[nchar(cols) == 1])
            colnames(y) <- cols
        }
    }
    else {
        y[x < 0] <- ""
    }
    rownames(y) <- paste(rownames(x), " ")
    print(prettyTable(y))
    cat("\n")
}
`print.admisc_using` <- function(x, ...) {
    if (is.list(x)) {
        nms <- apply(attr(x, "split", exact = TRUE), 1, function(x) {
            paste(x, collapse = ", ")
        })
        for (i in seq(length(x))) {
            cat(nms[i], "\n")
            cat(paste(c(rep("-", nchar(nms[i])), "\n"), collapse = ""))
            if (is.null(x[[i]])) {
                cat("No data.\n")
            }
            else {
                print(x[[i]])
            }
            if (i < length(x)) {
                cat("\n")
            }
        }
    }
    else if (is.matrix(x)) {
        class(x) <- setdiff(class(x), "usage")
        if (ncol(x) == 1) {
            x[] <- prettyNum(round(x, 3))
        }
        else if (ncol(x) > 1) {
            x[] <- gsub("NA", "", prettyNum(round(x, 3)))
        }
        for (i in seq(ncol(x))) {
            splitcol <- strsplit(x[, i], split = "[.]")
            nchars <- unlist(lapply(lapply(splitcol, "[", 2), nchar))
            if (!all(is.na(nchars))) {
                maxnchar <- max(nchars, na.rm = TRUE)
                x[, i] <- unlist(lapply(splitcol, function(x) {
                    if (length(x) == 1) {
                        x <- c(x, paste(c(rep("0", maxnchar)), collapse = ""))
                    }
                    else if (length(x) == 2) {
                        x[2] <- paste(x[2], paste(rep("0", maxnchar - nchar(x[2])), collapse = ""), sep = "")
                    }
                    return(paste(x, collapse = "."))
                }))
            }
        }
        maxwidth <- max(nchar(c(colnames(x), x)))
        colnames(x) <- format(colnames(x), justify = "right", width = maxwidth)
        print(noquote(format(x, justify = "right", width = maxwidth)))
    }
}
