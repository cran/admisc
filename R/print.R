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
                cat(prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
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
                    cat(prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
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
        cat(prettyString(expressions[i], getOption("width") - ncharSI, ncharSI, "+"))
        cat("\n", pO[i], sep = "")
        cat(prettyString(x[i], getOption("width") - ncharSI, ncharSI, "+"))
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
                cat(prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
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
                    cat(prettyString(strvctr, getOption("width") - flength, flength, "+"), "\n", sep = "")
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
`print.admisc_fobject` <- function(x, startend = TRUE, ...) {
    class(x) <- setdiff(class(x), "admisc_fobject")
    if (is.list(x)) {
        nms <- apply(attr(x, "split", exact = TRUE), 1, function(x) {
            paste(x, collapse = ", ")
        })
        cat(ifelse(startend, "\n", ""))
        for (i in seq(length(x))) {
            cat(nms[i], "\n")
            cat(paste(c(rep("-", nchar(nms[i])), "\n"), collapse = ""))
            if (is.null(x[[i]])) {
                cat("No data.\n")
            }
            else {
                if (is.matrix(x[[i]])) {
                    class(x[[i]]) <- c("admisc_fobject", class(x[[i]]))
                }
                class(x[[i]]) <- setdiff(class(x[[i]]), "admisc_fobject")
                print(x[[i]], startend = FALSE)
            }
            if (i < length(x)) {
                cat("\n")
            }
        }
        cat(ifelse(startend, "\n", ""))
    }
    else {
        if (is.matrix(x)) {
            if (!all(dim(x) > 0)) {
                stopError("Incorrect _fobject_ to print, in package admisc.")
            }
            rnms <- rownames(x)
            max.nchar.rnms <- max(nchar(encodeString(rnms)), na.rm = TRUE)
            for (i in seq(length(rnms))) {
                if (nchar(rnms[i]) < max.nchar.rnms) {
                    rnms[i] <- padLeft(rnms[i], max.nchar.rnms - nchar(rnms[i]))
                }
            }
            rownames(x) <- rnms
        }
        else if (is.atomic(x)) {
            x <- matrix(
                if (possibleNumeric(x)) round(asNumeric(x), 3) else x,
                nrow = 1,
                dimnames = list("", names(x))
            )
        }
        nax <- is.na(x)
        pN <- apply(x, 2, possibleNumeric)
        nms <- colnames(x)
        cx <- x
        for (c in seq(ncol(x))) {
            xc <- x[, c]
            max.nchar.nc <- max(nchar(xc), na.rm = TRUE)
            ndec <- 0
            if (pN[c]) {
                ndec <- min(numdec(xc), 3)
                x[, c] <- sprintf(
                    paste0("%", max.nchar.nc, ".", ndec, "f"),
                    asNumeric(xc)
                )
            }
            if (possibleNumeric(nms[c])) {
                nmsc <- sprintf(
                    paste0("%", max.nchar.nc, ".", ndec, "f"),
                    asNumeric(nms[c])
                )
                if (grepl("[.]", nmsc)) {
                    nmsc <- paste(
                        unlist(strsplit(nmsc, split = "[.]"))[1],
                        paste(rep(" ", ndec), collapse = "")
                    )
                }
                nms[c] <- nmsc
            }
        }
        x[nax] <- ""
        max.nchars <- max(nchar(c(encodeString(nms), x)), na.rm = TRUE)
        for (i in seq(length(nms))) {
            if (nchar(nms[i]) < max.nchars) {
                nms[i] <- padBoth(nms[i], max.nchars - nchar(nms[i]))
            }
        }
        for (i in seq(length(x))) {
            if (nchar(x[i]) < max.nchars) {
                x[i] <- padBoth(x[i], max.nchars - nchar(x[i]))
            }
        }
        colnames(x) <- nms
        cat(ifelse(startend, "\n", ""))
        print(noquote(x))
        cat(ifelse(startend, "\n", ""))
    }
}
