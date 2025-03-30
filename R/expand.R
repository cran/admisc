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

`expand` <- function(expression = "", snames = "", noflevels = NULL,
    partial = FALSE, implicants = FALSE, ...) {
    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))
    dots <- list(...)
    multivalue <- FALSE
    scollapse <- ifelse(is.element("scollapse", names(dots)), dots$scollapse, FALSE) 
    scollapse <- scollapse | grepl("[*]", expression)
    if (!is.null(noflevels)) {
        if (is.character(noflevels) & length(noflevels) == 1) {
            noflevels <- splitstr(noflevels)
        }
    }
    `remred` <- function(x) {
        if (nrow(x) > 1) {
            redundant <- logical(nrow(x))
            for (i in seq(nrow(x) - 1)) {
                if (!redundant[i]) {
                    for (j in seq(i + 1, nrow(x))) {
                        if (!redundant[j]) {
                            subsetrow <- checkSubset(x[c(i, j), , drop = FALSE])
                            if (!is.null(subsetrow)) {
                                redundant[c(i, j)[subsetrow]] <- TRUE
                            }
                        }
                    }
                }
            }
            x <- x[!redundant, , drop = FALSE]
        }
        return(x)
    }
    `dnf` <- function(x, noflevels = NULL, partial = FALSE) {
        if (is.null(noflevels)) {
            noflevels <- rep(2, ncol(x))
        }
        zeroc <- which(apply(x, 2, function(x) all(x == 0)))
        if (length(zeroc) > 0 & partial) {
            x <- x[, -zeroc, drop = FALSE]
        }
        result <- matrix(nrow = 0, ncol = ncol(x))
        rmin <- min(apply(x, 1, function(x) sum(x == 0)))
        for (i in seq(nrow(x))) {
            xi <- x[i, ]
            rxi <- sum(xi == 0)
            if (rxi > 0 & ifelse(partial, rxi > rmin, TRUE)) {
                wxi <- which(xi == 0)
                if (partial) {
                    combs <- combnk(rxi, rxi - rmin)
                    for (col in seq(ncol(combs))) {
                        wxic <- wxi[combs[, col]]
                        rest <- getMatrix(noflevels[wxic]) + 1
                        basemat <- matrix(rep(xi[-wxic], nrow(rest)), nrow = nrow(rest), byrow = TRUE)
                        resmat <- cbind(basemat, rest)[, order(c(seq(ncol(x))[-wxic], wxic)), drop = FALSE]
                        result <- rbind(result, resmat)
                    }
                }
                else {
                    rest <- getMatrix(noflevels[wxi]) + 1
                    basemat <- matrix(rep(xi[-wxi], nrow(rest)), nrow = nrow(rest), byrow = TRUE)
                    resmat <- cbind(basemat, rest)[, order(c(seq(ncol(x))[-wxi], wxi)), drop = FALSE]
                    result <- rbind(result, resmat)
                }
            }
            else {
                result <- rbind(result, xi)
            }
        }
        colnames(result) <- colnames(x)
        if (length(zeroc) > 0 & partial) {
            for (i in zeroc) {
                result <- cbind(result, 0)
            }
            result <- result[, order(c(seq(ncol(result))[-zeroc], zeroc)), drop = FALSE]
            colnames(result)[zeroc] <- names(zeroc)
        }
        return(unique(result))
    }
    if (is.character(expression)) {
        if (length(expression) > 1) {
            expression <- expression[1]
        }
        if (identical(snames, "")) {
            syscalls <- unlist(lapply(sys.calls(), deparse))
            usingwith <- "admisc::using\\(|using\\(|with\\("
            if (any(usingdata <- grepl(usingwith, syscalls))) {
                data <- get(
                    unlist(strsplit(gsub(usingwith, "", syscalls), split = ","))[1],
                    envir = length(syscalls) - tail(which(usingdata), 1)
                )
                if (is.data.frame(data) | is.matrix(data)) {
                    snames <- colnames(data)
                }
            }
        }
        snames <- splitstr(snames)
        multivalue <- any(grepl("\\[|\\]|\\{|\\}", expression))
        if (multivalue) {
            expression <- gsub("[*]", "", expression)
            checkMV(expression, snames = snames, noflevels = noflevels) 
        }
        if (!grepl("[+]", expression) & grepl("[,]", expression)) {
            if (multivalue) {
                values <- squareBrackets(expression)
                atvalues <- paste("@", seq(length(values)), sep = "")
                for (i in seq(length(values))) {
                    expression <- gsub(values[i], atvalues[i], expression)
                }
                expression <- gsub(",", "+", expression)
                for (i in seq(length(values))) {
                    expression <- gsub(atvalues[i], values[i], expression)
                }
            }
            else {
                oldway <- unlist(strsplit(gsub("[-|;|,|[:space:]]", "", expression), split = ""))
                if (!possibleNumeric(oldway) & length(oldway) > 0) {
                    expression <- gsub(",", "+", expression)
                }
            }
        }
        if (any(grepl("[(|)]", expression))) {
            bl <- expandBrackets(expression, snames = snames, noflevels = noflevels)
        }
        else {
            bl <- expression
        }
        if (identical(bl, "")) {
            return(classify("", "admisc_simplify"))
        }
        tlist <- list(expression = bl, snames = snames)
        if (!is.null(noflevels)) {
            tlist$noflevels <- noflevels
        }
        bl <- tryCatch(do.call(translate, tlist), error = function(e) e)
        if (is.list(bl)) {
            return(classify("", "admisc_simplify"))
        }
        expression <- matrix(nrow = 0, ncol = ncol(bl))
        colnames(expression) <- colnames(bl)
        for (i in seq(nrow(bl))) {
            expression <- rbind(expression, as.matrix(expand.grid(lapply(bl[i, ], function(x) {
                asNumeric(splitstr(x)) + 1
            }))))
        }
    }
    else if (!is.matrix(expression)) {
        stopError("The input should be either a character expression or a matrix.")
    }
    if (is.null(noflevels)) noflevels <- rep(2, ncol(expression))
    expression <- dnf(remred(expression), noflevels = noflevels, partial = partial)
    if (implicants) {
        for (i in seq(ncol(expression), 1)) {
            expression <- expression[order(expression[, i]), , drop = FALSE]
        }
        rownames(expression) <- NULL
        return(expression)
    }
    if (is.null(colnames(expression))) {
        stopError("The input matrix should have column names.")
    }
    scollapse <- scollapse | any(nchar(snames) > 1)
    expression <- writePIs(expression, multivalue, collapse = ifelse(scollapse, "*", ""))
    expression <- paste(expression, collapse = " + ")
    if (!identical(snames, "")) {
        attr(expression, "snames") <- snames
    }
    return(classify(expression, "admisc_simplify"))
}
