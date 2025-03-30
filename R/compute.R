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

`compute` <-
function(expression = "", data = NULL, separate = FALSE, ...) { 
    expression <- recreate(substitute(expression))
    if (grepl("<-|<=|=>|->", expression)) {
        stopError("This function is not intended to calculate parameters of fit.")
    }
    enchar <- nchar(expression)
    if (
        identical(substring(expression, 1, 2), "~(") &
        identical(substring(expression, enchar, enchar), ")")
    ) {
        expression <- paste0("1-", substring(expression, 3, enchar - 1))
    }
    negated <- identical(unname(substring(expression, 1, 2)), "1-")
    expression <- gsub("1-", "", expression)
    if (is.null(data)) {
        syscalls <- as.character(sys.calls())
        usingwith <- "admisc::using\\(|using\\(|with\\("
        if (any(usingdata <- grepl(usingwith, syscalls))) {
            dataname <- unlist(strsplit(gsub(usingwith, "", syscalls), split = ","))[1]
            data <- eval.parent(parse(text = dataname, n = 1))
        }
        else {
            colnms <- colnames(
                validateNames(
                    notilde(expression),
                    sort(eval.parent(parse(text = "ls()", n = 1)))
                )
            )
            data <- vector(mode = "list", length = length(colnms))
            for (i in seq(length(data))) {
                data[[i]] <- eval.parent(
                    parse(text = sprintf("get(\"%s\")", colnms[i]), n = 1)
                )
            }
            if (length(unique(unlist(lapply(data, length)))) > 1) {
                stopError("Objects should be vectors of the same length.")
            }
            names(data) <- colnms
            data <- as.data.frame(data)
        }
    }
    multivalue <- grepl("\\{|\\}|\\[|\\]", expression)
    if (!multivalue) {
        mvsop <- mvSOP(expression, data = data, ... = ...)
        ppm <- translate(mvsop, data = data, retlist = TRUE)
        rownames(ppm) <- trimstr(unlist(strsplit(expression, split = "\\+")))
    }
    else {
        ppm <- translate(expression, data = data, retlist = TRUE)
    }
    pp <- attr(ppm, "retlist")
    retain <- apply(ppm, 2, function(x) any(x >= 0))
    pp <- lapply(pp, function(x) x[retain])
    ppm <- ppm[, retain, drop = FALSE]
    data <- data[, retain, drop = FALSE]
    infodata <- getInfo(data)
    data <- infodata$data
    verify(data)
    tempList <- vector("list", length(pp))
    for (i in seq(length(pp))) {
        x <- which(ppm[i, ] >= 0)
        val <- pp[[i]][x]
        temp <- data[, colnames(ppm)[x], drop = FALSE]
        for (j in seq(length(val))) {
            if (!is.numeric(temp[, j]) & possibleNumeric(temp[, j])) {
                temp[, j] <- asNumeric(temp[, j])
            }
            nao <- na.omit(temp[, j])
            if (any(abs(nao - round(nao)) >= .Machine$double.eps^0.5)) { 
                if (length(val[[j]]) > 1) {
                    stopError("Multiple values specified for fuzzy data.")
                }
                if (val[[j]] == 0) {
                    temp[, j] <- 1 - temp[, j]
                }
            }
            else { 
                temp[, j] <- as.numeric(is.element(temp[, j], val[[j]]))
            }
        }
        if (ncol(temp) > 1) {
            temp <- apply(temp, 1, min, na.rm = FALSE)
        }
        tempList[[i]] <- temp
    }
    res <- as.data.frame(matrix(unlist(tempList), ncol = length(tempList)))
    colnames(res) <- rownames(ppm)
    if (ncol(res) > 1) {
        if (!separate) {
            res <- apply(res, 1, max, na.rm = FALSE)
        }
    }
    else {
        res <- as.vector(res[, 1])
    }
    if (negated) res <- 1 - res
    return(res)
}
