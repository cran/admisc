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

`using` <- function(data, expr, split.by = NULL, ...) {
    split.by <- substitute(split.by)
    sby <- all.vars(split.by)
    nsby <- all.names(split.by)
    if (length(setdiff(nsby, c("c", "+", "&", colnames(data)))) > 0) {
        stopError("Incorrect specification of the argument <split.by>.")
    }
    expr <- substitute(expr)
    vexpr <- all.vars(expr)
    if (any(vexpr == ".")) {
        vexpr <- colnames(data)
    }
    data <- data[, unique(c(vexpr, sby)), drop = FALSE]
    if (length(sby) == 0) {
        return(eval(expr = expr, envir = data, enclos = parent.frame()))
    }
    if (!all(is.element(sby, colnames(data)))) {
        stopError("One or more split variables not found in the data.")
    }
    sl <- lapply(sby, function(sb) {
        x <- data[[sb]]
        if (inherits(x, "declared") | inherits(x, "haven_labelled_spss")) {
            na_values <- attr(x, "na_values", exact = TRUE)
            labels <- attr(x, "labels", exact = TRUE)
            attributes(x) <- NULL
            x <- sort(unique(x)) 
            x <- x[!is.element(x, na_values)]
            if (!is.null(labels)) {
                havelabels <- is.element(x, labels)
                x[havelabels] <- names(labels)[match(x[havelabels], labels)]
            }
            return(as.character(x))
        }
        if (is.factor(x)) {
            return(levels(x))
        }
        else {
            stopError(
                sprintf(
                    "The split variable %s should be a factor or a declared / labelled variable.",
                    sb
                )
            )
        }
    })
    names(sl) <- sby
    noflevels <- unlist(lapply(sl, length))
    mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
    orep  <- cumprod(
        rev(
            c(rev(noflevels)[-1], 1)
        )
    )
    retmat <- sapply(seq_len(length(sl)), function(x) {
        rep.int(
            rep.int(
                seq_len(noflevels[x]),
                rep.int(mbase[x], noflevels[x])
            ),
            orep[x]
        )
    })
    slexp <- retmat
    for (i in seq(length(sl))) {
        slexp[, i] <- sl[[i]][retmat[, i]]
    }
    res <- vector(mode = "list", length = nrow(slexp))
    for (r in seq(nrow(slexp))) {
        selection <- rep(TRUE, nrow(data))
        for (c in seq(ncol(slexp))) {
            val <- slexp[r, c]
            x <- data[[sby[c]]] 
            attrx <- attributes(x)
            if (inherits(x, "declared") | inherits(x, "haven_labelled_spss")) {
                attributes(x) <- NULL
                na_index <- attrx[["na_index"]]
                if (!is.null(na_index)) {
                    nms <- names(na_index)
                    x[na_index] <- nms
                }
                labels <- attrx[["labels"]]
                if (!is.null(labels)) {
                    havelabels <- is.element(x, labels)
                    x[havelabels] <- names(labels)[match(x[havelabels], labels)]
                }
            }
            selection <- selection & (x == val)
        }
        if (sum(selection) > 0) {
            res[[r]] <- eval(
                expr = expr,
                envir = subset(data, selection),
                enclos = parent.frame()
            )
        }
    }
    if (all(unlist(lapply(res, is.atomic)))) {
        lengths <- unlist(lapply(res, length))
        result <- matrix(NA, nrow = length(res), ncol = max(lengths))
        for (i in seq(length(res))) {
            if (!is.null(res[[i]])) {
                result[i, seq(length(res[[i]]))] <- res[[i]]
            }
        }
        for (i in seq(ncol(slexp))) {
            slexp[, i] <- format(slexp[, i], justify = "right")
        }
        rownames(result) <- apply(slexp, 1, function(x) paste(x, collapse = ", "))
        if (max(lengths) == 1) {
            colnames(result) <- as.character(as.list(expr)[[1]])
        }
        else {
            colnames(result) <- names(res[[which.max(lengths)]])
        }
        res <- result
    }
    else {
        attr(res, "split") <- slexp
    }
    class(res) <- "admisc_using"
    return(res)
}