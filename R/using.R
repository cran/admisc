# Copyright (c) 2019 - 2024, Adrian Dusa
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

`using` <- function(data, expr, split.by = NULL, ...) {
    UseMethod("using")
}
`using.default` <- function(data, expr, ...) {
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), data, enclos = parent.frame())
}
`using.data.frame` <- function(data, expr, split.by = NULL, ...) {
    if (nrow(data) == 0) {
        stopError("There are no rows in the data.")
    }
    split.by <- substitute(split.by)
    sby <- all.vars(split.by)
    nsby <- all.names(split.by)
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr", "split.by"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    expr <- substitute(expr)
    vexpr <- all.vars(expr)
    vexpr <- vexpr[is.element(vexpr, names(data))]
    if (any(vexpr == ".")) {
        vexpr <- colnames(data)
    }
    if (length(sby) == 0) {
        return(eval(expr = expr, envir = data, enclos = parent.frame()))
    }
    csby <- setdiff(as.character(split.by), c("c", "+", "&"))
    test <- unlist(lapply(seq(length(csby)), function(i) {
        tryCatchWEM(eval(parse(text = csby[i]), envir = data, enclos = parent.frame()))
    }))
    if (length(test) > 0) {
        stopError(test[1])
    }
    sbylist <- lapply(
        lapply(csby, function(x) {
            eval(parse(text = x), envir = data, enclos = parent.frame())
        }),
        function(x) {
            if (inherits(x, "declared") || inherits(x, "haven_labelled")) {
                labels <- attr(x, "labels", exact = TRUE)
                na_values <- attr(x, "na_values")
                na_range <- attr(x, "na_range")
                if (!is.null(na_range)) {
                    if (length(na_range) > 2) {
                        stopError("Split by variable has a missing range with more than two values.")
                    }
                    na_values <- sort(union(
                        na_values,
                        seq(na_range[1], na_range[2])
                    ))
                }
                if (inherits(x, "haven_labelled")) {
                    x[is.element(x), na_values] <- NA
                }
                labels <- labels[!is.element(labels, na_values)]
                uniques <- sort(unique(c(undeclareit(x, drop = TRUE), labels)))
                names(uniques) <- uniques
                names(uniques)[match(labels, uniques)] <- names(labels)
                attributes(x) <- NULL
                return(factor(x, levels = uniques, labels = names(uniques)))
            }
            return(as.factor(x))
        }
    )
    names(sbylist) <- csby
    test <- table(sapply(sbylist, length))
    if (length(test) > 1 || nrow(data) != as.numeric(names(test))) {
        stopError("Split variables do not match the number of rows in the data.")
    }
    sl <- lapply(sbylist, function(x) {
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
            return(levels(x))
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
    data <- data[, vexpr, drop = FALSE]
    res <- vector(mode = "list", length = nrow(slexp))
    for (r in seq(nrow(slexp))) {
        selection <- rep(TRUE, nrow(data))
        for (c in seq(ncol(slexp))) {
            val <- slexp[r, c]
            x <- sbylist[[c]] 
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
        if (sum(selection, na.rm = TRUE) > 0) {
            res[[r]] <- eval(
                expr = expr,
                envir = subset(data, selection),
                enclos = parent.frame()
            )
        }
    }
    any_w_table <- any(
        sapply(res, function(x) class(x)[1] == "w_table")
    )
    if (all(unlist(lapply(res, is.atomic))) & !any_w_table) {
        classes <- unique(unlist(lapply(res, class)))
        classes <- setdiff(classes, c("integer", "double", "character", "numeric", "complex"))
        lengths <- unlist(lapply(res, length))
        result <- matrix(NA, nrow = length(res), ncol = max(lengths))
        for (i in seq(length(res))) {
            if (!is.null(res[[i]])) {
                result[i, seq(length(res[[i]]))] <- res[[i]]
            }
        }
        result[] <- coerceMode(round(result, 3))
        rownames(result) <- apply(slexp, 1, function(x) paste(x, collapse = ", "))
        if (max(lengths) == 1) {
            colnames(result) <- as.character(as.list(expr)[[1]])
        }
        else {
            colnames(result) <- names(res[[which.max(lengths)]])
        }
        res <- result
        class(res) <- c("admisc_fobject", "matrix")
    }
    else {
        attr(res, "split") <- slexp
        class(res) <- c("admisc_fobject", class(res))
    }
    return(res)
}
