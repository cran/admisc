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

`factorize` <- 
function(input, snames = "", noflevels = NULL, pos = FALSE, ...) {
    input <- recreate(substitute(input))
    if (identical(input, character(0))) {
        return(invisible(input))
    }
    snames <- recreate(substitute(snames))
    dots <- list(...)
    scollapse <- ifelse(is.element("scollapse", names(dots)), dots$scollapse, FALSE) 
    `pasteit` <- function(mat, comrows, cols, comvals, snames = "", mv = FALSE, collapse = "*", curly = FALSE) {
        if (!missing(cols)) {
            temp <- mat[comrows, -cols, drop = FALSE]
            if (mv) {
                cf <- paste(colnames(mat)[cols], ifelse(curly, "{", "["), comvals, ifelse(curly, "}", "]"), sep = "")
                rowsf <- lapply(seq(nrow(temp)), function(x) {
                    fname <- colnames(temp)
                    x <- temp[x, ]
                    return(paste(fname, ifelse(curly, "{", "["), x, ifelse(curly, "}", "]"), sep = "")[x >= 0])
                })
            }
            else {
                for (i in seq(length(cols))) {
                    if (comvals[i] == 0) {
                        colnames(mat)[cols[i]] <- paste("~", colnames(mat)[cols[i]], sep = "")
                    }
                }
                cf <- colnames(mat)[cols]
                rowsf <- lapply(seq(nrow(temp)), function(x) {
                    x <- temp[x, ]
                    nms <- names(x)
                    if (!is.null(nms)) {
                        nms[x == 0] <- paste("~", (nms[x == 0]), sep = "")
                        return(nms[x >= 0])
                    }
                })
            }
            trowsf <- table(unlist(rowsf))
            if (any(trowsf == length(rowsf))) {
                c2 <- names(trowsf)[trowsf == length(rowsf)]
                cf <- c(cf, c2[c2 != ""])
                rowsf <- lapply(rowsf, setdiff, c2)
            }
            rowsf1 <- lapply(rowsf[rowsf != ""], function(x) {
                x <- x[order(match(gsub("[^A-Za-z]", "", x), snames))]
                return(paste(x, collapse = collapse))
            })
            rowsf <- sapply(rowsf, paste, collapse = collapse)
            rowsf <- unique(setdiff(rowsf, ""))
            if (all(nchar(unique(notilde(rowsf))) == 1)) {
                tblchar <- table(notilde(rowsf))
                if (any(tblchar > 1)) {
                    for (ch in names(tblchar)[tblchar > 1]) {
                        rowsf <- rowsf[-which(notilde(rowsf) == ch)]
                    }
                }
            }
            rowsf <- paste(rowsf, collapse = " + ")
            cf <- paste(cf[order(match(gsub("[^A-Za-z]", "", cf), snames))], collapse = collapse)
            pasted <- paste(cf, rowsf, sep = "@")
        }
        else {
            if (mv) {
                pasted <- paste(sapply(seq(nrow(mat)), function(x) {
                    x <- mat[x, ]
                    paste(paste(names(x), ifelse(curly, "{", "["), x, ifelse(curly, "}", "]"), sep = "")[x >= 0], collapse = "*")
                }), collapse = " + ")
            }
            else {
                pasted <- paste(sapply(seq(nrow(mat)), function(x) {
                    colns <- colnames(mat)
                    colns[mat[x, ] == 0] <- paste("~", colns[mat[x, ] == 0], sep = "")
                    return(paste(colns[mat[x, ] >= 0], collapse = collapse))
                }), collapse = " + ")
            }
        }
        return(pasted)
    }
    `getFacts` <- function(mat, snames = "", mv = FALSE, collapse = "*", curly = FALSE) {
        cfound <- FALSE
        result <- list()
        for (cc in seq(ncol(mat))) {
            allcols <- combnk(ncol(mat), cc)
            for (cols in seq(ncol(allcols))) {
                temp <- mat[, allcols[, cols], drop = FALSE]
                uniq <- unique(temp)
                uniq <- uniq[apply(uniq, 1, function(x) all(x >= 0)), , drop = FALSE]
                if (nrow(uniq) > 0) {
                    for (i in seq(nrow(uniq))) {
                        rows <- logical(nrow(mat))
                        comrows <- apply(temp, 1, function(x) { all(x == unname(uniq[i, ])) })
                        if (sum(comrows) > 1) {
                            cfound <- TRUE
                            rows <- rows | comrows
                            pasted <- pasteit(  mat = mat,
                                                comrows = comrows,
                                                cols = allcols[, cols],
                                                comvals = unname(uniq[i, ]),
                                                snames = snames,
                                                mv = mv,
                                                collapse = collapse,
                                                curly = curly)
                            if (sum(rows) < nrow(mat)) {
                                result[[length(result) + 1]] <- Recall(mat[!rows, , drop = FALSE], snames = snames, mv = mv, collapse = collapse)
                                names(result)[length(result)] <- pasted
                            }
                            else {
                                result <- list(NA)
                                names(result) <- pasted
                            }
                        }
                    }
                }
            }
        }
        if (!cfound) {
            result <- list(NA)
            names(result) <- pasteit(mat = mat, snames = snames, mv = mv, collapse = collapse, curly = curly)
        }
        return(result)
    }
    `getSol` <- function(sol, pos = FALSE, noflevels = NULL, snames = "", mv = FALSE, collapse = "*", curly = FALSE) {
        pospos <- FALSE
        sol <- lapply(unique(lapply(sol, sort)), function(x) {
            x <- strsplit(gsub("@1 \\+ 1", "", x), split = "@")
            x <- lapply(x, function(x) {
                x <- unlist(strsplit(x, split = "@"))
                for (i in seq(length(x))) {
                    xi <- unlist(strsplit(x[i], split = " \\+ "))
                    for (j in seq(length(xi))) {
                        xi[j] <- pasteit(translate(xi[j], snames = snames), snames = snames, mv = mv, collapse = collapse, curly = curly)
                    }
                    x[i] <- paste(xi, collapse = " + ")
                }
                return(x)
            })
            if (pos) {
                tbl <- table(unlist(x))
                if (any(tbl > 1)) {
                    tbl <- names(tbl)[tbl > 1]
                    checked <- logical(length(x))
                    common <- vector(mode = "list", length(tbl))
                    names(common) <- tbl
                    for (i in seq(length(tbl))) {
                        for (j in seq(length(x))) {
                            if (!checked[j]) {
                                if (any(x[[j]] == tbl[i])) {
                                    common[[i]] <- c(common[[i]], setdiff(x[[j]], tbl[i]))
                                    checked[j] <- TRUE
                                }
                            }
                        }
                        common[[i]] <- sort(common[[i]])
                    }
                    common <- paste(as.vector(sapply(seq(length(common)), function(x) {
                        sort(c(paste("(", paste(common[[x]], collapse = " + "), ")", sep = ""),
                               paste("(", paste(tbl[x], collapse = " + "), ")", sep = "")))
                    })), collapse = collapse)
                    x <- x[!checked]
                    if (length(x) > 0) {
                        common <- paste(c(common, sapply(x[order(match(gsub("[^A-Za-z]", "", x), snames))], paste, collapse = collapse)), collapse = " + ")
                    }
                    return(common)
                }
                else {
                    x <- sort(sapply(x, function(y) {
                        if (length(y) == 1) {
                            return(y)
                        }
                        paste(y[1], collapse, "(", y[2], ")", sep = "")
                    }))
                }
            }
            else {
                x <- sapply(x, function(y) {
                    if (length(y) == 1) {
                        return(y)
                    }
                    res <- simplify(y[2], snames = snames, noflevels = noflevels, scollapse = identical(collapse, "*"))
                    if (identical(res, character(0))) {
                        return(res)
                    }
                    if (res == "") {
                        return(y[1])
                    }
                    paste(y[1], collapse, "(", res, ")", sep = "")
                })
                if (any(unlist(lapply(x, length)) == 0)) {
                    return(character(0))
                }
                x <- sort(x)
            }
            return(x)
        })
        if (any(unlist(lapply(sol, length)) == 0)) {
            return(character(0))
        }
        sol <- unlist(lapply(unique(sol), function(x) {
            paste(x, collapse = " + ")
        }))
        return(sol)
    }
    `factorizeit` <- function(x, pos = FALSE, noflevels = NULL, snames = "", mv = FALSE, curly = FALSE) {
        if (grepl("[(|)]", x)) {
            x <- expandBrackets(x, snames = snames, noflevels = noflevels)
        }
        trexp <- translate(x, snames = snames, noflevels = noflevels)
        snames <- colnames(trexp)
        collapse <- ifelse(any(nchar(snames) > 1) | mv | scollapse | grepl("[*]", x), "*", "")
        facts <- names(unlist(getFacts(mat = trexp, snames = snames, mv = mv, collapse = collapse, curly = curly)))
        facts <- lapply(facts, function(x) unlist(strsplit(x, split = "[.]")))
        facts <- unique(lapply(facts, sort))
        getSol(facts, pos = pos, noflevels = noflevels, snames = snames, mv = mv, collapse = collapse, curly = curly)
    }
    isol <- NULL
    if (methods::is(input, "QCA_min")) {
        noflevels <- input$tt$noflevels
        snames <- input$tt$options$conditions
        if (input$options$use.letters) {
            snames <- LETTERS[seq(length(snames))]
        }
        if (is.element("i.sol", names(input))) {
            elengths <- unlist(lapply(input$i.sol, function(x) length(x$solution)))
            isol <- paste(rep(names(input$i.sol), each = elengths), unlist(lapply(elengths, seq)), sep = "-")
            input <- unlist(lapply(input$i.sol, function(x) {
                lapply(x$solution, paste, collapse = " + ")
            }))
        }
        else {
            input <- unlist(lapply(input$solution, paste, collapse = " + "))
        }
    }
    else if (methods::is(input, "admisc_deMorgan")) {
        if (any(names(attributes(input)) == "snames")) {
            snames <- attr(input, "snames")
        }
        if (is.list(input)) {
            input <- unlist(input)
        }
    }
    else if (methods::is(input, "admisc_simplify")) {
        if (any(names(attributes(input)) == "snames")) {
            snames <- attr(input, "snames")
        }
    }
    if (is.character(input)) {
        if (!identical(snames, "")) {
            snames <- splitstr(snames)
        }
        mv <- any(grepl("\\[|\\{", unlist(input)))
        curly <- any(grepl("\\{", unlist(input)))
        result <- lapply(input, function(x) {
            factorizeit(x, pos = pos, snames = snames, noflevels = noflevels, mv = mv, curly = curly)
        })
        names(result) <- unname(input)
        if (!identical(snames, "")) {
            attr(result, "snames") <- snames
        }
        if (!is.null(isol)) {
            attr(result, "isol") <- isol
        }
        return(classify(result, "admisc_factorize"))
    }
}
