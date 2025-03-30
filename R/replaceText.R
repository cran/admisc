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

    replaceText <- function(
        expression, target = "", replacement = "", protect = "",
        boolean = FALSE, ...
    ) {
        dots <- list(...)
        if (!is.character(target)) {
            stopError("The argument <target> should be character.")
        }
        if (!is.character(replacement)) {
            stopError("The argument <replacement> should be character.")
        }
        if (!isTRUE(dots$checknone)) {
            if (length(target) == 1 && !isFALSE(dots$checktarget)) {
                target <- splitstr(target)
            }
            if (length(replacement) == 1) replacement <- splitstr(replacement)
            if (length(protect) == 1) protect <- splitstr(protect)
        }
        if (length(target) != length(replacement)) {
            stopError("Length of target different from the length of replacement.")
        }
        torder <- order(nchar(target), decreasing = TRUE)
        tuplow <- target[torder]
        ruplow <- replacement[torder]
        protect <- protect[order(nchar(protect), decreasing = TRUE)]
        if (
            all(target == toupper(target)) &
            all(expression != toupper(expression)) &
            !any(grepl("~", expression))
        ) {
            boolean <- TRUE
        }
        if (boolean) {
            tuplow <- rep(toupper(tuplow), each = 2)
            ruplow <- rep(toupper(ruplow), each = 2)
            tuplow[seq(2, length(tuplow), by = 2)] <- tolower(tuplow[seq(2, length(tuplow), by = 2)])
            ruplow[seq(2, length(ruplow), by = 2)] <- tolower(ruplow[seq(2, length(ruplow), by = 2)])
            torder <- order(nchar(tuplow), decreasing = TRUE)
            tuplow <- tuplow[torder]
            ruplow <- ruplow[torder]
        }
        getPositions <- function(expression, x, y = NULL, protect = NULL) {
            if (identical(x, "")) {
                return(NULL)
            }
            positions <- vector(mode = "list", length = 0)
            pos <- 0
            for (i in seq(length(x))) {
                escx <- gsub("([][{}*\\.])", "\\\\\\1", x[i]) 
                locations <- gregexpr(escx, expression)[[1]]
                if (any(locations > 0)) {
                    diffs <- c()
                    for (l in seq(length(locations))) {
                        tempd <- seq(locations[l], locations[l] + nchar(x[i]) - 1)
                        if (
                            !any(
                                is.element(
                                    tempd,
                                    c(unlist(positions), unlist(protect))
                                )
                            )
                        ) {
                            diffs <- c(diffs, tempd)
                        }
                    }
                    if (length(diffs) > 0) {
                        if (length(diffs) == 1) {
                            pos <- pos + 1
                            positions[[pos]] <- diffs
                            names(positions)[pos] <- y[i]
                        }
                        else {
                            start <- diffs[1]
                            for (v in seq(2, length(diffs))) {
                                if ((diffs[v] - diffs[v - 1]) > 1) {
                                    pos <- pos + 1
                                    positions[[pos]] <- seq(start, diffs[v - 1])
                                    if (!is.null(y)) {
                                        names(positions)[pos] <- y[i]
                                    }
                                    start <- diffs[v]
                                }
                            }
                            pos <- pos + 1
                            positions[[pos]] <- seq(start, diffs[length(diffs)])
                            if (!is.null(y)) {
                                names(positions)[pos] <- y[i]
                            }
                        }
                    }
                }
            }
            return(positions)
        }
        posprotect <- NULL
        if (!identical(protect, "")) {
            larger <- tuplow[nchar(tuplow) > max(nchar(protect))]
            if (length(larger) > 0) {
                posprotect <- getPositions(
                    expression,
                    x = larger
                )
            }
        }
        posprotect <- getPositions(
            expression,
            x = protect,
            protect = posprotect
        )
        positions <- getPositions(
            expression,
            x = tuplow,
            y = ruplow,
            protect = posprotect
        )
        covered <- logical(length(positions))
        pos2 <- positions
        if (length(positions) > 1) {
            for (i in seq(length(pos2) - 1)) {
                if (!covered[i]) {
                    for (j in seq(i + 1, length(pos2))) {
                        if (!covered[j]) {
                            if (all(is.element(seq(pos2[[j]][1], pos2[[j]][length(pos2[[j]])]), seq(pos2[[i]][1], pos2[[i]][length(pos2[[i]])])))) {
                                covered[j] <- TRUE
                            }
                        }
                    }
                }
            }
        }
        positions <- positions[!covered]
        if (length(positions) > 0) {
            first <- unlist(lapply(positions, "[[", 1))
            positions <- positions[order(first, decreasing = TRUE)]
            expression <- unlist(strsplit(expression, split = ""))
            for (i in seq(length(positions))) {
                if (length(positions[[i]]) == 1) {
                    expression[positions[[i]]] <- names(positions)[i]
                }
                if (length(positions[[i]] > 1)) {
                    start <- positions[[i]][1]
                    stop <- positions[[i]][length(positions[[i]])]
                    if (start == 1) {
                        expression <- c(names(positions)[i], expression[-seq(start, stop)])
                    }
                    else {
                        if (stop < length(expression)) {
                            expression <- c(expression[seq(start - 1)], names(positions)[i], expression[seq(stop + 1, length(expression))])
                        }
                        else {
                            expression <- c(expression[seq(start - 1)], names(positions)[i])
                        }
                    }
                }
            }
            expression <- paste(expression, collapse = "")
        }
        return(expression)
    }
