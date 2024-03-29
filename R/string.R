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

`trimstr` <- function(x, what = " ", side = "both") {
    if (is.element(what, c("*", "+"))) {
        what <- paste("\\", what, sep = "")
    }
    what <- ifelse(
        identical(what, " "),
        paste0("[[:space:]|", "\u00a0", "]"), 
        what
    )
    pattern <- switch(side,
        both = paste("^", what, "+|", what, "+$", sep = ""),
        left = paste("^", what, "+", sep = ""),
        right = paste(what, "+$", sep = "")
    )
    gsub(pattern, "", x)
}
`splitstr` <- function(x) {
    if (identical(x, "") || is.null(x)) return(x)
    x <- gsub("\\n", "", x)
    oldv <- newv <- NULL
    if (any(grepl(",", x) & grepl("\\{|\\[", x))) {
        curly <- grepl("\\{", x)
        squared <- grepl("\\[", x)
        if (curly & squared) {
            stopError(
                "Multi-value expressions should not mix curly and squared brackets."
            )
        }
        regexp <- ifelse(curly, "\\{[[:alnum:]|,|;]+\\}", "\\[[[:alnum:]|,|;]+\\]")
        oldv <- regmatches(x, gregexpr(regexp, x), invert = FALSE)[[1]]
        newv <- paste("XYZW", seq(length(oldv)), sep = "")
        x <- replaceText(x, oldv, newv)
    }
    y <- unlist(strsplit(x, split = ","))
    if (!is.null(oldv)) {
        for (i in seq(length(y))) {
            y[i] <- replaceText(y[i], newv, oldv)
        }
    }
    y <- trimstr(y)
    if (length(y) == 1) {
        y <- gsub("\\n", "", unlist(strsplit(gsub("[[:space:]]", "", y), split = ";")))
    }
    metacall <- match.call()$x
    if (metacall == "sort.by") {
        if (any(grepl("[=]", y))) {
            y <- t(as.data.frame(strsplit(gsub("[[:space:]]", "", y), split = "=")))
            values <- y[, 2] == TRUE
            names(values) <- y[, 1]
        }
        else {
            values <- !grepl("[+]", y)
            names(values) <- gsub("[+|-]", "", y)
        }
        return(values)
    }
    else if (metacall == "decreasing") {
        return(as.logical(y))
    }
    else if (metacall == "thresholds") {
        if (any(grepl("[=]", y))) {
            y <- t(as.data.frame(strsplit(gsub("[[:space:]]", "", y), split = "=")))
            values <- y[, 2]
            if (possibleNumeric(values)) {
                values <- asNumeric(values)
            }
            names(values) <- y[, 1]
        }
        else {
            if (possibleNumeric(y)) {
                values <- asNumeric(y)
            }
        }
        return(values)
    }
    else {
        if (possibleNumeric(y)) {
            y <- asNumeric(y)
        }
        return(y)
    }
}
`splitMainComponents` <- function(expression) {
    expression <- gsub("[[:space:]]", "", expression)
    ind.char <- unlist(strsplit(expression, split = ""))
    openclosed <- grepl("\\(", expression) | grepl("\\)", expression)
    if (openclosed) {
        open.brackets <- which(ind.char == "(")
        closed.brackets <- which(ind.char == ")")
        invalid <- ifelse(
            openclosed,
            length(open.brackets) != length(closed.brackets),
            TRUE
        )
        if (invalid) {
            stopError("Invalid expression, open bracket \"(\" not closed with \")\".")
        }
        all.brackets <- sort(c(open.brackets, closed.brackets))
        if (length(all.brackets) > 2) {
            for (i in seq(3, length(all.brackets))) {
                if (all.brackets[i] - all.brackets[i - 1] == 1) {
                    open.brackets <- setdiff(open.brackets, all.brackets[seq(i - 1, i)])
                    closed.brackets <- setdiff(closed.brackets, all.brackets[seq(i - 1, i)])
                }
                if (
                    all.brackets[i] - all.brackets[i - 1] == 2 &&
                    ind.char[all.brackets[i] - 1] != "+"
                ) {
                    open.brackets <- setdiff(open.brackets, all.brackets[seq(i - 1, i)])
                    closed.brackets <- setdiff(closed.brackets, all.brackets[seq(i - 1, i)])
                }
            }
        }
        for (i in seq(length(open.brackets))) {
            plus.signs <- which(ind.char == "+")
            last.plus.sign <- plus.signs[plus.signs < open.brackets[i]]
            if (length(last.plus.sign) > 0) {
                open.brackets[i] <- max(last.plus.sign) + 1
            }
            else {
                if (1 == 1) { 
                    open.brackets[i] <- 1
                }
            }
            next.plus.sign <- plus.signs[plus.signs > closed.brackets[i]]
            if(length(next.plus.sign) > 0) {
                closed.brackets[i] <- min(next.plus.sign) - 1
            }
            else {
                closed.brackets[i] <- length(ind.char)
            }
        }
        big.list <- vector(mode = "list", length = length(open.brackets) + 2)
        if (length(open.brackets) == 1) {
            if (open.brackets > 1) {
                big.list[[1]] <- paste(
                    ind.char[seq(1, open.brackets - 2)],
                    collapse = ""
                )
            }
            nep <- min(which(unlist(lapply(big.list, is.null))))
            big.list[[nep]] <- paste(
                ind.char[seq(open.brackets, closed.brackets)],
                collapse = ""
            )
            if (closed.brackets < length(ind.char)) {
                nep <- min(which(unlist(lapply(big.list, is.null))))
                big.list[[nep]] <- paste(
                    ind.char[seq(closed.brackets + 2, length(ind.char))],
                    collapse = ""
                )
            }
        }
        else {
            for (i in seq(length(open.brackets))) {
                if (i == 1) {
                    if (open.brackets[1] > 1) {
                        big.list[[1]] <- paste(
                            ind.char[seq(1, open.brackets[1] - 2)],
                            collapse = ""
                        )
                    }
                    nep <- min(which(unlist(lapply(big.list, is.null))))
                    big.list[[nep]] <- paste(
                        ind.char[seq(open.brackets[i], closed.brackets[i])],
                        collapse = ""
                    )
                }
                else {
                    nep <- min(which(unlist(lapply(big.list, is.null))))
                    big.list[[nep]] <- paste(
                        ind.char[seq(open.brackets[i], closed.brackets[i])],
                        collapse = ""
                    )
                    if (i == length(closed.brackets)) {
                        if (closed.brackets[i] < length(ind.char)) {
                            nep <- min(which(unlist(lapply(big.list, is.null))))
                            big.list[[nep]] <- paste(
                                ind.char[seq(closed.brackets[i] + 2, length(ind.char))],
                                collapse = ""
                            )
                        }
                    }
                }
            }
        }
        nulls <- unlist(lapply(big.list, is.null))
        if (any(nulls)) {
            big.list <- big.list[-which(nulls)]
        }
    }
    else {
        big.list <- list(expression)
    }
    return(big.list)
}
`splitBrackets` <- function(big.list) {
    return(lapply(big.list, function(x) {
        as.list(unlist(strsplit(unlist(strsplit(x, split="\\(")), split="\\)")))
    }))
}
`removeSingleStars` <- function(big.list) {
    return(lapply(big.list, function(x) {
        single.stars <- unlist(lapply(x, function(y) {
            return(y == "*")
        }))
        return(x[!single.stars])
    }))
}
`splitPluses` <- function(big.list) {
    return(lapply(big.list, function(x) {
        lapply(x, function(y) {
            plus.split <- unlist(strsplit(y, "\\+"))
            return(as.list(plus.split[plus.split != ""]))
        })
    }))
}
`splitStars` <- function(big.list, prod.split) {
    return(lapply(big.list, function(x) {
        lapply(x, function(y) {
            lapply(y, function(z) {
                star.split <- unlist(strsplit(z, ifelse(prod.split == "", "", paste("\\", prod.split, sep=""))))
                star.split <- star.split[star.split != ""]
                if (prod.split == "") {
                    tilda <- hastilde(star.split) & length(star.split) > 1
                    if (any(tilda)) {
                        tilda.pos <- which(tilda)
                        if (max(tilda.pos) == length(star.split)) {
                            stopError(paste("Unusual expression \"", z, "\": terminated with a \"~\" sign?", sep = ""))
                        }
                        star.split[tilda.pos + 1] <- paste("~", star.split[tilda.pos + 1], sep="")
                        star.split <- star.split[-tilda.pos]
                    }
                }
                return(as.list(star.split[star.split != ""]))
            })
        })
    }))
}
`splitTildas` <- function (big.list) {
    return(lapply(big.list, function(x) {
        lapply(x, function(y) {
            lapply(y, function(z) {
                lapply(z, function(w) {
                    if (hastilde(w)) {
                        wsplit <- unlist(strsplit(w, split = ""))
                        if (max(which(hastilde(wsplit))) > 1) {
                            stopError(paste("Unusual expression: ", w, ". Perhaps you meant \"*~\"?", sep = ""))
                        }
                        else {
                            return(c("~", notilde(w)))
                        }
                    }
                    else {
                        return(w)
                    }
                })
            })
        })
    }))
}
`solveBrackets` <- function(big.list) {
    bracket.comps <- which(unlist(lapply(big.list, length)) > 1)
    if (length(bracket.comps) > 0) {
        for (i in bracket.comps) {
            lengths <- unlist(lapply(big.list[[i]], length))
            indexes <- expand.grid(lapply(lengths - 1, seq, from = 0)) + 1
            ncol.ind <- ncol(indexes)
            i.list <- vector("list", length = nrow(indexes))
            for (j in seq(length(i.list))) {
                i.list[[j]] <- vector("list", length = prod(dim(indexes)))
                start.position <- 1
                for (k in seq(ncol.ind)) {
                    for (l in seq(length(big.list[[i]][[k]][[indexes[j, k]]]))) {
                        i.list[[j]][[start.position]] <- big.list[[i]][[k]][[indexes[j, k]]][[l]]
                        start.position <- start.position + 1
                    }
                }
                if (start.position <= length(i.list[[j]])) {
                    i.list[[j]] <- i.list[[j]][- seq(start.position, length(i.list[[j]]))]
                }
            }
            big.list[[i]] <- list(i.list)
        }
    }
    return(big.list)
}
`simplifyList` <- function(big.list) {
    lengths <- unlist(lapply(big.list, function(x) length(x[[1]])))
    bl <- vector("list", length = sum(lengths))
    pos <- 1
    for (i in seq(length(big.list))) {
        for (j in seq(lengths[i])) {
            blj <- unlist(big.list[[i]][[1]][[j]])
            if (hastilde(blj[1]) & nchar(blj[1]) == 1) {
                blj <- blj[-1]
                for (b in seq(length(blj))) {
                    if (tilde1st(blj[b])) {
                        blj[b] <- notilde(blj[b])
                    }
                    else {
                        blj[b] <- paste0("~", blj[b])
                    }
                }
            }
            bl[[pos]] <- unique(blj)
            pos <- pos + 1
        }
    }
    return(unique(bl[!unlist(lapply(bl, function(x) any(duplicated(notilde(x)))))]))
}
`getNonChars` <- function(x) {
    x <- gsub("^[[:space:]]+|[[:space:]]+$", "", unlist(strsplit(x, "\\+")))
    z <- vector(mode="list", length=length(x))
    for (i in seq(length(x))) {
        z[[i]] <- strsplit(gsub("[[:alnum:]]", "", x[i]), "+")[[1]]
    }
    z <- notilde(unique(unlist(z)))
    return(z[nzchar(z)])
}
