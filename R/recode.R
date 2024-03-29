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

`recode` <- function(x, rules, cut, values = NULL, ...) {
    UseMethod("recode")
}
`recode.declared` <- function(x, rules, cut, values = NULL, ..., na_values = TRUE) {
    na_index <- attr(x, "na_index")
    attributes(x) <- NULL
    if (!is.null(na_index) & isTRUE(na_values)) {
        nms <- names(na_index)
        if (possibleNumeric(nms)) {
            nms <- asNumeric(nms)
        }
        x[na_index] <- nms
    }
    NextMethod()
}
`recode.default` <- function(x, rules, cut, values = NULL, ...) {
    if (missing(x)) {
        stopError("Argument 'x' is missing.")
    }
    if (!is.atomic(x))   {
        stopError("The input 'x' should be an atomic vector / factor.")
    }
    if (all(is.na(x))) {
        stopError("Nothing to recode, all values are missing.")
    }
    dots <- recreate(list(...))
    as.factor.result  <- isTRUE(dots$as.factor.result)
    as.numeric.result <- !isFALSE(dots$as.numeric.result)
    factor.levels     <- splitstr(dots$levels)
    factor.labels     <- splitstr(dots$labels)
    factor.ordered    <- FALSE
    declared <- inherits(x, "declared")
    if (is.element("ordered", names(dots))) {
        factor.ordered <- dots$ordered
    }
    else if (is.element("ordered_result", names(dots))) {
        factor.ordered <- dots$ordered_result
    }
    if (is.element("cuts", names(dots)) & missing(cut)) {
        cut <- dots[["cuts"]]
    }
    if (is.logical(factor.labels)) { 
        factor.labels <- character(0)
    }
    if (is.null(values) && (!is.null(factor.levels) || !is.null(factor.labels))) {
        as.factor.result  <- TRUE
    }
    `getFromRange` <- function(a, b, uniques, xisnumeric) {
        copya <- a
        copyb <- b
        a <- ifelse(a == "lo", uniques[1], a)
        b <- ifelse(b == "hi", uniques[length(uniques)], b)
        if (xisnumeric) {
            a <- asNumeric(a)
            b <- asNumeric(b)
            if (a > b & (copya == "lo" | copyb == "hi")) return(NULL)
        }
        seqfrom <- which(uniques == a)
        seqto <- which(uniques == b)
        temp2 <- sort(unique(c(uniques, a, b)))
        if (length(seqfrom) == 0) {
            seqfrom <- which(uniques == temp2[which(temp2 == a) + 1])
        }
        if (length(seqto) == 0) {
            seqto <- which(uniques == temp2[which(temp2 == b) - 1])
        }
        if (length(c(seqfrom, seqto)) < 2) return(NULL)
        return(seq(seqfrom, seqto))
    }
    if (missing(cut)) {
        rules <- gsub(
            "\n|\t", "", gsub(
                "'", "", gsub(
                    ")", "", gsub(
                        "c(", "", rules, fixed = TRUE
                    )
                )
            )
        )
        if (length(rules) == 1) {
            semicolons <- gsub("[^;]", "", rules)
            equals <- gsub("[^=]", "", rules)
            if (nchar(equals) != nchar(semicolons) + 1) {
                stopError("The rules should be separated by a semicolon.")
            }
            rules <- unlist(strsplit(rules, split = ";"))
        }
        rulsplit <- strsplit(rules, split = "=")
        oldval <- unlist(lapply(lapply(rulsplit, trimstr), "[", 1))
        newval <- unlist(lapply(lapply(rulsplit, trimstr), "[", 2))
        temp <- rep(NA, length(x))
        elsecopy <- oldval == "else" & newval == "copy"
        if (any(elsecopy)) {
            if (is.factor(x)) {
                temp <- as.character(x)
            }
            else {
                temp <- x
            }
            newval <- newval[!elsecopy]
            oldval <- oldval[!elsecopy]
        }
        newval[newval == "missing" | newval == "NA"] <- NA
        if (any(oldval == "else")) {
            if (sum(oldval == "else") > 1) {
                stopError("Too many \"else\" statements.")
            }
            whichelse <- which(oldval == "else")
            oldval <- c(oldval[-whichelse], oldval[whichelse])
            newval <- c(newval[-whichelse], newval[whichelse])
        }
        oldval <- lapply(
            lapply(
                lapply(oldval, strsplit, split = ","),
                "[[", 1
            ),
            function(y) {
                lapply(
                    strsplit(y, split = ":"),
                    trimstr
                )
            }
        )
        newval <- trimstr(rep(newval, unlist(lapply(oldval, length))))
        if (any(unlist(lapply(oldval, function(y) lapply(y, length))) > 2)) {
            stopError("Too many : sequence operators.")
        }
        from <- unlist(lapply(oldval, function(y) lapply(y, "[", 1)))
        to <- unlist(lapply(oldval, function(y) lapply(y, "[", 2)))
        uniques <- if(is.factor(x)) levels(x) else sort(unique(x[!is.na(x)]))
        recoded <- NULL
        xisnumeric <- possibleNumeric(uniques)
        if (xisnumeric) {
            x <- asNumeric(x) 
            uniques <- asNumeric(uniques)
        }
        for (i in seq(length(from))) {
            if (!is.na(to[i])) { 
                torecode <- getFromRange(from[i], to[i], uniques, xisnumeric)
                if (!is.null(torecode)) {
                    vals <- uniques[torecode]
                    temp[x %in% vals] <- newval[i]
                    recoded <- c(recoded, vals)
                }
            }
            else { 
                if (from[i] == "else") {
                    temp[!is.element(x, recoded)] <- newval[i]
                }
                else if (from[i] == "missing" | from[i] == "NA") {
                    temp[is.na(x)] <- newval[i]
                }
                else {
                    temp[x == from[i]] <- newval[i]
                }
                recoded <- c(recoded, from[i])
            }
        }
    }
    else {
        if (length(cut) == 1 & is.character(cut)) {
            cut <- gsub(
                "\n|\t", "", gsub(
                    "'", "", gsub(
                        ")", "", gsub(
                            "c(", "", cut, fixed = TRUE
                        )
                    )
                )
            )
            cut <- trimstr(unlist(strsplit(cut, split = ",")))
            if (length(cut) == 1) {
                cut <- trimstr(unlist(strsplit(cut, split = ";")))
            }
        }
        if (possibleNumeric(cut)) {
            cut <- asNumeric(cut)
        }
        if (any(duplicated(cut))) {
            stopError("Cut values should be unique.")
        }
        if (is.null(values)) {
            values <- seq(length(cut) + 1)
        }
        else {
            if (length(values) == 1 & is.character(values)) {
                values <- gsub(
                    "\n|\t", "", gsub(
                        "'", "", gsub(
                            ")", "", gsub(
                                "c(", "", values, fixed = TRUE
                            )
                        )
                    )
                )
                values <- trimstr(unlist(strsplit(values, split = ",")))
                if (length(values) == 1) {
                    values <- trimstr(unlist(strsplit(values, split = ";")))
                }
            }
            if (length(values) == length(cut) + 1) {
                as.numeric.result <- possibleNumeric(values)
                if (as.numeric.result) {
                    values <- asNumeric(values)
                }
            }
            else {
                stopError(
                    paste0(
                        "There should be ", length(cut) + 1,
                        " values for ", length(cut), " cut value",
                        ifelse(length(cut) == 1, "", "s"), "."
                    )
                )
            }
        }
        if (is.factor(x)) {
            lx <- levels(x)
            minx <- lx[1]
            maxx <- lx[length(lx)]
            if (is.numeric(cut)) {
                insidex <- FALSE
            }
            else {
                insidex <- all(is.element(cut, lx))
            }
        }
        else {
            sx <- sort(x)
            minx <- sx[1]
            maxx <- sx[length(x)]
            if (is.character(x) & is.numeric(cut)) {
                insidex <- FALSE
            }
            else {
                insidex <- logical(length(cut))
                for (i in seq(length(cut))) {
                    insidex[i] <- cut[i] >= minx & cut[i] <= maxx
                }
            }
        }
        if (!all(insidex)) {
            message <- "Cut value(s) outside the input vector."
            if (declared) {
                message <- paste(message, "Consider using undeclare() before recoding.")
            }
            stopError(message)
        }
        if (is.factor(x)) {
            nx <- as.numeric(x)
            nlx <- seq(length(lx))
            nc <- match(cut, lx)
            temp <- rep(values[1], length(x))
            for (i in seq(length(cut))) {
                temp[nx > nc[i]] = values[i + 1]
            }
        }
        else {
            temp <- rep(values[1], length(x))
            for (i in seq(length(cut))) {
                temp[x > cut[i]] = values[i + 1]
            }
        }
        if (!is.null(factor.labels) && length(factor.labels) == 0 && is.numeric(cut)) {
            factor.labels <- values
        }
    }
    if (as.factor.result) {
        if (length(factor.levels) == 0) {
            factor.levels <- sort(unique(na.omit(temp)))
        }
        if (!is.null(factor.labels) && length(factor.labels) == 0) {
            factor.labels <- factor.levels
        }
        temp <- factor(
            temp,
            levels = factor.levels,
            labels = factor.labels,
            ordered = factor.ordered
        )
    }
    else if (as.numeric.result) {
        if (possibleNumeric(temp)) {
            temp <- asNumeric(temp)
        }
        if (!is.null(factor.labels)) {
            names(values) <- factor.labels
            attr(temp, "labels") <- values
        }
    }
    return(temp)
}
