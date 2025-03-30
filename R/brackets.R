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

`betweenBrackets` <- function(x, type = "[", invert = FALSE, regexp = NULL) {
    x <- recreate(substitute(x))
    typematrix <- matrix(c("{", "[", "(", "}", "]", ")", "{}", "[]", "()"), nrow = 3)
    tml <- which(typematrix == type, arr.ind = TRUE)[1]
    if (is.na(tml)) {
        tml <- 1
    }
    tml <- typematrix[tml, 1:2]
    if (is.null(regexp)) {
        regexp <- "[[:alnum:]|,]*"
    }
    result <- gsub(
        paste("\\", tml, sep = "", collapse = "|"),
        "",
        regmatches(
            x,
            gregexpr(
                paste("\\", tml, sep = "", collapse = regexp),
                x
            ),
            invert = invert
        )[[1]]
    )
    result <- gsub("\\*|\\+", "", unlist(strsplit(gsub("\\s+", " ", result), split = " ")))
    return(result[result != ""])
}
`insideBrackets` <- function(...) {
    .Deprecated(msg = "Function insideBrackets() is deprecated, use betweenBrackets().\n")
    betweenBrackets(...)
}
`outsideBrackets` <- function(x, type = "[", regexp = NULL) {
    x <- recreate(substitute(x))
    typematrix <- matrix(c("{", "[", "(", "}", "]", ")", "{}", "[]", "()"), nrow = 3)
    tml <- which(typematrix == type, arr.ind = TRUE)[1]
    if (is.na(tml)) {
        tml <- 1
    }
    tml <- typematrix[tml, 1:2]
    if (is.null(regexp)) {
        regexp <- "[[:alnum:]|,]*"
    }
    pattern <- paste("\\", tml, sep = "", collapse = regexp)
    result <- gsub(
        "\\*|\\+",
        "",
        unlist(
            strsplit(
                gsub(
                    "\\s+",
                    " ",
                    trimstr(gsub(pattern, " ", x))
                ),
                split = " "
            )
        )
    )
    return(result[result != ""])
}
`curlyBrackets` <- function(x, outside = FALSE, regexp = NULL) {
    x <- recreate(substitute(x))
    x <- paste(x, collapse = "+")
    if (is.null(regexp)) {
        regexp <- "\\{[[:alnum:]|,|;]+\\}"
    }
    x <- gsub("[[:space:]]", "", x)
    res <- regmatches(x, gregexpr(regexp, x), invert = outside)[[1]]
    if (outside) {
        res <- gsub(
            "\\*",
            "",
            unlist(strsplit(res, split = "\\+"))
        )
        return(res[res != ""])
    }
    else {
        return(gsub("\\{|\\}|\\*", "", res))
    }
}
`squareBrackets` <- function(x, outside = FALSE, regexp = NULL) {
    x <- recreate(substitute(x))
    x <- paste(x, collapse = "+")
    if (is.null(regexp)) {
        regexp <- "\\[[[:alnum:]|,|;]+\\]"
    }
    x <- gsub("[[:space:]]", "", x)
    res <- regmatches(x, gregexpr(regexp, x), invert = outside)[[1]]
    if (outside) {
        res <- gsub(
            "\\*",
            "",
            unlist(strsplit(res, split = "\\+"))
        )
        return(res[res != ""])
    }
    else {
        return(gsub("\\[|\\]|\\*", "", res))
    }
}
`roundBrackets` <- function(x, outside = FALSE, regexp = NULL) {
    x <- recreate(substitute(x))
    if (is.null(regexp)) {
        regexp <- "\\(([^)]+)\\)"
    }
    x <- gsub("[[:space:]]", "", x)
    res <- regmatches(x, gregexpr(regexp, x), invert = outside)[[1]]
    if (outside) {
        res <- unlist(strsplit(res, split="\\+"))
        return(res[res != ""])
    }
    else {
        return(gsub("\\(|\\)|\\*", "", res))
    }
}
`expandBrackets` <- function(
    expression, snames = "", noflevels = NULL, scollapse = FALSE
) {
    expression <- recreate(substitute(expression))
    snames <- splitstr(snames)
    star <- any(grepl("[*]", expression))
    multivalue <- any(grepl("\\[|\\]|\\{|\\}", expression))
    collapse <- ifelse(
        any(nchar(snames) > 1) | multivalue | star | scollapse,
        "*",
        ""
    )
    curly <- grepl("[{]", expression)
    sl <- ifelse( 
        identical(snames, ""),
        FALSE,
        ifelse(
            all(nchar(snames) == 1),
            TRUE,
            FALSE
        )
    )
    getbl <- function(expression, snames = "", noflevels = NULL) {
        bl <- splitMainComponents(gsub("[[:space:]]", "", expression))
        bl <- splitBrackets(bl)
        bl <- lapply(bl, function(x) {
            if (tilde1st(x[[1]]) & nchar(x[[1]]) == 1) {
                x <- x[-1]
                x[[1]] <- as.character(negate(x[[1]], snames = snames, noflevels = noflevels))
            }
            return(x)
        })
        bl <- removeSingleStars(bl)
        bl <- splitPluses(bl)
        blu <- unlist(bl)
        bl <- splitStars(
            bl,
            ifelse(
                (
                    sl | any(
                        hastilde(blu) & !tilde1st(blu)
                    )
                ) & !grepl("[*]", expression) & !multivalue,
                "",
                "*"
            )
        )
        bl <- solveBrackets(bl)
        bl <- simplifyList(bl)
        return(bl)
    }
    bl <- getbl(expression, snames = snames, noflevels = noflevels)
    if (length(bl) == 0) return("")
    bl <- paste(
        unlist(
            lapply(
                bl,
                paste,
                collapse = collapse
            )
        ),
        collapse = " + "
    )
    expressions <- translate(bl, snames = snames, noflevels = noflevels)
    snames <- colnames(expressions)
    redundant <- logical(nrow(expressions))
    if (nrow(expressions) > 1) {
        for (i in seq(nrow(expressions) - 1)) {
            if (!redundant[i]) {
                for (j in seq(i + 1, nrow(expressions))) {
                    if (!redundant[j]) {
                        subsetrow <- checkSubset(
                            expressions[c(i, j), , drop = FALSE],
                            implicants = FALSE
                        )
                        if (!is.null(subsetrow)) {
                            redundant[c(i, j)[subsetrow]] <- TRUE
                        }
                    }
                }
            }
        }
        expressions <- expressions[!redundant, , drop = FALSE]
        if (possibleNumeric(expressions)) {
            mat <- matrix(asNumeric(expressions) + 1, nrow = nrow(expressions))
            colnames(mat) <- colnames(expressions)
            expressions <- sortExpressions(mat) - 1
        }
        else {
            eorder <- order(
                apply(
                    expressions,
                    1,
                    function(x) sum(x < 0)
                ),
                decreasing = TRUE
            )
            expressions <- expressions[eorder, , drop = FALSE]
        }
    }
    expressions <- unlist(apply(expressions, 1, function(x) {
        result <- c()
        for (i in seq(length(snames))) {
            if (x[i] != -1) {
                if (multivalue) {
                    result <- c(
                        result,
                        paste(
                            snames[i],
                            ifelse(curly, "{", "["),
                            x[i],
                            ifelse(curly, "}", "]"),
                            sep = ""
                        )
                    )
                }
                else {
                    if (x[i] == 0) {
                        result <- c(result, paste("~", snames[i], sep = ""))
                    }
                    else {
                        result <- c(result, snames[i])
                    }
                }
            }
        }
        return(paste(result, collapse = collapse))
    }))
    return(paste(expressions, collapse = " + "))
}
