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

`invert` <- function(input, snames = "", noflevels = NULL, simplify = TRUE, ...) {
    input <- recreate(substitute(input))
    snames <- recreate(substitute(snames))
    dots <- list(...)
    scollapse <- ifelse(
        is.element("scollapse", names(dots)),
        dots$scollapse,
        FALSE
    ) 
    if (!is.null(noflevels)) {
        if (is.character(noflevels)) {
            noflevels <- splitstr(noflevels)
            if (possibleNumeric(noflevels)) {
                noflevels <- asNumeric(noflevels)
            }
            else {
                stopError("Invalid number of levels.")
            }
        }
    }
    isol <- NULL
    minimized <- methods::is(input, "QCA_min")
    if (minimized) {
        snames <- input$tt$options$conditions
        star <- any(nchar(snames) > 1)
        if (input$options$use.letters) {
            snames <- LETTERS[seq(length(snames))]
            star <- FALSE
        }
        noflevels <- input$tt$noflevels
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
        if (!star) {
            input <- gsub("[*]", "", input)
        }
    }
    if (methods::is(input, "admisc_deMorgan")) {
        input <- unlist(input)
    }
    if (!is.character(input)) {
        stopError("The expression should be a character vector.")
    }
    star <- any(grepl("[*]", input))
    if (!identical(snames, "")) {
        snames <- splitstr(snames)
        if (any(nchar(snames) > 1)) {
            star <- TRUE
        }
    }
    multivalue <- any(grepl("\\[|\\]|\\{|\\}", input))
    if (multivalue) {
        start <- FALSE
        if (is.null(noflevels) | identical(snames, "")) {
            stopError(
                paste(
                    "Set names and their number of levels are required",
                    "to negate multivalue expressions."
                )
            )
        }
    }
    scollapse <- scollapse | any(nchar(snames) > 1) | multivalue | star
    collapse <- ifelse(scollapse, "*", "")
    negateit <- function(
        x, snames = "", noflevels = NULL, simplify = TRUE, collapse = "*"
    ) {
        callist <- list(expression = x)
        callist$snames <- snames
        if (!is.null(noflevels)) callist$noflevels <- noflevels
        trexp <- do.call(translate, callist)
        snames <- colnames(trexp)
        if (is.null(noflevels)) {
            noflevels <- rep(2, ncol(trexp))
        }
        snoflevels <- lapply(noflevels, function(x) seq(x) - 1)
        sr <- nrow(trexp) == 1 
        trcols <- apply(trexp, 2, function(x) any(x != "-1"))
        negated <- paste(
            apply(trexp, 1, function(x) {
                wx <- which(x != -1) 
                x <- x[wx]
                nms <- names(x)
                x <- sapply(seq_along(x), function(i) {
                    paste(
                        setdiff(snoflevels[wx][[i]], splitstr(x[i])),
                        collapse = ","
                    )
                })
                if (multivalue) {
                    return(paste(
                        ifelse(sr | length(wx) == 1, "", "("),
                        paste(
                            nms, "[", x, "]",
                            sep = "",
                            collapse = " + "
                        ),
                        ifelse(sr | length(wx) == 1, "", ")"),
                        sep = ""
                    ))
                }
                else {
                    nms[x == 0] <- paste0("~", nms[x == 0])
                    return(paste(
                        ifelse(sr | length(wx) == 1, "", "("),
                        paste(nms, collapse = " + ", sep = ""),
                        ifelse(sr | length(wx) == 1, "", ")"),
                        sep = ""))
                }
            }),
            collapse = collapse
        )
        negated <- expandBrackets(
            negated,
            snames = snames,
            noflevels = noflevels,
            scollapse = scollapse
        )
        if (simplify) {
            callist$expression <- negated
            callist$scollapse <- identical(collapse, "*")
            callist$snames <- snames[trcols]
            if (!is.null(noflevels)) {
                callist$noflevels <- noflevels[trcols]
            }
            return(unclass(do.call("simplify", callist)))
        }
        return(negated)
    }
    result <- lapply(
        input,
        negateit,
        snames = snames,
        noflevels = noflevels,
        simplify = simplify,
        collapse = collapse
    )
    if (any(unlist(lapply(result, length)) == 0)) {
        return(invisible(character(0)))
    }
    names(result) <- unname(input)
    if (!minimized) {
        attr(result, "expressions") <- input
    }
    if (!identical(snames, "")) {
        attr(result, "snames") <- snames
    }
    if (!is.null(isol)) {
        attr(result, "isol") <- isol
    }
    attr(result, "minimized") <- minimized
    return(classify(result, "admisc_deMorgan"))
}
`deMorgan` <- function(...) {
    .Deprecated(msg = "Function deMorgan() is deprecated. Use function invert() instead.\n")
    negate(...)
}
`negate` <- function(...) {
    invert(...)
}
