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

`checkMV` <- function(
    expression, snames = "", noflevels = NULL, data = NULL, use.labels = FALSE, categories = list(), ...
) {
    curly <- any(grepl("[{]", expression))
    if (length(unlist(gregexpr(ifelse(curly, "[{]+", "\\[+"), expression))) != length(unlist(gregexpr(ifelse(curly, "[}]+", "\\]+"), expression)))) {
        stopError("Incorrect expression, opened and closed brackets don't match.")
    }
    dots <- list(...)
    if (is.element("categorical", names(dots))) {
        use.labels <- dots$categorical
        dots$categorical <- NULL
    }
    tempexpr <- gsub("[*|,|;|(|)]", "", expression)
    pp <- trimstr(unlist(strsplit(tempexpr, split = "[+]")))
    if (curly) {
        insb <- curlyBrackets(gsub("[*|(|)]", "", expression))
        tempexpr <- curlyBrackets(tempexpr, outside = TRUE)
    }
    else {
        insb <- squareBrackets(gsub("[*|(|)]", "", expression))
        tempexpr <- squareBrackets(tempexpr, outside = TRUE)
    }
    if (length(insb) != length(tempexpr)) {
        error <- TRUE
        if (use.labels) {
            tempexpr2 <- tempexpr[!is.element(tempexpr, names(unlist(unname(categories))))]
            error <- length(insb) != length(tempexpr2)
        }
        if (error) {
            stopError("Incorrect expression, some set names do not have brackets.")
        }
    }
    if (any(grepl("[a-zA-Z]", gsub("[,|;]", "", insb)))) {
        stopError("Invalid [multi]values, levels should be numeric.")
    }
    if (curly) {
        conds <- sort(unique(notilde(curlyBrackets(pp, outside = TRUE))))
    }
    else {
        conds <- sort(unique(notilde(squareBrackets(pp, outside = TRUE))))
    }
    if (is.null(data)) {
        if (is.null(noflevels)) {
            if (any(hastilde(expression))) {
                stopError("Negating a multivalue condition requires the number of levels.")
            }
        }
        else {
            if (identical(snames, "")) {
                stopError("Cannot verify the number of levels without the set names.")
            }
            snames <- splitstr(snames)
            if (is.character(noflevels)) {
                noflevels <- splitstr(noflevels)
            }
            if (length(noflevels) == 1 && is.numeric(noflevels) && length(snames) > 1) {
                noflevels <- rep(noflevels, length(snames))
            }
            if (length(snames) != length(noflevels)) {
                stopError("Length of the set names differs from the length of the number of levels.")
            }
            for (i in seq(length(tempexpr))) {
                if (!is.element(notilde(tempexpr[i]), snames)) {
                    stopError(sprintf("Condition %s not present in the set names.", tempexpr[i]))
                }
                if (max(asNumeric(splitstr(insb[i]))) > noflevels[match(notilde(tempexpr[i]), snames)] - 1) {
                    stopError(sprintf("Levels outside the number of levels for condition %s.", tempexpr[i]))
                }
            }
        }
    }
    for (i in seq(length(expression))) {
        checkValid(
            expression = expression[i],
            snames = "something", 
            data = data,
            categories = categories
        )
    }
}
