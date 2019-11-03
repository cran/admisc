# Copyright (c) 2019, Adrian Dusa
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

`checkMV` <- function(expression, snames = "", noflevels = NULL, data = NULL) {
    if (length(unlist(gregexpr("[{]+", expression))) != length(unlist(gregexpr("[}]+", expression)))) {
        cat("\n")
        stop(simpleError("Incorrect expression, opened and closed brackets don't match.\n\n"))
    }
    tempexpr <- gsub("[*|,|;|(|)]", "", expression)
    pp <- unlist(strsplit(tempexpr, split = "[+]"))
    insb <- curlyBrackets(gsub("[*|(|)]", "", expression))
    tempexpr <- curlyBrackets(tempexpr, outside = TRUE)
    if (length(insb) != length(tempexpr)) {
        cat("\n")
        stop(simpleError("Incorrect expression, some snames don't have brackets.\n\n"))
    }
    if (any(grepl("[a-zA-Z]", gsub("[,|;]", "", insb)))) {
        cat("\n")
        stop(simpleError("Invalid {multi}values, levels should be numeric.\n\n"))
    }
    conds <- sort(unique(toupper(notilde(curlyBrackets(pp, outside = TRUE)))))
    if (is.null(data)) {
        if (is.null(noflevels)) {
            if (any(hastilde(expression))) {
                cat("\n")
                stop(simpleError("Negating a multivalue condition requires the number of levels.\n\n"))
            }
        }
        else {
            if (identical(snames, "")) {
                cat("\n")
                stop(simpleError("Cannot verify the number of levels without the set names.\n\n"))
            }
            snames <- splitstr(snames)
            if (is.character(noflevels)) {
                noflevels <- splitstr(noflevels)
            }
            if (length(snames) != length(noflevels)) {
                cat("\n")
                stop(simpleError("Length of the set names differs from the length of the number of levels.\n\n"))
            }
            for (i in seq(length(tempexpr))) {
                if (!is.element(notilde(tempexpr[i]), snames)) {
                    cat("\n")
                    stop(simpleError(sprintf("Condition %s not present in the set names.\n\n", tempexpr[i])))
                }
                if (max(asNumeric(splitstr(insb[i]))) > noflevels[match(notilde(tempexpr[i]), snames)] - 1) {
                    cat("\n")
                    stop(simpleError(sprintf("Levels outside the number of levels for condition %s.\n\n", tempexpr[i])))
                }
            }
        }
    }
    else { 
            if (length(setdiff(conds, colnames(data))) > 0) {
                cat("\n")
                stop(simpleError("Parts of the expression don't match the column names from \"data\" argument.\n\n"))
            }
    }
    if (!identical(snames, "")) {
        if (length(setdiff(conds, toupper(splitstr(snames)))) > 0) {
            cat("\n")
            stop(simpleError("Parts of the expression don't match the set names from \"snames\" argument.\n\n"))
        }
    }
}
