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

`verify` <- function(data) {
    if (is.data.frame(data)) {
        if (is.null(colnames(data))) {
            stopError("The dataset doesn't have any columns names.")
        }
        checkNumUncal <- lapply(data, function(x) {
            is_a_factor <- is.factor(x)
            is_a_declared <- inherits(x, "declared")
            x <- setdiff(x, c("-", "dc", "?"))
            is_possible_numeric <- admisc::possibleNumeric(x)
            uncal <- mvuncal <- FALSE
            if (is_possible_numeric & !is_a_declared) {
                y <- na.omit(admisc::asNumeric(x))
                if (any(y > 1) & any(abs(y - round(y)) >= .Machine$double.eps^0.5)) {
                    uncal <- TRUE
                }
                if (length(seq(0, max(y))) > 20) {
                    mvuncal <- TRUE
                }
            }
            return(c(is_possible_numeric, uncal, mvuncal, is_a_factor, is_a_declared))
        })
        checknumeric <- sapply(checkNumUncal, "[[", 1)
        checkuncal <- sapply(checkNumUncal, "[[", 2)
        checkmvuncal <- sapply(checkNumUncal, "[[", 3)
        checkfactor <- sapply(checkNumUncal, "[[", 4)
        checkdeclared <- sapply(checkNumUncal, "[[", 5)
        if (!all(checknumeric | checkfactor | checkdeclared)) {
            notnumeric <- colnames(data)[!checknumeric]
            errmessage <- paste("The causal condition",
                                ifelse(length(notnumeric) == 1, " ", "s "),
                                paste(notnumeric, collapse=", "),
                                ifelse(length(notnumeric) == 1, " is ", " are "),
                                "not numeric.", sep="")
            stopError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = ""))
        }
        if (any(checkuncal)) {
            uncalibrated <- colnames(data)[checkuncal]
            errmessage <- paste("Uncalibrated data.\n",
            "Fuzzy sets should have values bound to the interval [0 , 1] and all other sets should be crisp.\n",
            "Please check the following condition", ifelse(length(uncalibrated) == 1, "", "s"), ":\n",
            paste(uncalibrated, collapse = ", "), sep="")
            stopError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = ""))
        }
        if (any(checkmvuncal)) {
            uncalibrated <- colnames(data)[checkmvuncal]
            errmessage <- paste("Possibly uncalibrated data.\n",
            "Multivalue conditions with more than 20 levels are unlikely to be (properly) calibrated.\n",
            "Please check the following condition", ifelse(length(uncalibrated) == 1, "", "s"), ":\n",
            paste(uncalibrated, collapse = ", "), sep="")
            stopError(paste(strwrap(errmessage, exdent = 7), collapse = "\n", sep = ""))
        }
    }
    else if (is.vector(drop(data))) {
        if (!possibleNumeric(data)) {
            stopError("Non numeric input.")
        }
    }
}
