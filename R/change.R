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

`change` <- function(x, ...) {
    UseMethod("change")
}
`change.default` <- function(x, ...) {
    return(x)
}
`change.QCA_tt` <- function(x, ...) {
    metacall <- match.call(expand.dots = TRUE)
    callargs <- as.list(metacall[-1])
    if (!requireNamespace("QCA", quietly = TRUE)) {
        enter <- ifelse(isFALSE(callargs$enter), "", "\n") 
        message(
            paste(
                enter,
                "Error: Package QCA is needed to change a truth table.",
                enter,
                sep = ""
            )
        )
        return(invisible(character(0)))
    }
    nullargs <- sapply(callargs, is.null)
    nullnms <- names(nullargs)[nullargs]
    if (any(nullargs)) {
        callargs <- callargs[!nullargs]
    }
    if (length(callargs) == 1 & length(nullnms) == 0) {
        return(x) 
    }
    object <- callargs[["x"]]
    `modify` <- function(x) {
        calls <- sapply(x, is.call)
        if (any(calls)) {
            for (i in which(calls)) {
                x[[i]] <- as.call(Recall(as.list(x[[i]])))
            }
        }
        if (as.character(x[[1]]) == "findRows") {
            if (is.null(x$obj)) {
                x$obj <- object
            }
        }
        return(x)
    }
    callargs <- modify(callargs)
    callist <- as.list(x$call) 
    ttname <- as.character(callargs[["x"]])
    for (i in seq(2, length(callist))) {
        callist[[i]] <- admisc::recreate(callist[[i]])
    }
    callist$data <- x$initial.data
    if (length(callargs) > 1) {
        for (i in seq(2, length(callargs))) {
            callargs[[i]] <- admisc::recreate(callargs[[i]])
        }
        for (nm in names(callargs)[-1]) {
            callist[[nm]] <- callargs[[nm]]
        }
    }
    if (length(nullnms) > 0) {
        for (nm in nullnms) {
            callist[[nm]] <- NULL
        }
    }
    x <- do.call("truthTable", callist[-1])
    callist$data <- ttname
    x$call <- as.call(callist)
    return(x)
}
