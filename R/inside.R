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

`inside` <- function(data, expr, ...) {
    UseMethod("inside")
}
`inside.data.frame` <- function(data, expr, ...) {
    dataname <- deparse(substitute(data))
    parent <- parent.frame()
    e <- evalq(environment(), data, parent)
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), e)
    l <- as.list(e, all.names = TRUE)
    l <- l[!vapply(l, is.null, NA, USE.NAMES = FALSE)]
    nl <- names(l)
    del <- setdiff(names(data), nl)
    data[nl] <- l
    data[del] <- NULL
    if (exists(dataname, parent)) {
        parent[[dataname]] <- data
    }
    else {
        structure_string <- paste(capture.output(dput(data)), collapse = " ")
        eval(
            parse(text = sprintf(paste(dataname, "<- %s"), structure_string)),
            envir = parent
        )
    }
}
`inside.list` <- function(data, expr, keepAttrs = TRUE, ...) {
    parent <- parent.frame()
    dataname <- deparse(substitute(data))
    e <- evalq(environment(), data, parent)
    if (missing(expr)) {
        args <- unlist(lapply(match.call(), deparse)[-1])
        args <- args[setdiff(names(args), c("data", "expr", "keepAttrs"))]
        if (length(args) > 1) {
            stopError("Missing or ambiguous expression")
        }
        expr <- str2lang(paste(names(args), args[[1]], sep = "<-"))
    }
    eval(substitute(expr), e)
    if (keepAttrs) { 
        l <- as.list(e, all.names=TRUE)
        nl <- names(l)
        del <- setdiff(names(data), nl) 
        data[nl] <- l
        data[del] <- NULL
    } else { 
        data <- as.list(e, all.names=TRUE)
    }
    if (exists(dataname, parent)) {
        parent[[dataname]] <- data
    }
    else {
        structure_string <- paste(capture.output(dput(data)), collapse = " ")
        eval(
            parse(text = sprintf(paste(dataname, "<- %s"), structure_string)),
            envir = parent
        )
    }
}
