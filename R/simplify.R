# Copyright (c) 2020, Adrian Dusa
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

`simplify` <- function(expression, snames = "", noflevels = NULL, ...) {
    other.args <- list(...)
    enter    <- ifelse(is.element("enter",   names(other.args)), "",  "\n") 
    all.sol  <- ifelse(is.element("all.sol", names(other.args)), other.args$all.sol, FALSE)
    scollapse <- ifelse(is.element("scollapse", names(other.args)), other.args$scollapse, FALSE) 
    scollapse <- scollapse | grepl("[*]", expression)
    if (identical(snames, "")) {
        syscalls <- unlist(lapply(sys.calls(), deparse))
        if (any(withdata <- grepl("with\\(", syscalls))) {
            data <- get(unlist(strsplit(gsub("with\\(", "", syscalls), split = ","))[1], envir = length(syscalls) - which(withdata))
            if (is.data.frame(data) | is.matrix(data)) {
                snames <- colnames(data)
            }
        }
    }
    multivalue <- any(grepl("[{|}]", expression))
    implicants <- expand(expression, snames = snames, noflevels = noflevels,
                        enter = enter, implicants = TRUE)
    if (identical(unclass(implicants), "")) {
        return(implicants)
    }
    if (is.null(noflevels)) {
        noflevels <- rep(2, ncol(implicants))
    }
    if (!requireNamespace("QCA", quietly = TRUE)) {
        cat(enter)
        stop("Package \"QCA\" is needed to make this work, please install it.", call. = FALSE)
    }
    dataset <- cbind(implicants - 1, 1)
    outcome <- paste(sample(LETTERS, 10), collapse = "")
    colnames(dataset)[ncol(dataset)] <- outcome
    sols <- QCA::minimize(dataset, outcome = outcome, all.sol = all.sol, simplify = TRUE)
    scollapse <- scollapse | any(nchar(colnames(implicants)) > 1) | any(grepl("[{]", unlist(sols$solution))) 
    expression <- unlist(lapply(sols$solution, function(x) {
        if (!scollapse) x <- gsub("\\*", "", x)
        return(paste(x, collapse = " + "))  
    }))
    if (!identical(snames, "")) {
        attr(expression, "snames") <- snames
    }
    return(classify(expression, "admisc_simplify"))
}
`sop` <- function(...) {
    .Deprecated(msg = "Function sop() is deprecated, and has been renamed to simplify()\n")
    simplify(...)
}
