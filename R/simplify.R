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

`simplify` <- function(expression = "", snames = "", noflevels = NULL, ...) {
    expression <- recreate(substitute(expression))
    snames <- recreate(substitute(snames))
    dots <- list(...)
    mvregexp <- "\\[|\\]|\\{|\\}"
    enter     <- if (is.element("enter",   names(dots)))   dots$enter     else "\n"
    all.sol   <- if (is.element("all.sol",   names(dots))) dots$all.sol   else FALSE
    scollapse <- if (is.element("scollapse", names(dots))) dots$scollapse else FALSE 
    if (identical(snames, "")) {
        syscalls <- unlist(lapply(sys.calls(), deparse))
        usingwith <- "admisc::using\\(|using\\(|with\\("
        if (any(usingdata <- grepl(usingwith, syscalls))) {
            data <- get(
                unlist(strsplit(gsub(usingwith, "", syscalls), split = ","))[1],
                envir = length(syscalls) - tail(which(usingdata), 1)
            )
            if (is.data.frame(data) | is.matrix(data)) {
                snames <- colnames(data)
            }
        }
    }
    scollapse <- scollapse | grepl("[*]", expression)
    multivalue <- any(grepl(mvregexp, expression))
    curly <- grepl("[{]", expression)
    if (multivalue) {
        if (is.null(noflevels) | identical(snames, "")) {
            stopError("Set names and their number of levels are required to simplify multivalue expressions.")
        }
    }
    implicants <- expand(expression, snames = snames, noflevels = noflevels,
                         implicants = TRUE)
    if (identical(unclass(implicants), "")) {
        return(implicants)
    }
    if (is.null(noflevels)) {
        noflevels <- rep(2, ncol(implicants))
    }
    version <- -1
    if (requireNamespace("QCA", quietly = TRUE)) {
        version <- compareVersion(
            packageDescription("QCA")$Version,
            "3.7"
        )
    }
    if (version < 0) {
        message(paste(enter, "Error: Package QCA (>= 3.7) is needed to make this work, please install it.", enter, sep = ""))
        return(invisible(character(0)))
    }
    dataset <- cbind(implicants - 1, 1)
    outcome <- paste(sample(LETTERS, 10), collapse = "")
    colnames(dataset)[ncol(dataset)] <- outcome
    test <- tryCatchWEM(sols <- QCA::minimize(dataset, outcome = outcome, all.sol = all.sol, simplify = TRUE))
    if (!is.null(test)) {
        if (!is.null(test$error)) {
            if (grepl("All truth table", test$error)) {
                return("")
            }
        }
    }
    scollapse <- scollapse |
                any(nchar(colnames(implicants)) > 1) |
                any(grepl(mvregexp, unlist(sols$solution)))
    expression <- unlist(lapply(sols$solution, function(x) {
        if (!scollapse) x <- gsub("\\*", "", x)
        return(paste(x, collapse = " + "))  
    }))
    if (curly) {
        expression <- gsub("\\[", "\\{", expression)
        expression <- gsub("\\]", "\\}", expression)
    }
    else {
        expression <- gsub("\\{", "\\[", expression)
        expression <- gsub("\\}", "\\]", expression)
    }
    if (!identical(snames, "")) {
        attr(expression, "snames") <- snames
    }
    return(classify(expression, "admisc_simplify"))
}
`sop` <- function(...) {
    .Deprecated(msg = "Function sop() is deprecated, and has been renamed to simplify()\n")
    simplify(...)
}
