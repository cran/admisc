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

`insideBrackets` <- function(x, type = "{", invert = FALSE) {
    typematrix <- matrix(c("{", "[", "(", "}", "]", ")", "{}", "[]", "()"), nrow = 3)
    tml <- which(typematrix == type, arr.ind = TRUE)[1]
    if (is.na(tml)) {
        tml <- 1
    }
    tml <- typematrix[tml, 1:2]
    result <- gsub(paste("\\", tml, sep = "", collapse = "|"), "",
        regmatches(x, gregexpr(paste("\\", tml, sep = "", collapse = "[[:alnum:]|,]*"), x), invert = invert)[[1]])
    result <- gsub("\\*|\\+", "", unlist(strsplit(gsub("\\s+", " ", result), split = " ")))
    return(result[result != ""])
}
`outsideBrackets` <- function(x, type = "{") {
    typematrix <- matrix(c("{", "[", "(", "}", "]", ")", "{}", "[]", "()"), nrow = 3)
    tml <- which(typematrix == type, arr.ind = TRUE)[1]
    if (is.na(tml)) {
        tml <- 1
    }
    tml <- typematrix[tml, 1:2]
    pattern <- paste("\\", tml, sep = "", collapse = "[[:alnum:]|,]*")
    result <- gsub("\\*|\\+", "", unlist(strsplit(gsub("\\s+", " ", trimstr(gsub(pattern, " ", x))), split = " ")))
    return(result[result != ""])
}
`curlyBrackets` <- function(x, outside = FALSE) {
    x <- paste(x, collapse = "+")
    regexp <- "\\{[[:alnum:]|,|;]+\\}"
    x <- gsub("[[:space:]]", "", x)
    res <- regmatches(x, gregexpr(regexp, x), invert = outside)[[1]]
    if (outside) {
        res <- gsub("\\*", "", unlist(strsplit(res, split="\\+")))
        return(res[res != ""])
    }
    else {
        return(gsub("\\{|\\}|\\*", "", res))
    }
}
`roundBrackets` <- function(x, outside = FALSE) {
    regexp <- "\\(([^)]+)\\)"
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
