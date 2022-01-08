# Copyright (c) 2019 - 2022, Adrian Dusa
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

`getName` <- function(x) {
    result <- rep("", length(x))
    x <- as.vector(gsub("1-", "", gsub("[[:space:]]", "", x)))
    for (i in seq(length(x))) {
        condsplit <- unlist(strsplit(x[i], split=""))
        startpos <- 0
        keycode <- ""
        if (any(condsplit == "]")) {
            startpos <- max(which(condsplit == "]"))
            keycode <- "]"
        }
        if (any(condsplit == "$")) {
            sp <- max(which(condsplit == "$"))
            if (sp > startpos) {
                startpos <- sp
                keycode <- "$"
            }
        }
        if (identical(keycode, "$")) {
            result[i] <- substring(x[i], startpos + 1)
        }
        else if (identical(keycode, "]")) {
            stindex <- max(which(condsplit == "["))
            filename <- paste(condsplit[seq(ifelse(any(condsplit == "("), which(condsplit == "("), 0) + 1, min(which(condsplit == "[")) - 1)], collapse="")
            ptn <- substr(x, stindex + 1, startpos)
            postring <- grepl("\"", ptn)
            ptn <- gsub("\"|]|,|\ ", "", ptn)
            stopindex <- ifelse(identical(condsplit[stindex - 1], "["), stindex - 2, stindex - 1)
            if (possibleNumeric(ptn)) {
                cols <- eval.parent(parse(text = paste("colnames(", filename, ")", sep = "")))
                if (!is.null(cols)) {
                    result[i] <- cols[as.numeric(ptn)]
                }
            }
            else {
                if (!grepl(":", ptn)) {
                    result <- ptn
                }
                if (!postring) { 
                    ptnfound <- FALSE
                    n <- 1
                    if (eval.parent(parse(text = paste0("\"", ptn, "\" %in% ls()")), n = 1)) {
                        ptn <- eval.parent(parse(text = paste("get(", ptn, ")", sep = "")), n = 1)
                        ptnfound <- TRUE
                    }
                    else if (eval.parent(parse(text = paste0("\"", ptn, "\" %in% ls()")), n = 2)) {
                        ptn <- eval.parent(parse(text = paste("get(\"", ptn, "\")", sep = "")), n = 2)
                        ptnfound <- TRUE
                        n <- 2
                    }
                    if (ptnfound) {
                        if (possibleNumeric(ptn)) {
                            result <- eval.parent(parse(text = paste("colnames(", filename, ")[", ptn, "]", sep = "")), n = n)
                        }
                        else {
                            result <- ptn
                        }
                    }
                }
            }
        }
        else {
            result <- x
        }
    }
    return(gsub(",|\ ", "", result))
}
