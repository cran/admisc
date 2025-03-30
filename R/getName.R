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

`getName` <- function(x, object = FALSE) {
    result <- rep("", length(x))
    x <- as.vector(gsub("1-", "", gsub("[[:space:]]", "", x)))
    condsplit <- unlist(strsplit(x, split = ""))
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
        if (object) {
            return(substring(x, 1, min(which(condsplit == "$")) - 1))
        }
        result <- substring(x, startpos + 1)
    }
    else if (identical(keycode, "]")) {
        objname <- substring(x, 1, min(which(condsplit == "[")) - 1)
        if (object) {
            return(objname)
        }
        nms <- character(0)
        for (target in c("names", "colnames")) {
            for (n in 1:2) {
                if (length(nms) == 0) {
                    testnms <- tryCatchWEM(
                        nms <- eval.parent(
                            parse(
                                text = paste(target, "(", objname, ")", sep = "")
                            ),
                            n = n
                        )
                    )
                }
            }
        }
        stindex <- max(which(condsplit == "["))
        stopindex <- ifelse(
            identical(condsplit[stindex - 1], "["),
            stindex - 2,
            stindex - 1
        )
        ptn <- gsub("]", "", substr(x, stindex + 1, startpos)) 
        if (substring(ptn, 1, 1) == ",") {
            ptn <- substring(ptn, 2)
        }
        if (substring(ptn, 1, 2) == "c(") {
            ptn <- substring(ptn, 3, nchar(ptn) - 1) 
        }
        postring <- grepl("'|\"", ptn)
        ptn <- gsub("'|\"|]|\ ", "", ptn)
        ptn <- unlist(strsplit(ptn, split = ","))
        if (length(ptn) == 1) {
            ptn <- unlist(strsplit(ptn, split = ":"))
        }
        if (possibleNumeric(ptn)) {
            if (length(nms) > 0) {
                result <- nms[as.numeric(ptn)]
            }
        }
        else {
            if (postring) {
                return(ptn)
            }
            if (length(nms) > 0) {
                if (all(is.element(ptn, nms))) {
                    return(ptn)
                }
            }
        }
    }
    else {
        result <- x
    }
    return(gsub(",|\ ", "", result))
}
