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

`numdec` <- function(x, each = FALSE, na.rm = TRUE, maxdec = 15) {
    maxdec <- min(15, maxdec)
    pN <- possibleNumeric(x, each = TRUE)
    if (sum(na.omit(pN)) == 0) {
        stopError("'x' should contain at least one (possibly) numeric value.")
    }
    if (is.character(x)) {
        x <- asNumeric(x)
    }
    result <- rep(NA, length(x))
    wpN <- which(pN)
    x <- abs(x[wpN])
    x <- x - floor(x) 
    x <- sub("0\\.", "",
        sub("0+$", "", 
            format(x, scientific = FALSE, digits = max(7, maxdec))
        )
    )
    if (any(w9 <- grepl("999999", x))) {
        x[w9] <- sub(
            "0+", "1", 
            sub("(*)999999.*", "\\1", x[w9]) 
        )
    }
    if (any(w0 <- grepl("000000", x))) {
        x[w0] <- sub("(*)000000.*", "\\1", x[w0])
    }
    result[wpN] <- nchar(x)
    if (each) {
        return(pmin(result, maxdec))
    }
    return(min(maxdec, max(result, na.rm = na.rm)))
}
