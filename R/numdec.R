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

`numdec` <- function(x, each = FALSE, na.rm = TRUE) {
    pN <- possibleNumeric(x, each = each)
    if (sum(pN) == 0) {
        stopError("'x' values should be numeric.")
    }
    result <- rep(NA, length(x))
    x <- asNumeric(x)
    hasdec <- agtb(x %% 1, 0)
    if (any(hasdec, na.rm = TRUE)) {
        if (each) {
            for (i in seq(length(x))) {
                if (pN[i]) {
                    result[i] <- 0
                    if (hasdec[i]) {
                        xi <- format(x[i], scientific = FALSE)
                        result[i] <- nchar(unlist(strsplit(xi, split = "[.]"))[2])
                    }
                }
            }
            return(result)
        }
        if (na.rm) {
            x <- na.omit(x)
        }
        if (any(is.na(x))) {
            return(NA)
        }
        x <- format(x, scientific = FALSE)
        return(nchar(unlist(strsplit(x, split = "[.]"))[2]))
    }
    if (each) {
        result[pN] <- 0
        return(result)
    }
    if ((each & sum(pN) == length(x)) || sum(pN) > 0) {
        return(0)
    }
    return(NA)
}
