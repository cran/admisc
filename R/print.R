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

`print.translate` <-
function(x, ...) {
    other.args <- list(...)
    cat("\n")
    original <- FALSE
    y <- matrix(as.vector(x), nrow=nrow(x))
    if (is.element("original", names(other.args))) {
        if (is.logical(other.args$original)) {
            original <- other.args$original[1]
        }
    }
    cols <- colnames(x)
    colnames(y) <- cols
    if (original) {
        minus <- any(y < 0)
        if (minus) {
            y[y >= 0] <- paste("", y[y >= 0])
            cols[nchar(cols) == 1] <- paste("", cols[nchar(cols) == 1])
            colnames(y) <- cols
        }
    }
    else {
        y[x < 0] <- ""
    }
    rownames(y) <- paste(rownames(x), " ")
    print(prettyTable(y))
    cat("\n")
}
