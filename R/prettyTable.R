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

`prettyTable` <-
function(input) {
    if (methods::is(input, "QCA_pic")) {
        class(input) <- "matrix" 
    }
    else {
        input <- as.matrix(input) 
    }
    if (is.logical(input)) {
        input2 <- input
        input[input2]  <- "x"
        input[!input2] <- "-"
    }
    if(is.null(colnames(input))) colnames(input) <- rep(" ", ncol(input))
    nchars <- nchar(colnames(input))
    colnames(input)[nchars == 1] <- format(colnames(input)[nchars == 1], width = 2, justify = "centre")
    nchars[nchars == 1] <- 2
    for (i in seq((ncol(input) - any(colnames(input) == "lines")))) {
        input[, i] <- format(format(input[, i]), width = nchars[i], justify = "centre")
    }
    rownames(input) <- paste(rownames(input), "")
    return(noquote(input))
}
