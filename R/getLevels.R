# Copyright (c) 2019 - 2024, Adrian Dusa
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

`getLevels` <- function(data) {
    data <- as.data.frame(data)
    colnames <- paste("V", ncol(data), sep = ".")
    pN <- sapply(data, possibleNumeric)
    noflevels <- rep(NA, ncol(data))
    ulevels <- rep(NA, ncol(data))
    noflevels[pN] <- apply(
        data[, pN, drop = FALSE],
        2,
        function(x) max(as.numeric(x))
    ) + 1
    ulevels <- apply(
        data,
        2,
        function(x) {
            return(length(unique(x)))
        }
    )
    noflevels[is.na(noflevels)] <- ulevels[is.na(noflevels)]
    factor <- unlist(lapply(data, is.factor))
    declared <- unlist(lapply(data, function(x) inherits(x, "declared")))
    noflevels[pN][
        apply(
            data[, pN, drop = FALSE],
            2,
            function(x) any(as.numeric(x) %% 1 > 0)
        )
    ] <- 2
    if (any(factor | declared)) {
        noflevels[factor | declared] <- pmin(noflevels[factor | declared], ulevels[factor | declared])
    }
    noflevels[noflevels == 1] <- 2
    return(noflevels)
}
