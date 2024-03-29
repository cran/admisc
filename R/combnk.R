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

`combnk` <- function(n, k, ogte = 0, zerobased = FALSE) {
    if (!is.numeric(k)) {
        stopError("Argument k should be numeric.")
    }
    if (length(k) != 1L) {
        stopError("Argument k should be a scalar of length 1.")
    }
    if (k < 0) {
        stopError("Argument k should be positive.")
    }
    len <- length(n)
    lngt1 <- len > 1
    if (lngt1) {
        if (len < k) {
            stopError("Argument k cannot be greater than the length of n.")
        }
    }
    else {
        if (!is.numeric(n)) {
            stopError("When scalar, argument n should be numeric.")
        }
        if (n < k) {
            stopError("Argument n should be greater than or equal to k.")
        }
    }
    copyn <- n
    if (lngt1) {
        n <- len
    }
    if (requireNamespace("QCA", quietly = TRUE)) {
        resmat <- QCA::combint(n = n, k = k, ogte = ogte, zerobased = zerobased)
    }
    else {
        e <- 0L
        ncols <- choose(n, k)
        h <- k - ncols == 1
        out <- vector(mode = "list", length = ncols)
        comb <- seq.int(k) - zerobased 
        comb[k] <- comb[k] - 1L
        last <- n == k
        i <- 1
        while (comb[1] != n - k + 1 || last) {
            last <- FALSE
            if (e < n - h) {
                h <- 1L
                e <- comb[k] + zerobased 
                comb[k] <- comb[k] + 1L
                if (comb[k] < ogte) {
                    comb[k] <- ogte
                    e <- ogte - 1
                }
            }
            else {
                e <- comb[k - h] + zerobased 
                h <- h + 1L
                under <- logical(h)
                for (j in seq(h)) {
                    under[j] <- (e + j - zerobased < ogte) 
                    comb[k - h + j] <- e + j - zerobased  
                }
                if (all(under)) {
                    comb[k] <- ogte
                    e <- ogte - 1
                    h <- 1L
                }
            }
            out[[i]] <- comb
            i <- i + 1
        }
        resmat <- do.call("cbind", out[!unlist(lapply(out, is.null))])
    }
    if (lngt1) {
        resmat <- matrix(copyn[resmat], nrow = nrow(resmat))
    }
    return(resmat)
}
