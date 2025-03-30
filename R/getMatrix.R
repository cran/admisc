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

`getMatrix` <- function(noflevels, depth = 0) {
    nofconds <- length(noflevels)
    pwr <- unique(noflevels)
    if (length(pwr) == 1) {
        create <- function(idx) {
            rep.int(c(sapply(seq_len(pwr) - 1, function(x) rep.int(x, pwr^(idx - 1)))),
                    pwr^nofconds/pwr^idx)
        }
        retmat <- sapply(rev(seq_len(nofconds)), create)
    } else {
        mbase <- c(rev(cumprod(rev(noflevels))), 1)[-1]
        orep  <- cumprod(rev(c(rev(noflevels)[-1], 1)))
        retmat <- sapply(seq_len(nofconds), function(x) {
           rep.int(rep.int(seq_len(noflevels[x]) - 1, rep.int(mbase[x], noflevels[x])), orep[x])
        })
    }
    if (is.vector(retmat)) {
        retmat <- matrix(retmat, nrow = 1)
    }
    if (depth > 0) {
        retmat <- retmat[apply(retmat, 1, function(x) sum(x > 0) <= depth ), , drop = FALSE]
    }
    return(retmat)
}
