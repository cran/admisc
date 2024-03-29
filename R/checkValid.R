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

`checkValid` <- function(
    expression = "", snames = "", data = NULL, categories = list()
) {
    if (identical(snames, "")) {
        stopError("The expression cannot be verified without <snames>.")
    }
    allnames <- splitstr(snames)
    if (!is.null(data)) {
        allnames <- colnames(data)
        infodata <- getInfo(data)
        if (any(infodata$factor)) {
            allnames <- c(allnames, names(unlist(infodata$categories)))
        }
    }
    else if (length(categories) > 0) {
        allnames <- c(allnames, names(unlist(categories)))
    }
    expression <- replaceText(
        expression,
        allnames,
        rep("", length(allnames))
    )
    if (any(grepl(":alpha:", expression))) { 
        stopError(
            sprintf(
                "Part(s) of the expression not found in the %s.",
                ifelse(
                    is.null(data),
                    "<snames> argument",
                    "data"
                )
            )
        )
    }
}
