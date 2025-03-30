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

`tryCatchWEM` <- function(expr, capture = FALSE) {
    #' modified version of http://stackoverflow.com/questions/4948361/how-do-i-save-warnings-and-errors-as-output-from-a-function
    toreturn <- list()
    output <- withVisible(withCallingHandlers(
        tryCatch(expr, error = function(e) {
            toreturn$error <<- e$message
            NULL
        }),
        warning = function(w) {
            toreturn$warning <<- c(toreturn$warning, w$message)
            invokeRestart("muffleWarning")
        },
        message = function(m) {
            toreturn$message <<- paste(toreturn$message, m$message, sep = "")
            invokeRestart("muffleMessage")
        }
    ))
    if (capture && output$visible && !is.null(output$value)) {
        toreturn$output <- capture.output(output$value)
        toreturn$value <- output$value
    }
    if (length(toreturn) > 0) {
        return(toreturn)
    }
}
