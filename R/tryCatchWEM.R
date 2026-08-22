# Copyright (c) 2019 - 2026, Adrian Dusa
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

#' Try functions to capture warnings, errors and messages.
#'
#' This function combines the base functions \bold{\code{tryCatch}()} and
#' \bold{\code{withCallingHandlers}()} for the specific purpose of capturing
#' not only  errors and warnings but messages as well.
#'
#' @name tryCatchWEM
#' @rdname tryCatchWEM
#' @rawRd
#' \usage{
#' tryCatchWEM(expr, capture = FALSE)
#' }
#'
#'
#'
#'
#'
#'
#' \arguments{
#'     \item{expr}{Expression to be evaluated.}
#'     \item{capture}{Logical, capture the visible output.}
#' }
#'
#' \details{
#' In some situations it might be important not only to test a function, but also
#' to capture everything that is written in the R console, be it an error, a warning
#' or simply a message.
#'
#' For instance package \bold{\pkg{QCA}} (version 3.4) has a Graphical User Interface
#' that simulates an R console embedded into a web based \bold{\pkg{shiny}} app.
#'
#' It is not intended to replace function \bold{\code{tryCatch}()} in any
#' way, especially not evaluating an expression before returning or exiting, it simply
#' captures everything that is printed on the console (the visible output).
#' }
#'
#'
#' \value{
#' A list, if anything would be printed on the screen, or an empty (NULL) object
#' otherwise.
#' }
#'
#' \author{
#' Adrian Dusa
#' }
#'
#' \keyword{functions}
NULL
#' @export
`tryCatchWEM` <- function(expr, capture = FALSE) {
    toreturn <- list()

    # Safe muffle handler for R < 4.0.0 compatibility
    safeMuffle <- function(restart_name) {
        if (exists("tryInvokeRestart", envir = baseenv())) {
            tryInvokeRestart(restart_name)
        } else {
            restarts <- computeRestarts()
            if (any(sapply(restarts, function(r) r$name == restart_name))) {
                invokeRestart(restart_name)
            }
        }
    }

    evaluate_code <- function() {
        withVisible(withCallingHandlers(
            tryCatch(expr,
                error = function(e) {
                    toreturn$error <<- e$message
                    NULL
                },
                interrupt = function(i) {
                    toreturn$interrupted <<- TRUE
                    NULL
                }
            ),
            warning = function(w) {
                toreturn$warning <<- c(toreturn$warning, w$message)
                safeMuffle("muffleWarning")
            },
            message = function(m) {
                toreturn$message <<- paste(toreturn$message, m$message, sep = "")
                safeMuffle("muffleMessage")
            }
        ))
    }

    if (capture) {
        captured <- capture.output({
            output <- evaluate_code()
            if (output$visible && !is.null(output$value)) {
                print(output$value)
            }
        })
        if (length(captured) > 0) {
            toreturn$output <- captured
        }
        if (exists("output") && !is.null(output$value)) {
            toreturn$value <- output$value
        }
    } else {
        evaluate_code()
    }

    if (length(toreturn) > 0) {
        return(toreturn)
    }
}
