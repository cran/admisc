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

#' @keywords internal
#' "_PACKAGE"
#'
#' @name admisc_package
#' @aliases admisc-package
#'
#' @title Adrian Dusa's Miscellaneous
#'
#' @description Contains functions used across packages 'DDIwR', 'QCA' and 'venn'.
#' Interprets and translates, factorizes and negates SOP - Sum of Products
#' expressions, for both binary and multi-value crisp sets, and extracts
#' information (set names, set values) from those expressions. Other functions
#' perform various checks if possibly numeric (even if all numbers reside in a
#' character vector) and coerce to numeric, or check if the numbers are whole. It
#' also offers, among many others, a highly versatile recoding routine and some
#' more flexible alternatives to the base functions `with()` and `within()`.
#' SOP simplification functions in this package use related minimization from
#' package **QCA**, which is recommended to be installed despite not being listed
#' in the Imports field, due to circular dependency issues.
#'
#' @author Adrian Dusa
#'
#' Maintainer: Adrian Dusa (dusa.adrian@unibuc.ro)
#'
#' @details
#' \tabular{ll}{
#'   Package: \tab admisc\cr
#'   Type: \tab Package\cr
#'   Version: \tab 0.40\cr
#'   Date: \tab 2026-03-26\cr
#'   License: \tab GPL (>= 3)\cr
#' }
#'
#' @importFrom utils read.csv write.csv write.table capture.output installed.packages packageDescription compareVersion remove.packages tail
#' @importFrom stats na.omit dist relevel
#' @importFrom methods is
#' @importFrom grDevices hcl
#' @useDynLib admisc, .registration = TRUE
NULL
