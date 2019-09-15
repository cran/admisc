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

`translate` <-
function(expression = "", snames = "", noflevels = NULL, data = NULL, ...) {
    other.args <- list(...)
    enter <- ifelse (is.element("enter", names(other.args)), "",  "\n") 
    if (identical(expression, "")) {
        cat(enter)
        stop(simpleError(paste0("Empty expression.", enter, enter)))
    }
    if (any(grepl("<=>", expression)) |
        any(grepl("=>", expression))  | 
        any(grepl("<=", expression))) {
        cat(enter)
        stop(simpleError(paste0("Incorrect expression.", enter, enter)))
    }
    if (!is.vector(snames)) {
        cat(enter)
        stop(simpleError(paste0("Set names should be a single string or a vector of names.", enter, enter)))
    }
    if (!is.null(data)) {
        if (is.null(colnames(data))) {
            cat(enter)
            stop(simpleError(paste0("Data does not have column names.", enter, enter)))
        }
        else {
            snamesorig <- colnames(data)
            colnames(data) <- toupper(colnames(data))
        }
    }
    if (!requireNamespace("QCA", quietly = TRUE)) {
        cat(enter)
        stop("Package \"QCA\" is needed for this function to work, please install it.", call. = FALSE)
    }
    if (is.null(data) & (identical(snames, "") | is.null(noflevels))) {
        syscalls <- parse(text = paste(unlist(lapply(sys.calls(), deparse)), collapse = "\n"))
        if (length(withdata <- grep("with\\(", syscalls)) > 0) {
            withdata <- withdata[length(withdata)]
            data <- get(unlist(strsplit(gsub("with\\(", "", syscalls[withdata]), split = ","))[1], envir = length(syscalls) - withdata)
        }
    }
    if (identical(snames, "")) {
        snamesorig <- snames
        if (!is.null(data)) {
            snames <- colnames(data)
        }
    }
    else { 
        snames <- toupper(splitstr(snames))
        if (!is.null(data)) {
            if (length(setdiff(snames, colnames(data))) > 0) {
                cat(enter)
                stop(simpleError(paste0("Part(s) of the \"snames\" not in the column names from the data.", enter, enter)))
            }
        }
    }
    if (is.null(noflevels)) {
        if (!is.null(data)) {
            if (identical(snames, "")) {
                noflevels <- QCA::getInfo(data)$noflevels
            }
            else {
                noflevels <- QCA::getInfo(data[, snames, drop = FALSE])$noflevels
            }
        }
    }
    else {
        noflevels <- splitstr(noflevels)
    }
    expression <- gsub("[[:space:]]|[^ -~]+", "", expression)
    if (identical("1-", substring(expression, 1, 2))) {
        explist <- list(expression = gsub("1-", "", expression), snames = snames)
        if (!is.null(noflevels)) explist$noflevels <- noflevels
        expression <- do.call(QCA::negate, explist)
    }
    if (any(grepl(",", gsub(",[0-9]", "", expression)))) {
        expression <- splitstr(expression)
    }
    arglist <- list(snames = snames)
    if (!is.null(noflevels)) {
        arglist$noflevels <- noflevels
    }
    expression <- unlist(lapply(expression, function(x) {
        if (grepl("[(|)]", x)) {
            x <- do.call(expandBrackets, c(list(expression = x), arglist)) 
        }
        return(x)
    }))
    pporig <- trimstr(unlist(strsplit(expression, split="[+]")))
    multivalue <- any(grepl("[{|}]", expression))
    expression <- gsub("[[:space:]]", "", expression)
    beforemessage <- "Condition"
    aftermessage <- "does not match the set names from \"snames\" argument"
    if (is.element("validate", names(other.args))) {
        if (is.null(data)) {
            beforemessage <- "Object"
            aftermessage <- "not found"
        }
        else {
            aftermessage <- "not found in the data"
        }
    }
    if (multivalue) {
        expression <- gsub("[*]", "", expression)
        expression <- toupper(expression)
        checkMV(expression, snames = snames, noflevels = noflevels, data = data)
        pp <- unlist(strsplit(expression, split="[+]"))
        conds <- sort(unique(toupper(notilde(curlyBrackets(pp, outside=TRUE)))))
        if (identical(snames, "")) {
            if (!is.null(data)) {
                    conds <- intersect(colnames(data), conds)
            }
        }
        else {
            if (all(is.element(toupper(conds), snames))) {
                conds <- snames
            }
            else {
                conds <- setdiff(toupper(conds), snames)
                if (length(conds) > 1) {
                    beforemessage <- paste(beforemessage, "s", sep = "")
                    aftermessage <- gsub("does", "do", aftermessage)
                }
                cat(enter)
                stop(simpleError(sprintf("%s '%s' %s.\n\n", beforemessage, paste(conds, collapse = ","), aftermessage)))
            }
        }
        if (any(hastilde(expression))) {
            if (is.null(noflevels)) {
                noflevels <- QCA::getLevels(data[, conds, drop = FALSE])
            }
        }
        retlist <- lapply(pp, function(x) {
            outx <- toupper(curlyBrackets(x, outside=TRUE))
            inx <- lapply(curlyBrackets(x), splitstr)
            remtilde <- notilde(outx)
            tbl <- table(remtilde)
            dupnot <- duplicated(remtilde)
            if (length(win <- which(hastilde(outx))) > 0) {
                for (i in win) {
                    inx[[i]] <- setdiff(seq(noflevels[which(conds %in% remtilde[i])]) - 1, inx[[i]])
                }
            }
            empty <- FALSE
            for (i in seq(length(conds))) {
                if (is.element(conds[i], remtilde[dupnot])) { 
                    wdup <- which(remtilde == conds[i])
                    inx[[wdup[1]]] <- intersect(inx[[wdup[1]]], inx[[wdup[2]]])
                    if (length(wdup) > 2) {
                        for (i in seq(3, length(wdup))) {
                            dupres <- intersect(dupres, inx[[wdup[i]]])
                        }
                    }
                    if (length(inx[[wdup[1]]]) == 0) {
                        empty <- TRUE
                    }
                }
            }
            ret <- as.list(rep(-1, length(conds)))
            names(ret) <- conds
            ret[notilde(outx[!dupnot])] <- inx[!dupnot]
            return(ret)
        })
        names(retlist) <- pporig
        retlist <- retlist[!unlist(lapply(retlist, function(x) any(unlist(lapply(x, length)) == 0)))]
        if (length(retlist) == 0) {
            cat(enter)
            stop(simpleError(paste0("The result is an empty set.", enter, enter)))
        }
    }
    else {
        pp <- unlist(strsplit(expression, split = "[+]"))
        if (any(grepl("[*]", expression))) {
            conds <- unique(notilde(unlist(strsplit(pp, split="[*]"))))
            if (!all(is.element(conds, c(tolower(conds), toupper(conds))))) {
                cat(enter)
                stop(simpleError(paste0("Conditions' names cannot contain both lower and upper case letters.", enter, enter)))
            }
            conds <- sort(unique(toupper(conds)))
            if (!identical(snames, "")) {
                if (!is.null(data)) {
                    if (all(is.element(conds, snames)) & all(is.element(conds, toupper(colnames(data))))) {
                        infodata <- QCA::getInfo(data[, conds, drop = FALSE])
                        valid <- which(infodata$noflevels >= 2)
                        invalid <- !any(infodata$hastime[valid]) & any(infodata$noflevels[valid] > 2)
                        if (invalid) {
                            cat(enter)
                            stop(simpleError(paste0("Expression should be multi-value, since it refers to multi-value data.", enter, enter)))
                        }
                    }
                }
                if (all(is.element(toupper(conds), snames))) {
                    conds <- snames
                }
                else {
                    conds <- setdiff(toupper(conds), snames)
                    if (length(conds) > 1) {
                        beforemessage <- paste(beforemessage, "s", sep = "")
                        aftermessage <- gsub("does", "do", aftermessage)
                    }
                    cat(enter)
                    stop(simpleError(sprintf("%s '%s' %s.\n\n", beforemessage, paste(conds, collapse = ","), aftermessage)))
                }
            }
            retlist <- lapply(pp, function(x) {
                x <- unlist(strsplit(x, split="[*]"))
                inx <- lapply(x, function(x) as.numeric(identical(x, toupper(x))))
                remtilde <- toupper(notilde(x))
                tbl <- table(remtilde)
                dupnot <- duplicated(remtilde)
                if (length(win <- which(hastilde(x))) > 0) {
                    for (i in win) {
                        inx[[i]] <- 1 - inx[[i]]
                    }
                }
                empty <- FALSE
                for (i in seq(length(conds))) {
                    if (is.element(conds[i], remtilde[dupnot])) { 
                        if (length(unique(unlist(inx[which(remtilde == conds[i])]))) > 1) {
                            empty <- TRUE
                        }
                    }
                }
                ret <- as.list(rep(-1, length(conds)))
                names(ret) <- conds
                if (!empty) {
                    ret[toupper(notilde(x[!dupnot]))] <- inx[!dupnot]
                }
                return(ret)
            })
            names(retlist) <- pporig
        }
        else {
            conds <- sort(unique(toupper(notilde(pp))))
            if (!is.null(data)) {
                if (all(is.element(conds, snames)) & all(is.element(conds, toupper(colnames(data))))) {
                    condlevels <- QCA::getLevels(data[, conds, drop = FALSE])
                    if (!any(is.na(condlevels))) {
                        if (any(condlevels > 2)) {
                            cat(enter)
                            stop(simpleError(paste0("Expression should be multi-value, since it refers to multi-value data.", enter, enter)))
                        }
                    }
                }
            }
            if (all(nchar(conds) == 1)) {
                if (!identical(snames, "")) {
                    if (all(is.element(toupper(conds), snames))) {
                        conds <- snames
                    }
                    else {
                        conds <- setdiff(toupper(conds), snames)
                        if (length(conds) > 1) {
                            beforemessage <- paste(beforemessage, "s", sep = "")
                            aftermessage <- gsub("does", "do", aftermessage)
                        }
                        cat(enter)
                        stop(simpleError(sprintf("%s '%s' %s.\n\n", beforemessage, paste(conds, collapse = ","), aftermessage)))
                    }
                }
                retlist <- lapply(pp, function(x) {
                    inx <- as.numeric(identical(x, toupper(x)))
                    if (hastilde(x)) {
                        inx <- 0
                    }
                    ret <- as.list(rep(-1, length(conds)))
                    names(ret) <- conds
                    ret[[toupper(notilde(x))]] <- inx
                    return(ret)
                })
                names(retlist) <- pporig
            }
            else {
                if (identical(snames, "")) {
                    snames <- sort(unique(toupper(unlist(strsplit(notilde(pp), split = "")))))
                }
                    conds <- snames
                if (all(is.element(toupper(notilde(pp)), snames))) {
                    if (!all(is.element(notilde(pporig), c(tolower(conds), toupper(conds))))) {
                        cat(enter)
                        stop(simpleError(paste0("Conditions' names cannot contain both lower and upper case letters.", enter, enter)))
                    }
                    retlist <- lapply(pp, function(x) {
                        inx <- as.numeric(identical(x, toupper(x)))
                        if (hastilde(x)) {
                            inx <- 1 - inx
                        }
                        ret <- as.list(rep(-1, length(conds)))
                        names(ret) <- conds
                        ret[[toupper(notilde(x))]] <- inx
                        return(ret)
                    })
                    names(retlist) <- pporig
                }
                else {
                    if (all(nchar(snames) == 1)) {
                        retlist <- lapply(pp, function(x) {
                            x <- unlist(strsplit(x, split = ""))
                            if (!all(is.element(tocheck <- toupper(x[!hastilde(x)]), toupper(conds)))) {
                                for (i in seq(length(tocheck))) {
                                    if (!is.element(tocheck[i], conds)) {
                                        cat(enter)
                                        stop(simpleError(sprintf("%s '%s' %s.\n\n", beforemessage, tocheck[i], aftermessage)))
                                    }
                                }
                            }
                            if (any(hastilde(x))) {
                                y <- which(hastilde(x))
                                if (max(y) == length(x)) {
                                    cat(enter)
                                    stop(simpleError(paste0("Incorrect expression, tilde not in place.", enter, enter)))
                                }
                                x[y + 1] <- paste("~", x[y + 1], sep="")
                                x <- x[-y]
                            }
                            inx <- lapply(x, function(x) as.numeric(identical(x, toupper(x))))
                            remtilde <- toupper(notilde(x))
                            tbl <- table(remtilde)
                            dupnot <- duplicated(remtilde)
                            if (length(win <- which(hastilde(x))) > 0) {
                                for (i in win) {
                                    inx[[i]] <- 1 - inx[[i]]
                                }
                            }
                            empty <- FALSE
                            for (i in seq(length(conds))) {
                                if (is.element(conds[i], remtilde[dupnot])) { 
                                    if (length(unique(unlist(inx[which(remtilde == conds[i])]))) > 1) {
                                        empty <- TRUE
                                    }
                                }
                            }
                            ret <- as.list(rep(-1, length(conds)))
                            names(ret) <- conds
                            if (!empty) {
                                ret[toupper(notilde(x[!dupnot]))] <- inx[!dupnot]
                            }
                            return(ret)
                        })
                        names(retlist) <- pporig
                    }
                    else {
                        perms <- function(x) {
                            if (length(x) == 1) {
                                return(x)
                            }
                            else {
                                res <- matrix(nrow = 0, ncol = length(x))
                                for(i in seq_along(x)) {
                                    res <- rbind(res, cbind(x[i], Recall(x[-i])))
                                }
                                return(res)
                            }
                        }
                        snames <- snames[unlist(lapply(snames, grepl, toupper(expression)))]
                        if (length(snames) == 0) {
                            cat(enter)
                            stop(simpleError(sprintf("Could not determine what '%s' is.\n\n", expression)))
                        }
                        if (length(snames) > 7) {
                            cat(enter)
                            stop(simpleError(paste0("Too many objects to search, try using the '*' sign to specify conjuctions.", enter, enter)))
                        }
                        im <- QCA::createMatrix(rep(3, length(snames)))[-1, , drop = FALSE]
                        mns <- matrix(nrow = 0, ncol = ncol(im))
                        noflevels <- rep(3, length(snames))
                        mns <- lapply(seq(2, 3^length(snames)), function(sn) {
                            sn <- QCA::getRow(sn, noflevels)
                            snames[sn == 1] <- tolower(snames[sn == 1])
                            snames <- snames[sn > 0]
                            if (length(snames) > 1) {
                                return(perms(snames))
                            }
                            else {
                                return(matrix(snames, 1, 1))
                            }
                        })
                        namespace <- unlist(lapply(mns, function(x) apply(x, 1, paste, collapse = "")))
                        if (any(duplicated(namespace))) {
                            cat(enter)
                            stop(simpleError(paste0("Impossible to translate: set names clash.", enter, enter)))
                        }
                        names(namespace) <- unlist(lapply(seq(length(mns)), function(x) paste(x, seq(nrow(mns[[x]])), sep = "_")))
                        matched <- match(notilde(pp), namespace)
                        if (any(is.na(matched))) {
                            cat(enter)
                            stop(simpleError(paste0("Incorrect expression, unknown set names (try using * for products).", enter, enter)))
                        }
                        matched <- names(namespace)[matched]
                        retlist <- lapply(seq(length(matched)), function(x) {    
                            ret <- as.list(rep(-1, length(conds)))
                            names(ret) <- conds
                            mx <- as.numeric(unlist(strsplit(matched[x], split="_")))
                            mx <- mns[[mx[1]]][mx[2], ]
                            inx <- lapply(mx, function(y) {
                                neg <- grepl(paste0("~", y), pp[x])
                                y <- as.numeric(identical(y, toupper(y)))
                                return(ifelse(neg, 1 - y, y))
                            })
                            ret[toupper(mx)] <- inx
                            return(ret)
                        })
                        names(retlist) <- pporig
                    } 
                } 
            } 
        } 
    } 
    retlist <- retlist[!unlist(lapply(retlist, function(x) all(unlist(x) < 0)))]
    retmat <- do.call(rbind, lapply(retlist, function(x) {
        xnames <- names(x)
        x <- unlist(lapply(x, paste, collapse = ","))
        names(x) <- xnames
        return(x)
    }))
    if (length(retmat) == 0) {
        cat(enter)
        stop(simpleError(paste0("Impossible to translate an empty set.", enter, enter)))
    }
    if (is.element("retlist", names(other.args))) {
        attr(retmat, "retlist") <- retlist
    }
    class(retmat) <- c("matrix", "translate")
    return(retmat)
}
