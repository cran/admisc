
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
