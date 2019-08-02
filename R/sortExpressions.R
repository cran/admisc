`sortExpressions` <- function(mat) {
    for (i in rev(seq(ncol(mat)))) {
        mat <- mat[order(mat[, i], decreasing = TRUE), , drop = FALSE]
        if (length(wx <- which(mat[, i] > 0)) > 0) {
            rest <- if (max(wx) == nrow(mat)) NULL else seq(max(wx) + 1, nrow(mat))
            mat <- mat[c(order(mat[wx, i]), rest), , drop = FALSE]
        }
    }
    return(mat[order(apply(mat, 1, function(x) sum(x > 0))), , drop = FALSE])
}

