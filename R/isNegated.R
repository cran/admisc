`isNegated` <- function(x) {
    notna <- !is.na(x)
    negated <- rep(NA, length(x))
    y <- x[notna]
    if (length(y) == 0) {
        return(NA)
    }
    
    if (any(y != tolower(y) & y != toupper(y))) {
        cat("\n")
        stop(simpleError("Conditions should not be specified using both upper and lower case letters.\n\n"))
    }
    
    y <- gsub("[[:space:]]", "", y)
    negy <- hastilde(y)
    
    oneminus <- grepl("1-", y)
    negy[oneminus] <- !negy[oneminus]
    y <- gsub("1-", "", notilde(y))
    
    lowercase <- y == tolower(y) & y != toupper(y)
    negy[lowercase] <- !negy[lowercase]
    negated[notna] <- negy

    return(negated)
}
