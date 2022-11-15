rngKind <- function(jaspResults, dataset, options) {
    result <- sample(1:1000, 1)
    jaspResults[["result"]] <- createJaspTable(title = "Result", data = list(x = result))
}