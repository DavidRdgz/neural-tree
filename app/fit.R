
get.data <- function() {
    iris
}

fit.model <- function(f, formula, data) {
    r.fit <- f(formula, data = data)
}
