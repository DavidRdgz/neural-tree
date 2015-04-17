library(nnet)
library(entropy)
library(parallel)

Node <- setRefClass(Class = "Node",
                    fields = list(
                                  id     = "numeric",
                                  parent = "numeric",
                                  s      = "data.frame",
                                  s.l    = "list",
                                  s.r    = "list",
                                  h      = "list",
                                  h.i    = "list"
                                  ),
                    methods = list(
                                   set = function(...){
                                        args   <- list(...)
                                        Map(assign, names(args), args)
                                   },
                                   init.split = function(...){
                                       args   <- list(...)

                                       e <- entropy.empirical(table(s$l), unit = "log2")

                                       # Check if 's' is worth splitting
                                       if (e < .35) {
                                           return(FALSE)
                                       } else {
                                           return(TRUE)
                                       }
                                   },
                                   init.splits = function(...) {
                                       args <- list(...)

                                       # Ok 's' is worth splitting
                                       lvls      <- levels(s$l)
                                       classj    <- mclapply(lvls, function(x) s$l == x)
                                       nnet.fit  <- mclapply(classj, function(x) nnet(x ~ ., data = s[,!names(s) %in% c("l")], size = 1, trace=FALSE ))
                                       nnet.pred <- mclapply(nnet.fit, function(x) predict(x , s[,!names(s) %in% c("l") ], type = "raw"))
                                       rez       <- Map(list, lvls, classj, nnet.fit, nnet.pred)
                                       h.i <<- rez
                                   },
                                   eval.splits = function(...) { 
                                       args  <- list(...)
                                       L <- h.i 

                                       nnet.fit   <- lapply(h.i, function(x) x[[3]])
                                       nnet.pred  <- lapply(h.i, function(x) x[[4]])
                                       candidates <- lapply(nnet.pred, function(x) round(x) == 1)

                                       h.s.r <- sapply(candidates, function(x) s.entropy(s, x))
                                       h.s.l <- sapply(candidates, function(x) s.entropy(s, !x))
                                       h.s   <- s.entropy(s, rep(TRUE, nrow(s)))
                                       delta.h.s <- h.s + h.s.l - h.s.r

                                       rank <- order(delta.h.s, decreasing = TRUE)
                                       h <<- list("nets"       = nnet.fit[rank],
                                                  "candidates" = candidates[rank],
                                                  "delta.h.s"  = delta.h.s[rank],
                                                  "h.s.r"      = h.s.r[rank],
                                                  "h.s.l"      = h.s.l[rank])
                                   },
                                   split = function(...){
                                       args   <- list(...)

                                       candidates <- h$candidates[[1]]
                                       s.r <<- list("candidates" = candidates,
                                                    "s" = s[candidates, ],
                                                    "h" = h$h.s.r[[1]])
                                       s.l <<- list("candidates" = !candidates,
                                                    "s" = s[!candidates, ],
                                                    "h" = h$h.s.l[[1]])
                                   },
                                   initialize = function(...) {
                                       callSuper(...)
                                       .self
                                   }
                                   )
                    )

s.entropy <- function(df, subset, ...) {
    args <- list(...)
    x  <- subset

    p <- sum(x)
    n <- length(x)
    return(p/n *entropy.empirical(table(df[x, "l"]), unit = "log2"))
}

sample2 <- function(...){
    args <- list(...)

    library(RCurl)
    spambase.file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"

    spambase.url  <- getURL(spambase.file)
    spambase.data <- read.csv(textConnection(spambase.url), header = FALSE)

    colnames(spambase.data)[ncol(spambase.data)] <- "l"
    spambase.data$l <- factor(spambase.data$l)

    n <- Node(id = 1, parent = 0, s = spambase.data)

    if (n$init.split()) {
        n$init.splits()
        n$eval.splits()
        n$split()
    }
    return(n)
}
sample <- function(...){
    args <- list(...)

    tmp.df <- iris
    colnames(tmp.df)[5] <- "l"

    n <- Node(id = 1, parent = 0, s = tmp.df)

    if (n$init.split()) {
        n$init.splits()
        n$eval.splits()
        n$split()
    }
    return(n)
}

