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
                                  purity.candidates = "list",
                                  net.candidates    = "list"
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
                                       net.candidates <<- rez
                                   },
                                   eval.splits = function(...) { 
                                       args  <- list(...)

                                       nnet.fit   <- lapply(net.candidates, function(x) x[[3]])
                                       nnet.pred  <- lapply(net.candidates, function(x) x[[4]])
                                       candidates <- lapply(nnet.pred, function(x) round(x) == 1)

                                       h.s.r <- sapply(candidates, function(x) s.entropy(s, x))
                                       h.s.l <- sapply(candidates, function(x) s.entropy(s, !x))
                                       h.s   <- s.entropy(s, rep(TRUE, nrow(s)))
                                       delta.h.s <- h.s + h.s.l - h.s.r

                                       rank <- order(delta.h.s, decreasing = TRUE)
                                       rez  <- Map(list, nnet.fit, candidates, delta.h.s, h.s.r, h.s.l)

                                       purity.candidates <<- lapply(rank, function(x) rez[[x]])
                                   },
                                   split = function(...){
                                       args   <- list(...)

                                       candidates <- purity.candidates[[1]][[2]]
                                       r.purity     <- purity.candidates[[1]][[4]]
                                       l.purity     <- purity.candidates[[1]][[5]]
                                       s.r <<- list("candidates" = candidates,
                                                    "s" = s[candidates, ],
                                                    "h" = r.purity)
                                       s.l <<- list("candidates" = !candidates,
                                                    "s" = s[!candidates, ],
                                                    "h" = l.purity)
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
        #n$split()
    }
    return(n)
}

