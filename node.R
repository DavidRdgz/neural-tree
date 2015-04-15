library(nnet)
library(entropy)

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
                                   init.split = function(df, ...){
                                       args   <- list(...)
                                       tble <- table(df$l)
                                       e <- entropy.empirical(tble, unit = "log2")

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
                                       rez <- list()
                                       for (i in levels(s$l)) {
                                           classj <- (s$l == i)
                                           nnet.fit  <- nnet(classj ~ ., data = s[,!names(s) %in% c("l")], size = 1, trace=FALSE )
                                           nnet.pred <- predict(nnet.fit, s[,!names(s) %in% c("l") ], type = "raw")
                                           rez[[i]]  <- list("net.fit"  = nnet.fit, 
                                                             "net.pred" = nnet.pred, 
                                                             "classj"   = classj)
                                       }
                                       h.i <<- rez
                                   },
                                   eval.splits = function(...) { 
                                       args  <- list(...)

                                       L          <- h.i
                                       labels     <- names(L)

                                       nnet.fit   <- lapply(L,"[[","net.fit")
                                       nnet.pred  <- lapply(L,"[[","net.pred")
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

sample <- function(...){
    args <- list(...)

    tmp.df <- iris
    colnames(tmp.df)[5] <- "l"

    n <- Node(id = "000", s = tmp.df)

    if (n$init.split()) {
        n$init.splits()
        n$eval.splits()
        n$split()
    }
    return(n)
}

