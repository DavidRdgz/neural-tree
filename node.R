library(nnet)
library(entropy)
library(parallel)

Node <- setRefClass(Class = "Node",
                    fields = list(
                                  id      = "numeric",
                                  parent  = "numeric",
                                  label   = "character",
                                  s       = "data.frame",
                                  #net     = "nnet",
                                  s.l     = "list",
                                  s.r     = "list",
                                  purity.candidates = "list",
                                  net.candidates    = "list"
                                  ),
                    methods = list(
                                   set = function(...){
                                       args   <- list(...)
                                       Map(assign, names(args), args)
                                   },
                                   is.leaf = function(...){
                                       args   <- list(...)
                                       tble <- sort(table(s$l), decreasing = TRUE)
                                       e <- entropy.empirical(tble, unit = "log2")
                                       print(e)

                                       # Check if 's' is worth splitting
                                       if (e < .35) {
                                           label   <<- names(tble)[1]
                                           #is.leaf <<- TRUE
                                           return(TRUE)
                                       } else {
                                           return(FALSE)
                                       }
                                   },
                                   init.splits = function(...) {
                                       args <- list(...)


                                       # Ok 's' is worth splitting
                                       lvls      <- levels(s$l)
                                       classj    <- lapply(lvls, function(x) s$l == x)

                                            #find cores / open processes
                                            #no_cores <- detectCores() - 1
                                            #cl <- makeCluster(no_cores, type="FORK")
                                            #nnet.fit  <- parLapply(cl, classj, function(x) nnet(x ~ ., data = s[,!names(s) %in% c("l")], size = 1, trace=FALSE))
                                            #stopCluster(cl)

                                       nnet.fit  <- lapply(classj, function(x) nnet(x ~ ., data = s[,!names(s) %in% c("l")], size = 1, trace=FALSE ))
                                       nnet.pred <- lapply(nnet.fit, function(x) predict(x , s[,!names(s) %in% c("l") ], type = "raw"))
                                       rez       <- Map(list, lvls, classj, nnet.fit, nnet.pred)
                                       net.candidates <<- rez
                                   },
                                   eval.splits = function(...) { 
                                       args  <- list(...)

                                       lvls       <- lapply(net.candidates, function(x) x[[1]])
                                       nnet.fit   <- lapply(net.candidates, function(x) x[[3]])
                                       nnet.pred  <- lapply(net.candidates, function(x) x[[4]])
                                       candidates <- lapply(nnet.pred, function(x) round(x) == 1)

                                       h.s.r <- sapply(candidates, function(x) s.entropy(s, x))
                                       h.s.l <- sapply(candidates, function(x) s.entropy(s, !x))
                                       h.s   <- s.entropy(s, rep(TRUE, nrow(s)))
                                       delta.h.s <- h.s + h.s.l - h.s.r

                                       print("candidate size: ")
                                       print(lapply(candidates, length))
                                       print("delta hs")
                                       print(delta.h.s)
                                       rank <- order(delta.h.s, decreasing = TRUE)
                                       rez  <- Map(list, lvls, nnet.fit, candidates, delta.h.s, h.s.r, h.s.l)

                                       purity.candidates <<- lapply(rank, function(x) rez[[x]])
                                   },
                                   split = function(...){
                                       args   <- list(...)

                                       candidates  <- purity.candidates[[1]][[3]]
                                       r.purity    <- purity.candidates[[1]][[5]]
                                       l.purity    <- purity.candidates[[1]][[6]]
                                       label      <<- purity.candidates[[1]][[1]]

                                       s.r        <<- list("candidates" = candidates,
                                                           "s" = s[candidates, ],
                                                           "h" = r.purity)
                                       s.l        <<- list("candidates" = !candidates,
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
    print("here")


    p <- sum(x)
    n <- length(x)
    return(p/n *entropy.empirical(table(df[x, "l"]), unit = "log2"))
}

iris.sample <- function(...){
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

spam.sample <- function(...){
    args <- list(...)

    library(RCurl)
    spambase.file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"

    spambase.url  <- getURL(spambase.file)
    spambase.data <- read.csv(textConnection(spambase.url), header = FALSE)

    colnames(spambase.data)[ncol(spambase.data)] <- "l"
    spambase.data$l <- factor(spambase.data$l)

    n <- Node(id = 1, parent = 0, s = spambase.data)

    if (n$is.leaf()) {
        n$init.splits()
        n$eval.splits()
        n$split()
    }
    return(n)
}

