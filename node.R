library(entropy) 
library(nnet) 

NODE <- function(...)
{
        data <- list(
            id = 0,
            parent = 0,
            s.l = 0,
            s.r = 0,
            s = c(),
            h = list(),
            h.i = list()
       )

        ## Set the name for the class
        class(data) <- append(class(data),"NODE")
        return(data)
}

set.node <- function(...) UseMethod("set.node")
set.node.NODE <- function(...){
    args <- list(...)
    object <- args[["object"]] 

    within(object,{
        for (i in names(args)) {
            assign(i, args[[i]], inherits = TRUE)
        }
        return(object)
    })
}

init.split.node <- function(...) UseMethod("init.split.node")
init.split.node.NODE <- function(...){
    args <- list(...)
    object <- args[["object"]]

    object$h.i <- unique(object$s[,"l"])
    return(object)
}

split.node <- function(...) UseMethod("split.node")
split.node.NODE <- function(...){
    args <- list(...)
    object <- args[["object"]]

    s <- object$s 

    rez <- list()
    for (i in levels(s$l)) {
		type <- s$l == i
		nnet.fit <- nnet(type ~ ., data = s[,!names(s) %in% c("l")], size = 1)
		nnet.pred <- predict(nnet.fit, s[,!names(s) %in% c("l") ], type = "raw")
        rez[[i]] <- list("net.fit" = nnet.fit, 
                         "net.pred" = nnet.pred, 
                         "class" = type)
    }
    object$h.i <- rez
    return(object)
}

eval.splits <- function(...) UseMethod("eval.splits")
eval.splits.NODE <- function(...) { 
    args <- list(...)
    object <- args[["object"]]
  
    L <- object$h.i
    labels <- names(L)
    nnet.fit <- lapply(L,"[[","net.fit")
    nnet.pred<- lapply(L,"[[","net.pred")
    nnet.pred<- lapply(nnet.pred, round)

    score <- sapply(nnet.pred, entropy.empirical)
    object$h <- list("order" = nnet.fit[order(score)],
                     "score" = score)
    return(object)
}

sample <- function(...){
    args <- list(...)

    #tmp.df <- read.csv("nt_tester.csv")
    tmp.df <- iris
    colnames(tmp.df)[5] <- "l"

    n <- NODE()
    n <- set.node(object = n, s = tmp.df)
    n <- split.node(object = n)

    n <- eval.splits(object = n)
    return(n)
}
