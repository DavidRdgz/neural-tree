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
            h = 0,
            h.i = c()
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
	lvls <- levels(s$l)

    rez <- list()
    for (i in unique(s$l)) {
		type = s$l == lvls[i]
		nnet.fit <- nnet(type ~ ., data = s[,!names(s) %in% c("l")], size = 1)
		nnet.pred <- predict(nnet.fit, s[,!names(s) %in% c("l") ])

        rez[[i]] <- list(nnet.fit, nnet.pred, type)
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
    nnet.fit <- sapply(L,"[[","nnet.fit")
    nnet.pred<- sapply(L,"[[","nnet.pred")

    score <- sapply(nnet.pred, entropy.empirical)
    nnet.fit[order(score)]
}
