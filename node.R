library(nnet)
library(entropy)
library(parallel)
library(caret)

Node <- setRefClass(Class = "Node",
                    fields = list(
                                  id      = "numeric",
                                  parent  = "numeric",
                                  label   = "character",
                                  leaf    = "logical",
                                  is.right= "character",
                                  S       = "data.frame",
                                  net     = "list",
                                  L       = "data.frame",
                                  R       = "data.frame",
                                  delta.purity = "numeric"
                                  ),
                    methods = list(
                                   set = function(...){
                                       args   <- list(...)
                                       Map(assign, names(args), args)
                                   },
                                   is.pure = function(cutoff, ...){
                                       args <- list(...)
                                       tble <- sort(table(S$l), decreasing = TRUE)
                                       e    <- entropy.empirical(tble, unit = "log2")

                                       # Check if 's' is worth splitting
                                       if (e < cutoff) {
                                           label <<- names(tble)[1]
                                           leaf  <<- TRUE
                                       }
                                   },
                                   split = function(...) {
                                       args <- list(...)

                                       lvls      <- levels(S$l)
                                       classj    <- lapply(lvls, function(x) S$l == x)
                                       r.p <- sapply(classj, function(x) sum(x)/length(x))

                                       my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(1,2,3))

                                       # fit neural net and get predictions per class
                                       tune.params <- train(S[, !names(S) %in% c("l")], S$l,
                                                        method = "nnet",
                                                        preProcess = "range", 
                                                        tuneGrid = my.grid,
                                                        tuneLength = 2,
                                                        trace = FALSE,
                                                        maxit = 100) 
                                       
                                       net.tmp <- nnet(l ~ . , data = S[sample(nrow(S)),], size = tune.params$bestTune$size , rang = .1, decay = tune.params$bestTune$decay, maxit = 400, trace = FALSE)

                                       r.pred <- lapply(classj, function(x) predict(net.tmp, S[x,], type = "class"))
                                       l.pred <- lapply(classj, function(x) predict(net.tmp, S[!x,], type = "class"))

                                       #evaluate which label gives best entropy score
                                       e.r <- sapply(r.pred, function(x) entropy.empirical(table(x)))
                                       e.l <- sapply(l.pred, function(x) entropy.empirical(table(x)))
                                       names(e.r) <- lvls

                                       r.p <- sapply(classj, function(x) sum(x)/length(x))
                                       
                                       e <- entropy.empirical(table(S$l))
                                       d.e <- e - (1-r.p)*e.l +  r.p*e.r
                                       names(d.e) <- lvls
                                       
                                       right.candidate <- names(sort(d.e, decreasing = TRUE))[1]

                                       pred <- predict(net.tmp, S, type = "class")
                                       R.tmp <- droplevels(S[pred == right.candidate, ])
                                       L.tmp <- droplevels(S[pred != right.candidate, ])
                                       
                                       label <<- right.candidate
                                       net   <<- list(net.tmp)
                                       R     <<- R.tmp
                                       L     <<- L.tmp
                                       delta.purity <<- d.e
                                   },
                                   initialize = function(...) {
                                       callSuper(...)
                                       .self
                                   }
                                   )
                    )


