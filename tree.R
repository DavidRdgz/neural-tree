source("node.R")
source("queue.R")

Tree <- setRefClass(Class = "Tree",
                    fields = list(
                                  tree= "list",
                                  queue = "Queue" 
                                  ),
                    methods = list(
                                   grow = function(...){
                                       args <- list(...)

                                       iter <- iter(cur.val = 1)
                                       while (length(queue$items) > 0 ){
                                           tmp       <- queue$pop()
                                           s.tmp     <- tmp[[1]]
                                           id.tmp    <- tmp[[2]]
                                           p.id.tmp  <- tmp[[3]]

                                           n <- Node(id = id.tmp, parent = p.id.tmp, s = s.tmp$s)

                                           if (n$init.split()) {
                                               n$init.splits()
                                               n$eval.splits()
                                               n$split()

                                               tree <<- append(tree, n)

                                               if (nrow(n$s.r$s) > 0) {
                                                   queue$push(list(n$s.r, iter$inc(), id.tmp))
                                               }
                                               if (nrow(n$s.l$s) > 0) {
                                                   queue$push(list(n$s.l, iter$inc(), id.tmp))
                                               }
                                           } else {
                                               tree <<- append(tree, n)
                                           }
                                       }

                                   },
                                   peek = function(...) {
                                       length(tree)
                                   },
                                   spine.labels = function(...) {
                                       tmp <- lapply(tree, function(x) names(x$h$delta.h.s[1]))
                                   },
                                   leaf.labels = function(...) {
                                       tmp <- lapply(tree, function(x) names(x$h$delta.h.s[1]))
                                       lapply(tree[tmp], function(x) table(x$s$l))
                                   },
                                   initialize = function(...) {
                                       callSuper(...)
                                       .self
                                   }
                                   )
                    )

iter <- setRefClass(Class = "iter",
                    fields = list(
                                  cur.val = "numeric"
                                  ),
                    methods = list(
                                   inc = function(...){
                                       previous <- cur.val
                                       cur.val <<- previous + 1
                                       return(previous)
                                   },
                                   initialize = function(...) {
                                       callSuper(...)
                                       .self
                                   }
                                   )
                    )

sample2 <- function(){
    df <- iris
    colnames(df)[5] <- "l"

    q <- Queue(items = list(
                            list(
                                 list(s = df), 1,0)
                            )
    )

    t <- Tree(tree = list(), queue= q)

    t$grow()
    t
}

sample <- function(){
    library(RCurl)
    #spambase.file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"
    spambase.file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data"
    spambase.url  <- getURL(spambase.file)
    spambase.data <- read.csv(textConnection(spambase.url), header = FALSE)
    spambase.data <- spambase.data[1:1000,]
    #colnames(spambase.data)[ncol(spambase.data)] <- "l"
    colnames(spambase.data)[1] <- "l"
    spambase.data$l <- factor(spambase.data$l)

    q <- Queue(items = list(
                            list(
                                 list(s = spambase.data),1,0)
                            )
    )

    t <- Tree(tree = list(), queue= q)

    t$grow()
    t
}




