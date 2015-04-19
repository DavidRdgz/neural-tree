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
                                       while (queue$size() > 0) {
                                           tmp       <- queue$pop()
                                           s.tmp     <- tmp[[1]]
                                           id.tmp    <- tmp[[2]]
                                           p.id.tmp  <- tmp[[3]]

                                           n <- Node(id = id.tmp, parent = p.id.tmp, s = s.tmp$s, leaf = FALSE)

                                           # Is there eough impurity to try to split
                                           if (!n$is.leaf()) {
                                               # Yes, but now check if single layer neural net
                                               # achieves enough purity
                                               print("Splits")
                                               n$init.splits()
                                               print("Order Splits")
                                               n$eval.splits()
                                               print("Split")
                                               n$split()

                                               tree <<- append(tree, n)

                                               if (nrow(n$s.r$s) > 0 && nrow(n$s.l$s) > 0) {
                                                   queue$push(list(n$s.r, iter$inc(), id.tmp))
                                                   queue$push(list(n$s.l, iter$inc(), id.tmp))
                                               } else {
                                                   # Not enough impurity to split based on neural net
                                                   # set as leaf
                                                   n$set(leaf = TRUE)
                                               }
                                           } else {
                                               # Not enough impurity to try initial split
                                               n$set(leaf = TRUE)
                                               tree <<- append(tree, n)
                                           }
                                       }
                                   },
                                   peek = function(...) {
                                       length(tree)
                                   },
                                   get.labels = function(...) {
                                       lapply(tree, function(x) x[["label"]])
                                   },
                                   get.leaves = function(...) {
                                       lapply(tree, function(x) if(x$leaf == TRUE) x$label)
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

iris.sample <- function(){
    ########
    #
    # Prereqs: e < .35
    #
    #######
    df <- iris
    colnames(df)[5] <- "l"

    q <- Queue(items = list( list(list(s = df), 1,0)))
    t <- Tree(tree = list(), queue= q)
    t$grow()
    t
}

spam.sample <- function(){
    ########
    #
    # Prereqs: e < .5
    #
    #######
    library(RCurl)
    spambase.file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"
    spambase.url  <- getURL(spambase.file)
    spambase.data <- read.csv(textConnection(spambase.url), header = FALSE)
    colnames(spambase.data)[ncol(spambase.data)] <- "l"
    spambase.data$l <- factor(spambase.data$l)

    q <- Queue(items = list(list(list(s = spambase.data),1,0)))
    t <- Tree(tree = list(), queue= q)
    t$grow()
    t
}

letter.sample <- function(){
    ########
    #
    # Prereqs: e < .5 
    #
    #######
    library(RCurl)
    letter.file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data"
    letter.url  <- getURL(letter.file)
    letter.data <- read.csv(textConnection(letter.url), header = FALSE)
    colnames(letter.data)[1] <- "l"
    letter.data$l <- factor(letter.data$l)
    letter.data   <-subset(letter.data, l %in% c("A","B","C","D"))

    q <- Queue(items = list(list(list(s = letter.data),1,0)))
    t <- Tree(tree = list(), queue= q)
    t$grow()
    t
}




