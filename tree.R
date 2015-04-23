source("node2.R")
source("queue.R")

Tree <- setRefClass(Class = "Tree",
                    fields = list(
                                  tree= "list"
                                  ),
                    methods = list(
                                   grow = function(init.node, cut.off, ...) {
                                       args <- list(...)

                                       q <- init.node
                                       q.count <- 1
                                       while(length(q) > 0) {
                                           #possible parallel processing with mclapply
                                           split.bucket <- lapply(q, function(x) splits(x, cut.off))
                                           t <- lapply(split.bucket, function(x) x$de.node)       # nodes needing no further splits
                                           q <- lapply(split.bucket, function(x) x$de.child)[[1]] # nodes needing further splitting
                                           print("Queue length")
                                           #print(length(q))
                                           print(q.count)
                                           q.count <- q.count + length(q)
    
                                           #clean up
                                           t <- t[lapply(t,length)>0]
                                           q <- q[lapply(q,length)>0]
                                           
                                           tree <<- append(tree, t)
                                       }
                                   },
                                   splits = function(df, cut.off, ...) {
                                       args <- list(...)
                                       node <- Node(id = 1, parent = 0, S = df, leaf = FALSE)

                                       node$is.pure(cut.off)
                                       if (!node$leaf) {
                                           node$split()
                                           if (nrow(node$L) >0 && nrow(node$R) > 0) {
                                                child.tmp <- list(node$L, node$R)
                                           } else {
                                               child.tmp <- list()
                                               node$set(leaf = TRUE)
                                           }
                                           spine.tmp <- node
                                       } else {
                                           child.tmp <- list()
                                           spine.tmp <- node
                                       }
                                       list("de.node" = spine.tmp, "de.child" = child.tmp)
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

iris.sample <- function(c.off, ...){
    ########
    #
    # Prereqs: e < .35
    #
    #######
    df <- iris
    colnames(df)[5] <- "l"
    df$l <- factor(df$l)
    t <- Tree()
    t$grow(list(df),c.off)
    #print(unlist(t$get.leaves()), t$peek())
    t
}

car.sample <- function(c.off, ...){
    ########
    #
    # Prereqs: e < .35
    #
    #######
    df <- mtcars
    colnames(df)[10] <- "l"
    df$l <- factor(df$l)
    t <- Tree()
    t$grow(list(df),c.off)
    t
}
spam.sample <- function(c.off, ...){
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
    
    t <- Tree()
    t$grow(list(spammbase.data),c.off)
    t
}

letter.sample <- function(c.off, ...){
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
    letter.data   <-droplevels(subset(letter.data, l %in% c("A","B","C","D") ))

    t <- Tree()
    t$grow(list(letter.data),c.off)
    #print("Get Leaves")
    #unlist(t$get.leaves())
    t
}




