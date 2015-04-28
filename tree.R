source("node.R")

Tree <- setRefClass(Class = "Tree",
                    fields = list(
                                  tree= "list"
                                  ),
                    methods = list(
                                   grow = function(init.node, cut.off, ...) {
                                       args <- list(...)

                                       q <- init.node
                                       q.count <- 1
                                       iter <- iter(cur.val = 1)
                                       parent <- 0
                                       while(length(q) > 0) {
                                           #possible parallel processing with mclapply
                                           split.bucket <- lapply(q, function(x) splits(x[[3]], cut.off, iter$inc(), x[[2]], x[[1]]))
                                           t <- lapply(split.bucket, function(x) x$de.node)       # nodes needing no further splits
                                           q <- lapply(split.bucket, function(x) x$de.child)[[1]] # nodes needing further splitting
                                           print("Queue length")
                                           print(q.count)
                                           q.count <- q.count + length(q)
    
                                           #clean up
                                           t <- t[lapply(t,length)>0]
                                           q <- q[lapply(q,length)>0]
                                           
                                           tree <<- append(tree, t)
                                       }
                                   },
                                   splits = function(df, cut.off, id, p.id, is.right, ...) {
                                       args <- list(...)
                                       print(p.id)
                                       node <- Node(id = id, parent = p.id, S = df, leaf = FALSE, is.right = is.right )

                                       node$is.pure(cut.off)
                                       if (!node$leaf) {
                                           node$split()
                                           if (nrow(node$L) >0 && nrow(node$R) > 0) {
                                                child.tmp <- list(list("Left", id, node$L), list("Right", id, node$R))
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
                                   get.tree = function(id, ...) {
                                       tree[c(sapply(tree, function(x) x$id == id))][[1]]
                                   },
                                   get.children = function(id, ...) {
                                       if (get.tree(id)$leaf) {
                                           print("No children")
                                       } else {
                                           tree[sapply(tree, function(x) x$parent == id)]
                                       }
                                   },
                                   get.parent = function(id, ...) {
                                       get.tree(get.tree(id)$parent)
                                   },
                                   traverse = function(v, ...) {
                                       id <- 1
                                       while (get.tree(id)$leaf) {
                                           label.tmp   <- predict(get.tree(id)$net, v, type = "class")
                                           children    <- get.children(id) 
                                           R           <- sapply(children, function(x) x$is.right == "Right")

                                           if (label.tmp == get.tree(id)$label) {
                                               id <- children[R]$id
                                           } else {
                                               id <- children[!R]$id
                                           }
                                       }
                                       return(get.tree(id)$label)
                                   }, 
                                   predict.nt = function(df, ...) {
                                       apply(df, 1, function(x) traverse(x))
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
                                   peek = function(...) {
                                       cur.val
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
    t$grow(list(list("None",0, df)),c.off)
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
    t$grow(list(list(0, letter.data)),c.off)
    #print("Get Leaves")
    #unlist(t$get.leaves())
    t
}




