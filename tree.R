source("node.R")
source("iter.R")
library(plyr)

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
                                        #print(sapply(tree, function(x) x$id == id))
                                       tree[unlist(lapply(tree, function(x) x$id == id))][[1]]
                                   },
                                   get.children = function(id, ...) {
                                       if (get.tree(id)$leaf) {
                                           print("No children")
                                       } else {
                                           tree[unlist(lapply(tree, function(x) x$parent == id))]
                                       }
                                   },
                                   get.parent = function(id, ...) {
                                       get.tree(get.tree(id)$parent)
                                   },
                                   traverse = function(v, ...) {
                                       id <- 1
                                       while (!get.tree(id)$leaf) {
                                           label.tmp   <- predict(get.tree(id)$net, v, type = "class")
                                           children    <- get.children(id) 
                                           R           <- unlist(lapply(children, function(x) x$is.right == "Right"))
                                           #print("predicted R")
                                           #print(children[R][[1]]$id)

                                           if (unlist(label.tmp) == get.tree(id)$label) {
                                               id <- children[R][[1]]$id
                                           } else {
                                               id <- children[!R][[1]]$id
                                           }
                                       }
                                       get.tree(id)$label
                                   }, 
                                   predict.nt = function(df, ...) {
                                       adply(df, 1, function(x) traverse(x), .expand = FALSE)[,2]
                                   },
                                   initialize = function(...) {
                                       callSuper(...)
                                       .self
                                   }
                                   )
                    )

