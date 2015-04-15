source("node2.R")
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
                                           tmp <- queue$pop()
                                           s.tmp <- tmp[[1]]
                                           id.tmp  <- tmp[[2]]
                                           p.id.tmp  <- tmp[[3]]

                                           n <- Node(id = id.tmp, parent = p.id.tmp, s = s.tmp$s)

                                           if (n$init.split(n$s)) {
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
                                   initialize = function(...) {
                                       callSuper(...)
                                       #
                                       # Initialise fields here (place holder)...
                                       #
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

sample <- function(){
    tmp.df <- iris
    colnames(tmp.df)[5] <- "l"

    q <- Queue(items = list(list(list(s = tmp.df), 1, 0)))
    t <- Tree(tree = list(), queue= q)

    t$grow()
    t
}




