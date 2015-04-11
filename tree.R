TREE <- function()
{
        data <- list(
            tree= list(),
            root = 0,
            leaves = 0
       )

        ## Set the name for the class
        class(data) <- append(class(data),"TREE")
        return(data)
}

add.node <- function(...) UseMethod("add.node")
add.node.TREE <- function(...){
    args <- list(...)
    object <- args[["object"]]
    id <- args[["id"]]
    object$tree[[args[["id"]]]] <- args[["node"]]
    #object$tree <- c(object$tree, list(id = args[["node"]]))
    return(object) 
}

sample <- function(){
    a <- NODE()
    a <- set.node(object = a, id = "A", l.child = "B", r.child = "C", 
                  content = list(value = 1))
    b <- NODE()
    b <- set.node(object = b, id = "B", content = list(value = -1))
    c <- NODE()
    c <- set.node(object = c, id = "C", content = list(value = 1))

    t <- TREE()
    t <- add.node(object = t, id = a$id, node = a)
    t <- add.node(object = t, id = b$id, node = b)
    t <- add.node(object = t, id = c$id, node = c)
    return(t)
}



