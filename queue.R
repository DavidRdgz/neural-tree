
Queue <- setRefClass(Class = "Queue",
                     fields = list(
                       name = "character",
                       items = "list"
                     ),
                     methods = list(
                       size = function() {
                         return(length(items))
                       },
                       push = function(item) {
                         items[[size()+1]] <<- item
                       },
                       pop = function() {
                         if (size() == 0) stop("Empty")
                         value <- items[[1]]
                         items[[1]] <<- NULL
                         value
                       },
                       initialize=function(...) {
                         callSuper(...)
                         .self
                       }
                     )
)
