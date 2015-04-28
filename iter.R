
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

