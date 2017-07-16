
knitContent <- function(content){
	class(content) <- append(class(content),"knitContent")
    return(content)
}

knit_print.knitContent = function(x, ...) {
    res <- paste(c("knitContent:", x), collapse = "\n")
	dep <- htmltools::htmlDependency(
    	name = "bfx",
    	version = "0.1",
    	src = system.file(package = "BFX"),
    	stylesheet = "css/bfx.css"
  	)
	res <- htmltools::attachDependencies(res, dep)
	knit_print(htmltools::browsable(res), ... )
}