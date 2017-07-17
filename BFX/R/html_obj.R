
knitContent <- function(content){
	class(content) <- append(class(content),"knitContent")
    return(content)
}

knit_print.knitContent = function(x, ...) {
    res <- paste(x, collapse = "\n")
	depBFX <- htmltools::htmlDependency(
    	name = "bfx",
    	version = "0.1",
    	src = system.file(package = "BFX"),
    	stylesheet = c("css/bfx.css", "css/vignette.css"),
        script = c("js/MathJax.js?config=TeX-AMS-MML_HTMLorMML")
  	)
    depRmd <- htmltools::htmlDependency(
        name = "rmd",
        version = "0.1",
        src = system.file(package = "rmarkdown"),
        stylesheet = c("rmarkdown/templates/html_vignette/resources/vignette.css")
    )
	res <- htmltools::attachDependencies(res, list(depBFX, depRmd))
	knit_print(htmltools::browsable(res), ... )
}