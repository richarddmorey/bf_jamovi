
knitContent <- function(content){
	class(content) <- append(class(content),"knitContent")
    return(content)
}

knit_print.knitContent <- function(x, ...) {
    res <- paste(x, collapse = "\n")
	depBFX <- htmltools::htmlDependency(
    	name = "bfx",
    	version = "0.1",
    	src = system.file(package = "BFX"),
    	stylesheet = c("css/bfx.css", "css/rmd.css"),
        script = c("js/MathJax.js?config=TeX-AMS-MML_HTMLorMML",
            "js/bfx.js")
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

# This is a temporary hack
get_rmd <- function(filename, envir = parent.frame()){

    fn <- system.file("rmd", filename, package = "BFX")
    outdir <- tempdir()
    outfn <- tempfile(tmpdir = outdir)

    env <- knitr::knit_global()
    env$CairoPNG = Cairo::CairoPNG

    Sys.setenv("RSTUDIO_PANDOC" = "/Applications/jamovi.app/Contents/MacOS/pandoc")
                       
    rmarkdown::render(fn, output_format = "html_fragment",
        output_dir = outdir, output_file = outfn, intermediates_dir = outdir,
        run_pandoc = TRUE, envir = envir)

    tmp <- c(readLines(outfn),
        "<script>
        MathJax.Hub.Queue(['Typeset',MathJax.Hub]);
        </script>")

    knitContent(paste(tmp, sep="\n"))
}


