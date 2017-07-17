
# This file is a generated template, your changes will not be overwritten

regressionClass <- R6::R6Class(
    "regressionClass",
    inherit = regressionBase,
    private = list(
        .init = function() {

            dep <- self$options$dep
            pred <- self$options$predictors
            data <- self$data
            table <- self$results$models
            state <- self$results$state
            param <- self$results$param
            html <- self$results$htmlTest

            
            #fn <- system.file("rmd", "test.Rmd", package = self$package)
            #outdir <- tempdir()
            #outfn <- tempfile(tmpdir = outdir)

            #env <- knitr::knit_global()
            #env$CairoPNG = Cairo::CairoPNG

            #Sys.setenv("RSTUDIO_PANDOC" = "/Applications/jamovi.app/Contents/MacOS/pandoc")
                       
            #rmarkdown::render(fn, output_format = "html_fragment",
            #    output_dir = outdir, output_file = outfn, intermediates_dir = outdir,
            #    run_pandoc = TRUE)

            #tmp <- c(readLines(outfn),
            #    "<script>
            #        MathJax.Hub.Queue(['Typeset',MathJax.Hub]);
            #    </script>")

            #tmp <- knitContent(paste(tmp, sep="\n"))


            #html$content <- tmp


            table$addRow(rowKey=character(), values=list(model='Intercept only'))

            if (length(pred) == 0)
                return()

            npred <- length(pred)

            models <- lapply(seq_len(npred), function(e){ 
                cmb <- combn(pred, e)
                pst <- apply(cmb, 2, paste, collapse = " + ")
                paste(dep, pst, sep = " ~ ")
            })

            models <- unlist(models)
            models <- lapply(models, formula)

            for (model in models) {
                terms <- attr(stats::terms(model), 'term.labels')
                terms <- jmvcore::decomposeTerms(terms)
                niceTerms <- sapply(terms, function(x) jmvcore::stringifyTerm(x))
                model <- paste(niceTerms, collapse=' + ')
                table$addRow(rowKey=terms, values=list(model=model))
            }

            selected <- table$rowSelected
            if(selected > table$rowCount) selected <- 0

            if(selected == 1) {
                paramTable <- private$.posterior(data, dep, c())
                param$addRow(rowKey=paramTable[1,1], values=as.list(paramTable[1,]))
            }else if(selected > 1) {
                model <- models[[ selected - 1 ]]
                terms <- attr(stats::terms(model), 'term.labels')
                terms <- jmvcore::decomposeTerms(terms)
                paramTable <- private$.posterior(data, dep, terms)
                nParams <- nrow(paramTable)
                for ( i in seq_len(nParams) ) {
                    param$addRow(rowKey=paramTable[i,1], values=as.list(paramTable[i,]))
                }
            }

            table$setSortKeys(col=1, seq_len(table$rowCount))
        },
        .run = function() {

            dep <- self$options$dep
            pred <- self$options$predictors

            if (is.null(dep))
                return()
            if (length(pred) == 0)
                return()

            table <- self$results$models

            selected <- table$rowSelected
            if(selected > table$rowCount) selected <- 0

            state <- self$results$state$state  # retrieve state from last time
            estState <- self$results$estimateState$state

            if(is.null(estState)) {
                estState <- list(paramTables = rep(list(NULL), table$rowCount))
            }

            data <- self$data
            data[[dep]] <- jmvcore::toNumeric(data[[dep]])
            for (f in pred) {
                data[[f]] <- jmvcore::toNumeric(data[[f]])
                attributes(data[[f]]) <- NULL  # BayesFactor doesn't like additional attributes
                }
            data <- jmvcore::naOmit(data)


            if (is.null(state)) {  # no state, so must calc all BFs

                state <- list(omitted=0, bfs=rep(list(NULL), table$rowCount))

                state$omitted <- base::attr(data, 'nRowsOmitted', exact=TRUE)

                if (selected > 0) {  # is a denominator selected
                    key <- table$rowKeys[[selected]]
                    if (selected > 1)
                        bfr <- private$.bf(data, dep, key)
                    else
                        bfr <- list(bf=1, aic = 0, bic = 0, p = 0, R2 = 0)
                    state$bfs[[selected]] <- bfr
                }

                for (rowNo in seq_len(table$rowCount)) {
                    if (rowNo == selected)
                        next()

                    
                    private$.populate(state, selected)  # populate the table
                    private$.checkpoint()               # send the results

                    # calc the next bayes factor
                    key <- table$rowKeys[[rowNo]]
                    state$bfs[[rowNo]] <- private$.bf(data, dep, key)
                }
            }

            image <- self$results$postPlot
            param <- self$results$param

            if(selected > 0) {
                if(is.null(estState$paramTables[[selected]])) {
                    key <- table$rowKeys[[selected]]
                    estState$paramTables[[selected]] <- private$.posterior(data, dep, key)
                }
                private$.populateEstimates(estState, selected)

                paramSel <- param$rowSelected
                if(paramSel > param$rowCount) paramSel <- 0

                if(paramSel > 0){
                    image$setVisible(TRUE)
                    est <- unlist(estState$paramTables[[selected]][paramSel, 2:3])
                    mn <- est[1]
                    sd <- est[2]
                    x <- seq(mn - 3*sd, mn + 3*sd, len = 500)
                    plotData <- list(x = x, y = dnorm(x, mn, sd))
                }else{
                    image$setVisible(FALSE)
                    plotData <- NULL
                }
                
                image$setState(plotData)

            }

            private$.populate(state, selected)
            self$results$state$setState(state)
            self$results$estimateState$setState(estState)  # store state for next time
        },
        .populateEstimates=function(estState, selected){
            if(selected < 1) return()

            param <- self$results$param

            paramTable <- estState$paramTables[[selected]]
            nParams <- nrow(paramTable)
            for ( i in seq_len(nParams) ) {
                    param$setRow(rowNo=i, values=as.list(paramTable[i,]))
            }
        },
        .populate=function(state, selected) {

            # use the state object to populate the table

            table <- self$results$models
            param <- self$results$param

            if ( ! is.null(state$omitted) && state$omitted != 0)
                table$setNote('excluded', jmvcore::format('{} row(s) were excluded due to missing values', state$omitted), init=FALSE)

            if (selected > 0)
                null <- state$bfs[[selected]]
            else
                null <- list(bf=1, aic=0, bic=0)

            for (rowNo in seq_along(state$bfs)) {
                bfr <- state$bfs[[rowNo]]
                if (is.null(bfr))
                    next()
                bfr$bf  <- bfr$bf / null$bf
                bfr$aic <- bfr$aic - null$aic
                bfr$bic <- bfr$bic - null$bic
                table$setRow(rowNo=rowNo, values=bfr)
            }

        },
        .bf=function(data, dep, terms) {
            if (length(terms) == 0) {
                lmo <- lm(formula(paste0(dep," ~ 1")), data  = data)
                bf <- 1
            } else {
                fmla <- jmvcore::constructFormula(dep, terms)
                fmla <- stats::formula(fmla)
                bfo <- BayesFactor::lmBF(fmla, data)
                lmo <- lm(fmla, data  = data)

                # Bayes factor
                bf <- BayesFactor::extractBF(bfo)[1,'bf']
            }

            R2 <- summary(lmo)$r.squared
            npred <- summary(lmo)$df[1] - 1
                
            # aic
            aic <- AIC(lmo)

            # bic
            bic <- BIC(lmo)

            list(bf=bf, aic=aic, bic=bic, p = npred, R2 = R2)
        },
        .posterior=function(data, dep, terms) {


            init <- nrow(data) < 1
            cnames <- c("(Intercept)", unlist(terms))

            if(init){
                paramTable <- data.frame(param = cnames, mean = NA, sd = NA, stringsAsFactors = FALSE)
            }else{

                if(length(terms) == 0) {
                    lmo <- lm(formula(paste0(dep," ~ 1")), data  = data)
                }else{
                    fmla0 <- jmvcore::constructFormula(dep, terms)
                    fmla <- stats::formula(fmla0)
                    lmo <- lm(fmla, data = data)                
                }
                est <- matrix(summary(lmo)$coefficients[,1:2], ncol=2)
                paramTable <- data.frame(param = cnames, mean = est[,1], sd = est[,2], stringsAsFactors = FALSE)
            }


            paramTable
        },
        .postPlot=function(image, ...) {
            plotData <- image$state
            if(is.null(plotData)) return(FALSE)
            plot(plotData$x, plotData$y, type = 'l')
            TRUE
        })
)
