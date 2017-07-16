
# This file is a generated template, your changes will not be overwritten

anovaClass <- R6::R6Class(
    "anovaClass",
    inherit = anovaBase,
    private = list(
        .init = function() {

            dep <- self$options$dep
            fmf <- self$options$fullFactors
            nmf <- self$options$nullFactors
            data <- self$data
            table <- self$results$anova
            state <- self$results$state
            param <- self$results$param

            table$addRow(rowKey=character(), values=list(model='Intercept only'))

            if (length(fmf) == 0)
                return()

            fmf <- c(fmf, nmf)
            fullFmla <- paste0(dep, '~', paste0(fmf, collapse='*'))
            fullFmla <- formula(fullFmla)

            options(BFfactorsMax=5)  # this isn't necessary with the new BayesFactor

            models <- BayesFactor::enumerateGeneralModels(
                fullFmla,
                whichModels='withmain',
                neverExclude=nmf)

            for (model in models) {
                terms <- attr(stats::terms(model), 'term.labels')
                terms <- jmvcore::decomposeTerms(terms)
                niceTerms <- sapply(terms, function(x) jmvcore::stringifyTerm(x))
                model <- paste(niceTerms, collapse=' + ')
                table$addRow(rowKey=terms, values=list(model=model))
            }

            selected <- table$rowSelected

            if(selected > 1) {
                model <- models[[ selected - 1 ]]
                terms <- attr(stats::terms(model), 'term.labels')
                terms <- jmvcore::decomposeTerms(terms)
                paramTable <- private$.posterior(data, dep, terms, TRUE)
                nParams <- nrow(paramTable)
                for ( i in seq_len(nParams) ) {
                    param$addRow(rowKey=paramTable[i,1], values=as.list(paramTable[i,]))
                }
            }

            table$setSortKeys(col=1, seq_len(table$rowCount))
        },
        .run = function() {

            dep <- self$options$dep
            fmf <- self$options$fullFactors

            if (is.null(dep))
                return()
            if (length(fmf) == 0)
                return()

            table <- self$results$anova

            selected <- table$rowSelected
            if (selected > table$rowCount)
                selected <- 0

            state <- self$results$state$state  # retrieve state from last time

            if (is.null(state)) {  # no state, so must calc all BFs

                state <- list(omitted=0, bfs=rep(list(NULL), table$rowCount))

                data <- self$data
                data[[dep]] <- jmvcore::toNumeric(data[[dep]])
                for (f in fmf) {
                    data[[f]] <- as.factor(data[[f]])
                    attributes(data[[f]]) <- NULL  # BayesFactor doesn't like additional attributes
                }
                data <- jmvcore::naOmit(data)

                state$omitted <- base::attr(data, 'nRowsOmitted', exact=TRUE)

                if (selected > 0) {  # is a denominator selected
                    key <- table$rowKeys[[selected]]
                    if (selected > 1)
                        bfr <- private$.bf(data, dep, key)
                    else
                        bfr <- list(bf=1, err=0)
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

            private$.populate(state, selected)
            self$results$state$setState(state)  # store state for next time
        },
        .populate=function(state, selected) {

            # use the state object to populate the table

            table <- self$results$anova
            if ( ! is.null(state$omitted) && state$omitted != 0)
                table$setNote('excluded', jmvcore::format('{} row(s) were excluded due to missing values', state$omitted), init=FALSE)

            if (selected > 0)
                null <- state$bfs[[selected]]
            else
                null <- list(bf=1, err=0)

            for (rowNo in seq_along(state$bfs)) {
                bfr <- state$bfs[[rowNo]]
                if (is.null(bfr))
                    next()
                bfr$bf  <- bfr$bf / null$bf
                bfr$err <- sqrt((bfr$err^2)+(null$err^2))
                table$setRow(rowNo=rowNo, values=bfr)
            }
        },
        .bf=function(data, dep, terms) {
            if (length(terms) == 0) {
                return(list(bf=1, err=0))
            } else {
                fmla <- jmvcore::constructFormula(dep, terms)
                fmla <- stats::formula(fmla)
                bfo <- BayesFactor::lmBF(fmla, data)
                bfr <- BayesFactor::extractBF(bfo)
            }
            list(bf=bfr[1,'bf'], err=bfr[1,'error'])
        },
        .posterior=function(data, dep, terms, init) {
            nParamIters <- self$options$nParamIters

            fmla0 <- jmvcore::constructFormula(dep, terms)
            fmla <- stats::formula(fmla0)
            #samples <- BayesFactor::lmBF(fmla, data, posterior = TRUE, 
            #    iterations = nParamIters, noSample = init)
            #cnames <- colnames(samples)
            print(nrow(data))
            cnames <- rep(fmla0, 10)
            n <- ifelse(init, 0, nParamIters)

            if(init){
                paramTable <- data.frame(param = cnames, mean = NA, stringsAsFactors = FALSE)
            }else{
                paramTable <- data.frame(param = cnames, mean = NA, stringsAsFactors = FALSE)
            }
            attr(paramTable, "iters") <- n

            paramTable
        })
)
