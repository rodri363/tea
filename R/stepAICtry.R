# file MASS/R/stepAIC.R
# copyright (C) 1994-2007 W. N. Venables and B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

#' A version of stepAIC(), using error handling to prevent
#' bad model fits during forward selection
#' @param object an object representing a model of an appropriate class. This
#'          is used as the initial model in the stepwise search. 
#'
#' @param scope defines the range of models examined in the stepwise search.
#'          This should be either a single formula, or a list containing
#'          components 'upper' and 'lower', both formulae.  See the
#'          details for how to specify the formulae and how they are
#'          used. 
#'
#' @param scale used in the definition of the AIC statistic for selecting the
#'          models, currently only for 'lm' and 'aov' models (see
#'          'extractAIC' for details). 
#'
#' @param direction the mode of stepwise search, can be one of '"both"',
#'          '"backward"', or '"forward"', with a default of '"both"'. If
#'          the 'scope' argument is missing the default for 'direction'
#'          is '"backward"'. 
#'
#' @param trace if positive, information is printed during the running of
#'          'stepAIC'. Larger values may give more information on the
#'          fitting process. 
#'
#' @param keep a filter function whose input is a fitted model object and
#'          the associated 'AIC' statistic, and whose output is
#'          arbitrary. Typically 'keep' will select a subset of the
#'          components of the object and return them. The default is not
#'          to keep anything. 
#'
#' @param steps the maximum number of steps to be considered.  The default is
#'          1000 (essentially as many as required).  It is typically used
#'          to stop the process early. 
#'
#' @param use.start if true the updated fits are done starting at the linear
#'          predictor for the currently selected model. This may speed up
#'          the iterative calculations for 'glm' (and other fits), but it
#'          can also slow them down. *Not used* in R. 
#'
#' @param k the multiple of the number of degrees of freedom used for the
#'          penalty. Only 'k = 2' gives the genuine AIC: 'k = log(n)' is
#'          sometimes referred to as BIC or SBC. 
#' @return the stepwise-selected model is returned, with up to two additional
#'     components.  There is an '"anova"' component corresponding to the
#'     steps taken in the search, as well as a '"keep"' component if the
#'     'keep=' argument was supplied in the call. The '"Resid. Dev"'
#'     column of the analysis of deviance table refers to a constant
#'     minus twice the maximized log likelihood: it will be a deviance
#'     only in cases where a saturated model is well-defined (thus
#'     excluding 'lm', 'aov' and 'survreg' fits, for example).


stepAICtry <-
  function(object, scope, scale = 0,
           direction = c("both", "backward", "forward"),
           trace = 1, keep = NULL, steps = 1000, use.start = FALSE, k = 2, ...)
{
    mydeviance <- function(x, ...)
    {
        dev <- deviance(x)
        if(!is.null(dev)) dev else extractAIC(x, k=0)[2L]
    }

    cut.string <- function(string)
    {
        if(length(string) > 1L)
            string[-1L] <- paste("\n", string[-1L], sep = "")
        string
    }

    re.arrange <- function(keep)
    {
        namr <- names(k1 <- keep[[1L]])
        namc <- names(keep)
        nc <- length(keep)
        nr <- length(k1)
        array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, namc))
    }

    step.results <- function(models, fit, object, usingCp=FALSE)
    {
        change <- sapply(models, "[[", "change")
        rd <- sapply(models, "[[", "deviance")
        dd <- c(NA, abs(diff(rd)))
        rdf <- sapply(models, "[[", "df.resid")
        ddf <- c(NA, abs(diff(rdf)))
        AIC <- sapply(models, "[[", "AIC")
        heading <- c("Stepwise Model Path \nAnalysis of Deviance Table",
                     "\nInitial Model:", deparse(as.vector(formula(object))),
                     "\nFinal Model:", deparse(as.vector(formula(fit))),
                     "\n")
        aod <-
            if(usingCp)
                data.frame(Step = change, Df = ddf, Deviance = dd,
                           "Resid. Df" = rdf, "Resid. Dev" = rd,
                           Cp = AIC, check.names = FALSE)
            else data.frame(Step = change, Df = ddf, Deviance = dd,
                            "Resid. Df" = rdf, "Resid. Dev" = rd,
                            AIC = AIC, check.names = FALSE)
        attr(aod, "heading") <- heading
        class(aod) <- c("Anova", "data.frame")
        fit$anova <- aod
        fit
    }

    Terms <- terms(object)
    object$formula <- Terms
    if(inherits(object, "lme")) object$call$fixed <- Terms
    else if(inherits(object, "gls")) object$call$model <- Terms
    else object$call$formula <- Terms
    if(use.start) warning("'use.start' cannot be used with R's version of glm")
    md <- missing(direction)
    direction <- match.arg(direction)
    backward <- direction == "both" | direction == "backward"
    forward <- direction == "both" | direction == "forward"
    if(missing(scope)) {
        fdrop <- numeric(0)
        fadd <- attr(Terms, "factors")
        if(md) forward <- FALSE
    } else {
        if(is.list(scope)) {
            fdrop <- if(!is.null(fdrop <- scope$lower))
                attr(terms(update.formula(object, fdrop)), "factors")
            else numeric(0)
            fadd <- if(!is.null(fadd <- scope$upper))
                attr(terms(update.formula(object, fadd)), "factors")
        } else {
            fadd <- if(!is.null(fadd <- scope))
                attr(terms(update.formula(object, scope)), "factors")
            fdrop <- numeric(0)
        }
    }
    models <- vector("list", steps)
    if(!is.null(keep)) keep.list <- vector("list", steps)
    ## watch out for partial matching here.
    if(is.list(object) && (nmm <- match("nobs", names(object), 0)) > 0)
        n <- object[[nmm]]
    else n <- length(residuals(object))
    fit <- object
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- bAIC[2L]
    if(is.na(bAIC))
        stop("AIC is not defined for this model, so stepAIC cannot proceed")
    nm <- 1
    Terms <- terms(fit)
    if(trace) {
        cat("Start:  AIC=", format(round(bAIC, 2)), "\n",
            cut.string(deparse(as.vector(formula(fit)))), "\n\n", sep='')
	utils::flush.console()
    }
    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - edf,
                         change = "", AIC = bAIC)
    if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    usingCp <- FALSE
    while(steps > 0) {
        #steps <- steps - 1
        AIC <- bAIC
        ffac <- attr(Terms, "factors")
        ## don't drop strata terms
        if(!is.null(sp <- attr(Terms, "specials")) &&
           !is.null(st <- sp$strata)) ffac <- ffac[-st,]
        scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
        aod <- NULL
        change <- NULL
        if(backward && length(scope$drop)) {
            aod <- dropterm(fit, scope$drop, scale = scale,
                            trace = max(0, trace - 1), k = k, ...)
            rn <- row.names(aod)
            row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep=" "))
            ## drop all zero df terms first.
            if(any(aod$Df == 0, na.rm=TRUE)) {
                zdf <- aod$Df == 0 & !is.na(aod$Df)
                nc <- match(c("Cp", "AIC"), names(aod))
                nc <- nc[!is.na(nc)][2L]
                ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
                if(any(ch)) {
                    warning("0 df terms are changing AIC")
                    zdf <- zdf[!ch]
                }
                ## drop zero df terms first: one at time since they
                ## may mask each other
                if(length(zdf) > 0L)
                    change <- rev(rownames(aod)[zdf])[1L]
            }
        }
        if(is.null(change)) {
            if(forward && length(scope$add)) {
				#print(formula(fit));
				#print(scope);
                #aodfTRY <- try(addterm(fit, scope$add, scale = scale,
                #                trace = max(0, trace - 1), k = k, ...));
				#if(!inherits(aodfTRY,"try-error")){
				#if fit worked, count a step
                aodfTRY <- try(addtermtry(fit, scope$add, scale = scale,
                                trace = max(0, trace - 1), k = k, ...));
        		steps <- steps - 1
				aodf <- aodfTRY;
                rn <- row.names(aodf)
                row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], sep=" "))
                aod <-
                    if(is.null(aod)) aodf
                    else rbind(aod, aodf[-1, , drop=FALSE])
				aodOLD <- aod;
				#}else{
				#if fit didn't work, don't count step, set aod to previous aod
				#RR may need to remove offending term from list???
				#	aod <- aodOLD;
					#scope <- drop.terms(scope,scope$add);
				#}
            }
            attr(aod, "heading") <- NULL
            if(is.null(aod) || ncol(aod) == 0) break
            ## need to remove any terms with zero df from consideration
            nzdf <- if(!is.null(aod$Df)) aod$Df != 0 | is.na(aod$Df)
            aod <- aod[nzdf, ]
            if(is.null(aod) || ncol(aod) == 0) break
            nc <- match(c("Cp", "AIC"), names(aod))
            nc <- nc[!is.na(nc)][1L]
            o <- order(aod[, nc])
            if(trace) {
		#print(aod[o,  ])
		utils::flush.console()
	    }
            if(o[1L] == 1) break
            change <- rownames(aod)[o[1L]]
        }
        usingCp <- match("Cp", names(aod), 0) > 0
        ## may need to look for a 'data' argument in parent
	fitTRY <- update(fit, paste("~ .", change), evaluate = FALSE)
        fitTRY <- try(eval.parent(fitTRY),TRUE);
		if(!inherits(fitTRY,"try-error")){
			fit <- fitTRY;
		}
        if(is.list(fit) && (nmm <- match("nobs", names(fit), 0)) > 0)
            nnew <- fit[[nmm]]
        else nnew <- length(residuals(fit))
        if(nnew != n)
            stop("number of rows in use has changed: remove missing values?")
        Terms <- terms(fit)
        bAIC <- extractAIC(fit, scale, k = k, ...)
        edf <- bAIC[1L]
        bAIC <- bAIC[2L]
        if(trace) {
            cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n",
                cut.string(deparse(as.vector(formula(fit)))), "\n\n", sep='')
	    utils::flush.console()
	}
        ## add a tolerance as dropping 0-df terms might increase AIC slightly
        if(bAIC >= AIC + 1e-7) break
        nm <- nm + 1
        models[[nm]] <-
            list(deviance = mydeviance(fit), df.resid = n - edf,
                 change = change, AIC = bAIC)
        if(!is.null(keep)) keep.list[[nm]] <- keep(fit, bAIC)
    }
    if(!is.null(keep)) fit$keep <- re.arrange(keep.list[seq(nm)])
    step.results(models = models[seq(nm)], fit, object, usingCp)
}

#' Version of addterm, designed to not fail during execution
#' Try fitting all models that differ from the current model by
#' adding a single term from those supplied, maintaining marginality.
#'
#' This function is generic; there exist methods for classes 'lm' and
#' 'glm' and the default method will work for many other classes.
#'
#' @param object An object fitted by some model-fitting function. 
#'
#' @param scope a formula specifying a maximal model which should include the
#' 	 current one. All additional terms in the maximal model with
#' 	 all marginal terms in the original model are tried. 
#'
#' @param scale used in the definition of the AIC statistic for selecting the
#' 	 models, currently only for 'lm', 'aov' and 'glm' models.
#' 	 Specifying 'scale' asserts that the residual standard error
#' 	 or dispersion is known. 
#'
#' @param test should the results include a test statistic relative to the
#' 	 original model?  The F test is only appropriate for 'lm' and
#' 	 'aov' models, and perhaps for some over-dispersed 'glm'
#' 	 models. The Chisq test can be an exact test ('lm' models with
#' 	 known scale) or a likelihood-ratio test depending on the
#' 	 method. 
#'
#'   @param k the multiple of the number of degrees of freedom used for the
#' 	 penalty. Only 'k=2' gives the genuine AIC: 'k = log(n)' is
#' 	 sometimes referred to as BIC or SBC. 
#'
#' @param sorted should the results be sorted on the value of AIC? 
#'
#' @param trace if 'TRUE' additional information may be given on the fits as
#' 	 they are tried. 
#'
#' @param ... arguments passed to or from other methods. 

addtermtry <-
    function(object, scope, scale = 0, test = c("none", "Chisq"),
             k = 2, sorted = FALSE, trace = FALSE, ...)
{
    if(missing(scope) || is.null(scope)) stop("no terms in scope")
    if(!is.character(scope))
        scope <- add.scope(object, update.formula(object, scope))
    if(!length(scope))
        stop("no terms in scope for adding to object")
#     newform <- update.formula(object,
#                               paste(". ~ . +", paste(scope, collapse="+")))
#     data <- model.frame(update(object, newform)) # remove NAs
#     object <- update(object, data = data)
    ns <- length(scope)
    ans <- matrix(nrow = ns + 1L, ncol = 2L,
                  dimnames = list(c("<none>", scope), c("df", "AIC")))
    ans[1L,  ] <- extractAIC(object, scale, k = k, ...)
    n0 <- length(object$residuals)
    env <- environment(formula(object))
    for(i in seq(ns)) {
        tt <- scope[i]
        if(trace) {
	    message("trying +", tt)
	    utils::flush.console()
        }
        nfit <- update(object, as.formula(paste("~ . +", tt)),
                       evaluate = FALSE)
	nfit <- try(eval(nfit, envir=env)) # was  eval.parent(nfit)
	if(!inherits(nfit,"try-error")){
		ans[i+1L, ] <- extractAIC(nfit, scale, k = k, ...)
			if(length(nfit$residuals) != n0)
				stop("number of rows in use has changed: remove missing values?")
	}#may need else to add a fluff value of AIC for everything to work...
    }
    dfs <- ans[, 1L] - ans[1L, 1L]
    dfs[1L] <- NA
    aod <- data.frame(Df = dfs, AIC = ans[, 2L])
    o <- if(sorted) order(aod$AIC) else seq_along(aod$AIC)
    test <- match.arg(test)
    if(test == "Chisq") {
	dev <- ans[, 2L] - k*ans[, 1L]
	dev <- dev[1L] - dev; dev[1L] <- NA
	nas <- !is.na(dev)
	P <- dev
	P[nas] <- safe_pchisq(dev[nas], dfs[nas], lower.tail=FALSE)
	aod[, c("LRT", "Pr(Chi)")] <- list(dev, P)
    }
    aod <- aod[o, ]
    head <- c("Single term additions", "\nModel:",
              deparse(as.vector(formula(object))))
    if(scale > 0)
        head <- c(head, paste("\nscale: ", format(scale), "\n"))
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    aod
}

