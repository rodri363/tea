#' Synthesize a variable via sequential regression , accounting for edit contraints
#' This function is meant to run on a single domain.
#'
#' @param model.spec a list of modeling options
#' \enumerate{
#' \item [[*]]$lower = an R formula (either as a formula object or a character),
#' giving the 'minimal' formula used to model.  This formula is used
#' in the initial fit.
#' \item [[*]]$upper = either:
#' an R formula (either as a formula object or a character),' giving the 'maximal' formula used to model.
#' a list of R formulas, in the form a step.gam 'scope' argument.
#' in either case: If not NULL, implies step-wise selection
#' from lower to upper.  If NULL, implies fit lower model only.
#' \item [[*]]$model = a character vector giving a supported modeling function (e.g. gam)
#' \item [[*]]$consistency = a character vector. "" or NULL indicates no consistency checking.
#' a length>1 character vector gives variable names used in the consistency checking system
#' for this variable. If length>1, must include all 'left hand side' variables at least.
#' }
#' The regressions implied by model.spec are run sequentially, and synthetic
#' data from each regression is inserted into the data \emph{before} the
#' next fit.
#' @param data data frame for modeling
#' @param db = A string giving the database used for the consistency checking system.
#' Defaults to NULL, which implies no checking.
#' @param verbose verbose output?
#' @return a new data frame with sequential synthetic values from each model
#' @author Rolando Rodriguez \email{rolando.a.rodriguez@@census.gov}

PEP.method.seq.reg <- function(model.spec, data, db=NULL,verbose=FALSE){
	for(spec in model.spec){
		#environment voodoo
		environment(spec) <- environment()
		print(spec$lower)
		#if running a gam
		if(spec$model == "gam"){
			fit <- try(gam(spec$lower,data=data,na.action=na.exclude))
			if(inherits(fit,"try-error")){
				print("Initial fit didn't work, doing nothing")
			}else{
				if(!is.null(spec$upper)) step.fit <- try(step.gam(fit,scope=spec$upper,data=data,trace=FALSE))
				if(inherits(step.fit,"try-error")){
					trydata <- try(PEP.gam.syn.pmm(fit,data,edit.vars=spec$consistency,db=db,niter=spec$niter,
						v.floor=spec$floor,v.ceiling=spec$ceiling))
					if(!inherits(trydata,"try-error")) data <- trydata
					else print("No luck with this model. Leaving data unchanged.")
				}else{
					trydata <- try(PEP.gam.syn.pmm(step.fit,data,edit.vars=spec$consistency,db=db,
						niter=spec$niter,v.floor=spec$floor,v.ceiling=spec$ceiling))
					if(!inherits(trydata,"try-error")) data <- trydata
					else print("No luck with this model. Leaving data unchanged.")
				}
			}
		}
		if(spec$model == "multinom"){
			fit <- try(multinom(spec$lower,data=data,model=TRUE,trace=FALSE))
			if(inherits(fit,"try-error")){
				print("Initial fit didn't work, doing nothing")
			}else{
				#environment voodoo
				environment(fit) <- environment()
				environment(fit$terms) <- environment()
				if(!is.null(spec$upper)) step.fit <- try(stepAIC(fit,
						scope=list(lower=spec$lower,upper=spec$upper), trace=0))
				if(inherits(step.fit,"try-error")){
					trydata <- try(PEP.multinom.syn.ml(fit,data,edit.vars=spec$consistency,
						db=db,niter=spec$niter,rescale=spec$rescale))
					if(!inherits(trydata,"try-error")) data <- trydata
					else print("No luck with this model. Leaving data unchanged.")
				}else{
					trydata <- try(PEP.multinom.syn.ml(step.fit,data,edit.vars=spec$consistency,
						db=db,niter=spec$niter,rescale=spec$rescale))
					if(!inherits(trydata,"try-error")) data <- trydata
					else print("No luck with this model. Leaving data unchanged.")
				}
			}
		}
	}
	return(data)
}
