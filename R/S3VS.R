
# Main function for all families
S3VS = function( 
	y, 
	X, 
	family = c("normal","binomial","survival"), 
	cor_xy = NULL, 
	surv_model = c("COX","AFT"),
	method_xy = c("topk","fixedthresh","percthresh"), 
	param_xy, 
	method_xx = c("topk","fixedthresh","percthresh"), 
	param_xx, 
	vsel_method = NULL,
	lambda = c("lambda.min","lambda.1se"),
	alpha = 0.5, 
	method_sel = c("conservative","liberal"), 
	method_rem = c("conservative_begin","conservative_end","liberal"), 
	sel_regout = FALSE, 
	rem_regout = FALSE, 
	update_y_thresh = 0.5,
	m = 100, 
	nskip = 3, 
	verbose = FALSE, 
	seed = NULL,
  parallel = FALSE
){
	family <- match.arg(family)
	surv_model <- match.arg(surv_model)
	method_xy <- match.arg(method_xy)
	method_xx <- match.arg(method_xx)
	method_sel <- match.arg(method_sel)
	method_rem <- match.arg(method_rem)
	lambda <- match.arg(lambda)

	method_xy <- switch(family,
	  "normal"   = switch(method_xy,
						  "topk" = "topk",
						  "fixedthresh" = "fixedcorthresh",
						  "percthresh" = "perccorthresh"),	  
	  "binomial" = switch(method_xy,
						  "topk" = "topk",
						  "fixedthresh" = "fixedetasqthresh",
						  "percthresh" = "percetasqthresh"),	  
	  "survival" = switch(method_xy,
						  "topk" = "topk",
						  "fixedthresh" = "fixedmuthresh",
						  "percthresh" = "percmuthresh"),	  
	  stop("Unsupported family: ", family)
	)

	method_xx <- switch(method_xx,
		"topk" = "topk",
		"fixedthresh" = "fixedcorthresh",
		"percthresh" = "perccorthresh"
	)

	if (is.null(vsel_method)) {
	vsel_method <- switch(family,
						  "normal"   = c("NLP","LASSO","ENET","SCAD","MCP"),
						  "binomial" = c("NLP","LASSO","ENET","SCAD","MCP"),
						  "survival" = c("AFTREG","AFTGEE","BRIDGE","PVAFT","LASSO","ENET")
						)
	}
	vsel_method <- match.arg(vsel_method,vsel_method)  # enforce choice

	# Do main analysis
	out <- switch(family,
		"normal" = S3VS_LM(y=y, X=X, cor_xy=cor_xy, method_xy=method_xy, param_xy=param_xy, method_xx=method_xx, param_xx=param_xx, method_sel=method_sel, method_rem=method_rem, rem_regout=rem_regout, vsel_method=vsel_method, lambda=lambda, alpha=alpha, m=m, nskip=nskip, verbose=verbose, seed=seed),
		"binomial" = S3VS_GLM(y=y, X=X, method_xy=method_xy, param_xy=param_xy, method_xx=method_xx, param_xx=param_xx, method_sel=method_sel, method_rem=method_rem, sel_regout=sel_regout, rem_regout=rem_regout, update_y_thresh=update_y_thresh, vsel_method=vsel_method, lambda=lambda, alpha=alpha, m=m, nskip=nskip, verbose=verbose, seed=seed),
		"survival" = S3VS_SURV(y=y, X=X, method_xy=method_xy, param_xy=param_xy, method_xx=method_xx, param_xx=param_xx, method_sel=method_sel, method_rem=method_rem, surv_model=surv_model, vsel_method=vsel_method, lambda=lambda, alpha=alpha, m=m, nskip=nskip, verbose=verbose, seed=seed, parallel=parallel),
		stop("Unsupported family: ", family)
	)

	out
}
# load('/Users/nsanyal/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/M01. Padmore Prempeh (passed)/Padmore_shared/Simulated_data/LM/data/800_20000.Rdata'); y = data$y[1,]; X = data$X; S3VS( y, X, family = "normal", cor_xy = NULL, surv_model = NULL, method_xy = "topk", param_xy = list(k = 1), method_xx = "topk", param_xx = list(k = 5), method_sel = "conservative", method_rem = "conservative_begin", rem_regout = FALSE, update_y_thresh = NULL, vsel_method = "LASSO", m = 100, nskip = 3, verbose = FALSE, seed = 123)
# load('/Users/nsanyal/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/M01. Padmore Prempeh (passed)/Padmore_shared/Simulated_data/GLM/data/800_20000.Rdata'); y = data$y[1,]; X = data$X; S3VS( y, X, family = "binomial", cor_xy = NULL, surv_model = NULL, method_xy = "topk", param_xy = list(k = 1), method_xx = "topk", param_xx = list(k = 5), method_sel = "conservative", method_rem = "conservative_begin", sel_regout = FALSE, rem_regout = TRUE, update_y_thresh = NULL, vsel_method = "LASSO", m = 100, nskip = 3, verbose = FALSE, seed = 123)
# load('/Users/nsanyal/Library/CloudStorage/OneDrive-UniversityofTexasatElPaso/M01. Padmore Prempeh (passed)/Padmore_shared/Simulated_data/Survival/data/200_10000.Rdata'); y = list(time=data$time[1,],status=data$status[1,]); X = data$X; S3VS( y, X, family = "survival", cor_xy = NULL, surv_model = "COX", method_xy = "topk", param_xy = list(k = 1), method_xx = "topk", param_xx = list(k = 5), method_sel = "conservative", method_rem = "conservative_begin", sel_regout = FALSE, rem_regout = FALSE, update_y_thresh = NULL, vsel_method = "LASSO", m = 100, nskip = 3, verbose = FALSE, seed = 123)


# Main function for LM
S3VS_LM = function(y, X, cor_xy = NULL, method_xy=c("topk","fixedcorthresh","perccorthresh"), param_xy, method_xx = c("topk","fixedcorthresh","perccorthresh"), param_xx, vsel_method = c("NLP","LASSO","ENET","SCAD","MCP"), lambda = c("lambda.min","lambda.1se"), alpha = 0.5, method_sel = c("conservative","liberal"), method_rem = c("conservative_begin","conservative_end","liberal"), rem_regout = FALSE, m = 100, nskip = 3, verbose = FALSE, seed = NULL)
{
	starttime <- Sys.time()
	if(!is.null(seed)) set.seed(seed)

	method_xy <- match.arg(method_xy)
	method_xx <- match.arg(method_xx)
	vsel_method <- match.arg(vsel_method)
	method_sel <- match.arg(method_sel)
	method_rem <- match.arg(method_rem)
	lambda <- match.arg(lambda)

	varsselected = NULL
	varsleft = colnames(X)
	max_nocollect = 0 
	y_update <- y
	selected_iterwise = list()
	iter = 0
	while(looprun(varsselected,varsleft,max_nocollect,m,nskip)){
		# enter ith iteration
		iter = iter + 1
		if(verbose) cat( "-------------", "\n", "Iteration ", iter, "\n", "-------------", "\n", sep = "") 
		
		leadvars <- get_leadvars_LM(y=y_update, X=X[,varsleft,drop=F], method=method_xy, param=param_xy)
		if(length(leadvars)==0){
			cat("No leading variables, so terminating here.\n")
			break
		} else{
			leadsets <- lapply(leadvars,function(var) get_leadsets(x_lead=X[,var], X=X[,varsleft], method=method_xx,param=param_xx))
			fit <- lapply(leadsets,function(set) VS_method_LM(y=y_update, X=X[,set,drop=F], vsel_method=vsel_method, lambda=lambda, alpha=alpha, verbose=verbose))
			select_leadsets <- lapply(fit,function(x)x$sel)
			noselect_leadsets <- lapply(fit,function(x)x$nosel)
			if(length(select_leadsets[[1]])>0){
				select <- select_vars(listselect=select_leadsets, method=method_sel)
				remove <- NULL
				selected_iterwise <- c(selected_iterwise,select)
				y_update <- update_y_LM(y=y_update, X=X, vars=select)
			} else{
				select <- NULL
				remove <- remove_vars(listnotselect=noselect_leadsets, method=method_rem)
				# remove_regout <- remove[ abs(cor(y_update,x[,remove,drop=F]))[1,] < 0.1 ]
				max_nocollect <- max_nocollect + 1
				if(verbose) {cat("***","nskip=",max_nocollect,"***","\n")}
				selected_iterwise <- c(selected_iterwise,'')
				if(rem_regout){
					y_update <- update_y_LM(y=y_update, X=X, vars=remove)
				} 
			}
			varsselected <- c(varsselected,select)
			varsleft <- setdiff(varsleft,union(select,remove))
		}	
	}

  runtime <- round(difftime(Sys.time(), starttime, units = "sec"),2)
	cat("=================================", "\n","Number of selected variables: ", length(varsselected), "\n", "Time taken: ", runtime, " sec", "\n",  "=================================", "\n", sep = "")  

	list(selected = varsselected, selected_iterwise = selected_iterwise, runtime = runtime)
}



# Main function for GLM
S3VS_GLM <- function(y, X, method_xy = c("topk","fixedetasqthresh","percetasqthresh"), param_xy, method_xx = c("topk","fixedcorthresh","perccorthresh"), param_xx, vsel_method = c("NLP","LASSO","ENET","SCAD","MCP"), lambda = c("lambda.min","lambda.1se"), alpha = 0.5, method_sel = c("conservative","liberal"), method_rem = c("conservative_begin","conservative_end","liberal"), sel_regout = FALSE, rem_regout = FALSE, update_y_thresh = NULL, m = 100, nskip = 3, verbose = FALSE, seed = NULL) 
{
  starttime <- Sys.time()
  if(!is.null(seed)) set.seed(seed)

  method_xy <- match.arg(method_xy)
	method_xx <- match.arg(method_xx)
	vsel_method <- match.arg(vsel_method)
	method_sel <- match.arg(method_sel)
	method_rem <- match.arg(method_rem)
	lambda <- match.arg(lambda)
  
  varsselected = NULL
  varsleft = colnames(X)
  max_nocollect = 0 
  y_update <- y
  selected_iterwise = list()
  iter = 0
  while(looprun(varsselected,varsleft,max_nocollect,m,nskip)){
    iter = iter + 1
    if(verbose) cat("-------------", "\n", "Iteration ", iter, "\n", "-------------", "\n", sep = "") 
    
    leadvars <- get_leadvars_GLM(y=y_update, X=X[,varsleft,drop=F], method=method_xy, param=param_xy)
    if(length(leadvars) == 0){
      cat("No leading variables, terminating here.\n")
      break
    } else {
      leadsets <- lapply(leadvars, function(var) unlist(get_leadsets(x_lead=X[,var], X=X[,varsleft, drop=F], method=method_xx, param=param_xx), use.names = FALSE))      
      # if(verbose) print(leadsets)  # Debug print for leadsets
      fit <- lapply(seq_along(leadsets), function(set) VS_method_GLM(y=y_update, X=X[, leadsets[[set]], drop=F], vsel_method=vsel_method, lambda=lambda, alpha=alpha, verbose=verbose))
      select_leadsets <- lapply(fit, function(x) x$sel)
      noselect_leadsets <- lapply(fit, function(x) x$nosel)      
      # if(verbose) {
      #   print(select_leadsets)  # Debug print for selected leadsets
      # }      
      if(length(select_leadsets) > 0 && !is.null(select_leadsets[[1]]) && length(select_leadsets[[1]]) > 0){
        select <- select_vars(listselect=select_leadsets, method=method_sel)
        selected_iterwise <- c(selected_iterwise, select)
        if(sel_regout){
          y_update <- update_y_GLM(y=y_update, X=X, vars=select, update_y_thresh=update_y_thresh)
        }          
        remove <- NULL
      } else {
        select <- NULL
        remove <- remove_vars(listnotselect=noselect_leadsets, method=method_rem)
        max_nocollect <- max_nocollect + 1
        if(verbose) {cat("***","nskip=",max_nocollect,"***","\n")}
        if(rem_regout){
          y_update <- update_y_GLM(y=y_update, X=X, vars=remove, update_y_thresh=update_y_thresh)
        }
      }
      varsselected <- c(varsselected, select)
      varsleft <- setdiff(varsleft, union(select, remove))
    }
  }

  runtime <- round(difftime(Sys.time(), starttime, units = "sec"),2)
  cat("=================================", "\n","Number of selected variables: ", length(varsselected), "\n", "Time taken: ", runtime, " sec", "\n",  "=================================", "\n", sep = "")  

  list(selected = varsselected, selected_iterwise = selected_iterwise, runtime = runtime)
}



# Main function for SURV
S3VS_SURV = function( y, X, surv_model = c("COX","AFT"), method_xy = c("topk","fixedmuthresh","percmuthresh"), param_xy, method_xx = c("topk","fixedcorthresh","perccorthresh"), param_xx, vsel_method = c("LASSO","ENET","AFTREG","AFTGEE","BRIDGE","PVAFT"), lambda = c("lambda.min","lambda.1se"), alpha = 0.5, method_sel = c("conservative","liberal"), method_rem = c("conservative_begin","conservative_end","liberal"), m = 100, nskip = 3, verbose = FALSE, seed = NULL, parallel = FALSE)
{
  starttime <- Sys.time()
  if(!is.null(seed)) set.seed(seed)

  surv_model <- match.arg(surv_model)
  method_xy <- match.arg(method_xy)
	method_xx <- match.arg(method_xx)
	vsel_method <- match.arg(vsel_method)
	method_sel <- match.arg(method_sel)
	method_rem <- match.arg(method_rem)
	lambda <- match.arg(lambda)
  
  varsselected = NULL
  varsleft = colnames(X)
  max_nocollect = 0 
  
  time <- y$time
  delta <- y$status
  time_update <- time
  selected_iterwise = list()
  iter = 0
  while(looprun(varsselected,varsleft,max_nocollect,m,nskip)){
    # enter ith iteration
    iter = iter + 1
    y_update <- list(time=time_update,status=delta)
    if(verbose) cat( "-------------", "\n", "Iteration ", iter, "\n", "-------------", "\n", sep = "") 

    leadvars <- get_leadvars_SURV(y=y_update, varsselected = varsselected, varsleft=varsleft, X=X, method=method_xy, param=param_xy, surv_model=surv_model, parallel=parallel)    
    if(length(leadvars)==0){
      cat("No leading variables, so terminating here.\n")
      break
    } else{
      leadsets <- lapply(leadvars,function(var) get_leadsets(x_lead = X[,var], X = X[,varsleft], method = method_xx, param = param_xx))
      fit <- lapply(leadsets,function(set) VS_method_SURV(y = y_update, X = X[,set,drop=F], surv_model = surv_model, vsel_method = vsel_method, lambda=lambda, alpha=alpha, verbose = verbose))
      select_leadsets <- lapply(fit,function(x)x$sel)
      noselect_leadsets <- lapply(fit,function(x)x$nosel)
      if(length(select_leadsets[[1]])>0){
        select <- select_vars(listselect = select_leadsets, method = method_sel)
        remove <- NULL
        selected_iterwise <- c(selected_iterwise,select)
        # time_update <- update_y(time_update,X,select)
      } else{
        select <- NULL
        remove <- remove_vars(listnotselect = noselect_leadsets, method = method_rem)
        # remove_regout <- remove[ abs(cor(time_update,x[,remove,drop=F]))[1,] < 0.1 ]
        max_nocollect <- max_nocollect + 1
        if(verbose) {cat("***","nskip=",max_nocollect,"***","\n")}
        selected_iterwise <- c(selected_iterwise,'')
      }
      varsselected <- c(varsselected,select)
      varsleft <- setdiff(varsleft,union(select,remove))
    }				
  }

  runtime <- round(difftime(Sys.time(), starttime, units = "sec"),2)
  cat("=================================", "\n","Number of selected variables: ", length(varsselected), "\n", "Time taken: ", runtime, " sec", "\n",  "=================================", "\n", sep = "")  

  list(selected = varsselected, selected_iterwise = selected_iterwise, runtime=runtime)
}



# Function to select leading variables for all families
get_leadvars <- function(y, X, family = c("normal","binomial","survival"), surv_model = c("AFT", "COX"), method = c("topk", "fixedthresh", "percthresh"), param, varsselected = NULL, varsleft = colnames(X), parallel = FALSE){  
	# Select method
	family <- match.arg(family)
	surv_model <- match.arg(surv_model)
	method <- match.arg(method)

	method <- switch(family,
	  "normal"   = switch(method,
						  "topk" = "topk",
						  "fixedthresh" = "fixedcorthresh",
						  "percthresh" = "perccorthresh"),	  
	  "binomial" = switch(method,
						  "topk" = "topk",
						  "fixedthresh" = "fixedetasqthresh",
						  "percthresh" = "percetasqthresh"),	  
	  "survival" = switch(method,
						  "topk" = "topk",
						  "fixedthresh" = "fixedmuthresh",
						  "percthresh" = "percmuthresh"),	  
	  stop("Unsupported family: ", family)
	)

	# Main function
	out <- switch(family,
		"normal"   = get_leadvars_LM(y = y, X = X, method = method, param = param),
		"binomial" = get_leadvars_GLM(y = y, X = X, method = method, param = param),
		"survival" = get_leadvars_SURV(y = y, varsselected = varsselected, varsleft = varsleft, X = X, method = method, param = param, surv_model = surv_model, parallel = parallel),
		stop("Unsupported family: ", family)
	)
	out
}



# Function to select leading variables for LM
get_leadvars_LM <- function(y, X, method = c("topk","fixedcorthresh","perccorthresh"), param)
{
	method <- match.arg(method)
	sort_abscor <- sort(abs(cor(y,X)[1,]),decreasing=T) # sorted absolute correlation of y with x vars
	vars <- switch(
		method,
		"topk" = {
			names(sort_abscor[1:param$k])
		},
		"fixedcorthresh" = {
			names(sort_abscor[sort_abscor >= param$thresh])
		},
		"perccorthresh" = {
			names(sort_abscor[sort_abscor >= param$thresh/100 * sort_abscor[1]])
		}
	)
	vars
}



# Function to select leading variables for GLM
get_leadvars_GLM <- function(y, X, method = c("topk", "fixedetasqthresh", "percetasqthresh"), param) {
  method <- match.arg(method)
  if (is.null(colnames(X))) colnames(X) <- paste0("V", seq_len(ncol(X)))

  # center y and X (intercept is implicit)
  yc  <- y - mean(y)
  Xc  <- sweep(X, 2, colMeans(X), FUN = "-")

  # sums of squares
  y_ss  <- sum(yc^2)
  x_ss  <- colSums(Xc^2)

  # handle constant predictors to avoid NaNs
  nonconst <- x_ss > 0

  # correlations via dot products (vectorized), then eta^2 = r^2
  dots  <- as.numeric(crossprod(yc, Xc))           # length p
  r <- rep(NA_real_, length(dots))
  r[nonconst] <- dots[nonconst] / sqrt(y_ss * x_ss[nonconst])
  etasq <- r^2                                    
  names(etasq) <- colnames(X)

  # sort once for methods that need ordering
  ord <- order(etasq, decreasing = TRUE, na.last = NA)
  etasq_sorted <- etasq[ord]

  if (method == "topk") {
    k <- min(param$k, length(etasq_sorted))
    return(names(etasq_sorted)[seq_len(k)])
  } else
  if (method == "fixedetasqthresh") {
    thresh <- param$thresh
    keep <- etasq >= thresh
    keep[is.na(keep)] <- FALSE
    return(names(etasq)[keep][order(etasq[keep], decreasing = TRUE)])
  } else
  if (method == "percetasqthresh") {
    max_eta <- max(etasq, na.rm = TRUE)
    thresh_val <- (param$thresh / 100) * max_eta
    keep <- etasq >= thresh_val
    keep[is.na(keep)] <- FALSE
    return(names(etasq)[keep][order(etasq[keep], decreasing = TRUE)])
  }

  stop("Unknown method")
}



# Function to select leading variables for SURV
# (Use parallel = TRUE with the following for faster execution)
get_leadvars_SURV <- function(y, X, surv_model = c("AFT", "COX"), method = c("topk", "fixedmuthresh", "percmuthresh"), param, varsselected = NULL, varsleft = colnames(X), parallel = FALSE) {
  
	# only when user asks for parallel
  if (isTRUE(parallel)) {
    if (!requireNamespace("future", quietly = TRUE))
      stop("Set parallel=FALSE or install.packages('future')", call. = FALSE)
    if (!requireNamespace("future.apply", quietly = TRUE))
      stop("Set parallel=FALSE or install.packages('future.apply')", call. = FALSE)

    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)

    # set maxSize just for this call (restore afterwards)
    old_max <- getOption("future.globals.maxSize")
    on.exit(options(future.globals.maxSize = old_max), add = TRUE)
    options(future.globals.maxSize = 2 * 1024^3)

    future::plan(future::multisession)
  }

  method <- match.arg(method)
  surv_model <- match.arg(surv_model)
  if (!length(varsleft)) return(character(0))

  if (surv_model == "AFT") {
    # Build once
    dat   <- data.frame(time = y$time, event = y$status, X, check.names = FALSE)
    ySurv <- survival::Surv(dat$time, dat$event)

    base_X <- if (length(varsselected)) dat[, varsselected, drop = FALSE] else
              data.frame(row.names = seq_len(nrow(dat)))

    # Precompute NA mask shared across candidates (time, event, base vars)
    base_ok <- stats::complete.cases(dat$time, dat$event,
                                     if (length(varsselected)) base_X else NULL)

    fit1 <- function(j) {
      # Reuse base_X; add the candidate once
      df <- base_X
      df[[j]] <- dat[[j]]

      # Drop rows with NA in candidate, plus any already excluded by base_ok
      ok <- base_ok & stats::complete.cases(dat[[j]])
      if (!any(ok)) return(-Inf)

      # Identical behavior to original (same formula), but we pass subset=ok
      survival::survreg(
        ySurv ~ .,
        data  = df,
        subset = ok,
        model = FALSE, x = FALSE, y = FALSE
      )$loglik[2]
    }
    
    if (parallel) {
      if (!requireNamespace("future.apply", quietly = TRUE))
        stop("Set parallel=FALSE or install.packages('future.apply')")
      mu_results <- future.apply::future_sapply(varsleft, fit1, USE.NAMES = TRUE)
    } else {
      mu_results <- vapply(varsleft, fit1, numeric(1))
      names(mu_results) <- varsleft
    }
		
    sort_mu <- sort(mu_results, decreasing = TRUE)
    if (method == "topk") {
      return(head(names(sort_mu), param$k))
    } else if (method == "fixedmuthresh") {
      return(names(sort_mu)[abs(sort_mu) >= param$thresh])
    } else { # percmuthresh
      return(names(sort_mu)[sort_mu >= sort_mu[1] + (1-param$thresh/100) * sort_mu[1]])
    }
  }

  # -------- COX path (fast + identical to coxph) --------
  n  <- NROW(X)
  rn <- if (!is.null(rownames(X))) rownames(X) else as.character(seq_len(n))
  y_mat_full <- cbind(time = as.numeric(y$time), status = as.numeric(y$status))
  storage.mode(y_mat_full) <- "double"; rownames(y_mat_full) <- rn

  dat <- as.data.frame(X, check.names = FALSE)
  MM_base_full <- if (length(varsselected)) {
    stats::model.matrix(~ . - 1, data = dat[, varsselected, drop = FALSE])
  } else NULL
  if (!is.null(MM_base_full)) rownames(MM_base_full) <- rn
  wts_full <- rep(1, n); off_full <- rep(0, n)

  fit_one <- function(jname) {
    MM_j_full <- stats::model.matrix(~ . - 1, data = dat[, jname, drop = FALSE])
    rownames(MM_j_full) <- rn

    ok <- complete.cases(y_mat_full, if (!is.null(MM_base_full)) MM_base_full, MM_j_full)
    if (sum(ok) < 2 || sum(y_mat_full[ok, 2]) == 0) return(-Inf)

    X_ok <- if (is.null(MM_base_full)) MM_j_full[ok, , drop = FALSE]
            else cbind(MM_base_full[ok, , drop = FALSE], MM_j_full[ok, , drop = FALSE])

    # ensure proper type/shape
    X_ok <- as.matrix(X_ok)
    storage.mode(X_ok) <- "double"
    p <- ncol(X_ok)
    if (is.null(p) || p == 0) return(-Inf)

    survival::coxph.fit(
      x        = X_ok,
      y        = y_mat_full[ok, , drop = FALSE],
      strata   = rep.int(1L, sum(ok)),
      weights  = wts_full[ok],
      offset   = off_full[ok],
      init     = rep(0, p),                     # <- key change
      control  = survival::coxph.control(iter.max = 20L),
      method   = "efron",
      rownames = rn[ok]
    )$loglik[2]
  }

  if (parallel) {
    if (!requireNamespace("future.apply", quietly = TRUE))
      stop("Set parallel=FALSE or install.packages('future.apply')")
    mu_results <- future.apply::future_sapply(varsleft, fit_one, USE.NAMES = TRUE)
  } else {
    mu_results <- vapply(varsleft, fit_one, numeric(1))
    names(mu_results) <- varsleft
  }

  sort_mu <- sort(mu_results, decreasing = TRUE)
  if (method == "topk") {
    head(names(sort_mu), param$k)
  } else if (method == "fixedmuthresh") {
    names(sort_mu)[abs(sort_mu) >= param$thresh]
  } else {
    names(sort_mu)[exp(sort_mu) >= param$thresh/100 * exp(sort_mu[1])]
  }
}


# Function to select leading sets for all families
get_leadsets <- function(x_lead, X, method = c("topk","fixedcorthresh","perccorthresh"), param){
	method <- match.arg(method)
	sort_abscor <- sort(abs(cor(x_lead,X)[1,]),decreasing=T) # sorted absolute correlation of y with x vars
	vars <- switch(
		method,
		"topk" = {
			names(sort_abscor[1:param$k])
		},
		"fixedcorthresh" = {
			names(sort_abscor[sort_abscor >= param$thresh])
		},
		"perccorthresh" = {
			names(sort_abscor[sort_abscor >= param$thresh/100 * sort_abscor[1]])
		}
	)
	vars
}



# Function for variable selection for all families
VS_method <- function(y, X, family, surv_model = NULL, vsel_method, lambda=c("lambda.min","lambda.1se"), alpha = 0.5, p_thresh = 0.1, gamma = 0.9, verbose = FALSE){
	lambda <- match.arg(lambda)
	out <- switch(family,
		"normal"   = VS_method_LM(y = y, X = X, vsel_method = vsel_method, lambda=lambda, alpha=alpha, verbose = verbose),
		"binomial" = VS_method_GLM(y = y, X = X, vsel_method = vsel_method, lambda=lambda, alpha=alpha, verbose = verbose),
		"survival" = VS_method_SURV(y = y, X = X, surv_model = surv_model, vsel_method = vsel_method, lambda=lambda, alpha=alpha, verbose = verbose, p_thresh = p_thresh, gamma = gamma),
		stop("Unsupported family: ", family)
	)
	out
}



# Function for variable selection for LM
VS_method_LM <- function(y, X, vsel_method, lambda=c("lambda.min","lambda.1se"), alpha = 0.5, verbose = FALSE){
	lambda <- match.arg(lambda)
	if(verbose) cat("input:", colnames(X), "\n" )
	if(vsel_method=="NLP"){
		nlp_model = modelSelection( y, X, priorCoef=momprior(tau=0.2), priorModel=modelbbprior(1,1), niter=2000, center=T, scale=T, verbose=F )
		sel = colnames(X)[ which(nlp_model$postMode == 1) ] # collect the HPPM vars
	} else
	if(vsel_method %in% c("LASSO", "ENET")){
		if((vsel_method=="LASSO") && (ncol(X)>1)){
			lasso_model <- glmnet::cv.glmnet(as.matrix(X), y, alpha = 1)
			coef <- coef(lasso_model, s = lambda)
			sel <- names(which(coef[-1,1] != 0))
		} else 
		if((vsel_method=="ENET") && (ncol(X)>1)){
			enet_model <- glmnet::cv.glmnet(X, y, alpha = alpha)
			coef <- coef(enet_model, s = lambda)
			sel <- names(which(coef[-1,1] != 0))
		} else{
			lm_model <- lm(y ~ X)
			sel = if(summary(lm_model)$coefficients[2,"Pr(>|t|)"] < 0.01) colnames(X) else NULL
		}	
	} else
	if(vsel_method %in% c("SCAD","MCP")){
		if(ncol(X)>1){
			fit <- cv.ncvreg(X, y, alpha = 1, penalty = vsel_method)

			i.min <- fit$min
			thresh <- fit$cve[i.min] + fit$cvse[i.min]
			i.1se <- min(which(fit$cve <= thresh))
			lambda.1se <- fit$lambda[i.1se]

			coef <- coef(fit, lambda = lambda.1se)
			sel <- names(which(coef[-1] != 0))
		} else{
			lm_model <- lm(y ~ X)
			sel = if(summary(lm_model)$coefficients[2,"Pr(>|t|)"] < 0.01) colnames(X) else NULL
		}	
	}
	if(verbose) cat("selected :", sel, "\n" )
	nosel <- setdiff(colnames(X),sel)
	list(sel=sel,nosel=nosel)
}



# Function for variable selection for GLM
VS_method_GLM <- function(y, X, vsel_method, lambda=c("lambda.min","lambda.1se"), alpha = 0.5, verbose = FALSE) {
  lambda <- match.arg(lambda)
	if (verbose) cat("input:", colnames(X), "\n")

  sel <- NULL
  nosel <- NULL

  if(vsel_method=="NLP"){
		nlp_model = modelSelection( y, X, family="binomial", priorCoef=momprior(tau=0.2), priorModel=modelbbprior(1,1), niter=2000, center=T, scale=T, verbose=F )
		sel = colnames(X)[ which(nlp_model$postMode == 1) ] # collect the HPPM vars
	} else
	if(vsel_method %in% c("LASSO", "ENET")){
		if((vsel_method=="LASSO") && (ncol(X)>1)){
			lasso_model <- glmnet::cv.glmnet(as.matrix(X), y, family = "binomial", alpha = 1)
			coef <- coef(lasso_model, s = lambda)
			sel <- names(which(coef[-1,1] != 0))
		} else 
		if((vsel_method=="ENET") && (ncol(X)>1)){
			enet_model <- glmnet::cv.glmnet(as.matrix(X), y, family = "binomial", alpha = alpha)
			coef <- coef(enet_model, s = lambda)
			sel <- names(which(coef[-1,1] != 0))
		} else{
			lm_model <- glm(y ~ X, family = "binomial")
			sel = if(summary(lm_model)$coefficients[2,"Pr(>|z|)"] < 0.01) colnames(X) else NULL
		}	
	} else
	if(vsel_method %in% c("SCAD","MCP")){
		if(ncol(X)>1){
			fit <- cv.ncvreg(X, y, family = "binomial", alpha = 1, penalty = vsel_method)

			i.min <- fit$min
			thresh <- fit$cve[i.min] + fit$cvse[i.min]
			i.1se <- min(which(fit$cve <= thresh))
			lambda.1se <- fit$lambda[i.1se]

			coef <- coef(fit, lambda = lambda.1se)
			sel <- names(which(coef[-1] != 0))
		} else{
			lm_model <- glm(y ~ X, family = "binomial")
			sel = if(summary(lm_model)$coefficients[2,"Pr(>|z|)"] < 0.01) colnames(X) else NULL
		}	
	}
  nosel <- setdiff(colnames(X), sel)
  if (verbose) {
    cat("selected:", sel, "\n")
    # cat("Not selected variables:", nosel, "\n")
  }
  list(sel = sel, nosel = nosel)
}




# Function for variable selection for SURV
VS_method_SURV <- function(y, X, surv_model, vsel_method, lambda=c("lambda.min","lambda.1se"), alpha = 0.5, p_thresh = 0.1, gamma = 0.9, verbose = FALSE, ...){

  lambda <- match.arg(lambda)
	if (verbose) cat("Input:", colnames(X), "\n")
  
  time <- y$time
  delta <- y$status
	if(surv_model=="AFT"){
      if (vsel_method == "AFTREG") {
      	auxfit <- eha::aftreg( formula = Surv(time, delta) ~ ., data = as.data.frame(X))
      	p_values <- summary(auxfit)$coefficients[,"Wald p"]
      	sel <- names(p_values[p_values < p_thresh])
      } else if (vsel_method == "AFTGEE") {
      aft_model <- aftgee::aftgee(Surv(time, delta) ~ ., data = as.data.frame(X), id = seq_len(nrow(X)))
      p_values <- summary(aft_model)$coef[, "p.value"]
      sel <- names(p_values[p_values < p_thresh])
      sel <- setdiff(sel, "(Intercept)")
    }else if (vsel_method == "BRIDGE") {
      bridge_fit <- bridge_aft(y, X, gamma = gamma)
      beta_values <- bridge_fit$beta
      sel <- names(beta_values[beta_values != 0])
    }else if (vsel_method == "PVAFT"){
      data_pvaft <- as.data.frame(X)
      data_pvaft$time <- time  # Add survival time column
      data_pvaft$delta <- delta 
      pvaft_results <- afthd::pvaft(m = 1, n = ncol(X), STime = "time", Event = "delta", p = p_thresh, data = data_pvaft)
      sel <- rownames(pvaft_results)
    }
  } else if(surv_model=="COX"){
    if((ncol(X)>1) && (vsel_method == "LASSO")){
      surv_obj <- survival::Surv(time, delta)
      fit <- glmnet::cv.glmnet(x = as.matrix(X), y = surv_obj, family = "cox", alpha = 1, cox.ties = 'breslow')
      coefs <- coef(fit, s = lambda)
      sel <- rownames(coefs)[which(as.vector(coefs) != 0)]
    } else
    if((ncol(X)>1) && (vsel_method == "ENET")){
      surv_obj <- survival::Surv(time, delta)
      fit <- glmnet::cv.glmnet(x = as.matrix(X), y = surv_obj, family = "cox", alpha = alpha, cox.ties = 'breslow')
      coefs <- coef(fit, s = lambda)
      sel <- rownames(coefs)[which(as.vector(coefs) != 0)]
    } else{
      dat <- data.frame(time=time,event=delta,X)
      fit <- survival::coxph(formula = Surv(time,event) ~ X, data=dat)
      sel <- if(summary(fit)$waldtest['pvalue'] < 0.01) colnames(X) else NULL
    }
  }
    
  # Print selected variables
  if (verbose) cat("selected:", sel, "\n")
  
  # Identify non-selected variables
  nosel <- setdiff(colnames(X), sel)
  
  list(sel = sel, nosel = nosel)
}


# Aggregate selected variables for all families
select_vars <- function(listselect, method = c("conservative","liberal")) {
  method <- match.arg(method)
  k <- length(listselect)
  if (k == 0L) return(character(0))

  # duplicates don't affect set ops; removing them speeds up intersections
  listselect <- lapply(listselect, unique)

  if (method == "liberal") {
    return(unique(unlist(listselect, use.names = FALSE)))
  }

  # conservative: last non-empty intersection of the prefix {1..i}
  inter <- listselect[[1L]]
  if (length(inter) == 0L) return(character(0))
  best <- inter
  if (k >= 2L) {
    for (i in 2L:k) {
      inter <- intersect(inter, listselect[[i]])
      if (length(inter) == 0L) break
      best <- inter
    }
  }
  best
}



# Aggregate not-selected variables for removal in all families
remove_vars <- function(listnotselect, method = c("conservative_begin","conservative_end","liberal")) {
    method <- match.arg(method)
    k <- length(listnotselect)
    if (k == 0L) return(character(0))
    
    lst <- listnotselect
    
    if (method == "liberal") {
        return(unique(unlist(lst, use.names = FALSE)))
    } else
    if (method == "conservative_begin") {
        # Last non-empty intersection of prefixes {1..i}, i = 1..k
        inter <- lst[[1L]]
        empty_same <- inter[0]
        best <- if (length(inter)) inter else empty_same
        
        if (k >= 2L) {
            for (i in 2L:k) {
                if (!length(inter)) break
                xi <- lst[[i]]
                if (!length(xi)) {
                    inter <- empty_same
                } else {
                    inter <- intersect(inter, xi)
                    if (length(inter)) best <- inter
                }
            }
        }
        return(best)
    } else
    if(method == "conservative_end"){
        # Find rightmost non-empty set to act as the anchor for suffix intersections
        j <- k
        while (j >= 1L && !length(lst[[j]])) j <- j - 1L
        if (j < 1L) return(character(0))  # all sets empty
        
        # Build suffix intersections I_i = intersection of {i, i+1, ..., j}
        suf <- vector("list", j)
        suf[[j]] <- lst[[j]]
        if (j >= 2L) {
            for (i in (j-1L):1L) {
                if (!length(lst[[i]]) || !length(suf[[i+1L]])) {
                    suf[[i]] <- suf[[i+1L]][0]           # empty, same type
                } else {
                    suf[[i]] <- intersect(lst[[i]], suf[[i+1L]])
                }
            }
        }
        
        # Return the FIRST non-empty suffix intersection among {1..j}, {2..j}, {3..j}, ...
        for (i in 1L:j) {
            if (length(suf[[i]])) return(suf[[i]])
        }
        character(0)
    }
}


# Update response for all families
update_y <- function(y, X, family, vars, update_y_thresh = NULL){
	out <- switch(family,
		"normal" = update_y_LM(y=y, X=X, vars=vars),
		"binomial" = update_y_GLM(y=y, X=X, vars=vars, update_y_thresh=update_y_thresh),
		stop("Unsupported family: ", family)
	)
	out
}



# Update response for LM
update_y_LM <- function(y, X, vars){
	res <- lm(y ~ X[,vars,drop=F])$residuals
	res
}



# Update response for GLM
update_y_GLM <- function(y, X, vars, update_y_thresh = 0.5) {
  fit <- glm(y ~ X[, vars, drop = F], family = binomial())
  predicted_probs <- predict(fit, type = "response")
  updated_y <- ifelse(abs(y - predicted_probs) > update_y_thresh, y, round(predicted_probs))
  updated_y
}



# Loop check
looprun <- function(varsselected, varsleft, max_nocollect, m, nskip){
	ind <- ifelse( (length(varsselected) < m) && (length(varsleft) > 0) && (max_nocollect < nskip), 1, 0 )
	ind
}



# Prediction after S3VS for all families
pred_S3VS <- function(y, X, family, surv_model = NULL, method){
	out <- switch(family,
		"normal" = pred_S3VS_LM(y, X, method),
		"binomial" = pred_S3VS_GLM(y, X, method),
		"survival" = pred_S3VS_SURV(y, X, surv_model, method),
		stop("Unsupported family: ", family)
	)
	out
}



# Prediction after S3VS for LM
pred_S3VS_LM <- function(y, X, method){
	if(method=="NLP"){
		fitsel <- modelSelection::rnlp(y=y, x=X, outcometype="Continuous", family="normal", priorCoef=momprior(tau=0.2), priorVar=igprior(alpha=.01,lambda=.01))  #posterior samples of coefs
	    coef <- apply(fitsel[,-ncol(fitsel)],2,mean) #estimate is mean of posterior samples
	    y.pred <- c(X %*% coef)
	} else
	if(method=="LASSO"){
		fitsel <- glmnet::cv.glmnet(X, y, alpha = 1)
		coef <- coef(fitsel, s="lambda.1se")[-1,1]
		y.pred <- predict(fitsel,newx=X,s="lambda.1se")[,1]
	} else
	if(method %in% c("SCAD","MCP")){
		fitsel <- ncvreg::cv.ncvreg(X, y)
		i.min <- fitsel$min
		thresh <- fitsel$cve[i.min] + fitsel$cvse[i.min]
		i.1se <- min(which(fitsel$cve <= thresh))
		lambda.1se <- fitsel$lambda[i.1se]
		coef <- coef(fitsel, lambda=lambda.1se)[-1]
		y.pred <- predict(fitsel,X=X,lambda=lambda.1se)
	}
	mget(c("coef","y.pred"))
}



# Prediction after S3VS for GLM
pred_S3VS_GLM <- function(y, X, method=c("NLP","LASSO")){
  method <- match.arg(method)
  if(method=="NLP"){
    # Assuming rnlp can handle logistic regression, otherwise replace with an appropriate function
    fitsel <- modelSelection::rnlp(y=y, x=X, outcometype="Binary", family="binomial", priorCoef=momprior(tau=0.2), priorVar=igprior(alpha=.01,lambda=.01))
    coef <- apply(fitsel[,-ncol(fitsel)], 2, mean) # Estimate is mean of posterior samples
    y.pred <- 1 / (1 + exp(- (X %*% coef)))  # Logistic function to get probabilities
  } else if(method=="LASSO"){
    if(ncol(X) > 1){
      fitsel <- glmnet::cv.glmnet(X, y, family="binomial", alpha=1)
      coef <- coef(fitsel, s="lambda.1se")[-1, 1]
      y.pred <- predict(fitsel, newx=X, s="lambda.1se", type="response") # Get probabilities
    } else {
      fitsel <- glm(y ~ X, family=binomial())
      coef <- coef(fitsel)[-1]
      y.pred <- fitsel$fitted.values  # Probability predictions
    }
  }
  list(coef=coef, y.pred=y.pred)
}



# Prediction after S3VS for SURV
pred_S3VS_SURV <- function(y, X, surv_model = c("AFT","COX"), method = c("AFTREG","AFTGEE","COXGLMNET")){
	surv_model <- match.arg(surv_model)
	method <- match.arg(method)
	time <- y$time
	delta <- y$status
  if(surv_model=="AFT"){
  	if(method=="AFTREG"){
  		fit <- eha::aftreg(Surv(y$time, y$status) ~ ., data = as.data.frame(X))
  		coef <- summary(fit)$coefficients[,1]
  		y.pred <- as.vector( exp(-(y$time/(fit$scale * exp(fit$linear.predictors)))^(fit$shape)) )
  	} else if(method=="AFTGEE"){
  		fit <- aftgee::aftgee(Surv(y$time, y$status) ~ ., data = as.data.frame(X), id = seq_len(nrow(X)))
  		coef <- fit$coefficients[-1,2]
  		Xmat <- model.matrix(~ ., data = as.data.frame(X))
			beta <- fit$coefficients[,2]
			eta <- as.vector(Xmat %*% beta)
			resid_train <- log(y$time) - eta
			km0 <- survival::survfit(survival::Surv(resid_train, y$status) ~ 1)
			S0fun <- function(u) {
			  approx(
			    km0$time, km0$surv,
			    xout = u,
			    method = "constant",
			    yleft = 1,
			    yright = tail(km0$surv, 1)
			  )$y
			}
			y.pred <- S0fun(log(y$time) - eta)
  	} 
  } else
  if(surv_model=="COX"){
  	if(ncol(X)>1){
      if (method == "COXGLMNET") {
        surv_obj <- Surv(time, delta)
		    fitsel <- glmnet::cv.glmnet(x = as.matrix(X), y = surv_obj, family = "cox", alpha = 1, cox.ties = 'breslow')
		    coef <- coef(fitsel, s="lambda.1se")[-1,1]
		    y.pred <- predict(fitsel, newx=X, s="lambda.1se", type="response")
      }
    } else{
      dat <- data.frame(time=time,event=delta,X)
      fit <- survival::coxph(formula = Surv(time,event) ~ X, data=dat)
      coef <- fit$coefficients
      y.pred <- predict(fit,type="risk")
    }
  }
  mget(c("coef","y.pred"))
}



# Bridge fit for AFT SURV
bridge_aft <- function(y, X, gamma = 0.5, alpha = 1, max_iter = 100, tol = 1e-5) {
  
  time <- y$time
  delta <- y$status

  n <- length(time)
  p <- ncol(X)
  
  # Log-transform survival times for AFT
  log_time <- log(time)
  
  # Initial weights (Kaplan-Meier weights)
  # km_fit <- survival::survfit(survival::Surv(time, delta) ~ 1)
  # km_weights <- rep(1, n)  # For simplicity, set weights to 1
  
  # Initial LASSO fit to get starting beta
  lasso_fit <- glmnet::cv.glmnet(X, log_time, family = "gaussian", alpha = alpha, standardize = TRUE)
  beta_init <- as.vector(coef(lasso_fit, s = lasso_fit$lambda.1se))[-1] 
  
  beta_old <- beta_init
  
  # Iterative reweighted LASSO to approximate bridge penalty
  for (iter in 1:max_iter) {
    # Compute adaptive weights based on bridge penalty
    w_bridge <- (abs(beta_old) + 1e-6)^(gamma - 1)
    
    # Apply weights to covariates
    X_weighted <- scale(X, center = FALSE, scale = 1 / w_bridge)
    
    # Fit weighted LASSO
    lasso_fit <- glmnet::cv.glmnet(X_weighted, log_time, family = "gaussian", alpha = alpha)
    beta_new <- as.vector(coef(lasso_fit, s = lasso_fit$lambda.1se))[-1] / w_bridge  # Rescale back
    
    # Check convergence
    if (sum(abs(beta_new - beta_old)) < tol) {
      message(paste("Converged in", iter, "iterations"))
      break
    }
    
    beta_old <- beta_new
  }
  
  # Return results
  list(beta = beta_new, gamma = gamma, iterations = iter)
}














































