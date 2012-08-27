lsmeans = function(object, specs, contr=list(), at, cov.reduce = function(x, name) mean(x), 
                   fac.reduce = function(coefs, lev) apply(coefs, 2, mean), 
                   check.cells = TRUE, ...) 
{
    
    if (missing(specs)) stop("Must specify specs, e.g. 'pairwise ~ treatment'")
    
# Get RHS of model formula
    if (class(object) == "gls")
        Terms = getCovariateFormula(object)
    else
        Terms = delete.response(terms(object))
    # get the pure formula w/o extra stuff
    formrhs = formula(Terms)
    
# Figure out thecall (fixed effects part of model), bhat (=coefs), contrasts attr
    if (inherits(object, "mer")) {
        thecall = slot(object, "call")
        bhat = fixef(object)
        contrasts = attr(model.matrix(object), "contrasts")
    }
    else if (inherits(object, "lme")) {
        thecall = object$call
        bhat = fixef(object)
        contrasts = object$contrasts
    }
    else if (inherits(object, "gls")) {
        thecall = object$call
        bhat = coef(object)
        contrasts = object$contrasts        
    }
    else {
        thecall = object$call
        bhat = coef(object)
        contrasts = attr(model.matrix(object), "contrasts")
    }
    
    # Fixed-effects covariance matrix -- Happily, vcov works the same way for lm, lme, lmer
    V = vcov(object)
    
    # We'll work only with the non-NA elements of bhat
    used = which(!is.na(bhat))
    bhat = bhat[used]
    # should match vcov...
    if (length(bhat) != nrow(V)) stop("Something's wrong -- Mismatch between vcov() and non-missing coef() results")
    
    # All the variables in the model
    nm = all.vars(formrhs)
    
# Figure out if any are coerced to factor or ordered
    anm = all.names(formrhs)    
    coerced = anm[1 + grep("factor|ordered", anm)]
    
# Obtain a simplified formula -- needed to recover the data in the model    
    form = as.formula(paste("~", paste(nm, collapse = "+")))
    envir = attr(Terms, ".Environment")
    X = model.frame(form, eval(thecall$data, envir=envir), 
                    subset = eval(thecall$subset, envir=envir))
    # Now X contains the data used to fit the model, w/o any expansions (e.g. poly() calls)

# Start accumulating info for the vars. 
# baselevs has the levels of all factors, or the "at" values for all covariates
# xlev has the factor levels only, for use in model.frame and check.cells calls
    baselevs = xlev = matdat = list()
    # allow a vector of character strings
    if (is.character(specs)) specs = as.list(specs)
    # allow a single formula
    if (!is.list(specs)) specs = list(specs)
    
    for (xname in names(X)) {
        obj = X[[xname]]
        if (is.factor(obj)) {            
            baselevs[[xname]] = xlev[[xname]] = levels(obj)
        }
        else if (is.matrix(obj)) {
            # Matrices -- reduce columns thereof, but don't add to baselevs
            matdat[[xname]] = apply(obj, 2, cov.reduce, xname)
        }
        else {
            # single numeric pred but coerced to a factor - use unique values
            if (length(grep(xname, coerced)) > 0)             
                 baselevs[[xname]] = sort(unique(obj))
                
            # Ordinary covariates - summarize if not in 'at' arg
            else {
                if (!missing(at) && !is.null(at[[xname]]))
                    baselevs[[xname]] = at[[xname]]
                else 
                    baselevs[[xname]] = cov.reduce(obj, xname)
            }
        }
    }
    # OK. Now make a grid of the factor levels of interest, along w/ covariate "at" values
    grid = do.call("expand.grid", baselevs)
    # add any matrices
    for (nm in names(matdat))
        grid[[nm]] = matrix(rep(matdat[[nm]], each=nrow(grid)), nrow=nrow(grid))

    # It turns out that numerics coerced to factors are a real pain in the butt when it comes
    # to matching levels. Life will be simpler if we turn them into factors in the X matrix 
    # and update the base levels accordingly with the same labels
    for (var in coerced) {
        X[[var]] = factor(X[[var]])
        baselevs[[var]] = levels(X[[var]])
    }
    

    # Let's also get the list of empty cells while we have X
    if (check.cells) empties = empty.cells(formrhs, X)
    else empties = list()
    
    # Now make a new dataset with just the factor combs and covariate values we want for prediction
    # WARNING -- This will overwrite X, so get anything you need from X BEFORE we get here
    m = model.frame(Terms, grid, na.action = na.pass, xlev = xlev)
    X = model.matrix(Terms, m, contrasts.arg = contrasts)
    # use only the columns with non-missing regr coefs
    X = X[ , used]
    
    # If necessary revise grid with corced numeric factors replaced with factor levels
    if (length(coerced) > 0) grid = do.call("expand.grid", baselevs)
    
    # All factors (excluding covariates)
    allFacs = c(names(xlev), coerced)
    
    
    
    # Get a vector of terms in the model, for checking
    mod.terms = strsplit(as.character(formrhs[2])[[1]], "\\+")[[1]]
    
    # routine returns TRUE iff all elements of facs are contained in a model term with another predictor
    some.term.contains = function(facs) {
        for (trm in mod.terms) {
            flag = all(sapply(facs, function(f) length(grep(f,trm))>0))
            if (flag) 
                if (length(all.vars(as.formula(paste("~",trm)))) > length(facs)) 
                    return(TRUE)
        }
        return(FALSE)
    }
    
    
    # Initialize a list to hold the results to return
    results = list()
    for (i in 1:length(specs)) {
        form = specs[[i]]
        # convert a string to a formula
        if (is.character(form)) form = as.formula(paste("~",form))
        if (!inherits(form, "formula"))
            stop(paste("Incorrect formula specification:", form))
        method = byfacs = NULL
        if (length(form) == 3) { # no lhs
            method = all.vars(form[[2]])[1]
            form = form[-2]
        }
        
        # These are the variables involved; and the label to use in the results
        facs = all.vars(form)
        facs.lbl = paste(facs, collapse=":")
        if (some.term.contains(facs)) 
            warning(paste("lsmeans of",facs.lbl,
                          "may be misleading due to interaction with other predictor(s)"))
        
        ln = if (any(sapply(facs, function(nm) length(grep(nm, allFacs)) == 0)))
            stop(paste("Unknown factor(s) in specification:", paste(form, collapse=" ")))
        
        # identify "by" factors (after "|" in formula)
        b = strsplit(as.character(form[2]), "\\|")[[1]]
        if (length(b) > 1) byfacs = all.vars(as.formula(paste("~",b[2])))
        
        # create the grid of factor combinations
        levs = list()
        for (f in facs) levs[[f]] = baselevs[[f]]
        combs = do.call("expand.grid", levs)
        
        # For each comb, find the needed lin. comb. of bhat to estimate
        # (These will end up being the COLUMNS of K)
        K = apply(combs, 1, function(lev) {
            matches = apply(grid, 1, function(row) {
                #### DEL if (is.numeric(lev)) all(abs(as.numeric(row[facs]) - lev) < .001)  else 
                all(row[facs] == lev)
            })
            nmat = sum(matches)
            if (nmat == 0) stop(paste("Can't predict at level", lev, "of", "facs.lbl"))
            else fac.reduce(X[matches, , drop=FALSE], lev)
        })
        dimnames(K)[[2]] = apply(combs, 1, paste, collapse=", ")
        
# Toss a NA in any entry where levels are a subset of empty-cell levels...
        for (emp in empties) {
            ne = names(emp)
            if (all(sapply(facs, function(f) f %in% ne))) {
                testset = emp[, facs, drop=FALSE]   # only the subset of factors
                flags = apply(combs, 1, function(row) {
                    any(apply(testset, 1, function(tst) all(row==tst)))
                })
                for (j in which(flags)) K[1,j] = NA
            }
        }
        
        # Here is the fcn I'll call to table an estimate of k'beta
        do.est = function(k) {
            if (any(is.na(k))) 
                lsm = se = trat = NA
            else {
                lsm = sum(k * bhat)
                se = sqrt(sum(k * (V %*% k)))
                trat = lsm / se
            }
            c(estimate=lsm, SE=se, t.ratio=trat)
        }
        
        # LS means
        lsms = as.data.frame(t(apply(K,2,do.est)))
        rnames = row.names(lsms)
        results[[paste(facs.lbl, "lsmeans")]] = lsms
        
        # Do requested contrasts
        if (! is.null(method)) {
            fn = paste(method, "lsmc", sep=".")
            confcn = if (exists(fn, mode="function")) get(fn) 
                else NULL
            if (is.null(byfacs)) bylist = list(1:nrow(combs))
            else {
                bg = list()
                for (f in byfacs) bg[[f]] = baselevs[[f]]
                bygrid = do.call("expand.grid", bg)
                bylist = lapply(1:nrow(bygrid), function(row) {
                    bylevs = bygrid[row,]
                    if (length(byfacs)>1) flags = apply(combs[ , byfacs], 1, function(r) all(r==bylevs))
                    else flags = combs[,byfacs] == bylevs
                    which(flags)
                })
                bylabs = apply(bygrid, 1, paste, collapse=",")
                bycols = sapply(byfacs, grep, names(combs))
                rnames = combs[ ,-bycols]
                if (!is.null(ncol(rnames))) rnames = apply(rnames, 1, paste, collapse=",")
            }
            Clist = list()
            zer = rep(0, nrow(lsms))
            nby = length(bylist)
            for (i in 1:nby) {
                rows = bylist[[i]]
                cl = if(is.null(confcn)) contr[[method]] 
                    else confcn(rnames[rows] , ...)
                if (is.null(cl)) stop(paste("Unknown contrast family:", method))
                clx = lapply(cl, function(cc) {
                    ccc = zer; ccc[rows]=cc; ccc
                })
                if (nby > 1) names(clx) = paste(names(clx), "|", bylabs[i])
                Clist = c(Clist, clx)
            }
            methdesc = attr(cl, "desc")
            if (is.null(methdesc)) methdesc = method
            results[[paste(facs.lbl,methdesc)]] = as.data.frame(t(sapply(Clist, function(con) {
                nz = which(abs(con) > .0001)
                k = K[ , nz] %*% con[nz]
                do.est(k)
            })))
        }
    }
    results
}


### functions to implement different families of contrasts
### All return a matrix or data frame whose columns are the desired contrasts coefs
### with appropriate row and column names

# all pairwise trt[i] - trt[j], i < j
pairwise.lsmc = function(levs,...) {
    k = length(levs)
    M = data.frame(levs=levs)
    for (i in 1:(k-1)) {
        for (j in (i+1):k) {
            con = rep(0,k)
            con[i] = 1
            con[j] = -1
            nm = paste(levs[i], levs[j], sep = " - ")
            M[[nm]] = con
        }
    }
    row.names(M) = levs
    M = M[-1]
    attr(M, "desc") = "pairwise differences"
    M
}

# all pairwise trt[j] - trt[i], j > i
revpairwise.lsmc = function(levs,...) {
    k = length(levs)
    M = data.frame(levs=levs)
    for (i in 2:k) {
        for (j in 1:(i-1)) {
            con = rep(0,k)
            con[i] = 1
            con[j] = -1
            nm = paste(levs[i], levs[j], sep = " - ")
            M[[nm]] = con
        }
    }
    row.names(M) = levs
    M = M[-1]
    attr(M, "desc") = "pairwise differences"
    M
}

# Poly contrasts - scaled w/ integer levels like most tables
# ad hoc scaling works for up to 13 levels
poly.lsmc = function(levs, max.degree=min(6,k-1)) {
    nm = c("linear", "quadratic", "cubic", "quartic", paste("degree",5:20))
    k = length(levs)
    M = as.data.frame(poly(1:k, min(20,max.degree)))
    for (j in 1:ncol(M)) {
        con = M[ ,j]
        pos = which(con > .01)
        con = con / min(con[pos])
        z = max(abs(con - round(con)))
        while (z > .05) {
            con = con / z
            z = max(abs(con - round(con)))
        }
        M[ ,j] = round(con)
    }
    row.names(M) = levs
    names(M) = nm[1:ncol(M)]
    attr(M, "desc") = "polynomial contrasts"
    M
}

# All comparisons with a control
trt.vs.ctrl.lsmc = function(levs, ref=1) {
    k = length(levs)
    M = data.frame(levs=levs)
    for (i in 1:k) {
        if (i == ref) next
        con = rep(0,k)
        con[i] = 1
        con[ref] = -1
        nm = paste(levs[i], levs[ref], sep = " - ")
        M[[nm]] = con
    }
    row.names(M) = levs
    M = M[-1]
    attr(M, "desc") = "differences from control"
    M
}

# control is 1st level
trt.vs.ctrl1.lsmc = function(levs, ...) {
    trt.vs.ctrl.lsmc(levs, ref=1)
}

# control is last level
trt.vs.ctrlk.lsmc = function(levs, ...) {
    trt.vs.ctrl.lsmc(levs, ref=length(levs))
}

