lsmeans = function(object, specs, contr=list(), at, cov.reduce = function(x, name) mean(x), 
                   fac.reduce = function(coefs, lev) apply(coefs, 2, mean), ...) {
    if (missing(specs)) stop("Must specify specs, e.g. 'pairwise ~ treatment'")
    
    Terms = delete.response(terms(object))
    
    form = Terms[[2]]
    if (inherits(object, "mer")) {
        for (i in 1:length(form))
            if (substr(as.character(form[i]),1,1) == "(") form[i] = NULL
        thecall = slot(object, "call")
        bhat = fixef(object)
        contrasts = attr(model.matrix(object), "contrasts")
    }
    else if (inherits(object, "lme")) {
        thecall = object$call
        bhat = fixef(object)
        contrasts = object$contrasts
    }
    else {
        thecall = object$call
        bhat = coef(object)
        contrasts = attr(model.matrix(object), "contrasts")
    }
    V = vcov(object)
    
    nm = all.vars(form)
    form = as.formula(paste("~", paste(nm, collapse = "+")))
    envir = attr(Terms, ".Environment")
    X = model.frame(form, eval(thecall$data, envir=envir), 
                    subset = eval(thecall$subset, envir=envir))
    baselevs = xlev = list()
    if (!missing(specs) && !is.list(specs)) specs = list(specs)
    for (i in 1:length(X))
        if (is.factor(X[[i]])) {
            baselevs[[i]] = levels(X[[i]])
            xlev = c(xlev, baselevs[[i]])
        }
    else {
        xname = names(X)[i]
        if (!missing(at) && !is.null(at[[xname]]))
            baselevs[[i]] = at[[xname]]
        else 
            baselevs[[i]] = cov.reduce(X[[i]], xname)
    }
    names(baselevs) = names(X)
    grid = do.call("expand.grid", baselevs)
    m = model.frame(Terms, grid, na.action = na.pass, xlev = xlev)
    X = model.matrix(Terms, m, contrasts.arg = contrasts)
    
    results = list()
    for (i in 1:length(specs)) {
        form = specs[[i]]
        if (!inherits(form, "formula"))
            stop("Desired lsmeans should be expressed in formulas")
        method = byfacs = NULL
        if (length(form) == 3) { # no lhs
            method = all.vars(form[[2]])[1]
            form = form[-2]
        }
        facs = all.vars(form)
        b = strsplit(as.character(form[2]), "\\|")[[1]]
        if (length(b) > 1) byfacs = all.vars(as.formula(paste("~",b[2])))
        levs = list()
        for (f in facs) levs[[f]] = baselevs[[f]]
        combs = do.call("expand.grid", levs)
        K = apply(combs, 1, function(lev) {
            matches = apply(grid, 1, function(row) {
                all(row[facs] == lev)
            })
            nmat = sum(matches)
            if (nmat == 0) stop(paste("Can't predict at", lev))
            else fac.reduce(X[matches, , drop=FALSE], lev)
        })
        # each COLUMN of K has the linear comb we want to predict
        dimnames(K)[[2]] = apply(combs, 1, paste, collapse=",")
        # Here is the fcn I'll call to table an estimate of k'beta
        do.est = function(k) {
            lsm = sum(k * bhat)
            se = sqrt(sum(k * (V %*% k)))
            trat = lsm / se
            c(estimate=lsm, SE=se, t.ratio=trat)
        }
        
        # LS means
        nm = paste(facs, collapse=":")
        lsms = as.data.frame(t(apply(K,2,do.est)))
        rnames = row.names(lsms)
        results[[paste(nm, "lsmeans")]] = lsms
        
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
                    if (length(byfacs)>1) flags = apply(combs[ ,byfacs], 1, function(r) all(r==bylevs))
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
                clx = lapply(cl, function(cc) {
                    ccc = zer; ccc[rows]=cc; ccc
                })
                if (nby > 1) names(clx) = paste(names(clx), "|", bylabs[i])
                Clist = c(Clist, clx)
            }
            methdesc = attr(cl, "desc")
            if (is.null(methdesc)) methdesc = method
            results[[paste(nm,methdesc)]] = as.data.frame(t(sapply(Clist, function(con) {
                k = K %*% con
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

