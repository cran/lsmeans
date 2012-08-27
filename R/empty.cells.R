# This function checks for empty cells 
# that also appear together in a term of form (which is rhs of model)
# Then it tabulates each such case and identifies empty cells

empty.cells = function(form, data) {
    # Create a matrix with all possible factor combinations
    facnames = all.vars(form)
    gl = list()
    if (length(facnames) < 2) return (gl)
    for (nm in facnames) gl[[nm]] = c(FALSE,TRUE)
    g = do.call("expand.grid", gl)
    combs = apply(g[-1,], 1, function(r) facnames[r])

    # Also get a list of factors in each term
    tm = strsplit(as.character(form[length(form)])[[1]], "\\+")[[1]]
    trms = lapply(tm, function(txt) all.vars(as.formula(paste("~",txt))))

    # OK, now find out which of combs is contained in one or more trms
    combs.used = list()
    for (cm in combs){
        flags = rep(TRUE, length(trms))
        for(nm in cm) {
            tst = unlist(sapply(trms, function(tm) length(grep(nm,tm)) > 0))
            flags = flags & tst
        }
        if (any(flags)) combs.used[[paste(cm,collapse=":")]] = cm  
    }

    tbls = with(data, lapply(combs.used, function(x) do.call("table", sapply(x,as.name))))
    result = list()
    idx = 1
    
    for (tbl in tbls) {
        empties = which(tbl==0)
        if(length(empties) > 0) {
            result[[idx]] = do.call("expand.grid", dimnames(tbl))[empties, ]
            idx = idx + 1
        }
    }
    result
}
