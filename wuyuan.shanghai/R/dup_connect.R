dup_connect<-function (data, id, dup.var)
{
    for (i in 1:length(id)) {
        if (i == 1) {
            res.id = data[, id[i]]
        }
        else {
            res.id = paste0(res.id, data[, id[i]])
        }
    }
    data.id = data[, id]
    if (length(id) == 1) {
        data.id = data.frame(data.id)
        colnames(data.id) = id
    }
    data.u = do:::row.freq(data.id)
    res.id.u = unique(res.id)
    for (i in 1:length(dup.var)) {
        dup.i = dup.var[i]
        for (j in 1:length(res.id.u)) {
            if (j == 1)
                res.j = NULL
            x.j = data[res.id == res.id.u[j], dup.i]
            res.j = c(res.j, paste0(x.j, collapse = ";"))
        }
        if (i == 1) {
            res = res.j
        }
        else {
            res = cbind(res, res.j)
        }
    }
    res = data.frame(res)
    colnames(res) = dup.var
    cbind(data.u, res)
}
