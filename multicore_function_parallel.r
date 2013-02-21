# Matthew Leonowicz written multicore function for parallelization
mcf <- function(fun=f,iters,clusters=8,export=ls(),...){
        assign("on",Sys.time(),pos=1)
        require(parallel)
        assign("fun",fun,pos=1)
        assign("iters",iters,pos=1)
        assign("clusters",clusters,pos=1)
        assign("cl",makeForkCluster(clusters),pos=1)
        assign("export",export,pos=1)
        clusterExport(cl,export)
        out <- clusterApply(cl=cl,x=iters,fun=fun,...)
        stopCluster(cl)
        assign("off",Sys.time(),pos=1)
        print(difftime(off,on))
        rm(fun,iters,clusters,cl,export,on,off,pos=1)
        return(out)
} 