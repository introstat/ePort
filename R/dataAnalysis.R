globalVariables(c("txtProgressBar", "setTxtProgressBar", "na.omit", "hclust", "dist", "cutree", "combn", "pt", "p.adjust"))

#' Flag the unusual questions
#' 
#' Two methods are provided to detect the unusual questions.
#' See \code{method} for details.
#' 
#' @param set a vector to identify the question set
#' @param value a numeric vector, usually is the correct percentage.
#' @param method "cluster" or "outlier". "cluster" uses hierarchical 
#' clustering method, and cut the tree by the parameter \code{thres}.
#' "outlier" will mark the values which differ from any other values
#' in the same set by \code{thres}.
#' @param thres the threshold of the height of hclust.
#' @return a vector of asterisks to mark the questions
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' 
flag = function(set,value,method="outlier",thres){
  stopifnot(length(set)==length(value))
  uniset = unique(set)
  res = rep(0,length(set))
  for (i in 1:length(uniset)){
    idx = which(set == uniset[i])
    if (length(idx)<2) next
    crtv = value[idx]
    if (method=="cluster"){
      m=hclust(dist(crtv))
      if (any(m$height > thres)){
        # plot(hclust(dist(crtv)),labels=as.character(crtv))
        memb = cutree(m, k=sum(m$height>thres)+1)
        res[idx] = order(order(tapply(crtv,memb,mean),decreasing=TRUE))[memb]-1
      }
    } else {
      m=as.matrix(dist(crtv))>=thres
      outl = sort(unique(which(m) %% length(crtv)))
      outl[outl==0] = length(crtv)
      res[idx[outl]] = 1
    }
  }
  return(sapply(res, function(s) paste(rep('*',s),collapse='')))
}


#' Run the multiple comparison
#' 
#' @param est the estimated values of the fixed effects
#' @param estcov the covariance matrix of the fixed effects
#' @param coefname a vector of all levels (should contain one more level than est)
#' @param n the number of rows in the design matrix of the model
#' @param ordermatrix logical. If coefname should be ordered in the matrix or not.
#' @param method the multiple comparison methods. See ?p.adjust
#' @return a matrix of p-values between each pair of levels
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' 
multipleComparison = function(est,estcov,coefname,n,ordermatrix=TRUE,method="bonferroni"){
  stopifnot(length(coefname)==(length(est)+1))
  r=length(coefname)
  idx=t(combn(r,2)-1)
  res=matrix(coefname[idx+1],ncol=2)
  cmtrx=matrix(0,nrow=nrow(idx),ncol=r-1)
  for (i in 1:nrow(idx)){
    cmtrx[i, idx[i,1]] = 1
    cmtrx[i, idx[i,2]] = -1
  }
  compmean=(cmtrx%*%est)[,1]
  compsd=sqrt(diag(cmtrx%*%estcov%*%t(cmtrx)))
  pdist=pt(compmean/compsd,df=n-r+1)
  pvalues=2*pmin(pdist,1-pdist)
  multicomp=p.adjust(pvalues, method=method)
  res=data.frame(res,pvalue=multicomp,stringsAsFactors=FALSE)
  pmtrx=matrix(0,ncol=r,nrow=r)
  ordname = if(ordermatrix){coefname[order(c(0,est),decreasing=T)]}else{coefname}
  colnames(pmtrx)=ordname
  rownames(pmtrx)=ordname
  for (i in 1:nrow(res)) pmtrx[res[i,2],res[i,1]]=round(res[i,3],4)
  pmtrx=pmtrx+t(pmtrx)
  pmtrx[upper.tri(pmtrx,diag=TRUE)]=NA
  pmtrx=pmtrx[-1,-r,drop=FALSE]
  return(pmtrx)
}


#' Cluster students by performance
#' 
#' @param dat Data frame with rows being the students and
#' columns being the scores over the semester
#' @param l Maximum number of clusters to try
#' @return a Data frame with the left part being the scaled
#' \code{dat} and the right part being the clustering labels
#' for 1:\code{l} clusters
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' 
clusterStudents = function(dat,l=9){
  nc = ncol(dat)
  dat = na.omit(dat)
  clust_dat = t(apply(dat,1,function(x){
    if (all(x==x[1])) return(rep(0,nc)) else return(scale(x)[,1])
  }))
  colnames(clust_dat) = colnames(dat)
  hc = hclust(dist(clust_dat),method='ward.D')
  clhc = data.frame(clust_dat,cl1=1L)
  for (i in 2:l){
    clhc[,nc+i] = cutree(hc, i)
  }
  colnames(clhc)[1:l+nc]=paste('cl',1:l,sep='')
  clhc = clhc[,c(1:l+nc,1:nc)]
  attr(clhc,"height") = rev(hc$height)[1:l]
  return(clhc)
}


#' Calculate the criterion values for different number of clusters
#' 
#' The criteria include SSR, SSE, and R-squared
#' 
#' @param dat Output of function \code{cl}
#' @param ntopic Number of topics
#' @param l Number of clusters
#' @return Data frame of three columns: cluster, criteria, and values
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @importFrom reshape2 melt
#' @export
#' 
clusterCriteria = function(dat,ntopic,l=9){
  res = data.frame(SSR=rep(0,l),SSE=rep(0,l))
  rownames(res) = paste('cluster',1:l,sep='')
  globalmean = colMeans(dat[,1:ntopic+l])
  SST = sum(apply(dat[,1:ntopic+l],1,function(x)sum((x-globalmean)^2)))
  res[1,]=c(0,SST)
  for (i in 2:l) {
    tmp = dat[,c(i,1:ntopic+l)]
    groupmean = by(tmp[,-1],tmp[,1],colMeans)
    groupsize = table(tmp[,1])
    SSR = sum(unlist(lapply(groupmean,function(x){sum((x-globalmean)^2)}))*groupsize)
    SSE = sum(apply(tmp,1,function(x){sum((x[-1]-groupmean[[x[1]]])^2)}))
    res[i,] = c(SSR, SSE)
  }
  res$Rsquare = res$SSR / SST
  res$cluster = 1:l
  res2 = reshape2::melt(res[-1,-1],id.vars='cluster')
  colnames(res2)[2] = 'criteria'
  return(res2)
}


#' Use bootstrap to find the best number of clusters
#' 
#' Results of the function \code{clusterCriteria} for bootstrap samples
#' 
#' @param dat Data frame, same format as function \code{cl}
#' @param nt Number of topics
#' @param nboot Number of bootstrap samples
#' @param l Number of clusters
#' @param seed Random seed
#' @param txtbar Whether or not the text progress bar is turned on
#' @return Data frame of the criteria values for \code{nboot} samples
#' @author Xiaoyue Cheng <\email{xycheng@@iastate.edu}>
#' @export
#' 
clusterBootstrap = function(dat,nt,nboot=100,l=9,seed=201311,txtbar=TRUE){
  set.seed(seed)
  n = nrow(dat)
  resboot = matrix(0,nrow=2*(l-1),ncol=nboot)
  if (txtbar) pb = txtProgressBar(min = 0, max = nboot, style = 3, label="Bootstrap iteration ...")
  for (s in 1:nboot){
    tmpsample = sample(1:n,replace=TRUE)
    tmpdat = dat[tmpsample,]
    clboot = clusterStudents(tmpdat,l=l)
    clusterCriteriaboot = clusterCriteria(clboot,nt,l=l)
    resboot[,s] = clusterCriteriaboot$value
    if (txtbar) setTxtProgressBar(pb, s)
  }
  if (txtbar) close(pb)
  dfboot=data.frame(clusterCriteriaboot[,1:2],resboot)
  colnames(dfboot)[-(1:2)]=paste('b',1:nboot,sep='')
  return(dfboot)
}
