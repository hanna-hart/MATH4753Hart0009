myci<-function(iter=10000,x1,x2,fun="mean",alpha=0.05,...){  #Notice where the ... is repeated in the code
  n1=length(x1)   #sample size
  n2=length(x2)

  y1=sample(x1,n1*iter,replace=TRUE)
  y2=sample(x2,n2*iter,replace=TRUE)

  rs1=matrix(y1,nr=n1,nc=iter,byrow=TRUE)
  rs2=matrix(y2,nr=n2,nc=iter,byrow=TRUE)
  xstat1=apply(rs1,2,fun) # xstat is a vector and will have iter values in it
  xstat2=apply(rs2,2,fun)
  xxstat=xstat1-xstat2
  ci=quantile(xxstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xxstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat1=matrix(x1,nr=n1,nc=1,byrow=TRUE)
  mat2=matrix(x2,nr=n2,nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte1=apply(mat1,2,fun)
  pte2=apply(mat2,2,fun)
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=3)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=3)

  # plot the point estimate 1/2 way up the density
  text(pte1,max(para$density)/2,round(pte1,2),cex=3)
  text(pte2,max(para$density)/2,round(pte2,2),cex=3)

  return(list(ci=ci,fun=fun,x1=x1, x2=x2))# Some output to use if necessary
}
