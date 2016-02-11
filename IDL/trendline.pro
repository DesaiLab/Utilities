PRO trendline,xs,ys,xrange=xrange,xint=xint,xplot=xplot,yplot=yplot,yfit=yfit,degree=degree,yband=yband,r=r,err=err,fit=fit,sigma=sigma,pval=pval

  IF NOT keyword_set(degree) THEN degree = 1
  res = poly_fit(xs,ys,degree,yfit=yfit,yband=yband,sigma=sigma)
  r = correlate(xs,ys)
  err = correlate(ys,yfit)
  fit = res
  pval = corr_ttest(corr=err,n=n_elements(xs)+1-degree)
  IF NOT keyword_set(xrange) THEN xrange = [min(xs),max(xs)]
  IF NOT keyword_set(xint) THEN xint = (xrange[1]-xrange[0]) / 10.0
  xplot = numgen(float(xrange[0]),float(xrange[1]),xint)
  yplot = replicate(res[0],n_elements(xplot))
  FOR i =1,degree DO yplot = yplot + (xplot^i * res[i])

END
