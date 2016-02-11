FUNCTION granger,ix,iy,lags=lags,sig=sig
;test for granger causlist 
;does x explain y, more than previous values of y?

;first auto-regress y for lags

  x = zapbadval(reform(ix[*]))
  y = zapbadval(reform(iy[*]))

  IF n_elements(sig) EQ 0 THEN sig = 0.05

  nel = min([n_elements(x),n_elements(y)])
  IF n_elements(lags) EQ 0 THEN BEGIN
    tlag = 1
    keepgoing = 1b
    truen = edof(y)
    WHILE tlag LT (nel/4) AND keepgoing DO BEGIN
      corr = correl(y[0:nel-tlag-1],y[tlag:nel-1])
;      nel1 = n_elements(y[tlag:nel-1])
      nel1 = (truen-tlag) > 2
      pval = corr_ttest(corr=corr,n=nel1)
      IF pval GT sig THEN keepgoing = 0b ELSE tlag++
    ENDWHILE 
    lags = lindgen(tlag)+1
  ENDIF 

  mlg = max(lags)
  nel_lg = nel-mlg
  nlag = n_elements(lags)

  indep1 = make_array(nlag,nel_lg,/float,value=nan())
  FOR i = 0,n_elements(lags)-1 DO indep1[i,*] = y[mlg-lags[i]:nel_lg+mlg-lags[i]-1]

  ytest = y[mlg:nel-1]
  nvalid = n_elements(ytest)

  test1 = regress(indep1,ytest,ftest=ft1,correlation=c1,yfit=yf)
  sq = total((yf-ytest)^2)
  sigma = sqrt(sq/nvalid) > 1e-32
  LL1 = (-1.0) * nvalid * alog(4.1327314 * sigma)
  aic1 = (2 * nlag) - (2 * LL1)

;  stop

;now 

;for each lagged value of x:
;include if ttest < sig, and new regress ftest < ftest1

  indep2 = indep1
  retained = [0]
  oft = ft1
  FOR i = 0,n_elements(lags)-1 DO BEGIN
    testx = x[mlg-lags[i]:nel_lg+mlg-lags[i]-1]
    truen_x = edof(testx)
    corr = correl(testx,ytest)
    good = where(finite(testx) AND finite(ytest),ngvals)
    nvals = truen_x < ngvals > 2
    pval = corr_Ttest(corr=abs(corr),n=nvals)
    IF pval LT sig THEN BEGIN
      print,'Found a significant correlation ',corr,' at lag ',lags[i]
      indep3 = [indep2,transpose(testx)]
      test2 = regress(indep3,ytest,ftest=ft2,correlation=c2,yfit=yf1)
      sq = total((yf1-ytest)^2)
      sigma = sqrt(sq/nvalid) > 1e-32
      LL2 = (-1.0) * nvalid * alog(4.1327314 * sigma)
      aic2 = (2 * n_elements(indep2[*,0])) - (2 * LL2)
;      IF ft2 GT ft1 THEN BEGIN
      IF aic2 LT aic1 THEN BEGIN 
        print,'  Retained, since it has lower AIC ',aic2,aic1
        indep2 = indep3
        retained = [retained,lags[i]]
;        ft1 = ft2
        aic1 = aic2
      ENDIF 
    ENDIF 
  ENDFOR
;stop
  IF n_elements(retained) EQ 1 THEN return,-1 ELSE return,retained[1:*]

END
