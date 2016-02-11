;-------------------------------------------------------------------------- -- 
; Significance Tests on Pearson's Correlation 
; Based on http://davidmlane.com/hyperstat/B62223.html 

FUNCTION CORR_TTEST,corr=corr,N=N,sl=sl, ro=ro, verbose = verbose
; corr: Pearson's correlation coefficient to be tested 
; N:    Number of samples 
; sl:   Significance level accepted (ex:0.05,0.001,) 
; ro:   Correlation value predicted by theory (Null Hypothesis) 
; Assumptions 
; 1. The N pairs of scores are sampled randomly and independently. 
; 2. The distribution of the two variables is bivariate normal. 
; NULL hypothesis is ro=ro 

  IF keyword_set(verbose) THEN ver = 1b ELSE ver = 0b
  IF n_elements(sl) EQ 0 THEN sl = 0.05
  IF N_Elements(ro) EQ 0 THEN ro=0 
  IF corr EQ 1.0  THEN corr=0.9999999d  ; avoid Floating divide by 0 
  IF ro   EQ 1.0  THEN ro=0.9999999d    ; avoid Floating divide by 0 
  IF corr EQ -1.0 THEN corr=-0.9999999d ; avoid Floating divide by 0 
  IF ro   EQ -1.0 THEN ro=-0.9999999d   ; avoid Floating divide by 0 
;-------------------------------------------------------------------------- -- 
; COMPUTE CONFIDENCE INTERVAL OF CORRELATION COEFFICIENT 
;-------------------------------------------------------------------------- -- 
; Conversion of Pearson's correlation to the normally distributed variable zp 
; Fisher's transformation 
  zp=0.5*alog((1+corr)/(1-corr)) 
;Compute zp standard error 
  sig_zp=1/sqrt(N-3) 
; Compute z value from significance level sl 
; 99% confidence interval example corresponds sl=0.01 and gives to 
  z=2.58 
  z=gauss_cvf((sl)/2.) 
  low_zp=zp-z*sig_zp 
  high_zp=zp+z*sig_zp 
  r_high=(exp(2*high_zp)-1)/(exp(2*high_zp)+1) 
  r_low=(exp(2*low_zp)-1)/(exp(2*low_zp)+1) 
  IF ver THEN print,'' 
  IF ver THEN print, "High End Case for r: ",r_high,"     Low End Case for r:   ",r_low 
; Preliminary result of significance based on Pearson correlation interval 
; If the 0 is included in the range between r_low and r_high, 
; You can't claim your result is Statistically significant at significance level (sl) 
; (or confidence level (1-sl) 
  IF r_low LT 0 AND r_high GT 0 THEN $ 
     IF ver THEN print,"This is NOT a statistically significant relationship!" ELSE $ 
        IF ver THEN print,"This is a statistically significant relationship!" 
;-------------------------------------------------------------------------- -- 
;-------------------------------------------------------------------------- -- 
; T test significance 
;-------------------------------------------------------------------------- -- 
  IF ver THEN print,'' 
  IF ver THEN print,'---T TEST result '
; if Null hypothesis is ro=0 
  IF ro EQ 0 THEN BEGIN 
    Df=N-2 
    t=corr*sqrt(Df)/sqrt(1-corr^2) 
    pt=2*(1-T_PDF(t, Df)) 
    pvalue = pt
    IF pt LT sl THEN $ 
       IF ver THEN print,"The correlation is significant repect to significance level ",string(sl,format='(F7.5)') ELSE $ 
          IF ver THEN print,"The correlation is NOT significant repect to significance level ",string(sl,format='(F7.5)') 
  ENDIF ELSE BEGIN 
; if Null hypothesis is ro<>0 
    zpro=0.5*alog((1+ro)/(1-ro)) 
    zt=(zp-zpro)/sig_zp 
    pzt=2*(1-GAUSS_PDF(zt)) 
    pvalue = pzt
    IF pzt LT sl THEN $ 
       IF ver THEN print,"The null hypothesis that the population correlation is     ",string(ro,format='(F7.4)')," can be rejected." ELSE $ 
          IF ver THEN print,"The null hypothesis that the population correlation is ",string(ro,format='(F7.4)')," CAN'T be rejected." 
  ENDELSE
  return,pvalue
END 
