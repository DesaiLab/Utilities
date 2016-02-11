;MCMC-IDL v1.0 by Ankur Desai (Dec 2010), based on ml-metro5.c by Bill Sacks and
;George Hurtt and Rob Braswell
;see mcmc_example below for a test case and mcmc for usage information
;Contact: Ankur R Desai, desai@aos.wisc.edu

;----EXAMPLE MODELS----
;See end of this code (mcmc_example) for an example code that uses this model for optimization
FUNCTION mcmc_testmodel,x,param,_EXTRA=ex
;a very simple model of NEE
  nee = (param[0] * exp(param[1]*(x[0,*]-15.0))) -(( param[2]*x[1,*]) / (param[3]+x[1,*]))
  return,nee
END

FUNCTION mcmc_testmodel2,x,param,_extra=ex
;a bit more complex NEE model that includes dormant and growing season difference
;10 parameters - leafon,leafoff,4 dormant, 4 growing season
  leafon = ((fix(param[0])-1)*24) > 0
  leafoff = ((fix(param[1])-1)*24) > leafon < 8759
  nee = fltarr(1,8760)
;winter
  nee[0:(leafon-1)] = (param[2] * exp(param[3]*(x[0,0:(leafon-1)]-15.0))) -(( param[4]*x[1,0:(leafon-1)]) / (param[5]+x[1,0:(leafon-1)]))
;summer
  nee[leafon:(leafoff-1)] = (param[6] * exp(param[7]*(x[0,leafon:(leafoff-1)]-15.0))) -(( param[8]*x[1,leafon:(leafoff-1)]) / (param[9]+x[1,leafon:(leafoff-1)]))
;fall  
  nee[leafoff:8759] = (param[2] * exp(param[3]*(x[0,leafoff:8759]-15.0))) -(( param[4]*x[1,leafoff:8759]) / (param[5]+x[1,leafoff:8759]))
  return,nee
END

;----DEFAULT COST FUNCTION----
FUNCTION mcmc_likelihood,x,y,param,model,valid,dat=dat,_EXTRA=ex,modout=modout
;default is to call model with x,param, compare output[0,*] to y[0,*]
;set dat to choose different columns to compare

 
  IF n_elements(dat) LE 1 THEN dat = [0,0]
  modout = call_FUNCTION(model,x,param,_EXTRA=ex)

  gv = where((valid[dat[1],*] EQ 1) AND (finite(modout[dat[0],*])) AND (finite(y[dat[1],*])),nvalid)
  IF nvalid GT 1 THEN BEGIN 
    sq = total( (modout[dat[0],gv] - y[dat[1],gv])^2    )
    sigma = sqrt(sq/nvalid) > 1e-32
;The true log liklihood function (constants removed):
;  loglike = (nvalid*alog(sigma))  +  (sq/(2*(sigma^2))) 
;If you do that math and propagate the constants in, you get 
    loglike = nvalid * alog(4.1327314 * sigma)
  ENDIF ELSE BEGIN
    loglike = 1e31
  ENDELSE 
  IF loglike EQ 1e31 THEN stop
  modout = modout[dat[0],*]
  return,loglike

;comparison is as follows
;sq= sum ((model-data)^2) 
;n = nvalid

;sigma = sqrt(sumsq/n)
;loglike += n * alog(sigma)
;loglike += sq / (2 * sigma^2)

END

;----MAIN PROGRAM----
PRO mcmc,x,y,param,$
         numatonce=numatonce,random_start=random_start,numchains=numchains,numspinups=numspinups,$
         iter=iter,valid_frac=valid_frac,validdata=validdata,model=model,likelihood=likelihood,$
         outputll=outputll,outputvalue=outputvalue,outputy=outputy,_EXTRA = ex,fast=fast,medium=medium,ranacc=ranacc,quiet=quiet,superfast=superfast

;MCMC parameter estimator
;based on sipnet
;Note: this can work for multiple data types (likelihood function
;would do the math)
;However, this is a single location version
;to make work at multiple locs, would need to modify likelihood function
;also no aggregation is done here
;SEE BELOW FOR AN EXAMPLE FOR HOW TO USE IN IDL

;---REQUIRED INPUTS---
;x is the input data for the model (no requirements on format except
;for what likelihood and model expect)
;y is the output data to compare (same as above)

;param is a structure  with the following properties
;param.name is an arry of name
;param.value is parameter value (initial guess) array
;param.max max value array
;param.min min value array
;param.knob is knob array (not used in this version, can be all zero)
;param.changeable is whether it should be fixed (0) or estimated (1)

;---OPTIONAL KEYWORDS---
;valid is % of datapoints in each interval that are valid (default is 100%)
;valid_frac is min % of datapoints to accept (default is 50%)

;model is the name of the model function (string) - default is "mcmc_testmodel"
;likelihood is the name of the likelihood function - it should call
;   model and compute the likelihood - default is "mcmc_likelihood"
;_EXTRA can be used to pass extra keywords to likelihood or model

;numatonce - how often to check for chain convergence (default is 10000)
;random_start - start at init guess if 0, else randomly within prior (default is 0)
;numchains - number of chains (default is 10)
;numspinups - how much burn in on final iteration (default is 125000)
;iter - max number of iterations both for chains and final (default is 375000)
;ranacc - tuning parameter for % of "worse" likelihoods to accept (default is -1) 
;/fast, /medium, and /superfast are keywords with different default settings for the above settings - all are faster than default (see below)
;/quiet turns off all printing of messages

;---OUTPUTS---
;outputll is the likelihood of output values, best value is first
;outputvalues are accepted parameter values (same format as param.value)
;outputy is the model run with the best outputvalues parameter set (same format as y)

  A_STAR = 0.4 ;(target rate)
  THRESH = 0.02 ;(+/- target rate)
  DEC = 0.99
  INC = DEC ^ ( (A_STAR-1)/A_STAR)
;  add_fraction = 0.5
  add_fraction = 1.0

;fast mode
  IF n_elements(fast) NE 0 THEN BEGIN
    numatonce = 1000l
    random_start = 1l
    numchains = 3l
    numspinups = 5000l
    iter = 10000l
  ENDIF 

;medium mode
  if n_elements(medium) ne 0 then begin
    numatonce = 10000l
    random_start = 1l
    numchains = 6l
    numspinups = 70000l
    iter = 150000l
    thresh = 0.025
  endif  

;super fast model
  IF n_elements(superfast) NE 0 THEN BEGIN
    numatonce = 1000l
    random_start = 0l
    numchains = 1l
    numspinups = 2000l
    iter = 5000l
  ENDIF 

;set defaults
  IF n_elements(numatonce) EQ 0 THEN numatonce = 10000l
  IF n_elements(random_start) EQ 0 THEN random_start = 0l
  IF n_elements(numchains) EQ 0 THEN numchains = 10l
  IF n_elements(numspinups) EQ 0 THEN numspinups = 125000l
  IF n_elements(iter) EQ 0 THEN iter = 375000l
  IF n_elements(ranacc) EQ 0 THEN ranacc = -1.0 ;set to -5.0 for compelx cost func
  IF n_elements(validdata) EQ 0 THEN BEGIN 
    validdata = y
    validdata[*] = 0
    gy = where(finite(y),ngy)
    IF ngy GT 0 THEN validdata[gy] = 1
  ENDIF 
  IF n_elements(valid_frac) EQ 0 THEN valid_frac = 0.5
  val = validdata GE valid_frac

  IF n_elements(model) EQ 0 THEN model = 'mcmc_testmodel'
  IF n_elements(likelihood) EQ 0 THEN likelihood = 'mcmc_likelihood'

  max = double(param.max)
  min = double(param.min)
  range = max-min
  change = where(param.changeable EQ 1,nchange)
   
  verybestll = double(-1e31)

;build some chains
  FOR c = 0,numchains-1 DO BEGIN

    IF ~keyword_set(quiet) THEN print,'Starting Chain ',c+1

;reset values
    value = double(param.value)
    IF (random_start EQ 1) AND (c GT 0) THEN value[change] = min[change] + (range[change] * randomu(systime(/sec),nchange))
;    knob = double(param.knob)
    knob = replicate(add_fraction,n_elements(range))
    converged = 0
    steps = 0l
    seed = systime(/sec)
    oldvalue = value
    bestvalue = value
    ll = (-1.0) * call_FUNCTION(likelihood,x,y,value,model,val,_extra=ex)
    bestll = ll
    ll_old = bestll

;go through the chain until convergence
    WHILE (converged EQ 0) && (steps LT iter) DO BEGIN 
      ichgs = change[long(randomu(seed,numatonce,/double)*nchange)]
      tune = randomu(seed,numatonce,/double)-0.5
      ran_accept = ranacc * randomu(seed,numatonce,gamma=1,/double)
;-5.0
      yes = 0l

      FOR k = 0l,numatonce-1l DO BEGIN
;randomly pick a parameter to change
        accept = 1
        ichg = ichgs[k]
        oldval = value[ichg] 
        newval = (knob[ichg] * range[ichg] * tune[k])+oldval
        IF (newval GT max[ichg]) OR (newval LT min[ichg]) THEN accept = 0

;run the model and calculate the likelihood
        IF accept EQ 1 THEN BEGIN
          value[ichg] = newval
          ll = (-1.0) * call_FUNCTION(likelihood,x,y,value,model,val,_extra=ex)
          IF (ll LE ll_old) && (ran_accept[k] GE (ll-ll_old)) THEN accept = 0
          IF (accept EQ 1) && (ll GT bestll) THEN BEGIN 
            bestll = ll
            bestvalue = value
          ENDIF
        ENDIF

;keep track of accepted parameter sets, tune knob
        IF accept EQ 1 THEN BEGIN 
          ll_old = ll
          yes++
          knob[ichg]*=INC
        ENDIF ELSE BEGIN
          value[ichg] = oldval
          knob[ichg] = (knob[ichg]*DEC)>(1e-9)
        ENDELSE

      ENDFOR

;check for convergence of this chain
      steps+=numatonce
      IF ~keyword_set(quiet) THEN print,'  iteration ',steps,' accept ',float(yes)/numatonce,' llmax ',bestll
;      IF float(yes)/numatonce GE a_star THEN BEGIN 
      IF abs(float(yes)/numatonce - a_star) LT thresh THEN BEGIN
        converged = 1
        IF ~keyword_set(quiet) THEN print,'  Chain ',c+1,' Converged LL: ',ll
        IF ~keyword_set(quiet) THEN print,'  Values: ',value
        IF bestll GE verybestll THEN BEGIN
          IF ~keyword_set(quiet) THEN print,'    And it is the best chain so far!'
          verybestll = bestll
          verybestvalue = bestvalue
          endvalue = value
          endll = ll
          endknob = knob
        ENDIF
      ENDIF ELSE BEGIN
        IF ~keyword_set(quiet) THEN print,'  Chain ',c+1,' not yet converged'
        yes = 0l
      ENDELSE 
 
    ENDWHILE 

    IF converged EQ 0 THEN IF ~keyword_set(quiet) THEN print,'  Chain ',c+1,' did not converge'

  ENDFOR

;start at end of best chain

  IF n_elements(endvalue) EQ 0 THEN BEGIN
    IF ~keyword_set(quiet) THEN print,'No chains converged, try changing mcmc iterations'
    IF ~keyword_set(quiet) THEN print,'Starting from best value'
    endvalue = bestvalue
    endll = bestll
    endknob = knob
;    stop
  ENDIF 

  value = endvalue
  ll_old = endll
  knob = endknob
  seed = systime(/sec)
  bestvalue = value
  bestll = endll

  ichgs = change[long(randomu(seed,iter,/double)*nchange)]
  tune = randomu(seed,iter,/double)-0.5
  ran_accept = ranacc * randomu(seed,iter,gamma=1,/double)
  yes = 0l
  yes2 = yes

  outputll = fltarr(iter)
  outputvalue = fltarr(n_elements(value),iter)  

  FOR k = 0l,iter-1l DO BEGIN 
    IF k MOD numatonce EQ 0 THEN IF ~keyword_set(quiet) THEN print,'Final iteration ',k,' accepted ',yes2,' saved ',yes

;randomly pick a parameter to change
    accept = 1
    ichg = ichgs[k]
    oldval = value[ichg]
    newval = (knob[ichg] * range[ichg] * tune[k])+oldval
    IF (newval GT max[ichg]) OR (newval LT min[ichg]) THEN accept = 0

;run the model and calculate the likelihood
    IF accept EQ 1 THEN BEGIN
      value[ichg] = newval
      ll = (-1.0) * call_FUNCTION(likelihood,x,y,value,model,val,_extra=ex)
      IF (ll LE ll_old) && (ran_accept[k] GE (ll-ll_old)) THEN accept = 0
      IF (accept EQ 1) && (ll GT bestll) THEN BEGIN 
        bestll = ll
        bestvalue = value
;        IF bestll GE verybestll THEN BEGIN
;          verybestll = bestll
;          verybestvalue = bestvalue
;        ENDIF 
      ENDIF
    ENDIF

;output values
    IF accept EQ 1 THEN BEGIN
      yes2++
      ll_old = ll
;if past numspinups, then start saving vals
      IF k GE numspinups THEN BEGIN 
        outputll[yes] = ll
        outputvalue[*,yes] = value
        yes++
      ENDIF 
    ENDIF ELSE BEGIN
      value[ichg] = oldval
    ENDELSE 

  ENDFOR

;create the history of accepted values
  IF yes EQ 0 THEN BEGIN 
    outputvalue = bestvalue
    outputll = bestll
  ENDIF ELSE BEGIN 
    outputvalue = [[bestvalue],[outputvalue[*,0:(yes-1)]]]
    outputll = [bestll,outputll[0:(yes-1)]]
    srt = reverse(sort(outputll))
    outputvalue = outputvalue[*,srt]
    outputll = outputll[srt]
  ENDELSE 

;output values if outputy is there
  IF arg_present(outputy) THEN BEGIN
    dummy = call_FUNCTION(likelihood,x,y,outputvalue[*,0],model,val,_extra=ex,modout=outputy)
  ENDIF 

  IF ~keyword_set(quiet) THEN print,'MCMC complete '
  IF ~keyword_set(quiet) THEN print,'Best LL: ',outputll[0]
  IF ~keyword_set(quiet) THEN print,'Values: ',outputvalue[*,0]

END

;----EXAMPLE CODE----
PRO mcmc_example,noise=noise
;A simple pseudo-inversion
;To run, start IDL
;type .compile mcmc.pro
;then type mcmc_example
;type retall once done to break out of debug mode

;let's make a simple PAR, temperature time series
  par1 = sin(!pi*(findgen(8760)-12)/12)*2000 > 0
  par2 = congrid(sin(!pi*findgen(365)/365),8760,/center)
  par = par1*par2
  temp = sin(!pi*findgen(8760)/8760)*30

;Set up the X values
  x = [transpose(temp),transpose(par)]

;The "true" parameters and output
  truth = [1.5,0.1,10.0,100.0]
  nee = mcmc_testmodel(x,truth)

;Add noise to the observations
  IF ~keyword_set(noise) THEN noise = 0.25
  obs = nee + (randomn(systime(/sec),n_elements(nee))*noise)

;The input parameter file for mcmc
  param = { name :       ['r1','r2',   'p1',  'p2'], $
            value :      [2.0,  0.3,   10.0,  500.0], $
            max :        [10.0, 1.0,   100.0, 10000.0], $
            min :        [0.0,  0.001, 0.0,   10.0], $
            knob :       [1.0,  1.0,  1.0,   1.0], $
            changeable : [1  ,  1   ,  1  ,   1  ] }

;Call MCMC
  mcmc,x,obs,param,/fast,outputll=outputll,outputvalue=outputvalue,outputy=y,model='mcmc_testmodel',likelihood='mcmc_likelihood'

;Plot outputs
  plot,nee,y,xrange=[-10,10],yrange=[-10,10],psym=1
  oplot,[-10,10],[-10,10],thick=2
  print,'Truth: ',truth
  print,'Best:  ',float(outputvalue[*,0])
  print,'r2 = ',correlate(nee,y)^2

;Stop
  stop
END


