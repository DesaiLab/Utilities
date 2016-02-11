;-------------------------------------------------------------
;+
;   SPECTRUM
;
; PURPOSE:
;   Compute Fourier Power Spectrum
;
;
; CALLING SEQUENCE:
;
;   f = SPECTRUM(x,dt,FREQ=freq,PERIOD=period)
;
;
; INPUTS:
;
;   x = original time series, of length N
;
;   dt = time interval between x measurements = Total time/N
;
;
; OUTPUT:
;
;   F = output power spectrum with N/2+1 components  (units are X^2)
;
;
; OPTIONAL KEYWORD INPUTS:
;
;   FRACTION = output F^2/TOTAL(F^2) (fractional power spectral density)
;
;   LAG1 = LAG 1 Autocorrelation, used for SIGNIF levels. Default is 0.0
;
;   SIGLVL = significance level to use. Default is 0.05 (5%)
;
;   TUKEY = smooth F using a Tukey filter. Default is no smoothing.
;
;   WIDTH = the effective width of the Tukey filter, as a fraction of the
;           number of points N. Default is 0.02
;
;
; OPTIONAL KEYWORD OUTPUTS:
;
;   FREQ = output frequency components [ FREQ(n) = n/(Ndt) ]
;
;   PERIOD = output period components [ PERIOD(n) = 1/FREQ(n) ]
;
;   SIGNIF = output significance levels as a function of FREQ
;
;   AMP = Fourier amplitude
;
;   PHASE = Fourier phase
;
;   BANDWIDTH = The width of the smoothing filter in units of frequency
;
;   DOF = Degrees of freedom for chi-square distribution for significances.
;         For no smoothing this is 2.0.
;         For the Tukey it is 2*N*WIDTH
;
;
; MODIFICATION HISTORY:
;   Written C. Torrence
;   5 Aug 1998 (CT): added BOXCAR filter
;   3 Feb 1999 (CT): remove BOXCAR, added TUKEY
;-
;-------------------------------------------------------------

FUNCTION SPECTRUM,x,dt,y, $
    FRACTION=fraction, $
    LAG1=lag1,SIGLVL=siglvl, $
    TUKEY=tukey,WIDTH=width, $
    FREQ=freq,PERIOD=period, $
    AMP=amp,PHASE=phase, $
    FFT_THEOR=fft_theor,SIGNIF=signif,DOF=dof, $
    BANDWIDTH=bandwidth

;    ON_ERROR,2
    IF (N_ELEMENTS(siglvl) LT 1) THEN siglvl = 0.95
    IF (N_ELEMENTS(lag1) LT 1) THEN lag1 = 0.0
    IF (N_ELEMENTS(width) LT 1) THEN width = 0.02

    N = N_ELEMENTS(x)
    fft_x = FFT( x(*) - TOTAL(x)/N ,-1)
    IF n_elements(y) NE 0 THEN BEGIN 
      fft_y=fft(y(*)-total(y)/N,-1)
      thex = fft_x[0:(N+1)/2-1]
      they = fft_y[0:(N+1)/2-1]
      amp = 2.0*sqrt(complex((real_part(thex)*Real_part(they))+(imaginary(thex)*imaginary(They))))

;cospectrum is 1/T * realpart conj(fft(x)) * fft(Y)
      IF ((n MOD 2) EQ 0) THEN $
;        amp = [amp,sqrt(2)*abs(conj(fft_x[N/2])*fft_y[N/2])]
        amp = [amp,sqrt(2)*sqrt(complex((real_part(fft_x[N/2])*real_part(fft_y[N/2]))+$
                      (imaginary(fft_x[N/2])*imaginary(fft_y[N/2]))))]
;      power_spec = 0.5*amp
    ENDIF ELSE BEGIN 
      amp = 2*ABS(fft_x[0:(N+1)/2-1])
      IF ((N MOD 2) EQ 0) THEN $ ;Nyquist is 1/2 power for N even
        amp = [amp,SQRT(2)*ABS(fft_x[N/2])]
      phase = (ATAN(IMAGINARY(fft_x),FLOAT(fft_x)))[0:(N+1)/2-1]
    ENDELSE 
    power_spec = 0.5*real_part(amp^2)
    variance = TOTAL(power_spec[1:*])

    IF KEYWORD_SET(fraction) THEN fraction=1./variance ELSE fraction=1

    nf = N/2 + 1
    freq=FINDGEN(nf)/(N*dt)
    period=[N*dt,1./freq(1:*)]

    dof = 2.
    IF KEYWORD_SET(tukey) THEN BEGIN
        m = (3./4)*width*N
        power_spec = FILTER_TUKEY(power_spec,m,N*dt,bandwidth,dof)
    ENDIF

    fft_theor = (1 - lag1^2)/(1 - 2*lag1*COS(dt*freq*2*!PI) + lag1^2)
    fft_theor = fft_theor*(variance*2./N*fraction)
    signif = fft_theor*(CHISQR_CVF(1.-siglvl,FLOAT(dof))/FLOAT(dof))

    RETURN,power_spec*fraction
END
