PRO psd,x,y
;use welch's method for power spectral density or cross spectrum

;if y is not defined then y = x
  IF n_elements(y) EQ 0 THEN y = x

;Welch's method
;  '{X}m = 1/m sum 0-(m-1) of FFT(xm)conj(fft(xm)) where xm = x[n+mN] n = 0 to N-1


;Cross-correlation 

  FOR m = 0,

  
END

;coherence  abs(psd(x,y))^2 / (psd(x)*psd(y))
