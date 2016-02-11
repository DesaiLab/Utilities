FUNCTION edgedetect,x,a
;square wavelet edge detector for use with spectral corrections
; input |         x = input signal to find edge in
;       |         a = width of wavelet (must be odd #)
;
;       |    ans(0) = non-normalized area under curve created by
;output |             mulitiplying wavelet by signal over width of wavelet
;       |             (by non-normalized, I mean just the sum of the
;       |             discrete values under the curve)
;       |    ans(1) = an index vector starting at index of x where
;       |             center of wavelet lies when the wavelet is at
;       |             left-most position.
  n = n_elements(x)
  IF (a MOD 2) EQ 0 THEN a = a + 1
  wav = fltarr(1,n-a+1)
  for i=0,n-a do begin
    wav(i)=total(x[i:i+(a+1)/2-1-1])-total(x[i+(a+1)/2+1-1:i+a-1])
  endfor
  t=findgen(1,n-a+1)+(a-1)/2
  ans=[wav,t]
  return,ans
END
