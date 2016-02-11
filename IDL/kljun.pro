FUNCTION kljun,sigmaw,ustar,z_m,zi,z0,x=x  ;2D model (original)
;compute for a range of X, 

;criteria -200<z/L<1
;u*>0.2
;zm>1m
;zm<h

  A_F = 0.18D
  A_C = 4.28D
  Bconst = 3.42D
  A_D = 1.68D
  zconst = Bconst-alog(double(z0))
  
  a = (A_F / zconst)
  b = 3.70
  c = A_C * zconst
  d = A_D * zconst
  
  alpha_1 = 0.8D
  alpha_2 = -0.8D

  IF n_elements(x) EQ 0 THEN x = dindgen(2000)

  xstar = ((sigmaw/ustar)^alpha_1)*(x/z_m)

  xconst = (xstar + d)/c

  fstar = a * xconst^b * exp(b * (1-xconst))

  f_y = fstar / (((sigmaw/ustar)^alpha_2) * ((1-(z_m/zi))^(-1.0)) * z_m)

  return,f_y
;standard cross wind is sigma_v * x / u(z)
;estimate z0 from wind profile? U/u* = k ln(z/z0) + psi(Z-D/L)
END

FUNCTION kljun_3d_FFPalong,x,sigW,ustar,zmeas,alpha1,a,b,c,d
;alongwind footprint
  Xstar = ((sigW/ustar)^alpha1) * x / zmeas
  Lprime = (Xstar + d) / c
  Fstar = a * (Lprime^b) * exp(b*(1-Lprime))
  return,Fstar
END

FUNCTION kljun_3d_FFPcross,x,y,sigV,sigW,ustar,zmeas,z0,h,Psi
  Tly = 0.08 * h^2 / (h - zmeas) / ustar
  Ulog = ustar / 0.4 * (alog(zmeas / z0) - (zmeas - z0) / zmeas - Psi)
  tau = sqrt((x / Ulog)^2 + ((zmeas - z0) / sigW)^2)
  sigma = tau / (1 + sqrt(tau / (2 * Tly))) * tau / Tly * sigV
  inside = ((-y^2) / (2 * (sigma^2))) > (-700.0D)
  Dy = (1.0D / (sqrt(2.0 * !dpi) * sigma)) * exp(double(inside))
  return,Dy
END

FUNCTION kljun_3d_Gam,t,b=b
  IF n_elements(b) EQ 0 THEN b = 3.7D
  return,t^b * exp(-t)
END

FUNCTION kljun_3d_FFPcrossY,y,sigma=sigma
  inside = ((-y^2) / (2 * (sigma^2))) > (-700.0D)
  Dy = (1.0D / (sqrt(2.0 * !dpi) * sigma)) * exp(double(inside))
  return,Dy
END

FUNCTION kljun_3d_FFPcrossXY,x,y,sigV,sigW,ustar,zmeas,z0,h,Psi
  Tly = 0.08 * h^2 / (h - zmeas) / ustar
  Ulog = ustar / 0.4 * (alog(zmeas / z0) - (zmeas - z0) / zmeas - Psi)
  tau = sqrt((x / Ulog)^2 + ((zmeas - z0) / sigW)^2)
  sigma =  tau / (1 + sqrt(tau / (2 * Tly))) * tau / Tly * sigV
  PHIcross = dblarr(n_elements(y)-1)
  pcross = 100.0D
  FOR i = 0,n_elements(y)-2 DO BEGIN
    IF float(pcross) GT 1e-5 THEN BEGIN 
      qsimp,'kljun_3d_FFPcrossY',double(y[i]),double(y[i+1]),pcross,sigma=sigma
      PHIcross[i] = pcross
    ENDIF 
  ENDFOR 
  PHIcross = PHIcross / (2 * total(PHIcross))
  return,PHIcross
END 

FUNCTION kljun_3d,angle=angle,Csize=Csize,sigV=sigV,sigW=sigW,ustar=ustar,zmeas=zmeas,z0=z0,h=h,Psi=Psi,radius=radius,cover=cover

  IF n_elements(angle) EQ 0 THEN angle = 0.0D ;wind direction
  IF n_elements(csize) EQ 0 THEN csize = 100.0D ;grid in meters
  IF n_elements(sigV) EQ 0 THEN sigV = 1.0D ;sigmaV m/s
  IF n_elements(sigW) EQ 0 THEN sigW = 0.1D ;sigmaW m/s
  IF n_elementS(ustar) EQ 0 THEN ustar = 1.0D ;u* m/s
  IF n_elements(zmeas) EQ 0 THEN zmeas = 30.0D ;height of obs (z-D) m
  IF n_elements(z0) EQ 0 THEN z0  = 1.0D ;roughness m
  IF n_elements(h) EQ 0 THEN h = 1000.0D ;PBL height
  IF n_elements(Psi) EQ 0 THEN Psi = 0.0D ;not sure

;Consts
  alpha1 = 0.8D
  Ac = 4.28D ;+/- 0.13
  Ad = 1.68D ;+/- 0.1
  Af = 0.18D ;+/- 0.01
  Ax = 2.59D ;+/- 0.17
  Bup = 3.42D ;+/- 0.35
  zconst = Bup-alog(double(z0))

;Params
  a = Af / zconst
  b = 3.7D ;+/- 0.30
  c = Ac * zconst
  d = Ad * zconst

;scaling variable
  scal = zmeas * (sigW/ustar)^(-alpha1)

;estimate or get size of matrix
  whrxn = ceil((-(d * scal + Csize/2)) / Csize)
  IF n_elements(radius) EQ 0 THEN BEGIN 
    xmax = Ax * zconst * scal
    fmax = a
    
    whri = xmax
    whro = fmax
    WHILE whro GT (fmax/100.0) DO BEGIN
      whri = whri + Csize
      whro = kljun_3d_FFPalong(whri,sigW,ustar,zmeas,alpha1,a,b,c,d)
    ENDWHILE 
    whrxp = ceil(whri/Csize)
    
    ymax = 0.0D
    x = whrxp * Csize
    y = ymax
    fmax = kljun_3d_FFPcross(x,y,sigV,sigW,ustar,zmeas,z0,h,Psi)
    
    whri = ymax
    whro = fmax
    WHILE whro GT (fmax/100.0) DO BEGIN 
      whri = whri + Csize
      y = whri
      whro = kljun_3d_FFPcross(x,y,sigV,sigW,ustar,zmeas,z0,h,Psi)
    ENDWHILE 
    
    whry = ceil(whri/Csize)
  ENDIF ELSE BEGIN
    whrxp = ceil(radius/Csize)
    whry = ceil(radius/Csize)
  ENDELSE 
  
  IF whrxn LT 0 THEN BEGIN
    xbou = [numgen(whrxn,-1.0)+0.5,numgen(1.0,whrxp)-0.5]*Csize
  ENDIF ELSE BEGIN
    xbou = [0,numgen(1.0,whrxp)+0.5]*Csize
  ENDELSE
  ybou = [0.0,numgen(1.0,whry)-0.5]*Csize

  xcen = (xbou[0:*] + xbou[1:*])/2.0
  ycen = (ybou[0:*] + ybou[1:*])/2.0

;alongwind 
  Lhat = (Xbou / scal + d) / c
  Gam = Qromb('kljun_3d_Gam',b*Lhat[0:n_elements(Lhat)-2],b*Lhat[1:n_elements(Lhat)-1])
  PHIalong = a * c * exp(b) * b^(-b) / b * Gam
  INTall = a * c * exp(b) * b^(-b) * gamma(b)
  cover = total(PHIalong)/INTall*100
;stop
;  PHIalong = PHIalong / total(PHIalong)

;crosswind
  PHIcross = dblarr(n_elements(Ycen),n_elements(Xcen))
  FOR i = 0,n_elements(Xcen)-1 DO BEGIN
    PHIcross[*,i] = kljun_3d_FFPcrossXY(Xcen[i],Ybou,sigV,sigW,ustar,zmeas,z0,h,Psi)
  ENDFOR 
  PHIcross = [reverse(phicross[1:*,*]),phicross[0,*]*2.0,phicross[1:*,*]]

;multiply the two
  PHI = PHIcross
  FOR i = 0,n_elements(PHIcross[*,0])-1 DO PHI[i,*] *= PHIalong

;Make a square/centered array
  dum = where(xcen GT 0,nlen1)
  dum = where(xcen LT 0,nlen2)
  pads = nlen1 - nlen2
  IF pads GT 0 THEN BEGIN 
    padarr = fltarr(n_elements(phicross[*,0]),pads)
    PHIc = [[padarr],[PHI]]
  ENDIF ELSE BEGIN
    IF pads LT 0 THEN BEGIN
    padarr = fltarr(n_elements(phicross[*,0]),pads*(-1))
    PHIc = [[padarr],[PHI]]
;    PHIc = [[PHI],[padarr]]
    ENDIF
  ENDELSE 
  pdiff = n_elementS(PHIc[*,0])-n_elements(PHIc[0,*])
  CASE 1 OF 
    pdiff GT 1 : BEGIN
      padarr = fltarr(n_elementS(phic[*,0]),pdiff/2)
      PHIcp = [[padarr],[PHIc],[padarr]]
    END 
    pdiff LT (-1)  : BEGIN
      padarr = fltarr(pdiff/(-2),n_elementS(phic[0,*]))
      PHIcp = [padarr,PHIc,padarr]
    END 
    ELSE : PHIcp = PHIc
  ENDCASE 
  
;Rotate into wind
  PHIcpr = rot(PHIcp,180.0-angle,/interp)
;  PHIcpr /= total(PHIcpr)
;stop
  return,PHIcpr / intall
END
