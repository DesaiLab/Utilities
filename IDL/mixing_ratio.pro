FUNCTION Mixing_Ratio, temp, press, rh, vpd=vpd, Es=Es, Ea=Ea
;calculates the water vapor mixing ratio in g/kg given 
;temp in Celcius, pressure in Mb, and relative humidity

;H = E/Es
;Es = 6.112e^((17.67T)/(243.5+T))
;MR = (.62197*E) / (P-E)

Es = 6.112 * exp( (17.67 * temp) / (243.5 + temp))  ;calc saturation vapor press in mb
Ea = rh * Es     ;calc vapor pressure in mb
mr = ( (.62197 * Ea) / (press - Ea) )  ;calc mixing ratio
vpd = (Es-Ea)/10.0 ;vapor pressure deficit in kPa
return, mr*1000.0  ;convert to g/kg from g/g

END
