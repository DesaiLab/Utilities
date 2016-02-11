PRO refline,value,y=y,linetype=linetype
;plot a refline at every point in value
;default is on x axis, unless y keyword is set
;default linetype is solid, 1 = dotted, 2 = dashed, 3 = dash dot
;4 = dash dotdotdot, 5 = long dashes
  IF keyword_set(y) THEN opprange = !x.crange ELSE opprange = !y.crange
  vals2 = interpol(opprange,100)
  FOR i = 0,n_elements(value)-1 DO BEGIN
    vals1 = replicate(value[i],100)
    IF keyword_set(y) THEN oplot,vals2,vals1,linestyle=linetype ELSE oplot,vals1,vals2,linestyle=linetype
  ENDFOR
END
