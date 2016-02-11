PRO PrintOff
  device, /close
  IF !version.os_family EQ 'Windows' THEN set_plot, 'WIN' ELSE set_plot, 'X'
  defsysv,'!NOWINDOW',exists = nowinyes
  IF nowinyes EQ 0 THEN BEGIN
    device,retain=1
    device,decomposed=0
    loadct,0,/silent
    !p.background=255
    !p.color=0
    !x.style=1
    !y.style=1
    !p.font = -1
  ENDIF 
END
