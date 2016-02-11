;Widget control to cleanup an arbritrary time series
;Main function is iclean = give it x and y, it returns locations
;that the user says are bad, -1 is none, just like where

PRO CleanWid_push_undo,ptr
;push the more recent state onto the undo stack
  undovals = (*ptr).undovals
  undovals[*,1:9] = undovals[*,0:8]
  undovals[*,0] = (*ptr).goodvals
  (*ptr).undovals = undovals
  undocut = (*ptr).undocut
  undocut[*,1:9] = undocut[*,0:8]
  undocut[*,0] = [((*ptr).xcutoff)[0],((*ptr).xcutoff)[1],((*ptr).ycutoff)[0],((*ptr).ycutoff)[1]]
  (*ptr).undocut = undocut
END

PRO CleanWid_pop_undo,ptr
;pop most recent undo state off the multiple undo stack
  undovals = (*ptr).undovals
  (*PTR).GOODVALS = undoVALS[*,0]
  undovals[*,0:8] = undovals[*,1:9]
  (*ptr).undovals = undovals
  undocut = (*ptr).undocut
  (*ptr).xcutoff = [undocut[0,0],undocut[1,0]]
  (*ptr).ycutoff = [undocut[2,0],undocut[3,0]]
  widget_control,(*ptr).xcb1,set_value=strtrim(string(undocut[0]),2)
  widget_control,(*ptr).xcb2,set_value=strtrim(string(undocut[1]),2)
  widget_control,(*ptr).ycb1,set_value=strtrim(string(undocut[2]),2)
  widget_control,(*ptr).ycb2,set_value=strtrim(string(undocut[3]),2)
  undocut[*,0:8] = undocut[*,1:9]
  (*ptr).undocut = undocut
END

PRO CleanWid_draw,ptr
;Draw or redraw the window
  win = (*ptr).FitWinNum
  wset,win
  !p.multi = 0

  gd = where((*ptr).goodvals EQ 1b,ngd)
  bd = where((*ptr).goodvals EQ 0b AND isnotnan((*ptr).y),nbd)

  IF ngd GT 0 THEN plot,((*ptr).x)[gd],((*ptr).y)[gd],xrange=(*ptr).xrange,yrange=(*ptr).yrange,psym=(*ptr).psym ELSE plot,[0,0],[0,0],/nodata,xrange=(*ptr).xrange,yrange=(*ptr).yrange,psym=(*ptr).psym
  IF nbd GT 0 THEN BEGIN
    bpsym = abs((*ptr).psym)
    IF bpsym EQ 0 THEN bpsym = 3
    oplot,((*ptr).x)[bd],((*ptr).y)[bd],psym=bpsym,color=210
  ENDIF

  oplot,[((*ptr).xrange)[0],((*ptr).xrange)[1]],[((*ptr).ycutoff)[0],((*ptr).ycutoff)[0]]
  oplot,[((*ptr).xrange)[0],((*ptr).xrange)[1]],[((*ptr).ycutoff)[1],((*ptr).ycutoff)[1]]
  oplot,[((*ptr).xcutoff)[0],((*ptr).xcutoff)[0]],[((*ptr).yrange)[0],((*ptr).yrange)[1]]
  oplot,[((*ptr).xcutoff)[1],((*ptr).xcutoff)[1]],[((*ptr).yrange)[0],((*ptr).yrange)[1]]
END

;EVENT HANDLERS

PRO CleanWid_Slider_Event,event
;Changing slider changes fuzzy tolerance
  WIDGET_CONTROL, event.top, GET_UVALUE = ptr
  (*ptr).fuzzytol = float(event.value)
END

PRO CleanWid_psym_Event,event
;Changing slider changes plot symbol (from -7 to positive 7)
  WIDGET_CONTROL, event.top, GET_UVALUE = ptr
  (*ptr).psym = float(event.value)
  cleanwid_draw,ptr
END

PRO CleanWid_Win_event,event
;left click in the window removes data points
;right click resurrects points from the dead
;middle mouse draws a zoom box, single click to unzoom to original range

  IF event.type LT 2 THEN BEGIN 
    IF event.press EQ 2 THEN BEGIN
      WIDGET_CONTROL, event.top, GET_UVALUE = ptr
      dataloc = convert_coord(event.x,event.y,/device,/to_data)
      (*ptr).zoomstart = float(dataloc[0:1])
    ENDIF
    
    IF event.release EQ 2 THEN BEGIN
      WIDGET_CONTROL, event.top, GET_UVALUE = ptr
      dataloc2 = float(convert_coord(event.x,event.y,/device,/to_data))
      dataloc = (*ptr).zoomstart
      IF (dataloc[0] EQ dataloc2[0]) THEN BEGIN
        (*ptr).xrange = (*ptr).initxr
;      (*ptr).yrange = (*ptr).inityr
      ENDIF ELSE BEGIN 
        dataloc = (*ptr).zoomstart
        xrange = [dataloc[0],dataloc2[0]]
;      yrange = [dataloc[1],dataloc2[1]]
        xrange = xrange[sort(xrange)]
;      yrange = yrange[sort(yrange)]
        (*ptr).xrange = xrange
;      (*ptr).yrange = yrange
        (*ptr).zoomstart = [nan(),nan()]
      ENDELSE
      (*ptr).zoomstart = [nan(),nan()]
      cleanwid_draw,ptr
      widget_control,(*ptr).xrb1,set_value=strtrim(string(((*ptr).xrange)[0]),2)
      widget_control,(*ptr).xrb2,set_value=strtrim(string(((*ptr).xrange)[1]),2)
    ENDIF
    
    IF (event.press EQ 1) OR (event.press EQ 4) THEN BEGIN
      WIDGET_CONTROL, event.top, GET_UVALUE = ptr
      dataloc = convert_coord(event.x,event.y,/device,/to_data)
      (*ptr).boxselect = float(dataloc[0:1])
    ENDIF
    
    IF (event.release EQ 1) OR (event.release EQ 4) THEN BEGIN 
      gv = byte(float(event.release)/4.0)
      bv = 1b-gv
      WIDGET_CONTROL, event.top, GET_UVALUE = ptr
      x = float((*ptr).x)
      y = float((*ptr).y)
      xrange = float((*ptr).xrange)
      xrange = float(xrange[1]-xrange[0])
      yrange = float((*ptr).yrange)
      yrange = float(yrange[1]-yrange[0])
      goodvals = (*ptr).goodvals
      r = where(goodvals EQ bv,nr,complement=bd,ncomplement=nbd)
      IF nr GT 0 THEN BEGIN 
        IF nbd GT 0 THEN BEGIN
          x[bd] = -1.0e20
          y[bd] = -1.0e20
        ENDIF
        xcoord = event.x
        ycoord = event.y
        dataloc = convert_coord(event.x,event.y,/device,/to_data)
        xbad = float(dataloc[0])
        ybad = float(dataloc[1])
        ft = float((*ptr).fuzzytol)
        boxselect = (*ptr).boxselect
        IF (boxselect[0] EQ xbad) AND (boxselect[1] EQ ybad) THEN BEGIN 
          tokill = (((abs(x-xbad)/xrange)*100.0) LT ft) AND (((abs(y-ybad)/yrange)*100.0) LT ft)
        ENDIF ELSE BEGIN 
          leftside = min([boxselect[0],xbad])
          rightside = max([boxselect[0],xbad])
          bottomside = min([boxselect[1],ybad])
          topside = max([boxselect[1],ybad])
          tokill = (x GE leftside AND x LE rightside AND y GE bottomside AND y LE topside)
        ENDELSE 
        (*ptr).boxselect = [nan(),nan()]
        wheretk = where(tokill,nwm)
        IF nwm GT 0 THEN BEGIN
;;push current state onto the undo stack
          cleanwid_push_undo,ptr
          goodvals[wheretk] = gv
          (*ptr).goodvals = goodvals
;redraw only those things that have changed
          pcolor = (event.release EQ 1)?210:0
          bpsym = abs((*ptr).psym)
          IF bpsym EQ 0 AND pcolor EQ 210 THEN cleanwid_draw,ptr ELSE BEGIN 
            oplot,((*ptr).x)[wheretk],((*ptr).y)[wheretk],psym=bpsym,color=pcolor
;         CleanWid_Draw,ptr
          ENDELSE 
        ENDIF
      ENDIF
    ENDIF
  ENDIF 
END

PRO CLEANWID_textbox_event,event
  WIDGET_CONTROL, event.id, GET_UVALUE = aptr, GET_VALUE = val
  WIDGET_CONTROL, EVENT.TOP, GET_UVALUE = PTR
  whichevent = (*aptr).myname
  CASE whichevent OF
    'XRANGEBOX1' : BEGIN
      (*ptr).xrange = [float(val),((*ptr).xrange)[1]]
      cleanwid_draw,ptr
    END 
    'XRANGEBOX2' : BEGIN
      (*ptr).xrange = [((*ptr).xrange)[0],float(val)]
      cleanwid_draw,ptr
    END 
    'YRANGEBOX1' : BEGIN
      (*ptr).yrange = [float(val),((*ptr).yrange)[1]]
      cleanwid_draw,ptr
    END 
    'YRANGEBOX2' : BEGIN
      (*ptr).yrange = [((*ptr).yrange)[0],float(val)]
      cleanwid_draw,ptr
    END     
    'XCUTOFFBOX1' : BEGIN
      oldcutoff = ((*ptr).xcutoff)[0]
      newcutoff = (float(val))[0]
      IF oldcutoff NE newcutoff THEN BEGIN 
        x = (*ptr).x
        oldgood = (*ptr).goodvals
        newgood = (oldgood AND (x GE newcutoff)) OR ((x GE newcutoff) AND (x LT oldcutoff))
        cleanwid_push_undo,ptr
        (*ptr).xcutoff = [newcutoff,((*ptr).xcutoff)[1]]
        (*ptr).goodvals = newgood
        cleanwid_draw,ptr
      ENDIF
    END 
    'XCUTOFFBOX2' : BEGIN
      oldcutoff = ((*ptr).xcutoff)[1]
      newcutoff = (float(val))[0]
      IF oldcutoff NE newcutoff THEN BEGIN 
        x = (*ptr).x
        oldgood = (*ptr).goodvals
        newgood = (oldgood AND (x LE newcutoff)) OR ((x LE newcutoff) AND (x GT oldcutoff))
        cleanwid_push_undo,ptr
        (*ptr).xcutoff = [((*ptr).xcutoff)[0],newcutoff]
        (*ptr).goodvals = newgood
        cleanwid_draw,ptr
      ENDIF
    END 
    'YCUTOFFBOX1' : BEGIN
      oldcutoff = ((*ptr).ycutoff)[0]
      newcutoff = (float(val))[0]
      IF oldcutoff NE newcutoff THEN BEGIN 
        y = (*ptr).y
        oldgood = (*ptr).goodvals
        newgood = (oldgood AND (y GE newcutoff)) OR ((y GE newcutoff) AND (y LT oldcutoff))
        cleanwid_push_undo,ptr
        (*ptr).ycutoff = [newcutoff,((*ptr).ycutoff)[1]]
        (*ptr).goodvals = newgood
        cleanwid_draw,ptr
      ENDIF
    END 
    'YCUTOFFBOX2' : BEGIN
      oldcutoff = ((*ptr).ycutoff)[1]
      newcutoff = (float(val))[0]
      IF oldcutoff NE newcutoff THEN BEGIN 
        y = (*ptr).y
        oldgood = (*ptr).goodvals
        newgood = (oldgood AND (y LE newcutoff)) OR ((y LE newcutoff) AND (y GT oldcutoff))
        cleanwid_push_undo,ptr
        (*ptr).ycutoff = [((*ptr).ycutoff)[0],newcutoff]
        (*ptr).goodvals = newgood
        cleanwid_draw,ptr
      ENDIF
    END 

    ELSE : print,whichevent,val

  ENDCASE

END


PRO CLEANWID_base_event, event
;Deal with button and base-resizing events

  WIDGET_CONTROL, event.id, GET_UVALUE = aptr
  whichevent = (*aptr).myname

  CASE whichevent OF

    'THEBUTTON' : BEGIN 
      CASE EVENT.VALUE OF

;Accept
        0: BEGIN                   
          WIDGET_CONTROL, EVENT.TOP, GET_UVALUE = PTR
          (*ptr).cancelflag = 0
          WIDGET_CONTROL, EVENT.TOP, /DESTROY
        END
        
;Reset
        1: BEGIN                   
          WIDGET_CONTROL, EVENT.TOP, GET_UVALUE = PTR
          cleanwid_push_undo,ptr
          (*PTR).GOODVALS = (*PTR).OLDGOODVALS
          (*ptr).xrange = (*ptr).initxr
          (*ptr).yrange = (*ptr).inityr
          (*ptr).xcutoff = (*ptr).initxc
          (*ptr).ycutoff = (*ptr).inityc
          widget_control,(*ptr).xcb1,set_value=strtrim(string(((*ptr).initxc)[0]),2)
          widget_control,(*ptr).xcb2,set_value=strtrim(string(((*ptr).initxc)[1]),2)
          widget_control,(*ptr).ycb1,set_value=strtrim(string(((*ptr).inityc)[0]),2)
          widget_control,(*ptr).ycb2,set_value=strtrim(string(((*ptr).inityc)[1]),2)
          widget_control,(*ptr).xrb1,set_value=strtrim(string(((*ptr).initxr)[0]),2)
          widget_control,(*ptr).xrb2,set_value=strtrim(string(((*ptr).initxr)[1]),2)
          widget_control,(*ptr).yrb1,set_value=strtrim(string(((*ptr).inityr)[0]),2)
          widget_control,(*ptr).yrb2,set_value=strtrim(string(((*ptr).inityr)[1]),2)
          cleanwid_draw,ptr
        END

;Undo
        2: BEGIN
          WIDGET_CONTROL, EVENT.TOP, GET_UVALUE = PTR
;pop most recent undo state
          cleanwid_pop_undo,ptr
          cleanwid_draw,ptr
        END

;Cancel
        3: BEGIN
          WIDGET_CONTROL, EVENT.TOP, GET_UVALUE = PTR
          (*PTR).cancelFLAG = 1
          WIDGET_CONTROL, EVENT.TOP, /DESTROY
        END

      ENDCASE
    END

    'THEBASE' : BEGIN
;If base window resizes, then change the draw window size
      fitwin = (*aptr).fitwin
      fitwinx = (*aptr).fitwinx
      fitwiny = (*aptr).fitwiny
      oldsize = (*aptr).winsize
      newsize = float([event.x,event.y])
      (*aptr).winsize = newsize
      basexchange = newsize[0]-oldsize[0]
      baseychange = newsize[1]-oldsize[1]
      fitwinx = fitwinx + (basexchange) ; /2.0)
      fitwiny = fitwiny + (baseychange)
      (*aptr).fitwinx = fitwinx
      (*aptr).fitwiny = fitwiny
      widget_control,fitwin,draw_xsize=fitwinx,draw_ysize=fitwiny
      CleanWid_draw,aptr
    END 
  ENDCASE
END

PRO Cleanwid_base_kill_event,wid
;Check to see if user kills widget without pressing one of the buttons
;this is equivalent to clicking cancel
  WIDGET_CONTROL, wid, GET_UVALUE = PTR
  IF (*ptr).cancelflag EQ 3 THEN (*PTR).cancelflag = 1
END

FUNCTION iClean,ix,iy,xrange=xrange,yrange=yrange,target=target,xcutoff=xcutoff,ycutoff=ycutoff,goodvals=goodvals,psym=psym,numkill=numkill,title=title,badval=badval

;Interactive data cleanup allows a user to remove bad data points
;in a point and click interface

;Left mouse button removes a point (turns it gray)
;Right mouse button unremoves a point
;Middle mouse button when dragged, zooms in the x (time) coordinate
;Middle mouse button when clicked, unzooms x coordinate to original range

;Inputs:
;ix = The x values, usually a time value (typically in sorted order)
;iy = The y values, the function values we are looking to kill
;ix is optional

;Optional Keywords:
;xrange = Initial x-range for plotting (can be changed by user)
;yrange = Initial y-range for plotting
;psym = Initial psym for plotting (default is 1, can be changed by user)
;title = Title of window
;xcutoff = Initial x cutoff ranges (2-element array, min and max)
;ycutoff = Initial y cutoff ranges (2-element array, min and max)
;target = Initial % of plotting area obliterated by a click (user changable)
;goodvals = Initial goodvals (same number of elements as y, 0 = bad, 1 = good)
;badval = What value of y is bad, default is NAN

;Output:
;Function outputs a list of array indices that the user says are bad
;-1 if no points are killed OR cancel is clicked
;numkill = number of points killed
;goodvals = 0 for bad, 1 for good for each data point
;xcutoff and ycutoff = the cutoff values

 
  oldexcept = !except
  !except = 0

  IF NOT keyword_set(iy) THEN BEGIN
    y = reform(ix)
    x = findgen(n_elements(y))
  ENDIF ELSE BEGIN
    x = reform(ix)
    y = reform(iy)
  ENDELSE

  xr = max(x,/nan)-min(x,/nan)
  yr = max(y,/nan)-min(y,/nan)
  IF NOT keyword_set(xrange) THEN xrange = [min(x,/nan)-(0.1*xr),max(x,/nan)+(0.1*xr)]
  IF NOT keyword_set(yrange) THEN yrange = [min(y,/nan)-(0.1*yr),max(y,/nan)+(0.1*yr)]

  IF NOT keyword_set(goodflag) THEN goodflag = 0b
  winsize = [nan(),nan()]
  IF n_elements(goodvals) EQ 0 THEN BEGIN
    IF NOT keyword_set(badval) THEN goodvals = isnotnan(y) ELSE goodvals = (y NE badval)
  ENDIF ELSE BEGIN
    IF NOT keyword_set(badval) THEN goodvals = goodvals AND isnotnan(y) ELSE goodvals = goodvals AND (y NE badval)
  ENDELSE
  wgd = where(goodvals,ngd)
  IF NOT keyword_set(xcutoff) THEN IF ngd GT 1 THEN xcutoff=[min(x[wgd],/nan),max(x[wgd],/nan)] ELSE xcutoff = [min(x,/nan),max(x,/nan)]
  IF NOT keyword_set(ycutoff) THEN IF ngd GT 1 THEN ycutoff=[min(y[wgd],/nan),max(y[wgd],/nan)] ELSE ycutoff = [min(y,/nan),max(y,/nan)]

  goodvals = goodvals AND (y GE ycutoff[0]) AND (y LE ycutoff[1]) AND (x GE xcutoff[0]) AND (x LE xcutoff[1])


  IF NOT keyword_set(title) THEN title = 'Interactive Data Cleanup'
  Base = WIDGET_BASE(TITLE = title, GROUP_LEADER=GROUP_ID, /tlb_size_events,xoffset=50,yoffset=100,kill_notify='CleanWid_base_kill_event')

;Button Group
  btndata = {myname:'THEBUTTON'}
  btnptr = PTR_NEW(btndata,/NO_COPY)
  menu = Cw_Bgroup( Base, ['Accept', 'Reset','Undo', 'Cancel'], /row, IDS=IDS, UVALUE=btnptr)

;Fuzzy tolerance slider
  IF NOT keyword_set(target) THEN target = 1.0
  fuzzytol = float(target)
  slider = widget_slider(Base,event_pro='CleanWid_slider_event',maximum=100,minimum=1,xoffset=210,yoffset=0,title='Tolerance',value=fuzzytol)

;Psym slider
  IF NOT keyword_set(psym) THEN psym = 1.0
  psymslider = widget_slider(Base,event_pro='CleanWid_psym_event',maximum=7,minimum=-7,xoffset=210,yoffset=40,title='Psym',value=psym)

;xrange,yrange,xcutoff,ycutoff text boxes

  xrb1data = {myname:'XRANGEBOX1'}
  xrb1ptr = PTR_NEW(xrb1data,/NO_COPY)
  xrangebox1 = widget_text(Base,/editable,event_PRO='CleanWid_textbox_event',uvalue=xrb1ptr,value=strtrim(string(float(xrange[0]))),xsize=8,ysize=1,xoffset=315)

  xrb2data = {myname:'XRANGEBOX2'}
  xrb2ptr = PTR_NEW(xrb2data,/NO_COPY)
  xrangebox2 = widget_text(Base,/editable,event_PRO='CleanWid_textbox_event',uvalue=xrb2ptr,value=strtrim(string(float(xrange[1]))),xsize=8,ysize=1,xoffset=315,yoffset=30)
  xrangeboxlab = widget_label(base,value='X Range',xoffset=320,yoffset=55)

  yrb1data = {myname:'YRANGEBOX1'}
  yrb1ptr = PTR_NEW(yrb1data,/NO_COPY)
  yrangebox1 = widget_text(Base,/editable,event_PRO='CleanWid_textbox_event',uvalue=yrb1ptr,value=strtrim(string(float(yrange[0]))),xsize=8,ysize=1,xoffset=380,yoffset=0)

  yrb2data = {myname:'YRANGEBOX2'}
  yrb2ptr = PTR_NEW(yrb2data,/NO_COPY)
  yrangebox2 = widget_text(Base,/editable,event_PRO='CleanWid_textbox_event',uvalue=yrb2ptr,value=strtrim(string(float(yrange[1]))),xsize=8,ysize=1,xoffset=380,yoffset=30)
  yrangeboxlab = widget_label(base,value='Y Range',xoffset=385,yoffset=55)

  xcb1data = {myname:'XCUTOFFBOX1'}
  xcb1ptr = PTR_NEW(xcb1data,/NO_COPY)
  xcutoffbox1 = widget_text(Base,/editable,event_PRO='CleanWid_textbox_event',uvalue=xcb1ptr,value=strtrim(string(float(xcutoff[0]))),xsize=8,ysize=1,xoffset=0,yoffset=30)

  xcb2data = {myname:'XCUTOFFBOX2'}
  xcb2ptr = PTR_NEW(xcb2data,/NO_COPY)
  xcutoffbox2 = widget_text(Base,/editable,event_PRO='CleanWid_textbox_event',uvalue=xcb2ptr,value=strtrim(string(float(xcutoff[1]))),xsize=8,ysize=1,xoffset=0,yoffset=60)
  xcutoffboxlab = widget_label(base,value='X Cutoff',xoffset=5,yoffset=85)

  ycb1data = {myname:'YCUTOFFBOX1'}
  ycb1ptr = PTR_NEW(ycb1data,/NO_COPY)
  ycutoffbox1 = widget_text(Base,/editable,event_PRO='CleanWid_textbox_event',uvalue=ycb1ptr,value=strtrim(string(float(ycutoff[0]))),xsize=8,ysize=1,xoffset=65,yoffset=30)

  ycb2data = {myname:'YCUTOFFBOX2'}
  ycb2ptr = PTR_NEW(ycb2data,/NO_COPY)
  ycutoffbox2 = widget_text(Base,/editable,event_PRO='CleanWid_textbox_event',uvalue=ycb2ptr,value=strtrim(string(float(ycutoff[1]))),xsize=8,ysize=1,xoffset=65,yoffset=60)
  ycutoffboxlab = widget_label(base,value='Y Cutoff',xoffset=70,yoffset=85)

;The Window
  fitwinx = 450
  fitwiny = 450
  fitwin = widget_draw(Base,/button_events,event_PRO='CleanWid_Win_event',xsize=fitwinx,ysize=fitwiny,xoffset=0,yoffset=100)

;Setup flags and multiple undo
  oldgoodvals = goodvals
  cancelflag = 3b
  undovals = reform(replicate_arr(goodvals,10),n_elements(goodvals),10)
  cut = float([xcutoff[0],xcutoff[1],ycutoff[0],ycutoff[1]])
  undocut = reform(replicate_arr(cut,10),n_elements(cut),10)
  zoomstart = float([nan(),nan()])
  boxselect = float([nan(),nan()])

;Create a pointer to a structure containing all the info for widget events
  dta = {myname: 'THEBASE', FitWinNum:0, FitWin:FitWin, Fitwinx:fitwinx, $
         FitWiny:fitwiny, winsize:winsize,  x:x, y:y, xrange:float(xrange), $
         yrange:float(yrange), xcutoff:float(xcutoff), ycutoff:float(ycutoff), $
         goodvals:goodvals, undovals:undovals, oldgoodvals: oldgoodvals,  $
         fuzzytol:float(fuzzytol), cancelflag:cancelflag, psym:psym, $
         initxr:float(xrange), inityr:float(yrange), initxc:float(xcutoff), $
         inityc:float(ycutoff), undocut:undocut, xcb1:xcutoffbox1, xcb2:xcutoffbox2, $
         ycb1:ycutoffbox1, ycb2:ycutoffbox2, xrb1:xrangebox1, xrb2:xrangebox2, $
         yrb1:yrangebox1, yrb2:yrangebox2, zoomstart:zoomstart, boxselect:boxselect}
  ptrdta = PTR_NEW(dta,/NO_COPY)

;PART 3. Start the widget!

;Setup Colors
  loadct,0
  !p.background=255
  !p.color=0
  !x.style=1
  !y.style=1

;Realize widget
  WIDGET_CONTROL, Base,set_uvalue=ptrdta  
  WIDGET_CONTROL, Base, /REALIZE

;Find the window numbers of the draw windows and base size and set checkbox
  WIDGET_CONTROL, FitWin, GET_VALUE = FitWinNum
  (*ptrdta).FitWinNum = FitWinNum
  widget_control,Base,tlb_get_Size=origsize
  (*ptrdta).winsize = origsize

;Draw the original data
  CleanWid_Draw,ptrdta

;Run Xmanager - Now the widge starts running and calling event handlers
  XManager, "CleanWid_base", Base

;PART 4. Clean up and get return values

;Get the return values
  cancelflag = (*ptrdta).cancelflag
  goodvals = (*ptrdta).goodvals
  xcutoff = (*ptrdta).xcutoff
  ycutoff = (*ptrdta).ycutoff
  numkill = 0
  IF cancelflag GE 1 THEN killvals = 1-oldgoodvals ELSE killvals = 1-goodvals
  wkillvals = where(killvals,numkill)

;Put the colors back
  loadct,0
  !p.background=255
  !p.color=0
  !p.multi = 0

;Destroy pointers
  ptr_free,btnptr
  ptr_free,xrb1ptr
  ptr_free,xrb2ptr
  ptr_free,yrb1ptr
  ptr_free,yrb2ptr
  ptr_free,xcb1ptr
  ptr_free,xcb2ptr
  ptr_free,ycb1ptr
  ptr_free,ycb2ptr
  ptr_free,ptrdta
  !except = oldexcept

  return,wkillvals
END

PRO start_cleanwid
  x = findgen(100)
  y = randomu(200,100)
  val = iclean(x,y,title='Sample Interactive Cleaning')
  print,val
END
