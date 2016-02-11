FUNCTION tvwarp,img,xstart,ystart,xwidth,ywidth,wxstart,wystart,wxwidth,wywidth,zvalue=zvalue

  xorig = [xstart,xstart+xwidth-1,xstart,xstart+xwidth-1]
  yorig = [ystart,ystart,ystart+ywidth-1,ystart+ywidth-1]
  IF NOT keyword_set(zvalue) THEN thez = 0.0 ELSE thez = zvalue
  thez = (thez - !z.s[0]) / !z.s[1]

  trans = convert_coord(xorig,yorig,replicate(thez,4),/data,/to_device,/t3d)

;  xc = xorig * !x.s[1] + !x.s[0] ;Normalized X coord
;  yc = yorig * !y.s[1] + !y.s[0] ;Normalized Y
;  p = [[xc],[yc],[fltarr(4)],[replicate(1,4)]] # !P.T 
;  u1 = p[*,0]/p[*,3] * !d.x_vsize ;Scale U coordinates to device
;  v1 = p[*,1]/p[*,3] * !d.y_vsize ;and V
;  u2 = p[*,0]/p[*,3] * xwidth ;Scale U coordinates to device
;  v2 = p[*,1]/p[*,3] * ywidth ;and V
  
  u = transpose(trans[0,*])
  v = transpose(trans[1,*])

  u0 = min(u) & v0 = min(v)     ;Lower left corner of screen box
  su = max(u)- u0+1 & sv = max(v) - v0+1 ;Size of new image

  POLYWARP, xorig,yorig, u-u0, v-v0, 1, kx, ky
  A = POLY_2D(img, kx, ky, 0, su, sv, missing=!p.background)

  wxstart = u0
  wystart = v0
  wxwidth = su
  wywidth = sv
;  stop

  return,a
END

PRO tvbar,x1,y1,x2,y2,brange,dtarange,nbins,ntext=ntext,nformat=nformat,creverse=creverse,bartitle=bartitle,t3d=t3d,zvalue=zvalue,barcharsize=barcharsize,font=font,nval=nval


  IF NOT keyword_set(barcharsize) THEN barcharsize = 1.0

  IF keyword_set(t3d) THEN BEGIN
    IF NOT keyword_set(zvalue) THEN thez = 0.0 ELSE thez = zvalue
    thez = (thez - !z.s[0]) / !z.s[1]
  ENDIF

  bx1 = x1+1
  bx2 = (x2 - ( (x2-x1)/1.75))
  bx3 = x2 + ( (x2-x1)/4)
  by3 = (y2+y1)/2.0
  
  xs = [bx1-1,bx2,bx2,bx1-1,bx1-1]
  ys = [y1,y1,y2,y2,y1]
  pcoord = convert_coord(xs,ys,/device,/to_data,t3d=t3d)
  plots,pcoord[0,*],pcoord[1,*],t3d=t3d,z=thez,/noclip

  IF (brange[0] GT brange[1]) THEN BEGIN 
    binrange = reverse(brange)
    reverseflag = 1
  ENDIF ELSE BEGIN
    binrange = brange
    reverseflag = 0
  ENDELSE

  nbins = abs(nbins)

  IF keyword_set(creverse) THEN reverseflag = 1

  bins = interpol(binrange,nbins)
  IF reverseflag EQ 1 THEN bins = reverse(bins)
  binloc = interpol([y1,y2-1],nbins+1)

  FOR i = 0,n_elements(binloc)-2 DO BEGIN
    by1 = binloc[i]
    by2 = binloc[i+1]
    dcoord = convert_coord([bx1,bx2,bx2,bx1,bx1],[by1,by1,by2,by2,by1],/device,/to_data,t3d=t3d)
    polyfill,dcoord[0,*],dcoord[1,*],color=bins[i],t3d=t3d,z=thez,/noclip
  ENDFOR
  
  IF NOT keyword_set(ntext) THEN ntext = 10

  textloc = interpol([y1,y2],ntext)
  IF ((dtarange[1]-dtarange[0]+1) LT ntext) OR keyword_set(nformat) THEN BEGIN
    IF NOT keyword_set(nformat) THEN nformat = '(f20.2)'
    textval = strtrim(string(interpol(dtarange,ntext),format=nformat),2)
  ENDIF ELSE BEGIN
    textval = strtrim(string(long(interpol(dtarange,ntext))),2)
  ENDELSE

  IF keyword_set(nval) THEN textval[0:( (n_elements(nval)-1) < (n_elements(textval)-1) )] = nval[0:( (n_elements(nval)-1) < (n_elements(textval)-1))]

  FOR i = 0,n_elements(textloc)-1 DO BEGIN
    xycoord = convert_coord(bx2+2,textloc[i],/device,/to_data,t3d=t3d)
    xyouts,xycoord[0,*],xycoord[1,*],textval[i],clip=[x1,y1,x2,y2],t3d=t3d,z=thez,charsize=barcharsize,font=font
  ENDFOR

  IF keyword_set(bartitle) THEN BEGIN
    xycoord = convert_coord(bx3,by3,/device,/to_data,t3d=t3d)
    xyouts,xycoord[0,*],xycoord[1,*],bartitle,charsize=barcharsize,alignment=0.5,orientation=270,t3d=t3d,z=thez,/noclip,font=font
  ENDIF

END

PRO tvclip,cliprange,bounds,xrange,yrange,img,ixstart,iystart,ixwidth,iywidth,verbose=verbose

;cliprange is lower-left xy   and upper-right xy
;bounds is lower-left xy and upper-right xy
;xrange is leftx , rightx
;yrange is lowery, uppery
;img fits in bounds

;ixstart is set to left xcoordinate in device space
;iystart is set to bottom ycoordinate in device space
;ixwidth is set to x width in device space
;iywidth is set to y width in device space

coordspace = float([xrange[0],yrange[0],xrange[1],yrange[1]])
devspace = float([cliprange[0],cliprange[1],cliprange[2],cliprange[3]])
xbounds_coord = float([bounds[0],bounds[2]])
ybounds_coord = float([bounds[1],bounds[3]])

img_xsize = n_elements(img[*,0])
img_ysize = n_elements(img[0,*])
imgclip = float([0,0,img_xsize-1,img_ysize-1])

;maybe +1 and -1 for border?

;covert coordinate bounds into device bounds

lineslope,[coordspace[2],devspace[2],coordspace[0],devspace[0]],x_b,x_a
xbounds_dev = (x_b * xbounds_coord) + x_a

lineslope,[coordspace[3],devspace[3],coordspace[1],devspace[1]],y_b,y_a
ybounds_dev = (y_b * ybounds_coord) + y_a

;mapping of image array units to device coordinates

lineslope,[xbounds_dev[1],imgclip[2],xbounds_dev[0],imgclip[0]],imgx_b,imgx_a
lineslope,[ybounds_dev[1],imgclip[3],ybounds_dev[0],imgclip[1]],imgy_b,imgy_a

;now look at each coordinate

IF xbounds_dev[0] LT devspace[0] THEN BEGIN
  ;left side of image is past dev box
  IF keyword_set(verbose) THEN print,'left clipping ',xbounds_dev[0],devspace[0]
  imgclip[0] = (imgx_b * devspace[0]) + imgx_a
  xbounds_dev[0] = devspace[0]
ENDIF

IF xbounds_dev[1] GT devspace[2] THEN BEGIN
  ;right side of image is past dev box
  IF keyword_set(verbose) THEN print,'right clipping ',xbounds_dev[1],devspace[2]
  imgclip[2] = (imgx_b * devspace[2]) + imgx_a
  xbounds_dev[1] = devspace[2]
ENDIF

IF ybounds_dev[0] LT devspace[1] THEN BEGIN
  IF keyword_set(verbose) THEN print,'bottom clipping ',ybounds_dev[0],devspace[1]
  ;bottom of image is below dev box
  imgclip[1] = (imgy_b * devspace[1]) + imgy_a
  ybounds_dev[0] = devspace[1]
ENDIF

IF ybounds_dev[1] GT devspace[3] THEN BEGIN
  IF keyword_set(verbose) THEN print,'top clipping ',ybounds_dev[1],devspace[3]
  ;top of image is above dev box
  imgclip[3] = (imgy_b * devspace[3]) + imgy_a
  ybounds_Dev[1] = devspace[3]
ENDIF

;clip image to new imgclip
img = img[imgclip[0]:imgclip[2],imgclip[1]:imgclip[3],*]

;compute device coords of new image
ixstart = xbounds_dev[0]
ixend = xbounds_dev[1]
ixwidth = ixend-ixstart

iystart = ybounds_dev[0]
iyend = ybounds_dev[1]
iywidth = iyend-iystart

IF keyword_set(verbose) THEN BEGIN
  print,'cliprange ',cliprange
  print,'bounds ',bounds
  print,'coordspace ',coordspace
  print,'devspace ',devspace
  print,'xbounds_dev',xbounds_dev
  print,'ybounds_dev',ybounds_dev
  print,'imgclip ',imgclip
  print,'ixstart,iystart,ixend,iyend'
  print,ixstart,iystart,ixend,iyend
  print,'xywidth ',ixwidth,iywidth
ENDIF
;stop

END


PRO tvplot,oldimg,xdta,ydta,title=title,cubic=cubic,interp=interp,color=color,psym=psym,symsize=symsize,font=font,linestyle=linestyle,thick=thick,charsize=charsize,charthick=charthick,nodata=nodata,true=true,order=order,noscl=noscl,noaxis=noaxis,yrange=yrange,xrange=xrange,xtitle=xtitle,ytitle=ytitle,xticks=xticks,yticks=yticks,xtickv=xtickv,ytickv=ytickv,xtickname=xtickname,ytickname=ytickname,xstyle=xstyle,ortho=ortho,nobar=nobar,ntext=ntext,nformat=nformat,zrange=zrange,verbose=verbose,bounds=bounds,creverse=creverse,crange=crange,nbins=nbins,ticklen=ticklen,gridstyle=gridstyle,xminor=xminor,yminor=yminor,subtitle=subtitle,tickformat=tickformat,zvalue=zvalue,nullvalue=nullvalue,ixrange=ixrange,iyrange=iyrange,over=over,transparency=transparency,bartitle=bartitle,t3d=t3d,channel=channel,barcharsize=barcharsize,nval=nval,nan=nan,_extra=extra

  on_error,2

;Procedure to display image with data on top and scale bar on right
;xrange and yrange set up axes 
;zrange maps image values to color values 
;crange selects color values to use (crange[0] = color for zrange[0])
;ixrange and iyrange / or bounds sets of location of the image

  ;Set up the palette
  IF n_elements(nullvalue) NE 0 THEN BEGIN
    wherenonnull = where(oldimg ne nullvalue,nonnullcount)
    IF (nonnullcount GT 0) AND (NOT keyword_set(zrange)) THEN BEGIN
      zrange = [min(oldimg[wherenonnull],/nan),max(oldimg[wherenonnull],/nan)]
    ENDIF
  ENDIF

  IF NOT keyword_set(zrange) THEN zrange = [min(oldimg,/nan),max(oldimg,/nan)] 
  IF NOT keyword_set(crange) THEN crange = [0,!d.table_size-1]
  IF keyword_set(creverse) THEN crange = reverse(crange)
  ncolor = (crange[1]-crange[0])+1
  ;newimg maps zrange[0] to crange[0] and zrange[1] to crange[1]
  IF keyword_set(noscl) THEN img = reform(oldimg) ELSE BEGIN
    img=reform(oldimg)
    zrangemax = where(img GT max(zrange),zrangemaxcount)
    zrangemin = where(img LT min(zrange),zrangemincount)
    IF zrangemaxcount GT 0 THEN img[zrangemax] = max(zrange)
    IF zrangemincount GT 0 THEN img[zrangemin] = min(zrange)
    lineslope,float([zrange[0],crange[0],zrange[1],crange[1]]),zcm,zcb
    img = (img*zcm)+zcb
    IF keyword_set(verbose) THEN print,zcm,zcb,min(oldimg),max(oldimg),min(img),max(img)
  ENDELSE

  ;make nullvalue transparent
  IF n_elements(nullvalue) NE 0 THEN BEGIN
    wherenull = where(oldimg eq nullvalue,nullcount)
    IF nullcount GT 0 THEN img[wherenull] = !p.background
  ENDIF

  ;make nan transparent
  IF keyword_set(nan) THEN BEGIN
    wherenan = where(isnan(oldimg),nancount)
    IF nancount GT 0 THEN img[wherenan] = !p.background
  ENDIF

  ;rearrange if true color
  IF keyword_set(true) THEN BEGIN
;if true is 1 or 2, make it to be like 3
    CASE true OF 
      1 : BEGIN
        xsz = n_elements(img[0,*,0])
        ysz = n_elements(img[0,0,*])
        img = [[[reform(img[0,*,*],xsz,ysz,1)]],[[reform(img[1,*,*],xsz,ysz,1)]],[[reform(img[2,*,*],xsz,ysz,1)]]]
      END 
      2: BEGIN 
        xsz = n_elements(img[*,0,0])
        ysz = n_elements(img[0,0,*])        
        img = [[[reform(img[*,0,*],xsz,ysz,1)]],[[reform(img[*,1,*],xsz,ysz,1)]],[[reform(img[*,2,*],xsz,ysz,1)]]]        
      END 
    ENDCASE
    nobar = 1
    vtrue = 3
  ENDIF ELSE vtrue = 0
 
  ;flip if order is chosen
  IF keyword_set(order) THEN img = reverse(img,2)

  ;If no plotting data then set them to bounds of img
  IF NOT keyword_set(xdta) THEN BEGIN
    xdta = [0,n_elements(img[*,0])-1]
    ydta = [0,n_elements(img[0,*])-1]
    nodata = 1
  ENDIF

  IF keyword_set(over) THEN BEGIN
    xrange = !x.crange
    yrange = !y.crange
    nobar = 1
  ENDIF

  IF NOT keyword_set(xrange) THEN IF keyword_set(ixrange) THEN xrange = ixrange ELSE xrange = [min(xdta,/nan),max(xdta,/nan)]
  IF NOT keyword_set(yrange) THEN IF keyword_set(iyrange) THEN yrange = iyrange ELSE yrange = [min(ydta,/nan),max(ydta,/nan)]

   ;set up plot area and get coordinate information
  IF (NOT keyword_set(over)) THEN plot,[0],[0],/nodata,xstyle=5,ystyle=5,t3d=t3d,zvalue=zvalue,xrange=xrange,yrange=yrange
  cliprange = !p.clip
  xstart = cliprange[0]+1
  ystart = cliprange[1]+1
  xwidth = cliprange[2]-xstart
  ywidth = cliprange[3]-ystart

  ;calculate plotting area
  shrink = 0.1
  IF NOT keyword_set(nobar) THEN xwidth = xwidth-(xwidth*shrink)
  IF keyword_set(ortho) THEN BEGIN
    minwidth = min([xwidth,ywidth])
    xwidth = minwidth
    ywidth = minwidth
  ENDIF
  newcliprange = [xstart,ystart,xstart+xwidth+1,ystart+ywidth+1]

  IF keyword_set(verbose) THEN print,'Initial xystart/width ',xstart,ystart,xwidth,ywidth

  ;plot title and initial axes
  IF NOT keyword_set(over) THEN plot,xdta,ydta,title=title,yrange=yrange,xrange=xrange,/nodata,font=font,charsize=charsize,charthick=charthick,xticks=1,yticks=1,xtickname=[' ',' '],ytickname=[' ',' '],xstyle=xstyle,ystyle=1,position=[xstart-1,ystart-1,xstart+xwidth,ystart+ywidth],/device,/noerase,ticklen=ticklen,xminor=1,yminor=1,subtitle=subtitle,zvalue=zvalue,xgridstyle=xgridstyle,ygridstyle=ygridstyle,_extra=extra,xticklen=xticklen,yticklen=yticklen,t3d=t3d

  ;Do image clipping/expansion 

  IF NOT keyword_set(bounds) THEN bounds = [xrange[0],yrange[0],xrange[1],yrange[1]]
  IF keyword_set(ixrange) THEN bounds = [ixrange[0],bounds[1],ixrange[1],bounds[3]]
  IF keyword_set(iyrange) THEN bounds = [bounds[0],iyrange[0],bounds[2],iyrange[1]]
  tvclip,newcliprange,bounds,xrange,yrange,img,ixstart,iystart,ixwidth,iywidth,verbose=verbose

  ;if t3d then warp image
  IF keyword_set(t3d) THEN BEGIN
    img = tvwarp(img,bounds[0],bounds[1],bounds[0]+bounds[2],bounds[1]+bounds[3],ixstart,iystart,ixwidth,iywidth,zvalue=zvalue)
   ENDIF
  
  ;display the image - use x/ysize if scalable pixels (!d.flags bit 1)
  IF (!d.flags AND 1) NE 0 THEN BEGIN
    tv,img,ixstart,iystart,true=vtrue,xsize=ixwidth,ysize=iywidth,channel=channel
  ENDIF ELSE BEGIN
    IF vtrue EQ 0 THEN newimg = congrid(img,ixwidth,iywidth,interp=interp,cubic=cubic) ELSE  newimg = congrid(img,ixwidth,iywidth,3,interp=interp,cubic=cubic)
    IF keyword_set(transparency) THEN BEGIN
      IF (transparency LT 1.0) AND (transparency GT 0.0) THEN BEGIN
        curimg = tvrd(ixstart,iystart,ixwidth,iywidth,true=vtrue)
        newimg = (newimg * transparency) + (curimg * (1.0 - transparency))
      ENDIF
    ENDIF
    tv,newimg,ixstart,iystart,true=vtrue,channel=channel
  ENDELSE

  ;redraw the axes
  IF NOT keyword_set(over) THEN begin
    IF NOT keyword_set(noaxis) THEN BEGIN
      axis,yaxis=2,ystyle=1,charsize=charsize,charthick=charthick,color=color,font=font,xminor=xminor,yminor=yminor,xthick=xthick,ythick=ythick,xtickformat=xtickformat,xticklen=xticklen,xtickname=xtickname,xticks=xticks,xtickv=xtickv,xtitle=xtitle,subtitle=subtitle,zvalue=zvalue,ytickformat=ytickformat,yticklen=yticklen,ytickname=ytickname,yticks=yticks,ytickv=ytickv,_extra=extra,ticklen=ticklen,ytitle=ytitle,t3d=t3d
      axis,xaxis=2,xstyle=xstyle,charsize=charsize,charthick=charthick,color=color,font=font,xminor=xminor,yminor=yminor,xthick=xthick,ythick=ythick,xtickformat=xtickformat,xticklen=xticklen,xtickname=xtickname,xticks=xticks,xtickv=xtickv,xtitle=xtitle,subtitle=subtitle,zvalue=zvalue,ytickformat=ytickformat,yticklen=yticklen,ytickname=ytickname,yticks=yticks,ytickv=ytickv,ytitle=ytitle,_extra=extra,ticklen=ticklen,t3d=t3d
    ENDIF
  ENDIF

  ;plot the data
  IF NOT keyword_set(nodata) THEN oplot,xdta,ydta,color=color,psym=psym,symsize=symsize,linestyle=linestyle,thick=thick,zvalue=zvalue,t3d=t3d,_extra=extra

  ;draw the scalebar
  IF NOT keyword_set(over) THEN BEGIN
    IF NOT keyword_set(nobar) THEN BEGIN
      xloc1 = xstart+xwidth+1
      yloc1 = ystart
      xloc2 = xloc1 + (xwidth*shrink)
      yloc2 = ystart+ywidth+1
      xrng = xloc2-xloc1
      yrng = yloc2-yloc1
      xmargin = 0.1
      ymargin = 0.0
      xmargin = xrng*xmargin
      ymargin = yrng*ymargin
      x1 = xloc1+xmargin
      y1 = yloc1+ymargin
      x2 = xloc2-xmargin
      y2 = yloc2-ymargin
      npixels = (y2-y1)-2
      IF keyword_set(noscl) THEN BEGIN
        binrange = zrange
        dtarange = zrange
      ENDIF ELSE BEGIN
        dtarange = zrange
        binrange = crange
      ENDELSE
      IF NOT keyword_set(nbins) THEN nbins = min([binrange[1]-binrange[0],npixels])
      tvbar,x1,y1,x2,y2,binrange,dtarange,nbins,ntext=ntext,nformat=nformat,creverse=creverse,bartitle=bartitle,t3d=t3d,zvalue=zvalue,barcharsize=barcharsize,font=font,nval=nval
    ENDIF
  ENDIF
END

