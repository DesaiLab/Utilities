pro imap,data,xax=xax,yax=yax,title=title,sub_title=sub_title $
	,step=step,post=post,filename=filename $
	,noimage=noimage,nocontour=nocontour,nobar=nobar $
	,ytitle=ytitle,xtitle=xtitle,bar_title=bar_title $
	,time=time,reverse=reverse,y_n_ticks=y_n_ticks $
	,x_n_ticks=x_n_ticks,y_b_ticks=y_b_ticks $
	,overplot=overplot,coloff=coloff,area=area
;+
;routine to make colour image of temperature data with overlayed countour
;plot and color bar. by choice also to a postscript file.
; created : jan-1995	last update : 25.11.96
; G. Kargl MPAE
;area keyword (structure) contains information about used device window size, margins...
;example : imap,data,/nocontour
;	   contour,ins,position=[area.px(0),area.py(0),area.px(1),area.py(1)] $
;		,/device,col=100,/noer,xst=1,yst=1
;produces contour plot into the same area
;-
;-------------------------------------------------------------------------
on_error,2

if not keyword_set(step) then step=20			;Number of contour steps
if not keyword_set(title) then title=' '		;Image Title
if not keyword_set(sub_title) then sub_title=' '	;Image Sub Title
if not keyword_set(filename) then filename='imap.ps'	;default filename
if not keyword_set(bar_title) then bar_title=' '	;Y axis title of bar
if not keyword_set(ytitle) then ytitle=' '		;Image Y axis title
if not keyword_set(xtitle) then xtitle=' '		;Image X axis title
if not keyword_set(time) then time=0			;Make Time axis
if not keyword_set(reverse) then reverse=0		;Reverse Y axis range
if not keyword_set(y_n_ticks) then y_n_ticks=5		;Number of Y axis ticks
if not keyword_set(x_n_ticks) then x_n_ticks=6		;Number of X axis ticks
if not keyword_set(y_b_ticks) then y_b_ticks=4		;Number of Bar Y axis ticks
data_size=size(data)
if not keyword_set(xax) then xax=findgen(data_size(1))	;default for X axis
if not keyword_set(yax) then yax=findgen(data_size(2))	;default for Y axis
;if not keyword_set(coloff) then coloff=[0,0]		;offset index for color table
;if not keyword_set(nobar) then nobar=0			;plot without color bar
	
	if keyword_set(post) then begin
		plot_dev=!d.name			;save current plot device
		set_plot,'PS',/copy
		device,/color,bits_per_pixel=8,/encapsul,/landscape,/time,/bold ,file=filename
;		coloff(1)=coloff(1) > 15		;minimum color offset for postscript
		print,'Postscript on and plot to ',filename
	endif


max_data=max(data)					;Data maximum value
min_data=min(data)					;Data minimum value
bins=fix((max_data-min_data)/step)			;Number of contour bins (max 30)
if bins lt 1 then bins=1
if bins gt 30 then begin
	bins=30
	step=fix((max_data-min_data)/bins)
endif
print,'Contour Steps : ',bins
	level_vec= fix(min_data)+findgen(bins)*step	;position of contour levels
	siz=size(level_vec)
	label_vec=replicate(1,siz(1))			;label contour levels
	
;select ratio of image window
	if keyword_set(noimage) or keyword_set(nobar) then begin
		shrink=1.
	endif else begin
		shrink=0.85
	endelse

	siz=size(data)					;get size of image
	if not keyword_set(overplot) then begin
		xmargin_save=!x.margin			;save old margins
		ymargin_save=!y.margin
		!x.margin=[10,10]			;enhance x margins
		!y.margin=[6,6]				;enhance y margins
		
		plot,[0,1],xstyle=5,ystyle=5,/nodata $	;dummy plot
		,title=title,subtitle=sub_title		;to establish
							;coordinate system
	endif
;-------------------------------------------------------------------------
;plot contour and image of datafield

	xwinsave=!x.window				;save old plot window
	ywinsave=!y.window

;get size of plot window in normalised coordinates	;contains the image
	xpos= [!x.window(0),!x.window(1)*shrink]	
	ypos= !Y.window					

;get size of plot window in device coordinates
	px= xpos * !D.X_vsize				
	py= ypos * !D.Y_vsize

;size in x and y in device units
	swx = px(1) - px(0) +1
	swy = py(1) - py(0) +1

;image size
	six= float(siz(1))				;image sizes
	siy= float(siz(2))
	area={xpos:xpos,ypos:ypos,px:px,py:py,swx:swx,swy:swy $
		,xmargin:!x.margin,ymargin:!y.margin $
		,xwinsave:xwinsave,ywinsave:ywinsave}
;erase
;-------------------------------------------------------------------------
	mx= !d.n_colors						;brightest color
	tvlct,r,g,b,/get					;save color table
;stretch color table
if keyword_set(coloff) then begin
print,'!d.table_size',!d.table_size,' Color offset',coloff	;available colors
	print,'Create new color table'
		rp=congrid(r(coloff(0):mx-coloff(1)-1),mx-1)
			rp(0)=r(0)
		gp=congrid(g(coloff(0):mx-coloff(1)-1),mx-1)
			gp(0)=r(0)
		bp=congrid(b(coloff(0):mx-coloff(1)-1),mx-1)
			bp(0)=r(0)
	tvlct,rp,gp,bp						;load new color table
endif
;-------------------------------------------------------------------------
;check plot device and plot image
	if not keyword_set(noimage) then begin
		if (!d.flags and 1) ne 0 then begin		;scalable pixels?
;scalable pixels (eg. Postscript)
			resc_data=congrid(data,256,256)		;rescale image for more colors
			tvscl,resc_data,px(0),py(0), ysize=swy ,xsize=swx,/device
		endif else begin
;non scalable pixels
			tv,poly_2d( bytscl(data) $		;expand image
			,[[0,0],[(six-1)/swx,0]] $
			,[[0,(siy-1)/swy],[0,0]] $
			,1,swx,swy ),px(0),py(0)
		endelse
	endif
;-------------------------------------------------------------------------

if not keyword_set(nocontour) then begin		;nocontour=1 -> no contour plot
contour,data,xax,yax,/noer,xstyle=5,ystyle=5, $		;make contour plot to plot window
	levels=level_vec,c_labels=label_vec, $		;make contour leves, no axes
	position=[px(0),py(0),px(1),py(1)],/device
;-------------------------------------------------------------------------
;left y axis
	if reverse then begin
		yrange=[yax(n_elements(yax)-1),yax(0)]
	endif else begin
		yrange=[yax(0),yax(n_elements(yax)-1)]
	endelse
tick_value=findgen(siz(2))
axis,yaxis=0,yminor=1,ystyle=1,yticks=y_n_ticks, $
	ytitle=ytitle,yrange=yrange
;-------------------------------------------------------------------------
;right y axis						;could be used for independent axis
;axis,yaxis=1,ystyle=1,yticks=1
;axis,yaxis=1,yminor=1,ystyle=1,yticks=y_n_ticks, $	;on right side of plot
;	yrange=yrange
;	ytitle=ytitle, $
;-------------------------------------------------------------------------
;lower x axis
axis,xaxis=0,xstyle=1,  $
	xtitle=xtitle,xminor=3,xticks=x_n_ticks, $
	xrange=[min(xax),max(xax)]
;-------------------------------------------------------------------------
;upper x axis
 if time then begin					;make rotation period to virtual
	axis,xaxis=1,xstyle=1,  $			;24 hours
		xtitle='Local Time',xminor=4,xticks=x_n_ticks, $
		xrange=[min(xax*24./360.),max(xax*24./360.)], $
		xtickformat="(f6.2,'!U h')"
endif else begin					;could be used for independent axis
  ;axis,xaxis=1,xstyle=1,xticks=1,xrange=[min(xax),max(xax)]
	;axis,xaxis=1,xstyle=1,  $			;on upper side of plot
	;	xtitle=xtitle,xminor=3,xticks=x_n_ticks, $
	;	xrange=[min(xax),max(xax)]
endelse
endif
;-------------------------------------------------------------------------
;-------------------------------------------------------------------------
;plot color bar
;if >noimage< then no image and color bar, if >nobar< no  color bar
if not keyword_set(noimage) and not keyword_set(nobar) then begin

	bar=fltarr(2,siz(2))					;define bar array

	bar_step=(max_data - min_data)/siz(2)			;color step of bar over range of
	bar(0,*)=findgen(siz(2))*bar_step+min_data		;image dynamic
	bar(1,*)=bar(0,*)

	broad=10						;default broadness of c-bar
	bar=rebin(bar,broad,siz(2))				;make bar image
;-------------------------------------------------------------------------
	siz=size(bar)
		!x.window=xwinsave				;restore total plot window size
		!y.window=ywinsave
;get size of bar window in normalised coordinates
	xpos= [!x.window(1)*shrink+0.11,!x.window(1)]
	ypos= !Y.window

	px= xpos * !D.X_vsize
	py= ypos * !D.Y_vsize
	pxc=px(sort(px))

	swx = (pxc(1) - pxc(0) +1) >4
	swy = (py(1) - py(0) +1) >1

	six= float(siz(1))					;bar image sizes
	siy= float(siz(2))
;-------------------------------------------------------------------------
	if (!d.flags and 1) ne 0 then begin			;scalable pixels?
		;scalable pixels (eg. Postscript)
		tvscl,bar,pxc(0),py(0), ysize=swy ,xsize=swx	;plot bar image
	endif else begin
		;non scalable pxels (eg. Bitmaps, X-Window)
		tv,poly_2d(bytscl(bar), $			;expand bar image
		[[0,0],[six/swx,0]],[[0,siy/swy],[0,0]], $
			1,swx,swy),pxc(0),py(0)
	endelse
	
;make frame and establish new plot
	contour,bar,/noer,xstyle=1,ystyle=9,/nodata, $
	position=[pxc(0),py(0),pxc(1),py(1)],/device, $	;window
	xticks=1,yticks=1,ytickname=[' ',' '], $
	xtickname=[' ',' '],xminor=-1,yminor=-1

	axis,yaxis=1,yrange=[min_data,max_data],ystyle=1,ytitle=bar_title; $	;plot bar axis
;		,yticks=y_b_ticks
		!x.window=xwinsave				;restore total plot window size
		!y.window=ywinsave
		!x.margin=xmargin_save				;restore old margins
		!y.margin=ymargin_save

endif
;-------------------------------------------------------------------------
if keyword_set(post) then begin
	device, /close
	set_plot,plot_dev				;restore plot device
	if keyword_set(coloff) then tvlct,r,g,b		;restore color table
	Print,'Postscript off'
endif
;-------------------------------------------------------------------------
end
