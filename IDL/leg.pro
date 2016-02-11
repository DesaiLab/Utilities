PRO leg, text, linestyle=linestyle, linethick=linethick, psym=psym, pos=pos, size=size, $
         thick=thick, data=data, normal=normal, box=box, nosym=nosym, color=color, $
		 xwinsize=xwinsize, ywinsize=ywinsize, where=where, whiteout=whiteout, t3d=t3d
;
;PURPOSE
;	Plots legend
;INPUTS
;	text = legend text 
;OPTIONAL INPUTS
;	linestyle = linestyle to be used for legend (default = [0,1,2,3,4,5,6,0, 1,2,...])
;	linethick = linethickness (default = [1,1,1,...])
;	psym = symbol to be used for legend
;	pos = upper left corner of legend in data or normal coordinates
;	      (default = [0.6,0.97] in normal coordinates)
;	size = character size (default = 1.)
;	thick = line and character thickness (default = 1.)
;	data = data coordinate flag (default = 0)
;	normal = normal coordinate flag (default = 1)
;	box = box flag (default = 1)
;	nosym = no symbol flag (default = 0)
;	color = colors(default = 0)
;MODIFICATION HISTORY
;	Authors: Christoph Senff
;	Last modified: 03/07/97	
;=======================================================================
; Check arguments
;=======================================================================
	if n_elements(text) eq 0 then begin
		print, 'Provide legend text'
		retall
	endif
	numb = n_elements(text)
	if n_elements(linestyle) eq 0 then begin
		linestyle = indgen(6)
		while n_elements(linestyle) lt numb do $
			linestyle = [linestyle, indgen(6)]
		linestyle = linestyle(0:numb-1)
	endif
	if n_elements(linestyle) ne numb then begin
		print, 'linestyle has less/more elements than text'
		retall
	endif
	if n_elements(linethick) eq 0 then linethick = replicate(1,numb)
	if n_elements(linethick) ne numb then begin
		print, 'linethick has less/more elements than text'
		retall
	endif
	if n_elements(psym) eq 0 then psym = replicate(0, numb)
	if n_elements(psym) ne numb then begin
		print, 'psym has less/more elements than text'
		retall
	endif
	index = where(psym gt 0)
	n_index = n_elements(index)
	if n_elements(pos) eq 0 then begin
		pos = [.6, .97]
		data = 0
		normal = 1
	endif
	if n_elements(size) eq 0 then size = 1.
	if n_elements(thick) eq 0 then thick = 1.	
	if n_elements(data) eq 0 and n_elements(normal) eq 0 then begin
		data = 0 
		normal = 1
	endif
	if n_elements(data) ne 0 and n_elements(normal) eq 0 then normal = 0
	if n_elements(data) eq 0 and n_elements(normal) ne 0 then data = 0
	if (data eq 0 and normal eq 0) or (data eq 1 and normal eq 1) then begin
		print, 'Set either data or normal to 1'
		retall
	endif
	if n_elements(box) eq 0 then box = 1
	if n_elements(nosym) eq 0 then nosym = 0
	if n_elements(color) eq 0 then color = replicate(0,numb)
	if n_elements(where) eq 0 then where = 'ul'
	if n_elements(whiteout) eq 0 then whiteout = 1
	if n_elements(t3d) eq 0 then t3d = 0
;=====================================================================
; Calculate character width and height in data coordinates
;=====================================================================
	cw = !d.x_ch_size * size
	ch = !d.y_ch_size * size
	c2_size = convert_coord(2.*cw, 2.*ch, /dev, /to_data)
    c_size = convert_coord(cw, ch, /dev, /to_data)
	if !x.type then cw = alog10(c2_size(0)) - alog10(c_size(0)) $
	else cw = c2_size(0) - c_size(0)
	if !y.type then ch = alog10(c2_size(1)) - alog10(c_size(1)) $
	else ch = c2_size(1) - c_size(1)
;=====================================================================
; If pos is in normal coordinates convert to data coordinates
;=====================================================================
	del_x = !x.crange(1) - !x.crange(0)
	del_y = !y.crange(1) - !y.crange(0)
	if normal then begin
        xpos = !x.crange(0) + del_x*pos(0)
        ypos = !y.crange(0) + del_y*pos(1)
		if !x.type then xpos = 10^xpos
		if !y.type then ypos = 10^ypos
		position = [xpos, ypos]
	endif else position = pos
;=====================================================================
; Determine position of legend elements
;=====================================================================
	presymlen = replicate(1.*cw, numb)
	symlen = replicate(0.03*del_x, numb)
	pretexlen = replicate(2.*cw, numb)
	if index(0) ne -1 then begin
		presymlen(index) = 1.5*cw
		symlen(index) = 0.
		pretexlen(index) = 3.*cw
	endif
	if nosym then begin
		presymlen = replicate(1.*cw, numb)
		symlen = replicate(0., numb)
		pretexlen = replicate(0., numb)
	endif
;=======================================================
; Determine text length after removing "!"'s
;=======================================================
	len = intarr(n_elements(text))
	for i=0, n_elements(text)-1 do begin
		j = 0
		cnt = 0
		while(j ne -1) do begin
			j=strpos(text(i), '!', j)
			if j ne -1 then begin
				cnt = cnt + 1
				j = j + 1
			endif
		endwhile
		len(i) = strlen(text(i)) - 2*cnt
	endfor
	texlen = max(len)*cw
;=====================================================================
; Determine size and position of bounding box
;=====================================================================
	xboxsize = max(presymlen+symlen+pretexlen) + texlen
	yboxsize = (numb + 0.8) * ch
	if where eq 'ul' then begin
		if !x.type then begin
			xbox = position(0) * [1., 10^xboxsize]
			ybox = position(1) / [10^yboxsize, 1.]
		endif else begin
			 xbox = position(0) + [0., xboxsize]
			 ybox = position(1) - [yboxsize, 0.]
		endelse
	endif else if where eq 'ur' then begin
		if !x.type then begin
			xbox = position(0) / [10^xboxsize, 1.]
			ybox = position(1) / [10^yboxsize, 1.]
		endif else begin
			 xbox = position(0) - [xboxsize, 0.]
			 ybox = position(1) - [yboxsize, 0.]
		endelse
	endif else if where eq 'll' then begin
		if !x.type then begin
			xbox = position(0) * [1., 10^xboxsize]
			ybox = position(1) * [1., 10^yboxsize]
		endif else begin
			 xbox = position(0) + [0., xboxsize]
			 ybox = position(1) + [0., yboxsize]
		endelse
	endif else if where eq 'lr' then begin
		if !x.type then begin
			xbox = position(0) / [10^xboxsize, 1.]
			ybox = position(1) * [1., 10^yboxsize]
		endif else begin
			 xbox = position(0) - [xboxsize, 0.]
			 ybox = position(1) + [0., yboxsize]
		endelse
	endif
;=====================================================================
; Line/symbol and text position 
;=====================================================================
	if !x.type then begin
		xsym_begin = xbox(0) * 10^presymlen
		xsym_end = xsym_begin * 10^symlen
		xtex_begin = xsym_end * 10^pretexlen
	endif else begin
		xsym_begin = xbox(0) + presymlen
		xsym_end = xsym_begin + symlen
		xtex_begin = xsym_end + pretexlen
	endelse
;	if nosym then xtex_begin = xsym_begin
	if index(0) ne -1 and n_index ne numb then $
		xtex_begin = replicate(max(xtex_begin),numb)
	if !y.type then begin
		ysym = ybox(1) / 10^((findgen(numb)+1.)*ch)
		ytex = ysym / 10^(0.2*ch)
	endif else begin
		ysym = ybox(1) - (findgen(numb)+1.)*ch
		ytex = ysym - 0.2*ch
	endelse 
;=====================================================================
; White out background where legend is to be plotted
;=====================================================================
	if whiteout then begin
	if !x.type then xdim = ceil(!x.s(1)*alog10(xbox(1)/xbox(0)) * xwinsize) $
	else xdim = ceil(!x.s(1)*(xbox(1)-xbox(0)) * xwinsize)
	if !y.type then ydim = ceil(!y.s(1)*alog10(ybox(1)/ybox(0)) * ywinsize) $
	else ydim = ceil(!y.s(1)*(ybox(1)-ybox(0)) * ywinsize)
	if !x.type then xtv = !x.s(1)*alog10(xbox(0))+!x.s(0) $
	else xtv = !x.s(1)*xbox(0)+!x.s(0)
	if !y.type then ytv = !y.s(1)*alog10(ybox(0))+!y.s(0) $
	else ytv = !y.s(1)*ybox(0)+!y.s(0)
	tv, replicate(byte(!p.background),xdim,ydim), xtv, ytv, $
		xsize=float(xdim)/float(xwinsize), $
	    ysize=float(ydim)/float(ywinsize), /normal
	endif
;=====================================================================
; Plot legend
;=====================================================================
	for i=0, numb-1 do begin
		if nosym eq 0 then $
			oplot, [xsym_begin(i), xsym_end(i)], [ysym(i), ysym(i)], $
                   linestyle = linestyle(i), thick = linethick(i), $
                   psym=psym(i), color=color(i), symsize=size, t3d=t3d
		xyouts, xtex_begin(i), ytex(i), text(i), al = 0.0, chars = size, $
                chart = thick, t3d=t3d
	endfor
;=====================================================================
; Plot box, if specified
;=====================================================================
	if box then begin
		oplot, xbox, [ybox(1), ybox(1)], thick=thick, t3d=t3d
		oplot, [xbox(1), xbox(1)], ybox, thick=thick, t3d=t3d
		oplot, xbox, [ybox(0), ybox(0)], thick=thick, t3d=t3d
		oplot, [xbox(0), xbox(0)], ybox, thick=thick, t3d=t3d
	endif
;=====================================================================
	END
