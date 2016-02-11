;+
; NAME:
;       PRINTWINDOW
;
; PURPOSE:
;       The purpose of this program is to provide a quick-and-easy way
;       to print the current IDL Direct Graphics window (!D.WINDOW) to
;       the printer device centered on a 8.5" x 11" piece of paper
;       with 1" margins (if needed). These margins will only be
;       accurate if the unprintable area for the printer is set to
;       zero. If the printer and/or its driver has margins
;       (i.e.unprintable area) already specified this will offset the
;       IDL output. This routine attempts to match the size of the
;       print to the size of the graphics window on the monitor.
;
; CATEGORY:
;       Visual
;
; CALLING SEQUENCE:
;       PRINTWINDOW
;
; INPUT PARAMETERS:
;       None.
;
; KEYWORD PARAMETERS:
;       None.
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; DISCLAIMER:
;       This procedure is provided "as is" and is not supported or
;       maintained by Research Systems Inc.
;
; MODIFICATION HISTORY:
;       Written by: Adam Bielecki, April 2000.
;       Added support for 8-bit color and Landscape orientation
;            printing: Adam Bielecki, January 2002.
;-
 
PRO PRINTWINDOW
COMPILE_OPT HIDDEN
;error catching:
!error_state.code=0
catch,error
if error NE 0 then begin
    help,/last_message,output=traceback
    errarray=['Error Caught',traceback]
    dummy=dialog_message(errarray,/error)
    if !D.NAME EQ 'PRINTER' and size(scale,/type) NE 0 then device,scale_factor=scale
    set_plot,currDevice
    return
endif
;check to make sure IDL graphics window is open:
if !D.WINDOW EQ -1 then begin
    dummy=DIALOG_MESSAGE('No IDL graphics windows currently open !',/error)
    return
endif
;obtain current device:
currDevice=!D.NAME
;obtain current device resolution:
xppi=!D.X_PX_CM*2.54
yppi=!D.Y_PX_CM*2.54
;query visual class and TVRD the current graphics window:
if !D.N_COLORS GT 256 then begin
    if !ORDER EQ 1 then image=TVRD(TRUE=3,/ORDER) else image=TVRD(TRUE=3)
endif else begin
    if !ORDER EQ 1 then pseudoImage=TVRD(/ORDER) else pseudoImage=TVRD()
    TVLCT,r,g,b,/GET
    imageSize=SIZE(pseudoImage,/DIMENSIONS)
    image=BYTARR(imageSize[0],imageSize[1],3)
    image[*,*,0]=r[pseudoImage]
    image[*,*,1]=g[pseudoImage]
    image[*,*,2]=b[pseudoImage]
endelse
;change to the PRINTER device:
SET_PLOT,'PRINTER'
;present the user with printer properties dialog:
dummy=DIALOG_PRINTERSETUP()
if (dummy EQ 0) then begin
    SET_PLOT,currDevice
    RETURN
endif else begin
                                ;obtain original scale factor for Printer device:
    HELP,/DEV,OUTPUT=orig
    scalepos=STRPOS(STRUPCASE(orig),'SCALE FACTOR')
    scaleindex=WHERE(scalepos NE -1,count)
    if count EQ 0 then begin
        dummy=DIALOG_MESSAGE('Indeterminable Scale Factor for PRINTER device.',/error)
        SET_PLOT,currDevice
        RETURN
    endif
    scaleindex=scaleindex[0]
    scalepos=scalepos[scaleindex]+13
    scale=STRTRIM(STRMID(orig[scaleindex],scalepos),2)
    scale=FLOAT(scale)
                                ;determine if output format selected
                                ;by user is Portrait or Landscape:
    orientpos=STRPOS(STRUPCASE(orig),'ORIENTATION')
    orientindex=WHERE(orientpos NE -1,count)
    if count EQ 0 then begin
        dummy=DIALOG_MESSAGE('Indeterminable Orientation for PRINTER device.',/error)
        SET_PLOT,currDevice
        RETURN
    endif
    orientindex=orientindex[0]
    orientpos=orientpos[orientindex]+12
    orient=STRUPCASE(STRTRIM(STRMID(orig[orientindex],orientpos),2))
                                ;set scale factor equal to one:
    DEVICE,scale_factor=1
                                ;compute appropriate size of graphic
                                ;in inches as it currently appears:
    ncols=(SIZE(image))[1]
  nrows=(SIZE(image))[2]
  xsize=ncols/xppi
  ysize=nrows/yppi
                                ;print the graphic:
  CASE orient OF
    'PORTRAIT' :  begin
                                ;scale output so there is at least a 1
                                ;inch border on each side of the
                                ;image:
        if xsize LE 6.5 and ysize LE 9 then begin 
;graphic does NOT have to be reduced in resolution:
            device,xoffset=0,yoffset=0,xsize=xsize,ysize=ysize,/inches
            xmarg=(8.5-xsize)/2
            ymarg=(11-ysize)/2
            TV,TEMPORARY(image),xmarg,ymarg,true=3,xsize=xsize,ysize=ysize,/inches
            DEVICE,/CLOSE
        endif else begin 
                                ;otherwise, find limiting dimension
                                ;and reduce resolution accordingly:
            xfac=6.5/xsize
            yfac=9/ysize
            if yfac LT xfac then begin ;the Y dimension is the limiting factor:
                newxsize=xsize*yfac
                newysize=ysize*yfac
                device,xoffset=0,yoffset=0,xsize=newxsize,ysize=newysize,/inches
                xmarg=(8.5-newxsize)/2
                ymarg=(11-newysize)/2
                TV,TEMPORARY(image),xmarg,ymarg,true=3,xsize=newxsize,ysize=newysize,/inches
                DEVICE,/CLOSE
            endif else begin  ;the X dimension is the limiting factor:
                newxsize=xsize*xfac
                newysize=ysize*xfac
                device,xoffset=0,yoffset=0,xsize=newxsize,ysize=newysize,/inches
                xmarg=(8.5-newxsize)/2
                ymarg=(11-newysize)/2
                TV,TEMPORARY(image),xmarg,ymarg,true=3,xsize=newxsize,ysize=newysize,/inches
                DEVICE,/CLOSE
            endelse
        endelse
    end
    'LANDSCAPE' : begin
                                ;scale output so there is at least a 1
                                ;inch border on each side of the
                                ;image:
        if xsize LE 9 and ysize LE 6.5 then begin 
                                ;graphic does NOT have to be reduced
                                ;in resolution:
            device,xoffset=0,yoffset=0,xsize=xsize,ysize=ysize,/inches
            xmarg=(11-xsize)/2
            ymarg=(8.5-ysize)/2
            TV,TEMPORARY(image),xmarg,ymarg,true=3,xsize=xsize,ysize=ysize,/inches
            DEVICE,/CLOSE
        endif else begin 
                                ;otherwise, find limiting dimension
                                ;and reduce resolution accordingly:
            xfac=9/xsize
            yfac=6.5/ysize
            if yfac LT xfac then begin ;the Y dimension is the limiting factor:
                newxsize=xsize*yfac
                newysize=ysize*yfac
                device,xoffset=0,yoffset=0,xsize=newxsize,ysize=newysize,/inches
                xmarg=(11-newxsize)/2
                ymarg=(8.5-newysize)/2
                TV,TEMPORARY(image),xmarg,ymarg,true=3,xsize=newxsize,ysize=newysize,/inches
                DEVICE,/CLOSE
            endif else begin  ;the X dimension is the limiting factor:
                newxsize=xsize*xfac
                newysize=ysize*xfac
                device,xoffset=0,yoffset=0,xsize=newxsize,ysize=newysize,/inches
                xmarg=(11-newxsize)/2
                ymarg=(8.5-newysize)/2
                TV,TEMPORARY(image),xmarg,ymarg,true=3,xsize=newxsize,ysize=newysize,/inches
                DEVICE,/CLOSE
            endelse
        endelse
    end
    else :        begin
        dummy=DIALOG_MESSAGE('Indeterminable Orientation for PRINTER device.',/error)
        DEVICE,scale_factor=scale
        SET_PLOT,currDevice
        RETURN
    end
ENDCASE
                                ;reset scale factor to original:
DEVICE,scale_factor=scale
endelse
                                ;reset current device to original:
SET_PLOT,currDevice
END