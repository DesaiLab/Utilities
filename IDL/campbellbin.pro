PRO campbellbin,fname,outfl,compress=compress,verbose=verbose,binary=binary
;Process Campbell Sci binary formatted Final Storage Files

;Basic Algorithm:
;open the file
;read 2 bytes at a time and decode
;if hi res, read the next two bytes
;At start of new line, output the previous line

;if no output file is given, then output is printed to screen

  IF n_elements(outfl) NE 0 THEN BEGIN
    openw,fo,outfl,/get_lun
  ENDIF ELSE BEGIN
    fo = -1
  ENDELSE 

  IF file_test(fname,/read) THEN BEGIN 
    openr,f,fname,/get_lun,/swap_if_big_endian,compress=compress
    WHILE NOT eof(f) DO BEGIN
      byt1 = 0b
      byt2 = 0b
      readu,f,byt1
      IF eof(f) THEN GOTO,skip ELSE readu,f,byt2
      bit1 = bytebin(byt1)
      bit2 = bytebin(byt2)
      IF bit1[3] EQ 1 AND bit1[4] EQ 1 AND bit1[5] EQ 1 THEN BEGIN
        CASE 1 OF 
          bit1[0] EQ 1 AND bit1[1] EQ 1 AND bit1[2] EQ 1 : BEGIN 
            ;Header - start of a new line
            num = binbyte([bit1[6:7],bit2])
            ;output the previous line
            IF n_elements(theline) NE 0 THEN printf,fo,theline,format='(a-255)'
;instead of printing the line you could process it here
            theline = string(num,format='(i10)')
            IF keyword_set(verbose) THEN print,'Header ',num
          END 
          bit1[2] EQ 0 : BEGIN
            ;start of 1st high res byte, read the next two
            byt3 = 0b
            byt4 = 0b
            IF eof(f) THEN GOTO,skip ELSE readu,f,byt3
            IF eof(f) THEN GOTO,skip ELSE readu,f,byt4
            bit3 = bytebin(byt3)
            bit4 = bytebin(byt4)
            IF bit3[0] EQ 0 AND bit3[1] EQ 0 AND bit3[2] EQ 1 $
              AND bit3[3] EQ 1 AND bit3[4] EQ 1 AND bit3[5] EQ 1 THEN BEGIN 
              sign = (bit1[1]*(-2.0))+1.0
              expo = binbyte([bit1[6],bit1[7],bit1[0]])
              num = binbyte([bit3[7],bit2,bit4])
              num = sign * float(num) * (10^((-1.0)*expo))
              IF expo EQ 0 THEN theline = theline + string(num,format='(i10)') ELSE theline = theline +string(num,format='(f10.'+strtrim(string(expo),2)+')')
              IF keyword_set(verbose) THEN print,'Hi res value ',num
            ENDIF ELSE BEGIN 
              IF keyword_set(verbose) THEN print,'Bad hires values, skipping ',bit1,bit2,bit3,bit4
            ENDELSE 
          END 
          bit1[0] EQ 0 AND bit1[1] EQ 1 AND bit1[2] EQ 1 : BEGIN
            IF keyword_set(verbose) THEN print,'DUMMY!'      ;dummy values
          END 
          ELSE : BEGIN
            IF keyword_set(verbose) THEN print,'Error, skipping 2 bits ',bit1,bit2
          END 
        ENDCASE   
      ENDIF ELSE BEGIN
        ;Standard LO RES value
        sign = (bit1[0]*(-2.0))+1.0
        expo = binbyte([bit1[1],bit1[2]])
        num = binbyte([bit1[3:7],bit2])
        num = sign * float(num) * (10^((-1.0)*expo))
        IF expo EQ 0 THEN theline = theline + string(num,format='(i10)') ELSE theline = theline + string(num,format='(f10.'+strtrim(string(expo),2)+')')
        IF keyword_set(verbose) THEN print,'Lo res Value: ',num
      ENDELSE 
    ENDWHILE 
skip:
    IF n_elements(theline) NE 0 THEN printf,fo,theline,format='(a-255)'
    free_lun,f
    IF fo NE -1 THEN free_lun,fo
  ENDIF
END
