PRO	EOFcalc,field,lats,eofs,pcs,svls,FVE,cutoff



;EOFcalc
; calculates the EOFs of a given field.

; corrected the variances -

; modified
; from /home/dan/idl/EOFcalc.pro
; 27/1/04

; field =  array containing input field (assumed to be (lon,lat,time) )
; lats  =  a vector containing lattitude coordinates (lat)
; eofs  =  output array containing eofs (lon,lat,cutoff)
; pcs   =  output array containing pcs  (time,cutoff)
; svls  =  output array containing the singular values (cutoff)
; FVE   =  output array containing the Fractional Variance of the
;          input array Explained by a given EOF (cutoff)
; cutoff=  The number of Number of EOFs to retain (max = no. time points)


s=size(field)
nlons=s(1)
nlats=s(2)
ntims=s(3)

; w*w=L
; where X.X^T E=LE
; hence tr(X.X^T)=tr(L)=tr(w^2)


;      Applying latitude weighting.
        fac=!pi/180.
        for j=0,nlats-1 do begin
              cfac=sqrt(abs(cos(lats(j)*fac)))
              field(*,j,*)=field(*,j,*)*cfac
	endfor


;       Reform data into a 2d array
        data=reform(field,nlons*nlats,ntims,/overwrite)


; subtract time mean from data to produce anomalies.
	for i=0,nlons*nlats-1 do begin
		data(i,*)=data(i,*)-total(data(i,*))/ntims
	endfor


; this is TR(X.X^T) = total sum of variance
; note that in the EOF basis TR(Lambda) is the total sum of variance
; but that Lambda is the Eigenvalue matrix of X.X^T (W=sqrt(Lambda) in svd)

	tr=0
	for jk=0,nlons*nlats-1 do begin
		tr=tr+total(data(jk,*)*data(jk,*))
	endfor

	print,'sum of (space) variances:',tr/(ntims-1)

		svdc,data(*,*),w,u,v,/column
		svdsrt,w,u,v,/column

;cut off
		u=u(*,0:cutoff-1)
		w=w(0:cutoff-1)
		v=v(*,0:cutoff-1)

		eofs=reform(u(*,*),nlons,nlats,cutoff)
		pcs=v
		svls=w
		FVE=(svls*svls)/tr
                print,'CHECK. Sum of space variances',total(w*w)/(ntims-1),tr/(ntims-1)


	;       Latitude de-weight 
        fac=!pi/180.
          for j=0,nlats-1 do begin
              cfac=1./sqrt(abs(cos(lats(j)*fac))) 
                     eofs(*,j,*)=cfac*eofs(*,j,*)
          endfor

end




pro svdsrt,w,u,v,row=row,column=column
; sorts singular values and left and right singular vectors after call to
; svdc into order of descending singular value
su=size(u)
sv=size(v)
index=reverse(sort(w))
w=w(index)
if (keyword_set(column)) then begin
 for k=0, su(1)-1 do u(k,*)=u(k,index)
 for k=0, sv(1)-1 do v(k,*)=v(k,index)
endif else begin
 for k=0, su(2)-1 do u(*,k)=u(index,k)
 for k=0, sv(2)-1 do v(*,k)=v(index,k)
endelse
return
end
