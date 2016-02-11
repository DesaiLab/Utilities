FUNCTION bin_avg_2d,bin1,bin2,data,std=std,se=se,nvals=nvals,median=median,min1=min1,min2=min2,max1=max1,max2=max2,nbins1=nbins1,nbins2=nbins2,omin1=omin1,omin2=omin2,omax1=omax1,omax2=omax2,binsize1=binsize1,binsize2=binsize2,locations1=locations1,locations2=locations2,badval=badval,hist1=hist1,hist2=hist2
  
;bin averaging in 2 dimensions
;where bin1, bin2 and data are all time series with same number of elements
;bin1 is x (column), bin2 is y (row)

;INPUT

;bin1 - time series of data you want to bin by for the column
;bin2 - time series of data you want to bin by for the row
;data - the time series you want binned
;badval - missing data value - default is nan

;binsize1 - size of each bin for the col (binsize2 for row)
;nbins1 - number of bins for the col (nbins2 for row)
;max1 - max bin you want to use for the col (max2 for the row)
;min1 - min bin you want to use for the col (min2 for the row)

;OUTPUT

;return value - mean of data for each bin (array is bin1 x bin2)
;nvals - number of finite values in each bin
;median - median of data for each bin
;std - standard deviation of data for each bin
;se - standard error of data for each bin

;locations1 - minimum bindata value for each bin col (locations2 for row)
;omax1 - max bin for the col (omax2 for row)
;omin1 - min bin for the col (omax1 for row)
;hist1 - histogram output for the col (hist2 for the row)

  IF n_elements(badval) EQ 0 THEN badval = !values.f_nan
  gv = where(finite(bin1) AND (bin1 NE badval),ngv)
  gv2 = where(finite(bin2) AND (bin2 NE badval),ngv2)
  IF (ngv GT 0) AND (ngv2 GT 0) AND (n_elements(bin1) EQ n_elements(bin2)) AND (n_elements(bin1) EQ n_elements(data)) THEN BEGIN 
    hist1 = histogram(bin1[gv],binsize=binsize1,nbins=nbins1,max=max1,min=min1,locations=locations1,omin=omin1,omax=omax1)
    hist2 = histogram(bin2[gv2],binsize=binsize2,nbins=nbins2,max=max2,min=min2,reverse_indices=ri,locations=locations2,omin=omin2,omax=omax2)
    mns = make_array(n_elements(hist1),n_elements(hist2),value=badval,/float)
    IF arg_present(median) THEN median = mns
    IF arg_present(std) OR arg_present(se) THEN std = mns
    IF arg_present(nvals) OR arg_present(se) THEN nvals = make_array(n_elements(hist1),n_elements(hist2),value=0,/long)
    IF arg_present(se) THEN se = mns
    FOR i = 0,n_elements(hist2)-1 DO BEGIN
      IF ri[i+1] NE ri[i] THEN BEGIN
        thedata = data[ri[ri[i]:ri[i+1]-1]]
        thebin = bin1[ri[ri[i]:ri[i+1]-1]]
        output = bin_avg(thebin,thedata,std=std1,median=median1,min=omin1,max=omax1,binsize=binsize1,nbins=nbins1,nvals=nvals1,badval=badval)
        mns[*,i] = output
        IF arg_present(median) THEN median[*,i] = median1
        IF arg_present(std) OR arg_present(se) THEN std[*,i] = std1
        IF arg_present(nvals) OR arg_present(se) THEN nvals[*,i] = nvals1
      ENDIF
    ENDFOR 
    IF arg_present(se) THEN BEGIN 
      gv3 = where((nvals NE 0) AND (nvals NE badval) AND finite(nvals),ngv3)
      IF ngv3 GT 0 THEN se[gv3] = std[gv3] / sqrt(nvals[gv3])
    ENDIF 
  ENDIF ELSE mns = badval
  return,mns
END
