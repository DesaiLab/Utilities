FUNCTION anova_test,data,ftest=ftest
;one-way f-test anova
;each column is a set of data, ignore NAN
;return significance, and ftest

  groupmean = mean(data,dim=2,/nan)
  grandmean = mean(data,/nan)
  groupn = total(finite(data),2,/nan)
;  elemn = total(finite(data),1,/nan)
  betgroup = total((groupmean-grandmean)^2*groupn)
  bg_dof = n_elements(data[*,0])-1
  Msb = betgroup / bg_Dof
  allmean = reform(replicate_arr(groupmean,n_elements(data[0,*])),n_elements(data[*,0]),n_elements(data[0,*]))
  withgroup = total((data-allmean)^2)
  wg_dof = total(groupn-1)
  Msw = withgroup / wg_dof
  Ftest = Msb/Msw
  sig = 1.0-f_pdf(ftest,bg_dof,wg_dof)
  return,sig
END
