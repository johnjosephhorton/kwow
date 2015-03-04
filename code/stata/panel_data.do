clear
set more off 

use "/home/john/Dropbox/topics/search_and_matching/kwow/data/mturk_df_stata.dta"


quietly: xi: reg error social know i.WorkerId i.title, vce(cluster WorkerId)
eststo C1 

quietly: xi: reg v_trend_error social know i.WorkerId i.title, vce(cluster WorkerId)
eststo C2


esttab C1 C2 using "/home/john/Dropbox/topics/search_and_matching/kwow/writeup/tables/panel_models.tex", b(3) se(3) drop(_IWorkerId* _Ititle* _cons) varlabels(social "Social Index" know "Job Knowledge Index") title("Social Knowledge and Labor Market Information Accuracy") mlabel(error "Error" v_trend_error "Volume Trend Error") replace 


* Multi-Level Models 
xtmixed v_trend_error social know, || WorkerId:, covariance(independent) || title:, covariance(independent)

xtmixed error social know, || WorkerId:, covariance(independent) || title:, covariance(independent)
