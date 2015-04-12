#! /bin/sh

#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'avNNet'" > ../logs/ta_monthly_cv_avNNet_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'ctree'" > ../logs/ta_monthly_cv_ctree_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'cubist'" > ../logs/ta_monthly_cv_cubist_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'earth'" > ../logs/ta_monthly_cv_earth_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'gam'" > ../logs/ta_monthly_cv_gam_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'gbm'" > ../logs/ta_monthly_cv_gbm_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'glm'" > ../logs/ta_monthly_cv_glm_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'knn'" > ../logs/ta_monthly_cv_knn_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'nnet'" > ../logs/ta_monthly_cv_nnet_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'pcr'" > ../logs/ta_monthly_cv_pcr_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'pls'" > ../logs/ta_monthly_cv_pls_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'rf'" > ../logs/ta_monthly_cv_rf_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'svmLinear'" > ../logs/ta_monthly_cv_svmLinear_201503281815.out &
#nohup R --slave --vanilla --file=ta_monthly_cv.R --args mthd="'svmRadial'" > ../logs/ta_monthly_cv_svmRadial_201503281815.out &
nohup R --slave --vanilla --file=ta_monthly_krig_cv.R > ../logs/ta_monthly_cv_kriging_201504032215.out &

echo "all on its way..."

