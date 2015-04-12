#! /bin/sh

nohup R --slave --vanilla --file=ta_monthly_avNNet.R > ta_monthly_avNNet_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_ctree.R > ta_monthly_ctree_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_cubist.R > ta_monthly_cubist_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_earth.R > ta_monthly_earth_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_gam.R > ta_monthly_gam_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_gbm.R > ta_monthly_gbm_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_glm.R > ta_monthly_glm_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_knn.R > ta_monthly_knn_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_krig_new.R > ta_monthly_krig_new_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_nnet.R > ta_monthly_nnet_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_pcr.R > ta_monthly_pcr_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_pls.R > ta_monthly_pls_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_rf.R > ta_monthly_rf_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_svmLinear.R > ta_monthly_svmLinear_201409081815.out &
nohup R --slave --vanilla --file=ta_monthly_svmRadial.R > ta_monthly_svmRadial_201409081815.out &

echo "all on its way..."

