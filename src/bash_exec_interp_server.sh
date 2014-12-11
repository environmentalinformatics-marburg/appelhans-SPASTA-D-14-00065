#! /bin/sh

#nohup R --slave --vanilla --file=interp_avNNet.R > interp_avNNet_201409101300.out &
#nohup R --slave --vanilla --file=interp_ctree.R > interp_ctree_201409101300.out &
#nohup R --slave --vanilla --file=interp_cubist.R > interp_cubist_201409101300.out &
#nohup R --slave --vanilla --file=interp_nnet.R > interp_nnet_201409101300.out &
#nohup R --slave --vanilla --file=interp_rf.R > interp_rf_201409101300.out &
#nohup R --slave --vanilla --file=interp_gam.R > interp_gam_201409101300.out &
#nohup R --slave --vanilla --file=interp_gbm.R > interp_gbm_201409101300.out &
#nohup R --slave --vanilla --file=interp_earth.R > interp_earth_201409101300.out &
#nohup R --slave --vanilla --file=interp_kriging.R > interp_kriging_201409101300.out &
nohup R --slave --vanilla --file=interp_cubist_annual_ndvi.R > interp_cubist_annual_ndvi_201410171730.out &

echo "all on its way..."

