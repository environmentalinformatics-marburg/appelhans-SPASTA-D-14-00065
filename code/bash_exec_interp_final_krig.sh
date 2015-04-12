#! /bin/sh

#nohup R --slave --vanilla --file=interp_final.R --args mthd="'avNNet'" > ../logs/interp_final_avNNet_201504052000.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'ctree'" > ../logs/interp_final_ctree_201503281815.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'cubist'" > ../logs/interp_final_cubist_201504052000.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'earth'" > ../logs/interp_final_earth_201503281815.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'gam'" > ../logs/interp_final_gam_201503281815.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'gbm'" > ../logs/interp_final_gbm_201504052000.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'glm'" > ../logs/interp_final_glm_201503281815.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'knn'" > ../logs/interp_final_knn_201503281815.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'nnet'" > ../logs/interp_final_nnet_201503281815.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'pcr'" > ../logs/interp_final_pcr_201503281815.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'pls'" > ../logs/interp_final_pls_201503281815.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'rf'" > ../logs/interp_final_rf_201504052000.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'svmLinear'" > ../logs/interp_final_svmLinear_201503281815.out &
#nohup R --slave --vanilla --file=interp_final.R --args mthd="'svmRadial'" > ../logs/interp_final_svmRadial_201503281815.out &
nohup R --slave --vanilla --file=interp_final_krigDSN.R > ../logs/interp_final_krigDSN_201504082000.out &
nohup R --slave --vanilla --file=interp_final_krigD.R > ../logs/interp_final_krigD_201504082000.out &

echo "all on its way..."
