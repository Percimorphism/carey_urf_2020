library(stats)
library(vegan)
library(rgl)
library(MASS)
library(Matrix)
library(randomForest)
library(plotly)
library(scatterplot3d)
source('hw_util.R')


# # Hardy-Weinberg Curve
# num_of_points=1000
# hw_data_object=hw_data(num_of_points)
# hw_data=hw_data_object[[1]]
# t=hw_data_object[[2]]
# 
# # we pick a special point on the curve, and look at its 1-nn
# sp_pt = 400

####################################################
# With no noise on the curve
####################################################
build_distance_matrices<-function(t, num_of_points){
  #Geodesic Distance on HW curve
  D_geo=hw_geodesic(t, num_of_points)
  # URF 
  g_signal=randomForest(hw_data, keep.forest=FALSE, proximity=TRUE)
  W_urf = g_signal$proximity
  D_urf = (1 - W_urf)
  # isomap distance matrix
  D=as.matrix(dist(hw_data))
  # pick k to be the smallest number that does not give a seg-error
  iso_d = isomapdist(D, k=9)
  D_iso = as.matrix(iso_d)
  return(list('D_geo'=D_geo, 'D_urf'=D_urf, 'D_iso'=D_iso ))
}


get_true_X_star<-function(D_geo, sp_pt){
  # point closest to p_sp
  # 1) Truth (geodesic)
  one_nn_geodist = sort(D_geo[sp_pt,])[2]
  rank_one_pt_geo = match(one_nn_geodist, D_geo[sp_pt,])
  return(rank_one_pt_geo)
}


get_R_iso<-function(D_iso, rank_one_pt_geo, sp_pt){
  # 2) the rank of "rank one point" in Isomap wrt the sp_pt
  iso_dist_rk1_sppt = D_iso[sp_pt, rank_one_pt_geo]
  iso_sort = sort(D_iso[sp_pt,])
  R_iso = match(iso_dist_rk1_sppt, iso_sort) - 1
  if (R_iso == 0){return(1)}
  else{return(R_iso)}
}


get_R_urf<-function(D_rf, rank_one_pt_geo, sp_pt){
  # 3) the rank of "rank one point" in URF wrt the sp_pt
  # Urf distance from special point to the true rank one point
  urf_dist_rk1_sppt = D_rf[sp_pt, rank_one_pt_geo]
  rf_sort = sort(D_rf[sp_pt,])
  R_urf = match(urf_dist_rk1_sppt, rf_sort) - 1
  if (R_urf == 0){return(1)}
  else{return(R_urf)}
}


#note the knn parameter k=9 might need to be changed.
build_distance_matrices_noise<-function(hw_noise_data){
  # URF noise
  g_noise=randomForest(hw_noise_data, keep.forest=FALSE, proximity=TRUE)
  W_urf_noise = g_noise$proximity
  D_urf_noise = (1 - W_urf_noise)
  # isomap distance matrix, with noise
  D_noise=as.matrix(dist(hw_noise_data))
  # pick k to be the smallest number that does not give a seg-error
  iso_d_noise = isomapdist(D_noise, k=9)
  D_iso_noise = as.matrix(iso_d_noise)
  return(list('D_urf_noise'=D_urf_noise, 'D_iso_noise'=D_iso_noise ))
}
