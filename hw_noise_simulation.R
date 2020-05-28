source('hw_util.R')
source('hw_simulation.R')


# Hardy-Weinberg Curve with noise
noise_dim = 5
v = 10

noise=generate_high_dim_gaussian_noise(num_of_points, noise_dim, v)
hw_noise_data=cbind(hw_data, noise)

# URF noise
g_noise=randomForest(hw_noise_data, keep.forest=FALSE, proximity=TRUE)
W_rf_noise = g_noise$proximity
D_rf_noise = (1 - W_rf_noise)
# isomap distance matrix, with noise
D_noise=as.matrix(dist(hw_noise_data))
# pick k to be the smallest number that does not give a seg-error
iso_d_noise = isomapdist(D_noise, k=9)
D_iso_noise = as.matrix(iso_d_noise)


# 1) the rank of "rank one point" in Isomap wrt the sp_pt (noise)
iso_dist_rk1_sppt_noise = D_iso_noise[sp_pt, rank_one_pt_geo]
iso_sort_noise = sort(D_iso_noise[sp_pt,])
R_iso_noise = match(iso_dist_rk1_sppt_noise, iso_sort_noise) - 1


# 2) the rank of "rank one point" in URF wrt the sp_pt (noise)
# Urf distance from special point to the true rank one point
urf_dist_rk1_sppt_noise = D_rf_noise[sp_pt, rank_one_pt_geo]
rf_sort_noise = sort(D_rf_noise[sp_pt,])
R_urf_noise = match(urf_dist_rk1_sppt_noise, rf_sort_noise) - 1

rank_list = c(R_iso, R_urf, R_urf_noise, R_iso_noise)
print(c('R_iso', 'R_urf', 'R_urf_noise', 'R_iso_noise'))

print(rank_list)
