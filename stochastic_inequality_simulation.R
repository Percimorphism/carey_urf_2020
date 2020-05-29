source('rank_util.R')
source('hw_util.R')

# Hardy-Weinberg Curve
num_of_points=1000
hw_data_object=hw_data(num_of_points)
hw_data=hw_data_object[[1]]
t=hw_data_object[[2]]

# noise parameter
noise_dim = 5
v = 1
#generate noise and concatenate onto HW
noise=generate_high_dim_gaussian_noise(num_of_points, noise_dim, v)
hw_noise_data=cbind(hw_data, noise)

#list of X in Carey's Conjecture, for stochastic
set.seed(1)
special_point_list = sort(sample(c(2:num_of_points-2), 200, replace = FALSE, prob = NULL))

distance_matrices = build_distance_matrices(t, num_of_points)
D_geo = distance_matrices$D_geo
D_urf = distance_matrices$D_urf
D_iso = distance_matrices$D_iso

distance_matrices_noise = build_distance_matrices_noise(hw_noise_data)
D_urf_noise = distance_matrices_noise$D_urf_noise
D_iso_noise = distance_matrices_noise$D_iso_noise

rank_list_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(rank_list_df)<- c('R_iso', 'R_urf', 'R_urf_noise', 'R_iso_noise')
for (sp_pt in special_point_list){
  X_star = get_true_X_star(D_geo, sp_pt)
  R_iso = get_R_iso(D_iso, X_star, sp_pt)
  R_urf = get_R_urf(D_urf, X_star, sp_pt)
  
  R_iso_noise = get_R_iso(D_iso_noise, X_star, sp_pt)
  R_urf_noise = get_R_urf(D_urf_noise, X_star, sp_pt)
  
  rank_list_df[nrow(rank_list_df) + 1,] = c(R_iso, R_urf, R_urf_noise, R_iso_noise)
}

print(rank_list_df)

  