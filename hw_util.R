library(stats)
library(MASS)
library(Matrix)
library(scatterplot3d)
library(rerf)
library(vegan)
library(umap)


hw_data<-function(num_of_points=500){
  set.seed(1)
  t=runif(num_of_points, min = 0, max = 1)
  t=sort(t)
  x1=t^2
  x2=2*t*(1-t)
  x3=(1-t)^2
  data = cbind(x1, x2, x3)
  return(list(data, t))
}



# generate D_geo, this is the geodesic distance
hw_geodesic<-function(t, num_of_points){
  f = function(x){sqrt(8*(3*x^2-3*x+1))}
  D_geo = matrix(rep(0, num_of_points*num_of_points), nrow = num_of_points, ncol = num_of_points)
  j=1
  while (j <= num_of_points){
    i=1
    while( i <= num_of_points){
      D_geo[i,j] = abs(integrate(f,t[i],t[j])$value)
      i = i +1
    }
    j=j+1
  }
  return(D_geo)
}