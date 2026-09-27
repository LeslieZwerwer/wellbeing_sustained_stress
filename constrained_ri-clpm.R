rm(list = ls())
setwd("C:/Users/P269380/Documents/GOR covid-19/")

library(tidyverse)
library(lavaan)

df <- read_csv("./gor-covid-19/final scripts/data/06_riclpm_data.csv")

RICLPM <- '

  # Create between components (random intercepts)
  RIwb =~ 1*wb_w3 + 1*wb_w4 + 1*wb_w5 + 1*wb_w6 + 1*wb_w7 + 1*wb_w8 + 1*wb_w10 + 1*wb_w12
  
  RIstress =~ 1*stress_w3 + 1*stress_w4 + 1*stress_w5 + 1*stress_w6 + 1*stress_w7 + 1*stress_w8 + 1*stress_w10 + 1*stress_w12
  
  RIphysical =~ 1*physical_w3 + 1*physical_w4 + 1*physical_w5 + 1*physical_w6 + 1*physical_w7 + 1*physical_w8 + 1*physical_w10 + 
  1*physical_w12 
  
  RIel =~  1*el_w3 + 1*el_w4 + 1*el_w5 + 1*el_w6 + 1*el_w7 + 1*el_w8 + 1*el_w10 +1*el_w12
  
  # Create within-person centered variables
  wwb_w3 =~ 1*wb_w3
  wwb_w4 =~ 1*wb_w4
  wwb_w5 =~ 1*wb_w5 
  wwb_w6 =~ 1*wb_w6
  wwb_w7 =~ 1*wb_w7
  wwb_w8 =~ 1*wb_w8
  wwb_w10 =~ 1*wb_w10
  wwb_w12 =~ 1*wb_w12
  
  wstress_w3 =~ 1*stress_w3
  wstress_w4 =~ 1*stress_w4
  wstress_w5 =~ 1*stress_w5 
  wstress_w6 =~ 1*stress_w6
  wstress_w7 =~ 1*stress_w7
  wstress_w8 =~ 1*stress_w8
  wstress_w10 =~ 1*stress_w10
  wstress_w12 =~ 1*stress_w12
  
  wphysical_w3 =~ 1*physical_w3
  wphysical_w4 =~ 1*physical_w4
  wphysical_w5 =~ 1*physical_w5 
  wphysical_w6 =~ 1*physical_w6
  wphysical_w7 =~ 1*physical_w7
  wphysical_w8 =~ 1*physical_w8
  wphysical_w10 =~ 1*physical_w10
  wphysical_w12 =~ 1*physical_w12
  
  wel_w3 =~ 1*el_w3
  wel_w4 =~ 1*el_w4
  wel_w5 =~ 1*el_w5 
  wel_w6 =~ 1*el_w6
  wel_w7 =~ 1*el_w7
  wel_w8 =~ 1*el_w8
  wel_w10 =~ 1*el_w10
  wel_w12 =~ 1*el_w12
  
  
  # Estimate lagged effects between within-person centered variables
  wwb_w4       ~ a11*wwb_w3 + a12*wstress_w3 + a13*wphysical_w3 + a14*wel_w3 
wstress_w4   ~ a21*wwb_w3 + a22*wstress_w3 + a23*wphysical_w3 + a24*wel_w3 
wphysical_w4 ~ a31*wwb_w3 + a32*wstress_w3 + a33*wphysical_w3 + a34*wel_w3 
wel_w4       ~ a41*wwb_w3 + a42*wstress_w3 + a43*wphysical_w3 + a44*wel_w3 


  wwb_w5       ~ a11*wwb_w4 + a12*wstress_w4 + a13*wphysical_w4 + a14*wel_w4 
wstress_w5   ~ a21*wwb_w4 + a22*wstress_w4 + a23*wphysical_w4 + a24*wel_w4 
wphysical_w5 ~ a31*wwb_w4 + a32*wstress_w4 + a33*wphysical_w4 + a34*wel_w4 
wel_w5       ~ a41*wwb_w4 + a42*wstress_w4 + a43*wphysical_w4 + a44*wel_w4 


  wwb_w6       ~ a11*wwb_w5 + a12*wstress_w5 + a13*wphysical_w5 + a14*wel_w5 
wstress_w6   ~ a21*wwb_w5 + a22*wstress_w5 + a23*wphysical_w5 + a24*wel_w5 
wphysical_w6 ~ a31*wwb_w5 + a32*wstress_w5 + a33*wphysical_w5 + a34*wel_w5 
wel_w6       ~ a41*wwb_w5 + a42*wstress_w5 + a43*wphysical_w5 + a44*wel_w5 

  wwb_w7       ~ a11*wwb_w6 + a12*wstress_w6 + a13*wphysical_w6 + a14*wel_w6
wstress_w7   ~ a21*wwb_w6 + a22*wstress_w6 + a23*wphysical_w6 + a24*wel_w6 
wphysical_w7 ~ a31*wwb_w6 + a32*wstress_w6 + a33*wphysical_w6 + a34*wel_w6 
wel_w7       ~ a41*wwb_w6 + a42*wstress_w6 + a43*wphysical_w6 + a44*wel_w6 

  wwb_w8       ~ a11*wwb_w7 + a12*wstress_w7 + a13*wphysical_w7 + a14*wel_w7 
wstress_w8   ~ a21*wwb_w7 + a22*wstress_w7 + a23*wphysical_w7 + a24*wel_w7 
wphysical_w8 ~ a31*wwb_w7 + a32*wstress_w7 + a33*wphysical_w7 + a34*wel_w7 
wel_w8       ~ a41*wwb_w7 + a42*wstress_w7 + a43*wphysical_w7 + a44*wel_w7 


  wwb_w10       ~ a11*wwb_w8 + a12*wstress_w8 + a13*wphysical_w8 + a14*wel_w8 
wstress_w10   ~ a21*wwb_w8 + a22*wstress_w8 + a23*wphysical_w8 + a24*wel_w8 
wphysical_w10 ~ a31*wwb_w8 + a32*wstress_w8 + a33*wphysical_w8 + a34*wel_w8 
wel_w10       ~ a41*wwb_w8 + a42*wstress_w8 + a43*wphysical_w8 + a44*wel_w8 

  wwb_w12       ~ a11*wwb_w10 + a12*wstress_w10 + a13*wphysical_w10 + a14*wel_w10 
wstress_w12   ~ a21*wwb_w10 + a22*wstress_w10 + a23*wphysical_w10 + a24*wel_w10 
wphysical_w12 ~ a31*wwb_w10 + a32*wstress_w10 + a33*wphysical_w10 + a34*wel_w10 
wel_w12       ~ a41*wwb_w10 + a42*wstress_w10 + a43*wphysical_w10 + a44*wel_w10 




  # Estimate covariance between within-person centered variables at first wave
  wwb_w3 ~~ wstress_w3 
  wwb_w3 ~~ wphysical_w3
  wwb_w3 ~~ wel_w3
  
  wstress_w3 ~~ wphysical_w3
  wstress_w3 ~~ wel_w3
 
  wphysical_w3 ~~ wel_w3
 
  # Estimate covariances between residuals of within-person centered variables 
  # (i.e., innovations)
  #wave _w4
  wwb_w4 ~~ wstress_w4 
  wwb_w4 ~~ wphysical_w4
  wwb_w4 ~~ wel_w4
  
  wstress_w4 ~~ wphysical_w4
  wstress_w4 ~~ wel_w4
  
  wphysical_w4 ~~ wel_w4
  
  #wave 5 
  
  wwb_w5 ~~ wstress_w5 
  wwb_w5 ~~ wphysical_w5
  wwb_w5 ~~ wel_w5
  
  wstress_w5 ~~ wphysical_w5
  wstress_w5 ~~ wel_w5
 
  wphysical_w5 ~~ wel_w5
 
  #wave _w6 
  
  wwb_w6 ~~ wstress_w6 
  wwb_w6 ~~ wphysical_w6
  wwb_w6 ~~ wel_w6

  wstress_w6 ~~ wphysical_w6
  wstress_w6 ~~ wel_w6
 
  wphysical_w6 ~~ wel_w6
  
  #wave _w7 
  
  wwb_w7 ~~ wstress_w7 
  wwb_w7 ~~ wphysical_w7
  wwb_w7 ~~ wel_w7
 
  wstress_w7 ~~ wphysical_w7
  wstress_w7 ~~ wel_w7
 
  wphysical_w7 ~~ wel_w7
  
  #wave_w8 
  
  wwb_w8 ~~ wstress_w8 
  wwb_w8 ~~ wphysical_w8
  wwb_w8 ~~ wel_w8
 
  wstress_w8 ~~ wphysical_w8
  wstress_w8 ~~ wel_w8
  
  wphysical_w8 ~~ wel_w8
  
  #wave _w10 
  
  wwb_w10 ~~ wstress_w10 
  wwb_w10 ~~ wphysical_w10
  wwb_w10 ~~ wel_w10
  
  wstress_w10 ~~ wphysical_w10
  wstress_w10 ~~ wel_w10
 
  wphysical_w10 ~~ wel_w10
 
  #wave _w12 
  
  wwb_w12 ~~ wstress_w12 
  wwb_w12 ~~ wphysical_w12
  wwb_w12 ~~ wel_w12
  
  wstress_w12 ~~ wphysical_w12
  wstress_w12 ~~ wel_w12
 
  wphysical_w12 ~~ wel_w12
  
  # Estimate variance and covariance of random intercepts
  RIwb ~~ RIwb
  RIstress ~~ RIstress
  RIphysical ~~ RIphysical
  RIel ~~ RIel
  
  
  RIwb ~~ RIstress
  RIwb ~~ RIphysical
  RIwb ~~ RIel 
  
  RIstress ~~ RIphysical 
  RIstress ~~ RIel 
 
  RIphysical ~~ RIel 
  
  # Estimate (residual) variance of within-person centered variables
  wwb_w3 ~~ wwb_w3 # Variances
  wwb_w4 ~~ wwb_w4
  wwb_w5 ~~ wwb_w5
  wwb_w6 ~~ wwb_w6
  wwb_w7 ~~ wwb_w7
  wwb_w8 ~~ wwb_w8
  wwb_w10 ~~ wwb_w10
  wwb_w12 ~~ wwb_w12
  
  wstress_w3 ~~ wstress_w3 
  wstress_w4 ~~ wstress_w4 
  wstress_w5 ~~ wstress_w5 
  wstress_w6 ~~ wstress_w6 
  wstress_w7 ~~ wstress_w7 
  wstress_w8 ~~ wstress_w8 
  wstress_w10 ~~ wstress_w10 
  wstress_w12 ~~ wstress_w12
  
  wphysical_w3 ~~ wphysical_w3 
  wphysical_w4 ~~ wphysical_w4 
  wphysical_w5 ~~ wphysical_w5 
  wphysical_w6 ~~ wphysical_w6 
  wphysical_w7 ~~ wphysical_w7 
  wphysical_w8 ~~ wphysical_w8 
  wphysical_w10 ~~ wphysical_w10 
  wphysical_w12 ~~ wphysical_w12
  
  wel_w3 ~~ wel_w3 
  wel_w4 ~~ wel_w4 
  wel_w5 ~~ wel_w5 
  wel_w6 ~~ wel_w6 
  wel_w7 ~~ wel_w7 
  wel_w8 ~~ wel_w8 
  wel_w10 ~~ wel_w10 
  wel_w12 ~~ wel_w12
  
'
start <- Sys.time()
fit <- lavaan(RICLPM, data = df, missing = "ML", estimator = "MLR",
              meanstructure = TRUE, int.ov.free = TRUE)
Sys.time() - start


saveRDS(fit, "./gor-covid-19/final scripts/models/riclpm/riclpm_constrained.rds")