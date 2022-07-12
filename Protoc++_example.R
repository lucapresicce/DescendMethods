# Read c++ output
out_R = Proto_in_cplusplus()

# Read proto file in R
RProtoBuf::readProtoFiles(dir = "./inst/proto")
ls("RProtoBuf:DescriptorPool")

# Read message in R
writeLines(as.character(RProtoBuf::read(MyNamespace.MySave, out_R)))



# C++ example in Bayesian statistics

# Generate data
mu_true = c(0,1)
Sigma_true = matrix(c(1,0.5,0.5,1),nrow =2,byrow=T)
set.seed(12412)
data = mvtnorm::rmvnorm(n=150,mean=mu_true,sigma = Sigma_true)

# Set hyperparameters
mu0 = c(0,0)
Lambda0 = diag(2)
b = 3
D = diag(2)

# Set initial values
mu_init = c(0,0)
Sigma_init = diag(2)
Sigma_init = Sigma_true

# Set number of iterations
niter = 5000
burnin = 2500

res = BayesLM(data,niter,burnin,mu0,Lambda0,b,D,mu_init,Sigma_init)


# Read proto file in R
RProtoBuf::readProtoFiles(dir = "./inst/proto")
ls("RProtoBuf:DescriptorPool")

# Read message in R
R_res = ReadFromProto(res)
M = length(res)
mu_est = rep(0,2)
K_est  = matrix(0,2,2)
Cov_est  = matrix(0,2,2)
for(i in 1:M){
  mu_est = mu_est + R_res$mu[[i]]
  K_est  = K_est  + R_res$prec[[i]]
  Cov_est  = Cov_est  + solve(R_res$prec[[i]])
}
mu_est = mu_est/M
round(mu_est,digits = 5)
K_est = K_est/M
round(K_est,digits = 5)
Cov_est = Cov_est/M
round(Cov_est,digits = 5)


library(tidyverse)
tb_means = tibble(mu_true_x = mu_true[1],
                     mu_true_y = mu_true[2],
                     mu_est_x  = mu_est[1],
                     mu_est_y  = mu_est[2]) 

ellipse_plot = as_tibble(ellipse::ellipse(x = Cov_est, centre = mu_true))


data %>% as_tibble() %>%
  ggplot( aes(x = V1, y = V2,col='darkred') ) + geom_point() + theme_bw() +
  labs(x="xx", y="yy") + theme(legend.position="none") +  
  geom_point( data = tb_means, aes(x=mu_est_x, y = mu_est_y), 
              shape = 4, size = 2, stroke = 2,
              inherit.aes = F) +
  geom_path( data = ellipse_plot, aes(x=x, y=y),
             inherit.aes = F) 




