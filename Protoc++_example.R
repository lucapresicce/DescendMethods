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
data = mvtnorm::rmvnorm(n=2,mean=mu_true,sigma = Sigma_true)

# Set hyperparameters
mu0 = c(0,0)
Lambda0 = diag(2)
b = 3
D = diag(2)

# Set initial values
mu_init = c(0,0)
Sigma_init = diag(2)

# Set number of iterations
niter = 1
burnin = 1

res = BayesLM(data,niter,burnin,mu0,Lambda0,b,D,mu_init,Sigma_init)










