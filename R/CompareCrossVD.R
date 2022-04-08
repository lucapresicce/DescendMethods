## Testing sequantial and parallel CrossVD
set.seed(445)
n = 1000000
x1 = runif(n)
x2 = rnorm(n)
x3 = rnorm(n)
x4 = rnorm(n)
y = 1+3.6*x1 + 5.3*x2 + 4.2*x3 + 2.1*x4 + rnorm(n)
x <- cbind(1, x1, x2, x3, x4)
colnames(x) <- NULL
df <- list(X = x, Y = y)

tictoc::tic()
CrossVD(data = df, OPT = SteepD)
tictoc::toc()

tictoc::tic()
PcrossVD(data = df, OPT = SteepD)
tictoc::toc()

tictoc::tic()
CrossVD(data = df, K = 5, OPT = SteepD)
tictoc::toc()

tictoc::tic()
PcrossVD(data = df, K = 5, OPT = SteepD)
tictoc::toc()
