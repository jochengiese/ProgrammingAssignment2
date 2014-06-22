# dimension of the matrix. The larger, the more computer power is necessary
n <- 300

#make sure we have an invertible matrix
s <- svd(replicate(n,rnorm(n)));
# we do this by making sure that the singular values are >0 
A <- s$u %*% diag(runif(n)) %*% s$v

# now we apply this to our code
B <- makeCacheMatrix(A)

# now we invert the first time, actually performing the inverse
step1Time = system.time(Binv <- cacheSolve(B))
# now we invert the second time, taking from cache
step2Time = system.time(Binv2 <- cacheSolve(B))
#
# evalute
message("Performing the inversion calculation takes")
print(step1Time)
message("Accessing the cache with prestored inverse matrix takes")
print(step2Time)
message("Both received values differ (2-norm) by [should be 0]:")
err1 <- sum(abs((Binv-Binv2)^2))
print(err1)
message("inverse was computed correctly except for residual error:")
err2 <- sum((Binv %*% B$get() -diag(replicate(n,1)))^2)
print(err2)
