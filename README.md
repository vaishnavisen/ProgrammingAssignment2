ProgrammingAssignment2
======================

Matrix inversion is usually a costly computation and there may be some 
benefit to caching the inverse of a matrix rather than compute it repeatedly.

To do this, we make use of the following two functions:
1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
If the inverse has already been calculated (and the matrix has not changed),
then the cachesolve should retrieve the inverse from the cache.
