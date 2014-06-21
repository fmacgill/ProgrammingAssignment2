source("cachematrix.R")

test_that("basic tests", {
    B = matrix( c(2, 4, 3, 5), nrow=2,ncol=2)
    iB = solve(B)
    
    a <- makeCacheMatrix(B)    ##  create & initialize
    expect_null(a$getinverse())   ##  NULL
    expect_equal(iB, cacheSolve(a))    ##  compute, cache and return inverse
    expect_that(iB, equals(a$getinverse()))    ##  show the cached value

    C = matrix(c (2.3, 4.1, 5.2, 1.1, -0.9, 4.1, 5.2, 1.1, -0.9 ), nrow=3,ncol=3)
    iC = solve(C)
    a <- makeCacheMatrix()   ##  reuse name for new object
    a$set(C)     ##  initialize
    expect_null(a$getinverse())   ##  NULL
    expect_that(iC, equals(cacheSolve(a)))    ##  compute, cache and return inverse
    expect_that(iC, equals(a$getinverse()))     ##  show the cached value
    expect_that(iC, equals(cacheSolve(a)))    ##  show the cached value
})

