context("curry")

test_that("curry() multiple calls over simple 3-arity function w/ same results", {
  f <- curry(function(a,b,c) a + b + c)
  expect_identical( f(1)(2)(5), 8 )
  expect_identical( f(1, 2)(5), 8 )
  expect_identical( f(1)(2, 5), 8 )
  expect_identical( f(1, 2, 5), 8 )
})

test_that("curry() calls with named arguments", {
  f <- curry(function(a, b = NA){ 2*a + b})
  expect_identical( f(b=2)(1), f(1, 2) )
})

test_that("curry() set same named argument twice", {
  f <- curry(function(a, b){ a + b })
  expect_identical( f(b=1)(b=2)(1), f(b=2)(1) )
})

test_that("curry() set same positional argument twice (using named arg)", {
  f <- curry(function(a, b){ a + b})
  expect_identical( f(1)(a=2)(1), f(2,1) )
})

test_that("curry() lets function keep its original formals", {
  f <- function(a, b=1){formals()}
  g <- curry(f)
  expect_identical( formals(f), f() )
  expect_identical( formals(g), formals(f) )
})

test_that("curry() curried function returns partial application with new formals", {
  g <- curry(function(a,b){})
  expect_identical( formals(g(b=1))$b, 1 )
})

test_that("curry() a function with pos args and dots ... ", {
  c_lapply <- curry(lapply)(FUN=mean)
  expect_identical( c_lapply(list(a=c(1, NA)), na.rm = TRUE), list(a=1) )
})

test_that("curry() a function with no pos args, only dots ... ", {
  f <- function(...) sum(...)
  g <- curry(f)
  expect_identical( g(1:3), f(1:3) )
  expect_identical( g(1,2,3), f(1,2,3) )
})
