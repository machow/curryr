#' Let a function take partial arguments
#'
#' Currying a function means that you do not have to specify all of its
#' arguments at one time. If `f` is a function with three positional arguments,
#' and `g <- curry(f)`, the following are equivalent.
#'     - `g(1)(2)(3)`
#'     - `g(1)(2, 3)`
#'     - `g(1, 2)(3)`
#'     - `g(1, 2, 3)`
#'
#' In addition, named arguments may be specified at any time, to override their
#' default option. For example, `rnorm` is a function with the signature
#' `rnorm(n, mean = 0, sd = 1)`, so if `c_rnorm <- curry(rnorm)`, the following are
#' equivalent.
#'     - `c_rnorm(10, 1)`
#'     - `c_rnorm(10, mean = 1)`
#'     - `c_rnorm(mean = 1)(10)`
#'
#' @param f a function to be curried. May have both positional and default arguments,
#'   or niether.
#' @export
#' @examples
#' # Curry is designed to allow for quick partial arguments
#' # below shows how you might do it without curry
#' old_mean10 <- function(n) rnorm(n, mean = 10)
#'
#' # quickly give partial arguments to rnorm
#' c_rnorm <- curry(rnorm)
#' mean10 <- c_rnorm(mean=10)
#' mean20 <- c_rnorm(mean=20)
#' mean20_wide <- mean20(sd=10)
#'
#' # the following calls both generate 50 random values
#' mean20_wide(50)
#' mean20_wide(n=50)
#'
#' # Note that arguments will be evaluated lazily
#' # the code below runs 10 replications of generating normal data
#' c_replicate <- curry(replicate)
#' sim_dataset <- c_replicate(expr = mean20_wide(50))
#' sim_dataset(10)
#'
#' # Curry is especially useful when used with functions like Map
#' getMeans <- curry(lapply)(FUN=mean)
#' getMeans(list(a=1:10, b=10:20))
#'
#' # The following are equivalent
#' dat <- list(a=c(1,2,NA), b=10:20)
#'
#' lapply(dat, mean, na.rm=TRUE)
#' getMeans(dat, na.rm=TRUE)
#' curry(lapply)(FUN=mean, na.rm=TRUE)
curry <- function(f){
  part_f <- function(){
    # position variables from formals
    base::rm(list=base::ls())   # need to remove execution environment
    formf <- formals()          # but keep formal args
    pos_vars <- names(
      Filter(function(x) is.name(x) & as.character(x) == "", formf)
    )
    # check for positional args not in signature
    sig <- as.list(match.call())[-1]
    mandatory <- pos_vars[!pos_vars %in% names(sig) & pos_vars != "..."]
    # evaluate call when no positional args left
    if (!length(mandatory))
      return(do.call(f, sig, envir = parent.frame()))

    # otherwise, recreate formal args for f
    new_formf <- as.pairlist(c(
      formf[mandatory],         # remaining position args
      sig,                      # args from current call signature
      formf[!(names(formf) %in% c(mandatory, names(sig)))]   # leftover formals
    ))
    formals(f) <- new_formf
    # curry f with its new formals
    curry(f)
  }

  # give closure the original formals
  formals(part_f) <- formals(f)
  part_f
}
