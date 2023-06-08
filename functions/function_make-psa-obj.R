#' Create a PSA object
#' Source: DARTH (https://github.com/DARTH-git/cohort-modeling-tutorial-intro/)
#'
#' @description
#' Creates an object to hold probabilistic sensitivity analysis data,
#' while checking the data for validity. The object can then be
#' used for many standard cost-effectiveness analyses (see Details below).
#'
#' @param parameters Data frame with values for each simulation (rows) and parameter (columns).
#' The column names should be the parameter names.
#' @param cost For the data.frame, each simulation should be a row and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param effectiveness For the data.frame, each simulation should be a row and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param other_outcome data.frame containing values for another user-defined outcome.
#' Each simulation should be a row of the data frame, and each strategy should be a column.
#' Naming the columns of the data frames is not necessary, as they will be renamed with
#' the \code{strategies} vector.
#' @param strategies vector with the names of the strategies. Due to requirements in
#' certain uses of this vector, this function uses \code{\link{make.names}} to modify
#' strategy names as necessary. It is strongly suggested that you follow the rules
#' in the \code{\link{make.names}} help page, to avoid unexpected errors.
#'
#' @param currency symbol for the currency being used (ex. "$", "£")
#'
#' @details
#' The PSA object forms the backbone of one part of the \code{dampack} package.
#'
#' A scatterplot of the cost-effectiveness plane may be shown by running \code{plot}
#' on the output of \code{make_psa_obj}.
#'
#' Using this object, you may calculate:
#' \itemize{
#'   \item Cost-effectiveness acceptability curves (\code{\link{ceac}})
#'   \item Expected value of perfect information (\code{\link{calc_evpi}})
#'   \item Expected loss (\code{\link{calc_exp_loss}})
#'   \item One-way sensitivity analysis (\code{\link{owsa}})
#'   \item Two-way sensitivity analysis (\code{\link{twsa}})
#'   \item Metamodels (\code{\link{metamodel}})
#' }
#'
#' In addition, the PSA may be converted to a base-case analysis by using \code{summary}
#' on the PSA object. The output of \code{summary} can be used in \code{\link{calculate_icers}}.
#'
#'
#' @return An object of class \code{psa}
#'
#' @seealso \code{\link{summary.psa}}, \code{\link{plot.psa}}
#'
#' @examples
#' # psa input provided with package
#' data("example_psa")
#' psa <- make_psa_obj(example_psa$cost, example_psa$effectiveness,
#'                     example_psa$parameters, example_psa$strategies)
#'
#' # custom print and summary methods
#' print(psa)
#' summary(psa)
#'
#' # custom plot method; see ?plot.psa for options
#' plot(psa)
#'
#' @importFrom stringr str_replace
#' @export
make_psa_obj <- function(cost, effectiveness, parameters = NULL,
                         strategies = NULL, currency = "$", other_outcome = NULL) {
  
  # parameter names
  parnames <- names(parameters)
  
  # define psa as a named list
  psa_obj <- create_sa(parameters, parnames, effectiveness, strategies,
                       cost, currency, other_outcome)
  
  # give classes "psa" and "sa"
  class(psa_obj) <- c("psa", class(psa_obj))
  return(psa_obj)
}

check_psa_object <- function(psa) {
  if (!inherits(psa, "psa")) {
    stop(paste0("The psa results parameter must be an object of class `psa`.\n",
                "Please run the make_psa() function to create this object."))
  }
}

check_df_and_coerce <- function(obj) {
  obj_name <- deparse(substitute(obj))
  if (!inherits(obj, "data.frame")) {
    warning(paste0("\'", obj_name, "\'", " is not a data frame. coercing to data frame"))
    df <- as.data.frame(obj)
  } else {
    df <- as.data.frame(obj)
  }
  return(df)
}