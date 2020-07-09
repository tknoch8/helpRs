#' @title get pca summary from a prepped recipe that includes step_pca
#' @description get pca summary from a prepped recipe that includes step_pca
#' @usage get_pca_summary(prepped_pca_rec)
#' @param prepped_pca_rec the prepped pca recipe
#' @return a summary class object containing the standard deviation, proportion of variance, and cumulative proportion of all principal components.
#' @examples
#' \dontrun{
#' sparrows <- rio::import("https://raw.githubusercontent.com/StirlingCodingClub/Manuscripts_in_Rmarkdown/master/data/Bumpus_data.csv") %>%
#' mutate(surv = factor(surv),
#'       sex = factor(sex)) %>%
#' mutate(id = row_number()) %>%
#' mutate(id = paste0(sex, "_", surv, "_", id)) %>%
#' mutate(sex = if_else(sex == "male", 1, 0)) %>%
#' mutate(surv = if_else(surv == "alive", 1, 0))
#'
#' pca_rec <- recipe(~., data = sparrows) %>%
#'   update_role(id, surv, sex, new_role = "id") %>%
#'   step_normalize(all_predictors()) %>%
#'   step_pca(all_predictors())
#'
#' pca_prep <- prep(pca_rec)
#' get_pca_summary(pca_prep)
#' }
#' @export
get_pca_summary <- function(prepped_pca_rec) {
  prepped_pca_rec %>%
    pluck("steps") %>%
    pluck(2) %>%
    pluck("res") %>%
    summary()
}
