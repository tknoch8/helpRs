#' @title create a crontab job from an rscript
#' @description use the 'cronR' package to create a crontab job with one function
#' @usage make_crontab(.script, .frequency, .at = NULL)
#' @param .script  the path to the script to made into a crontab job
#' @param .frequency frequency at which to run, i.e. "daily" or "weekly" or "minutely"
#' @param .at the sub-frequency at which to run, i.e. for .frequency = "daily", .at could be "8AM"
#' @return a console printout of the crontab that was added for the current user
#' @examples
#' \dontrun{
#' make_crontab("user/my_folder/my_script.R", .frequency = "daily", .at = "8AM")
#' }
#' @importFrom cronR cron_rscript cron_add
#' @export
make_crontab <- function(.script, .frequency, .at = NULL) {

  if (is.null(.at)) {

    a <- cron_rscript(.script)

    cron_add(a, frequency = .frequency)

  } else {

    a <- cron_rscript(.script)

    cron_add(a, frequency = .frequency, at = .at)

  }

}
