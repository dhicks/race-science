args = commandArgs(trailingOnly = TRUE)
if ('--force' %in% args) {
    force = TRUE
} else {
    force = FALSE
}
# message(glue::glue('force is {force}'))
cli::cli_alert_warning('force is {force}')
