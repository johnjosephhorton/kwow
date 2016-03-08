#' Get mturk.df
#'
#' Get mturk.df
#'
#' @export


Get_mturk_df <- function(){
    readRDS(system.file("extdata/mturk_cooked.rds", package = "kwowR"))
}
