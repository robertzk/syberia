#' Install syberia.io distribution on your machine.
#'
#' @export
install_syberia <- function() {
  if (!"devtools" %in% utils::installed.packages()[,1]) install.packages("devtools")
  # Disable tcl/tk for installation from CRAN
  devtools::with_options(c(menu.graphics = FALSE), {
    if (!"bettertrace" %in% utils::installed.packages()[,1]) devtools::install_github('robertzk/bettertrace')
    library(bettertrace);
    if (!require(Ramd)) devtools::install_github("robertzk/Ramd")
    Ramd::packages("magrittr", "RJSONIO", "lubridate", "plyr", "timeDate", "arules", "AUC", "base64enc", "pROC", "rjson", "crayon", "yaml")

    package_list <- yaml::yaml.load(packagefile("packages.yml", read = TRUE))
    invisible(manual_box(package_list))
  })
}
