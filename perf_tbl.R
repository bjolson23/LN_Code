#' Model Performance HTML Table
#'
#' @param df Model performance dataframe.  This must have 4 columns: performance measure, base model, challenger model, and lift.
#' @param header_font Font color of the header row
#' @param header_bg Background color of the header row
#' @param perf_col_font Font color of the performance measure column
#' @param perf_col_bg Background color of the performance measure column
#' @param column_names Names for the header row (must be length 4)
#' @return A formatted table of model performance for HTML output
#' @examples
#' tbl <- perf_tbl(perf_list[["FPC_Summ"]])
#' @export
#' @import kableExtra

perf_tbl <- function(df, header_font = "white", header_bg = "lightskyblue",
                     perf_col_font = "#FCFCFC", perf_col_bg = "#989898",
                     column_names = c("Performance Measure", "Base Model", "Challenger", "Lift")){
  tbl <- kable(df,
               "html",
               escape = F,
               col.names = column_names,
               align = c('l', 'r', 'r', 'r')) %>%
    kable_styling(bootstrap_options = c('striped', 'hover'), full_width = F) %>%
    row_spec(0, bold = T, color = header_font, background = header_bg, align = 'c') %>%
    column_spec(4, bold = T) %>%
    column_spec(1, color = perf_col_font, background = perf_col_bg)

  return(tbl)
}
