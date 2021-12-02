#' Phone Bivariate
#'
#' Outputs an HTML Kable object that is ready for RMarkdown implementation
#' @param df Dataframe to operate on (phone bivariate without cumulative columns)
#' @param var_name Name of the variable for table output
#' @param var_cnt Number of variables that the bivariate was grouped by
#' @param ver_color HTML Hex color code for verification columns
#' @param dis_color HTML Hex color code for disconnect columns
#' @param width_col Minimum width of the first column
#' @param scroll_width Width of the scroll box
#' @return Kable with formatting for bivariates to be output in RMarkdown
#' @examples
#' bv_tbl <- phone_bv_tbl(df = march_telo_bv_cum)
#' @export
#' @import kableExtra


phone_bv_tbl <- function(df, var_name = '', var_cnt = 1, ver_color = "#18B200", disc_color = "#FF6A6A",  width_col = "5cm", scroll_width = "900px"){

  tbl_df <- df
  var_name <- ifelse(var_name == '', colnames(df)[1:var_cnt], var_name)

  ver_cols <- which(str_detect(colnames(df), "Ver"))
  disc_cols <- which(str_detect(colnames(df), "Disc"))
  wp_cols <- which(str_detect(colnames(df), "Wrong"))
  mach_cols <- which(str_detect(colnames(df), "Machine"))
  noans_cols <- which(str_detect(colnames(df), "NoAnswer"))
  cont_cols <- which(str_detect(colnames(df), "Contact"))
  tot_cols <- which(str_detect(colnames(df), "Total"))
  oth_cols <- which(str_detect(colnames(df), "Other"))


  tbl_cols <- c("Total #", "Total %", "Contact #", "Contact %",
                "Verified #", "Verified %", "Disc #", "Disc %",
                "Wrong Party #",  "Wrong Party %", "Machine #", "Machine %",
                "No Answer #", "No Answer %",  "Other #", "Other %")


  tbl_scroll <- kable(tbl_df, "html",
                      escape = F,
                      col.names = c(var_name, tbl_cols),
                      align = c(rep('c', var_cnt), rep(c('r', 'r'), 8))) %>%
    kable_styling(bootstrap_options = c('striped', 'hover')) %>%
    row_spec(0, bold = T, color = "white", background = "lightskyblue", align = 'c') %>%
    column_spec(ver_cols, background = ver_color, bold = TRUE, width_min = "2.5cm") %>%
    column_spec(disc_cols, background = disc_color, bold = TRUE, width_min = "2cm") %>%
    column_spec(mach_cols, width_min = "3cm") %>%
    column_spec(noans_cols, width_min = "3cm") %>%
    column_spec(cont_cols, width_min = "2.5cm") %>%
    column_spec(tot_cols, width_min = "2cm") %>%
    column_spec(oth_cols, width_min = "2.5cm") %>%
    column_spec(wp_cols, width_min = "4cm") %>%
    column_spec(1:var_cnt, color = "#FCFCFC", background = "#989898", width_min = width_col) %>%
    scroll_box(width = scroll_width)

  return(tbl_scroll)
}

