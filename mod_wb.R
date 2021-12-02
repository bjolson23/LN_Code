#' Model Workbook
#'
#' Create bivariates with multiple performance outcomes for attributes (includes cumulative percentages)
#' @param mod Model used in model assessment
#' @param mod_assess Model assessment object from assess_mod function
#' @param mkiv MKIV used for attribute creation
#' @return Excel workbook with holdout performance for the model and model summary
#' @examples
#' base_wb <- wb_fun(mod = mod_1, mod_assess = mod_assessment1, mkiv = mkiv)
#' @export
#' @import openxlsx
#' @import ezxl
#' @import xgboost
#' @import dplyr


mod_wb <- function(mod, mod_assess, mkiv){
  
  wb <- createWorkbook()
  wb <- ezxl(mod_assess, wb = wb, sheet = "Holdout Performance")
  
  pct_col_style <- createStyle(halign = 'right')
  addStyle(wb = wb, sheet = "Holdout Performance", pct_col_style, rows = 1:100, cols = c(3,5,6,8,9,11,12,14,15,17,18,20,21,23,24), gridExpand = TRUE)
  
  
  # get a data frame with all model variables
  imp <- xgb.importance(feature_names = mod$feature_names, mod)
  
  mod_summary <- as_tibble(imp[,c(1,2)]) # change summary to data frame
  mod_summary
  
  mod_summary <- mod_summary %>%
    left_join(mkiv[, c("var", "description", "group")], by = c("Feature" = "var")) %>% 
    rename(Description = description,
           Group = group) %>% 
    mutate(Group = ifelse(substr(Feature, start = 1, stop = 3) == "mpp", "PhonesPlus", Group)) %>% 
    mutate(Group = ifelse(substr(Feature, start = 1, stop = 4) == "minq", "Inquiries", Group))
  
  
  # summarize model by group tag
  
  mod_group_summary <- mod_summary %>%
    group_by(Group) %>%
    dplyr::summarise(
      variable_count = n(),
      group_rel_inf = sum(Gain)
    ) %>%
    arrange(desc(group_rel_inf))
  
  # create a summary of the groups that are not present in the model
  groups_in_mod <- unique(mod_summary$Group)
  all_groups <- unique(mkiv$group)
  
  groups_not_in_mod <- tibble(Groups_Not_In_Model = sort(setdiff(all_groups, groups_in_mod)))
  
  # output xlsx workbook
  
  addWorksheet(wb, "Model Vars Summary")
  writeDataTable(wb, "Model Vars Summary", mod_summary)
  
  writeDataTable(wb, "Model Vars Summary", mod_group_summary, startCol = 6, startRow = 1)
  
  writeDataTable(wb, "Model Vars Summary", groups_not_in_mod, startCol = 10, startRow = 1)
  
  setColWidths(wb, "Holdout Performance", cols = 1:24, widths = 'auto')
  setColWidths(wb, "Model Vars Summary", cols = 1:24, widths = 'auto')
  
  return(wb)
}
