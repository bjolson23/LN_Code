#' Model Comparison
#'
#' @param mod_assess1 Model assessment object (base)
#' @param mod_assess2 Model assessment object (new)
#' @return A KS lift table, a list of model performance lift comparison tables.
#' @examples
#' compare_mod <- compare_mods(mod_assess1 = base_mod_assess, mod_assess2 = new_mod_assess)
#' @export
#' @import formattable
#' @import dplyr
 
compare_mods <- function(mod_assess1, mod_assess2){
  
  ## KS Lift ##
  
  ks_1 <- list(mod_assess1[[2]], mod_assess1[[5]], mod_assess1[[8]], mod_assess1[[11]], mod_assess1[[14]])
  ks_1 <- lapply(ks_1, function(x) str_replace_all(x, "KS: ", ""))
  ks_1 <- lapply(ks_1, as.numeric)
  
  ks_2 <- list(mod_assess2[[2]], mod_assess2[[5]], mod_assess2[[8]], mod_assess2[[11]], mod_assess2[[14]])
  ks_2 <- lapply(ks_2, function(x) str_replace_all(x, "KS: ", ""))
  ks_2 <- lapply(ks_2, as.numeric)
  
  ks_df <- data.frame(Party = c("All Accounts", "First Party", "Third Party", "Fraud", "FraudPlus"),
                      Base_KS = unlist(ks_1),
                      Challenger_KS = unlist(ks_2)) %>% 
    mutate(KS_Lift = percent((Challenger_KS - Base_KS) / Base_KS))
  
  ## 1st Position Lift ##
  
  perf_tbl_1 <- lapply(list(mod_assess1[[3]], mod_assess1[[6]], mod_assess1[[9]], mod_assess1[[12]], mod_assess1[[15]]), function(x) as.data.frame(x))
  perf_tbl_2 <- lapply(list(mod_assess2[[3]], mod_assess2[[6]], mod_assess2[[9]], mod_assess2[[12]], mod_assess2[[15]]), function(x) as.data.frame(x))
  
  summ_all_df <- data.frame(Performance_Measure = c("Contact Rate", "Disconnect Rate", "Verified Rate", "Wrong Party Rate"),
                            Base = c(perf_tbl_1[[1]]$Contact_Rate[1], perf_tbl_1[[1]]$Disc_Rate[1], 
                                     perf_tbl_1[[1]]$Verified_Rate[1], perf_tbl_1[[1]]$WrongParty_Rate[1]),
                            Challenger = c(perf_tbl_2[[1]]$Contact_Rate[1], perf_tbl_2[[1]]$Disc_Rate[1], 
                                           perf_tbl_2[[1]]$Verified_Rate[1], perf_tbl_2[[1]]$WrongParty_Rate[1])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  summ_all_cont <- data.frame(Performance_Measure = c("Contactability"),
                              Base = round(as.numeric(summ_all_df$Base[1])/as.numeric(summ_all_df$Base[2]), 2),
                              Challenger = round(as.numeric(summ_all_df$Challenger[1])/as.numeric(summ_all_df$Challenger[2]), 2))%>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  summ_fpc_df <- data.frame(Performance_Measure = c("Contact Rate", "Disconnect Rate", "Verified Rate", "Wrong Party Rate"),
                            Base = c(perf_tbl_1[[2]]$Contact_Rate[1], perf_tbl_1[[2]]$Disc_Rate[1], 
                                     perf_tbl_1[[2]]$Verified_Rate[1], perf_tbl_1[[2]]$WrongParty_Rate[1]),
                            Challenger = c(perf_tbl_2[[2]]$Contact_Rate[1], perf_tbl_2[[2]]$Disc_Rate[1], 
                                           perf_tbl_2[[2]]$Verified_Rate[1], perf_tbl_2[[2]]$WrongParty_Rate[1])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  summ_fpc_cont <- data.frame(Performance_Measure = c("Contactability"),
                              Base = round(as.numeric(summ_fpc_df$Base[1])/as.numeric(summ_fpc_df$Base[2]), 2),
                              Challenger = round(as.numeric(summ_fpc_df$Challenger[1])/as.numeric(summ_fpc_df$Challenger[2]), 2))%>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  
  
  summ_tpc_df <- data.frame(Performance_Measure = c("Contact Rate", "Disconnect Rate", "Verified Rate", "Wrong Party Rate"),
                            Base = c(perf_tbl_1[[3]]$Contact_Rate[1], perf_tbl_1[[3]]$Disc_Rate[1], 
                                     perf_tbl_1[[3]]$Verified_Rate[1], perf_tbl_1[[3]]$WrongParty_Rate[1]),
                            Challenger = c(perf_tbl_2[[3]]$Contact_Rate[1], perf_tbl_2[[3]]$Disc_Rate[1], 
                                           perf_tbl_2[[3]]$Verified_Rate[1], perf_tbl_2[[3]]$WrongParty_Rate[1])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  summ_tpc_cont <- data.frame(Performance_Measure = c("Contactability"),
                              Base = round(as.numeric(summ_tpc_df$Base[1])/as.numeric(summ_tpc_df$Base[2]), 2),
                              Challenger = round(as.numeric(summ_tpc_df$Challenger[1])/as.numeric(summ_tpc_df$Challenger[2]), 2))%>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  
  
  summ_frd_df <- data.frame(Performance_Measure = c("Contact Rate", "Disconnect Rate", "Verified Rate", "Wrong Party Rate"),
                            Base = c(perf_tbl_1[[4]]$Contact_Rate[1], perf_tbl_1[[4]]$Disc_Rate[1], 
                                     perf_tbl_1[[4]]$Verified_Rate[1], perf_tbl_1[[4]]$WrongParty_Rate[1]),
                            Challenger = c(perf_tbl_2[[4]]$Contact_Rate[1], perf_tbl_2[[4]]$Disc_Rate[1], 
                                           perf_tbl_2[[4]]$Verified_Rate[1], perf_tbl_2[[4]]$WrongParty_Rate[1])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  summ_frd_cont <- data.frame(Performance_Measure = c("Contactability"),
                              Base = round(as.numeric(summ_frd_df$Base[1])/as.numeric(summ_frd_df$Base[2]), 2),
                              Challenger = round(as.numeric(summ_frd_df$Challenger[1])/as.numeric(summ_frd_df$Challenger[2]), 2))%>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  
  
  summ_new_df <- data.frame(Performance_Measure = c("Contact Rate", "Disconnect Rate", "Verified Rate", "Wrong Party Rate"),
                            Base = c(perf_tbl_1[[5]]$Contact_Rate[1], perf_tbl_1[[5]]$Disc_Rate[1], 
                                     perf_tbl_1[[5]]$Verified_Rate[1], perf_tbl_1[[5]]$WrongParty_Rate[1]),
                            Challenger = c(perf_tbl_2[[5]]$Contact_Rate[1], perf_tbl_2[[5]]$Disc_Rate[1], 
                                           perf_tbl_2[[5]]$Verified_Rate[1], perf_tbl_2[[5]]$WrongParty_Rate[1])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  summ_new_cont <- data.frame(Performance_Measure = c("Contactability"),
                              Base = round(as.numeric(summ_new_df$Base[1])/as.numeric(summ_new_df$Base[2]), 2),
                              Challenger = round(as.numeric(summ_new_df$Challenger[1])/as.numeric(summ_new_df$Challenger[2]), 2))%>% 
    mutate(Challenger_Lift = percent((as.numeric(Challenger)-as.numeric(Base))/(as.numeric(Base))))
  
  
  ## Top 3 Cumulative Lift ##
  
  summ_all_cum <- data.frame(Performance_Measure = c("Contact Cum Rate", "Disconnect Cum Rate", "Verified Cum Rate", "Wrong Party Cum Rate"),
                             Base = c(perf_tbl_1[[1]]$Contact_CumRate[3], perf_tbl_1[[1]]$Disc_CumRate[3], 
                                      perf_tbl_1[[1]]$Verified_CumRate[3], perf_tbl_1[[1]]$WrongParty_CumRate[3]),
                             Challenger = c(perf_tbl_2[[1]]$Contact_CumRate[3], perf_tbl_2[[1]]$Disc_CumRate[3], 
                                            perf_tbl_2[[1]]$Verified_CumRate[3], perf_tbl_2[[1]]$WrongParty_CumRate[3])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(percent(Challenger))-as.numeric(percent(Base)))/(as.numeric(percent(Base)))))
  
  summ_fpc_cum <- data.frame(Performance_Measure = c("Contact Cum Rate", "Disconnect Cum Rate", "Verified Cum Rate", "Wrong Party Cum Rate"),
                             Base = c(perf_tbl_1[[2]]$Contact_CumRate[3], perf_tbl_1[[2]]$Disc_CumRate[3], 
                                      perf_tbl_1[[2]]$Verified_CumRate[3], perf_tbl_1[[2]]$WrongParty_CumRate[3]),
                             Challenger = c(perf_tbl_2[[2]]$Contact_CumRate[3], perf_tbl_2[[2]]$Disc_CumRate[3], 
                                            perf_tbl_2[[2]]$Verified_CumRate[3], perf_tbl_2[[2]]$WrongParty_CumRate[3])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(percent(Challenger))-as.numeric(percent(Base)))/(as.numeric(percent(Base)))))
  
  
  summ_tpc_cum <- data.frame(Performance_Measure = c("Contact Cum Rate", "Disconnect Cum Rate", "Verified Cum Rate", "Wrong Party Cum Rate"),
                             Base = c(perf_tbl_1[[3]]$Contact_CumRate[3], perf_tbl_1[[3]]$Disc_CumRate[3], 
                                      perf_tbl_1[[3]]$Verified_CumRate[3], perf_tbl_1[[3]]$WrongParty_CumRate[3]),
                             Challenger = c(perf_tbl_2[[3]]$Contact_CumRate[3], perf_tbl_2[[3]]$Disc_CumRate[3], 
                                            perf_tbl_2[[3]]$Verified_CumRate[3], perf_tbl_2[[3]]$WrongParty_CumRate[3])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(percent(Challenger))-as.numeric(percent(Base)))/(as.numeric(percent(Base)))))
  
  
  summ_frd_cum <- data.frame(Performance_Measure = c("Contact Cum Rate", "Disconnect Cum Rate", "Verified Cum Rate", "Wrong Party Cum Rate"),
                             Base = c(perf_tbl_1[[4]]$Contact_CumRate[3], perf_tbl_1[[4]]$Disc_CumRate[3], 
                                      perf_tbl_1[[4]]$Verified_CumRate[3], perf_tbl_1[[4]]$WrongParty_CumRate[3]),
                             Challenger = c(perf_tbl_2[[4]]$Contact_CumRate[3], perf_tbl_2[[4]]$Disc_CumRate[3], 
                                            perf_tbl_2[[4]]$Verified_CumRate[3], perf_tbl_2[[4]]$WrongParty_CumRate[3])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(percent(Challenger))-as.numeric(percent(Base)))/(as.numeric(percent(Base)))))
  
  
  summ_new_cum <- data.frame(Performance_Measure = c("Contact Cum Rate", "Disconnect Cum Rate", "Verified Cum Rate", "Wrong Party Cum Rate"),
                             Base = c(perf_tbl_1[[5]]$Contact_CumRate[3], perf_tbl_1[[5]]$Disc_CumRate[3], 
                                      perf_tbl_1[[5]]$Verified_CumRate[3], perf_tbl_1[[5]]$WrongParty_CumRate[3]),
                             Challenger = c(perf_tbl_2[[5]]$Contact_CumRate[3], perf_tbl_2[[5]]$Disc_CumRate[3], 
                                            perf_tbl_2[[5]]$Verified_CumRate[3], perf_tbl_2[[5]]$WrongParty_CumRate[3])) %>% 
    mutate(Challenger_Lift = percent((as.numeric(percent(Challenger))-as.numeric(percent(Base)))/(as.numeric(percent(Base)))))
  
  
  
  ## Return Final List of Data Frames ##
  
  final_list <- list("KS" = ks_df,
                     "All_Summ" = summ_all_df,
                     "All_Cont" = summ_all_cont,
                     "All_Cum" = summ_all_cum,
                     "FPC_Summ" = summ_fpc_df,
                     "FPC_Cont" = summ_fpc_cont,
                     "FPC_Cum" = summ_fpc_cum,
                     "TPC_Summ" = summ_tpc_df,
                     "TPC_Cont" = summ_tpc_cont,
                     "TPC_Cum" = summ_tpc_cum,
                     "Fraud_Summ" = summ_frd_df,
                     "Fraud_Cont" = summ_frd_cont,
                     "Fraud_Cum" = summ_frd_cum,
                     "New_Summ" = summ_new_df,
                     "New_Cont" = summ_new_cont,
                     "New_Cum" = summ_new_cum)
  
  return(final_list)
  
  
}
