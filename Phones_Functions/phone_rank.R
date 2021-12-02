#' Phone Rank Table
#'
#' Creates a table for phone ranking performance for a scored dataframe. The dataframe should have at least an account variable to group by, a status field, and score.
#' @param df Dataframe to operate on.  
#' @param grouping_var Variable to group phones for ranking (most of the time an account number)
#' @param status Status variable with performance categories
#' @param score Score from the dataframe.  This can be either logodds or scaled score.
#' @param rank_name Name for ranking variable 
#' @return Ranked dataframe with performance
#' @examples
#' phn_rank <- phone_rank(df = ks_df_mod1, rank_name = "Mod1 Rank")
#' @export
#' @import formattable
#' @import janitor
#' @import dplyr



phone_rank <- function(df, grouping_var = "key_acct", status = "status", scoring_var = "score", rank_name = "challenger_rank"){
  
  names(df)[names(df) == status] <- 'status'
  names(df)[names(df) == scoring_var] <- 'score'
  
  df <- df %>% 
    group_by_at(grouping_var) %>% 
    mutate(mod_rank = order(order(score, decreasing = TRUE))) %>%  # rank new scores
    ungroup() %>% 
    mutate(across(ends_with("rank"),
                  function(x) ifelse(x > 5, 6, x)))
  
  var <- "mod_rank"
  
  dist_tot <- df %>% 
    group_by_at(var) %>% 
    dplyr::summarize(Count = n()) %>% 
    ungroup() %>% 
    mutate(Pct_Accts = percent(Count/nrow(df))) %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_ver <- df %>% 
    filter(status == 'VERIFIED') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_Verified = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_wp <- df %>% 
    filter(status == 'WRONG PARTY') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_WrongParty = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_disc <- df %>% 
    filter(status == 'DISC.') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_Disc = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_mach <- df %>% 
    filter(status == 'MACHINE') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_Machine = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_noans <- df %>% 
    filter(status == 'NO ANSWER') %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_NoAnswer = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_oth <- df %>% 
    filter(status %in% c('DECEASED', 'FAX', 'LEAD PHONE', 'NOT ENOUGH INFO')) %>% 
    group_by_at(var) %>% 
    dplyr::summarize(N_Other = n()) %>% 
    ungroup() %>% 
    arrange(desc(is.na(as.data.frame(.)[,1])))
  
  dist_fin <- dist_tot %>% 
    left_join(dist_ver, by = var) %>% 
    left_join(dist_wp, by = var) %>% 
    left_join(dist_disc, by = var) %>% 
    left_join(dist_mach, by = var) %>% 
    left_join(dist_noans, by = var) %>% 
    left_join(dist_oth, by = var) %>% 
    mutate(across(.cols = starts_with('N_'),
                  .fns = function(x) ifelse(is.na(x), 0, x))) %>% 
    mutate(N_Contact = N_Verified + N_Machine + N_NoAnswer)  %>%  
    mutate(Contact_CumRate = percent(cumsum(N_Contact)/sum(N_Contact)),
           Verified_CumRate = percent(cumsum(N_Verified)/sum(N_Verified)),
           WrongParty_CumRate = percent(cumsum(N_WrongParty)/sum(N_WrongParty)),
           Disc_CumRate = percent(cumsum(N_Disc)/sum(N_Disc)),
           Machine_CumRate = percent(cumsum(N_Machine)/sum(N_Machine)),
           NoAnswer_CumRate = percent(cumsum(N_NoAnswer)/sum(N_NoAnswer)),
           Other_CumRate = percent(cumsum(N_Other)/sum(N_Other))) %>% 
    mutate(across(ends_with("CumRate"), 
                  .fns = function(x) as.character(x))) %>% 
    adorn_totals('row', fill = "100.00%") %>%
    mutate(Verified_Rate = percent(N_Verified/Count),
           WrongParty_Rate = percent(N_WrongParty/Count),
           Disc_Rate = percent(N_Disc/Count),
           Machine_Rate = percent(N_Machine/Count),
           NoAnswer_Rate = percent(N_NoAnswer/Count),
           Other_Rate = percent(N_Other/Count),
           Contact_Rate = percent(N_Contact/Count)) %>%
    select(starts_with(var), Count, Pct_Accts, N_Contact, Contact_Rate, Contact_CumRate, 
           N_Verified, Verified_Rate, Verified_CumRate, N_Disc, Disc_Rate, Disc_CumRate,
           N_WrongParty, WrongParty_Rate, WrongParty_CumRate, N_Machine, Machine_Rate, Machine_CumRate,
           N_NoAnswer, NoAnswer_Rate, NoAnswer_CumRate, N_Other, Other_Rate, Other_CumRate)
  
  names(dist_fin)[1] <- rank_name
  
  
  return(dist_fin)
}
