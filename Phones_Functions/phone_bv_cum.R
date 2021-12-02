#' Phone Bivariate
#'
#' Create bivariates with multiple performance outcomes for attributes (includes cumulative percentages)
#' @param df Dataframe to operate on
#' @param var Variable to calculate bivariate on (must be a character)
#' @param status Status variable with performance categories
#' @param ver Category for verified
#' @param wp Category for wrong party
#' @param disc Category for disconnects
#' @param mach Category for machine
#' @param no_ans Category for no answer
#' @param other Category for other
#' @return Dataframe with bivariates calculated
#' @examples
#' bv1 <- phone_bv_cum(df = march_phone_df, var = "mpp_type", status = "Status_group")
#' @export
#' @import formattable
#' @import janitor
#' @import dplyr


phone_bv_cum <- function(df, var, status = "Status_group",
                         ver = "VERIFIED", wp = "WRONG PARTY", disc = "DISC.", mach = "MACHINE", no_ans = "NO ANSWER", other = "OTHER"){

  names(df)[names(df) == status] <- 'Status_group'

  df <- df %>%
    filter(Status_group != "") %>%
    mutate(Status_group = ifelse(!(Status_group %in% c(ver, wp, disc, mach, no_ans)), other, Status_group))

  dist_tot <- df %>%
    group_by_at(var) %>%
    dplyr::summarize(N_Total = n()) %>%
    ungroup() %>%
    arrange(desc(is.na(as.data.frame(.)[,1])))

  dist_ver <- df %>%
    filter(Status_group == ver) %>%
    group_by_at(var) %>%
    dplyr::summarize(N_Verified = n()) %>%
    ungroup() %>%
    arrange(desc(is.na(as.data.frame(.)[,1])))

  dist_wp <- df %>%
    filter(Status_group == wp) %>%
    group_by_at(var) %>%
    dplyr::summarize(N_WrongParty = n()) %>%
    ungroup() %>%
    arrange(desc(is.na(as.data.frame(.)[,1])))

  dist_disc <- df %>%
    filter(Status_group == disc) %>%
    group_by_at(var) %>%
    dplyr::summarize(N_Disc = n()) %>%
    ungroup() %>%
    arrange(desc(is.na(as.data.frame(.)[,1])))

  dist_mach <- df %>%
    filter(Status_group == mach) %>%
    group_by_at(var) %>%
    dplyr::summarize(N_Machine = n()) %>%
    ungroup() %>%
    arrange(desc(is.na(as.data.frame(.)[,1])))

  dist_noans <- df %>%
    filter(Status_group == no_ans) %>%
    group_by_at(var) %>%
    dplyr::summarize(N_NoAnswer = n()) %>%
    ungroup() %>%
    arrange(desc(is.na(as.data.frame(.)[,1])))


  dist_oth <- df %>%
    filter(Status_group == other) %>%
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
    arrange(desc(is.na(as.data.frame(.)[,1]))) %>%
    mutate(N_Contact = N_Verified + N_Machine + N_NoAnswer)  %>%
    mutate(Total_CumRate = percent(cumsum(N_Total)/sum(N_Total)),
           Contact_CumRate = percent(cumsum(N_Contact)/sum(N_Contact)),
           Verified_CumRate = percent(cumsum(N_Verified)/sum(N_Verified)),
           WrongParty_CumRate = percent(cumsum(N_WrongParty)/sum(N_WrongParty)),
           Disc_CumRate = percent(cumsum(N_Disc)/sum(N_Disc)),
           Machine_CumRate = percent(cumsum(N_Machine)/sum(N_Machine)),
           NoAnswer_CumRate = percent(cumsum(N_NoAnswer)/sum(N_NoAnswer)),
           Other_CumRate = percent(cumsum(N_Other)/sum(N_Other))) %>%
    mutate(across(ends_with("CumRate"),
                  .fns = function(x) as.character(x))) %>%
    adorn_totals('row', fill = "100.00%") %>%
    mutate(Pct_Total = percent(N_Total/nrow(df)),
           Verified_Rate = percent(N_Verified/N_Total),
           WrongParty_Rate = percent(N_WrongParty/N_Total),
           Disc_Rate = percent(N_Disc/N_Total),
           Machine_Rate = percent(N_Machine/N_Total),
           NoAnswer_Rate = percent(N_NoAnswer/N_Total),
           Other_Rate = percent(N_Other/N_Total),
           Contact_Rate = percent(N_Contact/N_Total)) %>%
    select(starts_with(var), N_Total, Pct_Total, Total_CumRate, N_Contact, Contact_Rate, Contact_CumRate,
           N_Verified, Verified_Rate, Verified_CumRate,N_Disc, Disc_Rate, Disc_CumRate,
           N_WrongParty, WrongParty_Rate, WrongParty_CumRate, N_Machine, Machine_Rate, Machine_CumRate,
           N_NoAnswer, NoAnswer_Rate, NoAnswer_CumRate, N_Other, Other_Rate, Other_CumRate)

  return(dist_fin)
}
