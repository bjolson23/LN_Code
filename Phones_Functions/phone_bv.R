#' Phone Bivariate
#'
#' Create bivariates with multiple performance outcomes for attributes
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
#' bv1 <- phone_bv(df = march_phone_df, var = "mpp_type", status = "Status_group")
#' @export
#' @import formattable
#' @import janitor
#' @import dplyr


phone_bv <- function(df, var, status = "Status_group",
                     ver = "VERIFIED", wp = "WRONG PARTY", disc = "DISC.", mach = "MACHINE", no_ans = "NO ANSWER", other = "OTHER"){

  names(df)[names(df) == status] <- 'Status_group'

  df <- df %>%
    filter(Status_group != "") %>%
    mutate(Status_group = ifelse(!(Status_group %in% c(ver, wp, disc, mach, no_ans)), other, Status_group))

  dist_tot <- df %>%
    group_by_at(var) %>%
    dplyr::summarize(Count = n()) %>%
    ungroup() %>%
    mutate(Pct_Accts = percent(Count/nrow(df)))

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
    mutate(N_Contact = N_Verified + N_Machine + N_NoAnswer) %>%
    adorn_totals('row') %>%
    mutate(Verified_Rate = percent(N_Verified/Count),
           WrongParty_Rate = percent(N_WrongParty/Count),
           Disc_Rate = percent(N_Disc/Count),
           Machine_Rate = percent(N_Machine/Count),
           NoAnswer_Rate = percent(N_NoAnswer/Count),
           Other_Rate = percent(N_Other/Count),
           Contact_Rate = percent(N_Contact/Count)) %>%
    select(starts_with(var), Count, Pct_Accts, N_Contact, Contact_Rate,
           N_Verified, Verified_Rate, N_Disc, Disc_Rate, N_WrongParty, WrongParty_Rate,
           N_Machine, Machine_Rate, N_NoAnswer, NoAnswer_Rate, N_Other, Other_Rate) %>%
    arrange(desc(is.na(as.data.frame(.)[,1])))

  return(dist_fin)
}
