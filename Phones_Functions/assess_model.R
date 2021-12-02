#' Model Assessment
#'
#' Creates a model assessment object.  This includes KS values and a phone rank table for all accounts and each party.
#' @param df Dataframe to operate on.
#' @param train Numeric vector of which rows are training accounts
#' @param test Numeric vector of which rows are test accounts
#' @param holdout Numeric vector of which rows are holdout accounts
#' @param good Column that contains the labels of good (verified) phones
#' @param score Score column from the dataframe.  This should be a logodds score.
#' @param party Party column from dataframe. This should only contain values of 1, 3, 4, and 5.
#' @return Model assessment object with KS values and phone rank tables
#' @examples
#' mod_assess <- assess_model(df = ks_df_mod1, train = train, test = test, holdout = holdout)
#' @export


assess_model <- function(df, train, test, holdout,
                         good = "R_GOOD", score = "score", party = "party"){

  names(df)[names(df) == score] <- 'score'
  names(df)[names(df) == good] <- 'R_GOOD'
  names(df)[names(df) == party] <- 'party'

  if(nrow(df)!=(length(train)+length(test)+length(holdout))){
    warning("length of dataframe does not match correct length")
  }
  df_holdout <- df[holdout,]
  print("Train KS")
  print(KS_func_general(df$score[train], df$R_GOOD[train]))
  print("Test KS")
  print(KS_func_general(df$score[test], df$R_GOOD[test]))
  print("Holdout KS")
  print(KS_func_general(df_holdout$score, df_holdout$R_GOOD))

  ks_tot <- KS_func_general(df_holdout$score, df_holdout$R_GOOD)
  ks1 <- KS_func_general(df_holdout$score[df_holdout$party==1], df_holdout$R_GOOD[df_holdout$party==1])
  ks3 <- KS_func_general(df_holdout$score[df_holdout$party==3], df_holdout$R_GOOD[df_holdout$party==3])
  ks4 <- KS_func_general(df_holdout$score[df_holdout$party==4], df_holdout$R_GOOD[df_holdout$party==4])
  ks5 <- KS_func_general(df_holdout$score[df_holdout$party==5], df_holdout$R_GOOD[df_holdout$party==5])

  rank_tot <- phone_rank(df_holdout)
  rank1 <- phone_rank(df_holdout[df_holdout$party==1,])
  rank3 <- phone_rank(df_holdout[df_holdout$party==3,])
  rank4 <- phone_rank(df_holdout[df_holdout$party==4,])
  rank5 <- phone_rank(df_holdout[df_holdout$party==5,])

  return(list("All Accts", paste0("KS: ", round(ks_tot,digits = 4)), rank_tot,
              "First Party Collections", paste0("KS: ",round(ks1,digits = 4)), rank1,
              "Third Party Collections", paste0("KS: ", round(ks3,digits = 4)), rank3,
              "Fraud", paste0("KS: ", round(ks4,digits = 4)), rank4,
              "New Accts", paste0("KS: ", round(ks5,digits = 4)), rank5))
}
