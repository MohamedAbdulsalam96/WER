# create a small function to get the adjusted returns. 
# It takes as input a df and return a df 
get_adjusted_return <- function(dff, start_date = today()-365){
  df <- dff %>% filter(Index >= start_date) %>% arrange(Index) %>% 
    mutate(adjusted_return = (Adjusted / first(Adjusted)) - 1) %>% 
    select(Index, adjusted_return)
  
  return(df)
}



# function calculate the essentials variables for each stock 
# DO I need week_number???????
create_big_df <- function(dff){
  yo <- dff
  yo$Index <- ymd(yo$Index)
  df <- yo %>% filter(Close != 0)
  yo <- TTR::chaikinVolatility(df[,3:4], n = 10)    #YES
  df <- bind_cols(df, chaikin_vol = yo)
  yo <- TTR::ATR(df[,3:5], n = 14) %>% as_tibble() %>% select(atr14 = atr)  #YES
  df <- bind_cols(df, yo) %>% mutate(atr14 = (atr14 / Close))
  yo <- TTR::ATR(df[,3:5], n = 40) %>% as_tibble() %>% select(atr40 = atr)  #YES
  df <- bind_cols(df, yo) %>% mutate(atr40 = RollingWindow::RollingStd(atr40 / Close, window = 21, na_method = "ignore"))
  yo <- TTR::ADX(df[,3:5], n = 14) %>% as_tibble() %>% select(adx = ADX, din = DIn, dip = DIp)  
  df <- bind_cols(df, yo)
  yo <- TTR::BBands(df[, 3:5]) %>% as_tibble() %>% select(down_bb = dn, up_bb = up)
  df <- bind_cols(df, yo) %>% mutate(down_bb = (Close / down_bb), up_bb = (Close / up_bb))  
  
  yo <- df %>% arrange(Index) %>% 
    mutate(year = year(Index), week_number = isoweek(Index), 
           days_of_week = wday(Index, label = TRUE, abbr = FALSE), 
           fut_highest_high = if_else(Close > Open, Close + 0.5*(High - Close), Open + 0.5*(High - Open)), 
           fut_max_ret = RollingWindow::RollingMax(fut_highest_high, window = 21), ## define window for future return
           fut_max_ret = lead(fut_max_ret, n = 21), ## define window for future return 
           fut_max_ret = (fut_max_ret / Close) - 1, 
           fut_max_ret_bin = if_else(fut_max_ret > 0.058, 1, 0), ## Define max expected return
           past1_ret = ((Close / lag(Close)) - 1) * 100, 
           past5_ret = ((Close / lag(Close, 5)) - 1) * 100, 
           past10_ret = ((Close / lag(Close, 10)) - 1) * 100, 
           past21_ret = ((Close / lag(Close, 21)) - 1) * 100, 
           past63_ret = ((Close / lag(Close, 63)) - 1) * 100, 
           past126_ret = ((Close / lag(Close, 126)) - 1) * 100, 
           past252_ret = ((Close / lag(Close, 252)) - 1) * 100, 
           fut5_ret = ((lead(Close, 5) / Close) - 1) * 100, 
           fut10_ret = ((lead(Close, 10) / Close) - 1) * 100, 
           fut21_ret = ((lead(Close, 21) / Close) - 1) * 100, 
           fut63_ret = ((lead(Close, 63) / Close) - 1) * 100,
           rsi14 = TTR::RSI(Close, n = 14), 
           rsi5 = TTR::RSI(Close,  n = 5), 
           monthly_sharp_Ratio = round(RollingWindow::RollingMean(past1_ret, window = 21, na_method = "ignore") / 
                                         RollingWindow::RollingStd(past1_ret, window = 21, na_method = "ignore"), 3), 
           yearly_sharp_Ratio = round(RollingWindow::RollingMean(past1_ret, window = 252, na_method = "ignore") / 
                                        RollingWindow::RollingStd(past1_ret, window = 252, na_method = "ignore"), 3),
           sma50 = TTR::SMA(Close, n = 50), 
           sma50_abov = (Close / sma50) - 1, 
           sma50_bin = if_else(sma50_abov > 0, 1, 0), 
           ema9 = TTR::EMA(Close, n = 9), 
           ema9_abov = (Close / ema9) - 1, 
           ema9_bin = if_else(ema9_abov > 0, 1, 0), 
           ema9_above_sma50 = if_else(ema9 > sma50, 1, 0), 
           ema9_above_sma50 = if_else(ema9_above_sma50 > lag(ema9_above_sma50), 1, 0), 
           
           volat_roc1_21days = RollingWindow::RollingStd(past1_ret, window = 21, na_method = "ignore"), 
           volat_roc5_21days = RollingWindow::RollingStd(past5_ret, window = 21, na_method = "ignore"), 
           volat_roc5_252days = RollingWindow::RollingStd(past5_ret, window = 252, na_method = "ignore"), 
           volat_roc21_252days = RollingWindow::RollingStd(past21_ret, window = 252, na_method = "ignore"), 
           
           roll_sd_sma50 = RollingWindow::RollingStd(sma50_abov, window = 250, na_method = "ignore"), 
           roll_mean_sma50 = RollingWindow::RollingMean(sma50_abov, window = 250, na_method = "ignore"), 
           diff_sd_sma50 = round((sma50 - roll_mean_sma50) / roll_sd_sma50, 2)) %>% 
    #filter(days_of_week == "Monday") %>% tail(., 12) %>% 
    #mutate(bi_monthly_yes = rep(seq(0, 1), 6)) %>% 
    select(Index, past1_ret, past5_ret, past10_ret, past21_ret, past63_ret, past126_ret, past252_ret, 
           monthly_sharp_Ratio, yearly_sharp_Ratio, diff_sd_sma50, 
           ema9_abov, sma50_abov, ema9_above_sma50, 
           volat_roc1_21days, volat_roc5_21days, volat_roc5_252days, volat_roc21_252days, 
           adx, din, dip, atr14, atr40, down_bb, up_bb, 
           fut5_ret, fut10_ret, fut21_ret, fut63_ret, fut_max_ret, fut_max_ret_bin) 
  return(yo)
}
