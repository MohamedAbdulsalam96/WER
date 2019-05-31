library(tidyverse)        # the usual
library(lubridate)        # to deal with dates
library(gridExtra)        # to stack the charts
library(scales)           # again to deal with dates but on the x-axis
library(bdscale)          # to remove the weekends using the scale_x_bd


geom_candlestick <- function(mapping = NULL, data = NULL, stat = "identity",
                             position = "identity", na.rm = TRUE, show.legend = NA,
                             inherit.aes = TRUE,
                             colour_up = "gray30", colour_down = "gray30",
                             fill_up = "green3", fill_down = "red",
                             ...) {
  
  linerange <- ggplot2::layer(
    stat = StatLinerangeBC, geom = GeomLinerangeBC, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  colour_up = colour_up, colour_down = colour_down, ...)
  )
  
  rect <- ggplot2::layer(
    stat = StatRectCS, geom = GeomRectCS, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fill_up = fill_up, fill_down = fill_down,
                  colour_up = colour_up, colour_down = colour_down, ...)
  )
  
  
  
  list(linerange, rect)
}

StatLinerangeBC <- ggplot2::ggproto("StatLinerangeBC", Stat,
                                    required_aes = c("x", "open", "high", "low", "close"),
                                    
                                    compute_group = function(data, scales, params,
                                                             fill_up, fill_down,
                                                             colour_up, colour_down) {
                                      
                                      data <-  data %>%
                                        dplyr::mutate(colour = ifelse(open < close, colour_up, colour_down))
                                      
                                      tibble::tibble(x = data$x,
                                                     ymin = data$low,
                                                     ymax = data$high,
                                                     colour = data$colour)
                                    })


StatRectCS <- ggplot2::ggproto("StatRectCS", Stat,
                               required_aes = c("x", "open", "high", "low", "close"),
                               
                               compute_group = function(data, scales, params,
                                                        fill_up, fill_down,
                                                        colour_up, colour_down) {
                                 
                                 data <-  data %>%
                                   dplyr::mutate(fill = ifelse(open < close, fill_up, fill_down),
                                                 ymin = ifelse(open < close, open, close),
                                                 ymax = ifelse(open < close, close, open))
                                 
                                 tibble::tibble(xmin = data$x - 0.45,
                                                xmax = data$x + 0.45,
                                                ymin = data$ymin,
                                                ymax = data$ymax,
                                                fill = data$fill)
                               })


GeomRectCS <- ggplot2::ggproto("GeomRectCS", GeomRect,
                      default_aes = aes(colour = NA,
                                        size = 0.5,
                                        linetype = 1,
                                        alpha = NA))


GeomLinerangeBC <- ggproto("GeomLinerangeBC", GeomLinerange,
                           default_aes = aes(size = 0.5,
                                             linetype = 1,
                                             alpha = NA))


create_candlestick <- function(df, tickerss){
  df$Index <- ymd(df$Index)
  
  #for the ADX
  yo <- TTR::ADX(df[,3:5], n = 13) %>% as_tibble() %>% select(-DX)
  
  # Adding the other variables such a moving averages and relative strength index
  df3 <- df %>% 
    mutate(sma200 = TTR:: SMA(Close, 200), 
           sma50 = TTR::SMA(Close, 50), 
           ema9 = TTR::EMA(Close, 9), 
           rsi14 = TTR::RSI(Close, 14), 
           rsi5 = TTR::RSI(Close, 5), 
           ppo_line = (TTR::EMA(Close, n = 12) - TTR::EMA(Close, n = 26)) / TTR::EMA(Close, n = 26) * 100, 
           ppo_signal = TTR::EMA(ppo_line, n = 9)) 
  
  df2 <- bind_cols(df3, yo) %>% 
    filter(Index >= today() - 400)
  
  
  # The main chart with the moving averages
  p1 <- ggplot(df2, aes(x=Index, y = Close)) + 
    geom_candlestick(aes(open = Open, high = High, low = Low, close = Close)) + 
    geom_line(aes(y = ema9), color = "red", size = 0.2) + 
    geom_line(aes(y = sma200), color = "darkorchid1", size = 0.3) + 
    # because I need to remember which chart is it (to which stock it belongs)
    annotate("text", x = df2$Index[10], y = 1.1 * df2$Close[10], label = tickerss, color = "white") + 
    geom_line(aes(y = sma50), color = "Turquoise 1", size = 0.3) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.3)) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    theme(axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0.2, 0.2, 0.1, 0.4, "cm"),       # This is to shrink the padding at the 4 side of the graph
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(color = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(color = "white", linetype = "dotted", size = 0.2),
          panel.grid.minor = element_blank())
  
  # graphing of the ppo part.  
  p2 <- ggplot(df2, aes(x = Index)) + 
    geom_line(aes(y = ppo_signal, color = "darkorchid1"), size = 0.4) + 
    geom_line(aes(y = ppo_line, color = "Royal Blue 1"), size = 0.5) + 
    geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 0.3) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.1)) + 
    ylab("PPO") + 
    theme(legend.position = "none",  
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0, 0.2, 0.1, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(color = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(color = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  p4 <- ggplot(df2, aes(x = Index)) + 
    geom_line(aes(y = DIp), color = "Turquoise 1", size = 0.2) + 
    geom_line(aes(y = DIn), color = "red", size = 0.2) + 
    geom_line(aes(y = ADX), color = "Gray 70", size = 0.3) + 
    scale_y_continuous(sec.axis = sec_axis(~.*1)) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, labels=date_format("%b '%y"), expand = c(0,0.1)) + 
    ylab("ADX") + 
    theme(legend.position = "none",  
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(angle = 90), 
          plot.margin = margin(0, 0.2, 0.1, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(color = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(color = "white", linetype = "dotted", size = 0.1),
          panel.grid.minor = element_blank())
  
  
  p3 <- ggplot(df2, aes(x = Index)) + 
    geom_line(aes(y = rsi14, color = "Dark Orange")) + 
    geom_line(aes(y = rsi5, color = "Gray 80"), linetype = "dotted", size = 0.4) + 
    scale_x_bd(business.dates=df2$Index, max.major.breaks = 20, 
               labels=date_format("%b '%y"), expand = c(0,0.5)) + 
    scale_y_continuous(sec.axis = sec_axis(~.)) + 
    ylab("RSI") + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.y = element_text(angle = 90), 
          axis.text.x = element_text(angle = 30, vjust = 0.5), 
          plot.margin = margin(0.0, 0.2, 0.2, 0.4, "cm"), 
          panel.background = element_rect(fill = "black"), 
          plot.background = element_rect(fill = "Gray 65"), 
          panel.grid.major.x = element_line(color = "white", linetype = "dotted", size = 0.2), 
          panel.grid.major.y = element_line(color = "white", linetype = "dotted", size = 0.2),
          panel.grid.minor = element_blank())
  
  
  yo <- grid.arrange(p1, p2, p4, p3, ncol = 1, heights = c(2.5, 1, 1, 1))
  yo
}


##################################################################################################
### plot for adjusted return 
##################################################################################################
plot_adjusted_return <- function(dff, title){
  ggplot(dff, aes(x = Index, y = adjusted_return, color = api_ticker)) + 
    geom_line() + 
    #scale_x_bd(business.dates=dff$Index, max.major.breaks = 12, labels=date_format("%b '%y"), 
     #          expand = c(0,0.3)) + 
    ylab(label = "Adjusted return") + 
    ggtitle(title) + 
    theme(legend.title = element_blank(),
          axis.title.x = element_blank(), 
          plot.title = element_text(hjust=0.5, face = "bold"), 
          panel.background = element_rect(fill = "black"), 
          #panel.grid.minor = element_line(color = "white", linetype = "dotted", size = 0.2), 
          panel.grid.minor.x = element_blank(), 
          plot.background = element_rect(fill = "Gray 75"), 
          legend.key = element_rect(fill = "Gray 75"), 
          legend.background = element_rect(fill = "Gray 75"), 
          panel.grid.major.x = element_line(color = "white", linetype = "dotted", size = 0.16), 
          panel.grid.major.y = element_line(color = "Gray 65", linetype = "solid", size = 0.2), 
          panel.grid.minor.y = element_line(color = "Gray 65", linetype = "dotted", size = 0.2),
          axis.text.x = element_text(angle = 60, vjust = 0.5))
}


