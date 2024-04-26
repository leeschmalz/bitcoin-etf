library(dplyr)
library(ggplot2)
library(data.table)
library(plotly)
library(htmlwidgets)

data_wide <- fread("./btc_etf_data.csv")
data_wide$date <- as.Date(data_wide$date, format="%m/%d/%y")
data_long <- data_wide %>% tidyr::pivot_longer(cols = setdiff(colnames(data_wide),"date"), names_to = 'ticker', values_to = 'flow')

p1 <- ggplot(data_long %>% filter(date>as.Date('2024-01-10')), aes(x=date, y=flow, fill=ticker)) +
  geom_bar(stat='identity') + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('Flow (BTC)')
p1 <- ggplotly(p1, width=20*96, height=8*96)
saveWidget(p1, file="./plots/daily_flows.html")

holdings_over_time <- data_long %>% group_by(ticker) %>% arrange(date) %>% mutate(balance = cumsum(flow))
p2 <- ggplot(holdings_over_time, aes(x=date, y=balance, fill=ticker)) +
  geom_bar(stat='identity') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ylab('Holdings (BTC)')
p2 <- ggplotly(p2, width=20*96, height=8*96)
saveWidget(p2, file="./plots/holdings.html")

gbtc_vs_new <- data_wide %>% filter(date>as.Date('2024-01-10')) %>% rowwise() %>% mutate(new9_total = sum(IBIT, FBTC, BITB, ARKB, BTCO, EZBC, BRRR, HODL, BTCW, DEFI)) %>% mutate(month = format(date, "%b"))
gbtc_vs_new$month <- factor(gbtc_vs_new$month, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

model <- lm(new9_total ~ GBTC, data = gbtc_vs_new)
r_squared <- summary(model)$r.squared
pearson_correlation <- cor(-gbtc_vs_new$GBTC, gbtc_vs_new$new9_total, method = "pearson")

p3 <- ggplot(gbtc_vs_new, aes(x=-GBTC, y=new9_total, label=date)) +
  geom_point(aes(color=month)) +
  geom_smooth(method='lm', formula= y~x, se=FALSE) +
  geom_abline(slope=1,intercept=0) +
  theme_bw() +
  ylab('New 9 Inflow') +
  xlab('GBTC Outflow') +
  annotate("text", x = 12000, y = 1000, label = sprintf("pearson r = %.2f", pearson_correlation), hjust = 1.1, vjust = 1.1, size = 5, fontface = "italic")
p3 <- ggplotly(p3, width=12*96, height=8*96)
saveWidget(p3, file="./plots/outflows_inflows_correlation.html")

p4 <- ggplot(gbtc_vs_new %>% filter(GBTC != 0 & new9_total != 0) %>% rename(`New 9`=new9_total), aes(x = date)) +
  geom_point(aes(y = GBTC, color = 'GBTC'), shape = 1) + # Shape is set to 1 for solid circles
  geom_line(aes(y = GBTC, color = 'GBTC')) +
  geom_point(aes(y = `New 9`, color = 'New 9'), shape = 1) + # Shape is set to 2 for triangles
  geom_line(aes(y = `New 9`, color = 'New 9')) +
  scale_color_manual(values = c('GBTC' = 'darkred', 'New 9' = 'darkblue')) +
  theme_minimal() +
  labs(color = "") +
  ylab("Daily Flow (BTC)")
p4 <- ggplotly(p4, width=12*96, height=8*96)
saveWidget(p4, file="./plots/outflows_inflows_over_time.html")
