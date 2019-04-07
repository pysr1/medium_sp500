# for scraping
library(rvest)
# blanket import for core tidyverse packages
library(tidyverse)
# tidy financial analysis 
library(tidyquant)
# tidy data cleaning functions
library(janitor)


# get the URL for the wikipedia page with all SP500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the SP500 table using rvest
tickers <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # one way to get table
  #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  # easier way to get table
  html_nodes(xpath = '//*[@id="constituents"]') %>% 
  html_table()
#create a list of tickers
sp500tickers <- tickers[[1]]
sp500tickers = sp500tickers %>% mutate(Symbol = case_when(Symbol == "BRK.B" ~ "BRK-B",
                                                          Symbol == "BF.B" ~ "BF-B",
                                                          TRUE ~ as.character(Symbol)))

symbols = sp500tickers$Symbol
# define a function that retunrs OHLC data with the ticker name
get_symbols = function(ticker = "AAPL"){
  df = tq_get(ticker, from = "2019-01-05", to = "2019-04-05")  %>%  mutate(symbol = rep(ticker, length(date)))
  return(df)
}
#create the dataframe of SP500 data by interating over our list of symbols and call our get symbols function each time
#the map function accomplishes this

tickers_df = map(symbols, get_symbols) %>% bind_rows()
tickers_df = tickers_df %>% 
  # left join with wikipedia data
  left_join(sp500tickers, by = c('symbol' = 'Symbol')) %>% 
  # make names R compatible
  clean_names() %>% 
  # keep only the columns we need
  select(date:security, gics_sector, gics_sub_industry)


#tickers_df = tq_get(symbols)
Rows = dim(tickers_df)[1]
Columns = dim(tickers_df)[2]
dimensions = data.frame(Rows, Columns)
print(dimensions)
daily_sector = tickers_df %>% group_by(security, gics_sector, symbol) %>% 
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily")

avg_return =daily_sector %>% 
  group_by(security, gics_sector, symbol) %>%
  summarise(avg_return = round(mean(daily.returns), 4),Volatility = sd(daily.returns), cv = Volatility/avg_return) %>% arrange(desc(avg_return), desc(Volatility))


avg_return = avg_return %>% 
  mutate(Indicator = case_when(symbol %in% c('FOX', 'FOXA') ~ "Fox News",
                               TRUE ~ "The Rest of the SP500"))
plot = avg_return %>% ggplot(aes(avg_return, Volatility, color = Indicator))+
  geom_text(aes(label = symbol), size = 3)+
  labs(title = "Average Return vs Volatility Over Last 3 Months In SP500", x = "Average Return", subtitle = "Data Source: Yahoo Finance")+
  theme_minimal()

plot