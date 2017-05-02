
#"Capital  in Twenty First Century" 


#Clear the workspace - remove all variables
rm(list = ls())


library(Quandl)
library(plotly)



titleVar  <- list(
          #family = "Ariel",
          size = 18
          #color = "#7f7f7f"
          )



########################################


#Page 36 - Income inequality in the United States, 1910-2010 

i1 <- Quandl("PIKETTY/TSI_1", api_key="yg2rjFAWcN1FgzoJ1naL")

plot_ly(i1, x=Date, y=Value,mode = 'lines+markers') %>%
  layout(title = '<b>Income inequality in the United States, 1910-2010</b>',
         titlefont = titleVar,
         xaxis = list(title = '<b>Years</b>'),
         yaxis = list (title = '<b>Share of top decile in national income</b>'))



#########################################


#Page 38 - Market value of private capital (% national income)

i2 <- Quandl("PIKETTY/TSI_2", api_key="yg2rjFAWcN1FgzoJ1naL")


plot_ly(i2, x=Date, y=Germany, name = "<b>Germany</b>", mode = 'lines+markers') %>%
  add_trace(x=Date, y = France, name = "<b>France</b>", connectgaps = TRUE) %>%
  add_trace(x=Date, y = Britain, name = "<b>Britain</b>", connectgaps = TRUE) %>%
  layout(title = '<b>The capital/income ratio in Europe, 1870-2010</b>',
         titlefont = titleVar,
         xaxis = list(title = '<b>Years</b>'),
         yaxis = list (title = '<b>Market value of private capital (% national income)</b>'))


#########################################


#Page 75 - The distribution of world output, 1700-2012

c1_1 <- Quandl("PIKETTY/TS1_1A", api_key="yg2rjFAWcN1FgzoJ1naL")

#GP: taking first 8 rows only, data since 1700 AD
c1_1 <- c1_1[1:8,]

#NOTE: code below gave none stacked graph, we need to stack one on top of another 
# plot_ly(data=c1_1, x=Date, y=`World output`, fill="tonexty", mode="lines")  %>%
#            add_trace(x=Date, y = Europe, fill="tonexty") %>%
#            add_trace(x=Date, y = America, fill="tonexty") %>%
#            add_trace(x=Date, y = Africa, fill="tonexty") %>%
#            add_trace(x=Date, y = Asia, fill="tonexty")
               


Years <- c1_1$Date
y1_real<- c1_1$Europe
y2_real<- c1_1$America
y3_real<- c1_1$Africa
y4_real<- c1_1$Asia




df= data.frame(Years, y1_real, y2_real, y3_real, y4_real)

df$y4_stck<- df$y4_real + df$y3_real + df$y2_real + df$y1_real
df$y3_stck<- df$y3_real + df$y2_real + df$y1_real
df$y2_stck<- df$y2_real + df$y1_real
df$y1_stck<- df$y1_real

 p1<- plot_ly(df, x=df$Years, y=df$y1_stck, text=df$y1_real, hoverinfo='x+text+name', name="Europe", fill="tonexty", type = 'scatter', mode = "markers") %>%
   layout(title = '<b>The distribution of world output, 1700-2012</b>',
          titlefont = titleVar,
          xaxis = list(title = '<b>Years</b>'),
          yaxis = list (title = '<b>Percentage of Total World Output</b>'))
 
 
 p2<- add_trace(p1, x=df$Years, y=df$y2_stck, text=df$y2_real, type = 'scatter', mode = "markers", hoverinfo='x+text+name', name="America", fill="tonexty")
 p3<- add_trace(p2, x=df$Years, y=df$y3_stck, text=df$y3_real, type = 'scatter', mode = "markers", hoverinfo='x+text+name', name="Africa", fill="tonexty") 
 p4<- add_trace(p3, x=df$Years, y=df$y4_stck, text=df$y4_real, type = 'scatter', mode = "markers", hoverinfo='x+text+name', name="Asia", fill="tonexty") 
      
 p4



rm(df, p1, p2, p3, p4, y1_real, y2_real, y3_real, y4_real, Years)



#########################################


#Page 76 - The distribution of world population, 1700-2012


c1_2 <- Quandl("PIKETTY/TS1_2A", api_key="yg2rjFAWcN1FgzoJ1naL")


#GP: taking first 8 rows only, data since 1700 AD
c1_2 <- c1_2[1:8,]


Years <- c1_2$Date
y1_real<- c1_2$Europe
y2_real<- c1_2$America
y3_real<- c1_2$Africa
y4_real<- c1_2$Asia


df= data.frame(Years, y1_real, y2_real, y3_real, y4_real)

df$y4_stck<- df$y4_real + df$y3_real + df$y2_real + df$y1_real
df$y3_stck<- df$y3_real + df$y2_real + df$y1_real
df$y2_stck<- df$y2_real + df$y1_real
df$y1_stck<- df$y1_real

p1<- plot_ly(df, x=Years, y=y1_stck, text=df$y1_real, hoverinfo='x+text+name', name="Europe", fill="tonexty") %>%
  layout(title = '<b>The distribution of world population, 1700-2012</b>',
         titlefont = titleVar,
         xaxis = list(title = '<b>Years</b>'),
         yaxis = list (title = '<b>Percentage of Total World Population</b>'))


p2<- add_trace(p1, x=Years, y=y2_stck, text=df$y2_real, hoverinfo='x+text+name', name="America", fill="tonexty")
p3<- add_trace(p2, x=Years, y=y3_stck, text=df$y3_real, hoverinfo='x+text+name', name="Africa", fill="tonexty")
p4<- add_trace(p3, x=Years, y=y4_stck, text=df$y4_real, hoverinfo='x+text+name', name="Asia", fill="tonexty")

p5<- layout(yaxis=list(showgrid=FALSE, showline=FALSE))
p5


rm(df, p1, p2, p3, p4, p5, y1_real, y2_real, y3_real, y4_real, Years)



#########################################


#Page 77 - Global inequality, 1700-2012: divergence then convergence ?


c1_3 <- Quandl("PIKETTY/TS1_3A", api_key="yg2rjFAWcN1FgzoJ1naL")

c1_3 <- c1_3[1:8,]


plot_ly(c1_3, x=Date, y=`Europe + America`, name = "<b>Europe + America</b>", mode = 'lines+markers') %>%
  add_trace(x=Date, y = `Asia + Africa`, name = "<b>Asia + Africa</b>", connectgaps = TRUE) %>%
  add_trace(x=Date, y = `Per capita output`, name = "<b>World</b>", connectgaps = TRUE) %>%
  layout(title = '<b>Global inequality, 1700-2012: divergence then convergence?</b>',
         titlefont = titleVar,
         xaxis = list(title = '<b>Years</b>'),
         yaxis = list (title = '<b>Per capita GDP(% of World Average)</b>'))


###########################################


#Page 82 - Exchange rate and purchasing power parity: euro/dollar

c1_4 <- Quandl("PIKETTY/TS1_7", api_key="Kv-CaGGQMdMbP2q7nER2")



plot_ly(c1_4, x=Date, y= c1_4$`exchange rate euro/dollar`, name = "<b>Exchange rate euro/dollar</b>", 
                                                           mode = 'lines+markers',  
                                                           marker = list(symbol = 'square')) %>%
  add_trace(x=Date, y = c1_4$`purchasing power parity euro/dollar`, name = "<b>Purchasing power parity euro/dollar</b>", 
                                                                    mode = 'lines+markers',  
                                                                    marker = list(symbol = "triangle-up-open"),
                                                                    connectgaps = TRUE) %>%
  layout(title = '<b>Exchange rate and purchasing power parity: euro/dollar</b>',
         titlefont = titleVar,
         xaxis = list(title = '<b>Years</b>'),
         yaxis = list (title = '<b></b>'))


###########################################



#Page 85 Exchange rate and purchasing power parity: euro/yuan


#c1_4 <- Quandl("PIKETTY/TS1_7", api_key="Kv-CaGGQMdMbP2q7nER2")


plot_ly(c1_4, x=Date, y= c1_4$`exchange rate euro/yuan`, name = "<b>Exchange rate euro/yuan</b>", 
        mode = 'lines+markers',  
        marker = list(symbol = 'square')) %>%
  add_trace(x=Date, y = c1_4$`purchasing power parity euro/yuan`, name = "<b>Purchasing power parity euro/yuan</b>", 
            mode = 'lines+markers',  
            marker = list(symbol = "triangle-up-open"),
            connectgaps = TRUE) %>%
  layout(title = '<b>Exchange rate and purchasing power parity: euro/yuan</b>',
         titlefont = titleVar,
         xaxis = list(title = '<b>Years</b>'),
         yaxis = list (title = '<b></b>'))


###########################################


c2_1 <- Quandl("PIKETTY/TS2_2C", api_key="Kv-CaGGQMdMbP2q7nER2", start_date="1700-01-01")


c2_1 <- c2_1[5:11,]

Years <- c2_1$Year
y1_real<- c2_1$Europe
y2_real<- c2_1$Amrica
y3_real<- c2_1$Africa
y4_real<- c2_1$Asia


df= data.frame(Years, y1_real, y2_real, y3_real, y4_real)

df$y4_stck<- df$y4_real + df$y3_real + df$y2_real + df$y1_real
df$y3_stck<- df$y3_real + df$y2_real + df$y1_real
df$y2_stck<- df$y2_real + df$y1_real
df$y1_stck<- df$y1_real

p1<- plot_ly(df, x=Years, y=y1_stck, text=df$y1_real, hoverinfo='x+text+name', name="Europe", fill="tonexty") %>%
  layout(title = '<b>The growth of world population, 1700-2012</b>',
         titlefont = titleVar,
         xaxis = list(title = '<b>Years</b>'),
         yaxis = list (title = '<b>World population (million inhabitants)</b>'))


p2<- add_trace(p1, x=Years, y=y2_stck, text=df$y2_real, hoverinfo='x+text+name', name="America", fill="tonexty")
p3<- add_trace(p2, x=Years, y=y3_stck, text=df$y3_real, hoverinfo='x+text+name', name="Africa", fill="tonexty")
p4<- add_trace(p3, x=Years, y=y4_stck, text=df$y4_real, hoverinfo='x+text+name', name="Asia", fill="tonexty")

p5<- layout(yaxis=list(showgrid=FALSE, showline=FALSE))
p5


rm(df, p1, p2, p3, p4, y1_real, y2_real, y3_real, y4_real, Years)












