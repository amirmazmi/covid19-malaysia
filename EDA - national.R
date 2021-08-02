#-------------------------------------------------------------------------------
#
#   Exploratory data analysis for KKM data
#       Data: https://github.com/MoH-Malaysia/covid19-public
#       
#       NATIONAL level data
# 
#       Author  : Amir Azmi
#       Created : 25 July 2021
#       Updated : 31 July 2021
#
#-------------------------------------------------------------------------------
library(pacman)
p_load(dplyr)
p_load(tidyr)
p_load(plotly)
p_load(RColorBrewer)
p_load(stringr)
p_load(RcppRoll)
p_load(lubridate)
p_load(pals)

#-------------------------------------------------------------------------------
relib <- function(){ 
    source("funcs.R") 
    cat("\n\n")
}
relib()

#-------------------------------------------------------------------------------
# root directory
dir_path <- "git repo data/covid19-public/"

#-------------------------------------------------------------------------------
# events data
events_generic <- events_gen()

#-------------------------------------------------------------------------------
# NATIONAL level data
#-------------------------------------------------------------------------------

# daily cases
df_dailycases <- read.csv( file.path(dir_path, "epidemic", "cases_malaysia.csv"), stringsAsFactors=F) %>% 
    mutate( 
        date = ymd(date),
        dow = wday(date, label=T, abbr=F),
        cumulative = cumsum(cases_new)
    ) %>% 
    select( !contains("cluster"))

df_dailycases <- cbind( df_dailycases, bbands( df_dailycases$cases_new, 14) ) %>% 
    mutate( cumMA = cumsum( coalesce(MA14, 0)) + MA14*0,
            cumUpper = cumsum( coalesce(upper, 0)) + upper*0,
            cumLower = cumsum( coalesce(lower, 0)) + lower*0,
            diffcumband = cumUpper - cumLower
    )

ma_col <- grep( "MA[0-9]{1,3}", names(df_dailycases),  value=T)

plot( seq_along(df_dailycases$date), df_dailycases$cases_new, type="l")

plot_dailycases_simple <- df_dailycases %>% 
    plot_ly( x=~date, y=~cases_new, type="scatter", mode="lines")
plot_dailycases_simple

# plot of dailycases - bollinger band
tsplot_dailycases <- df_dailycases %>% 
    plot_ly( x=~date, y=~cases_new, name="Daily cases", 
             type = "scatter", mode = "lines",
             hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                    "<b>Cases:  %{y:,f}</b>",
                                    "<extra></extra>"),
             line = list( color="blue", width=2)) %>% 
    add_lines( y=df_dailycases[,ma_col], name="Moving Average", 
               line=list(color="red", width=1.2),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( y=~upper, name="Upper band", line=list(color="orange", width=1),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( y=~lower, name="Lower band", line=list(color="orange", width=1), 
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    layout( title = "Daily cases - Bollinger Band",
            xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A"),
            yaxis = list ( title = "Daily Reported cases"),
            shapes = events_generic[[1]], annotations = events_generic[[2]]
    )

tsplot_dailycases

#-------------------------------------------------------------------------------
# cumulative cases
tsplot_cumulative_dailycases <- gaga %>% 
    plot_ly( x=~date, y=~cumulative, name="Daily cases", 
             type = "scatter", mode = "lines",
             hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                    "<b>Cases:  %{y:,f}</b>",
                                    "<extra></extra>"),
             line = list( color="blue", width=2)) %>% 
    add_lines( x=~date, y=~cumMA, name="Cumulative of Moving Average", 
               line=list(color="red", width=1.2),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( x=~date, y=~cumUpper, name="Cumulative of Upperband", 
               line=list(color="orange", width=1.2),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( x=~date, y=~cumLower, name="Cumulative of Lowerband", 
               line=list(color="orange", width=1.2),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( x=~date, y=~diffcumband, name="Diff of Bollinger band", 
               line=list(color="green", width=1.2),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    layout( title = "Cumulative Daily Cases",
            xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A"),
            yaxis = list ( title = "Cumulative Daily Reported cases"),
            shapes = events_generic[[1]], annotations = events_generic[[2]]
    )           
tsplot_cumulative_dailycases



#-------------------------------------------------------------------------------
# daily deaths
df_deaths <- read.csv( file.path(dir_path, "epidemic", "deaths_malaysia.csv"), stringsAsFactors=F) %>% 
                mutate( date = ymd(date))
df_deaths <- cbind( df_deaths, bbands( df_deaths$deaths_new, 14) ) %>% 
    mutate( cumMA = cumsum( coalesce(MA14, 0)) + MA14*0,
            cumUpper = cumsum( coalesce(upper, 0)) + upper*0,
            cumLower = cumsum( coalesce(lower, 0)) + lower*0,
            diffcumband = cumUpper - cumLower
    )
ma_col_deaths <- grep( "MA[0-9]{1,3}", names(df_deaths),  value=T)


# plot of dailycases - bollinger band
tsplot_dailydeaths <- df_deaths %>% 
    plot_ly( x=~date, y=~deaths_new, name="Daily deaths", 
             type = "scatter", mode = "lines",
             hovertemplate = paste0("%{x|%e %b %Y  %A}", "<br>",
                                    "<b>Deaths:  %{y:,f}</b>",
                                    "<extra></extra>"),
             line = list( color="blue", width=2)) %>% 
    add_lines( y=df_deaths[,ma_col_deaths], name="Moving Average", 
               line=list(color="red", width=1.2),
               hovertemplate = paste0("%{x|%e %b %Y   %A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( y=~upper, name="Upper band", line=list(color="orange", width=1),
               hovertemplate = paste0("%{x|%e %b %Y  %A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( y=~lower, name="Lower band", line=list(color="orange", width=1), 
               hovertemplate = paste0("%{x|%e %b %Y  %A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    layout( title = "Daily deaths - Bollinger Band",
            xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A"),
            yaxis = list ( title = "Daily Reported Deaths"),
            shapes = events_generic[[1]], annotations = events_generic[[2]]
    )

tsplot_dailydeaths



#-------------------------------------------------------------------------------
# daily testing
df_testing <- read.csv( file.path( dir_path, "epidemic", "tests_malaysia.csv"), stringsAsFactors=F)
names(df_testing) <- names(df_testing) %>% str_replace_all("\\.","_")
df_testing <- df_testing %>% 
    mutate( total = rtk_ag + pcr,
            date = ymd(date),
            dow = wday( date, label=T, abbr=F))

df_testing <- cbind( df_testing, bbands( df_testing$total, 14))
test_ma_col <- grep( "MA", names(df_testing),  value=T)

tsplot_testing <- df_testing %>% 
    plot_ly( x=~date, y=~total, name="<b>Total test</b>",
             type="scatter", mode="lines",
             hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                    "<b>Total test:  %{y:,f}</b>",
                                    "<extra></extra>"),
             line = list( color="blue")) %>% 
    add_lines( y=~rtk_ag, name="<b>RTK Antigen</b>", 
               line=list(color="dodgerblue", width=2),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "RTK antigen: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( y=~pcr, name="<b>RT-PCR</b>", 
               line=list(color="aquamarine3", width=2),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "RT-PCR: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( y=df_testing[,test_ma_col], name="Moving Average", 
               line=list(color="red", width=1.2),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( y=~upper, name="Upper band", line=list(color="darkorange", width=1),
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    add_lines( y=~lower, name="Lower band", line=list(color="darkorange", width=1), 
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "Level: %{y:,f}</b>",
                                      "<extra></extra>")) %>% 
    layout( title = "Daily cases - Bollinger Band",
            xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A"),
            yaxis = list ( title = "Daily Reported cases"),
            shapes = events_generic[[1]], annotations = events_generic[[2]]
    )

tsplot_testing


#-------------------------------------------------------------------------------
# combine testing and daily cases data 
df_test_dailycases <- df_testing %>% 
    select( date, total, dow, ) %>% 
    full_join( df_dailycases %>% select(date, cases_new), by="date") %>% 
    mutate( ideal_test=cases_new/0.05)

# plot time series testing vs cases
plot_test_dailycases <- df_test_dailycases %>% 
    plot_ly( type="scatter", mode="lines",
             x=~date, y=~total, 
             name="Total test", line = list( color="red", width=1.5),
             hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                    "<b>Cases: %{y:,f}</b><br>",
                                    "<extra></extra>")) %>% 
    # add_lines( y=~total, name="Total test", line = list( color="blue"),
    #            hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
    #                                   "<b>Cases: %{y:,f}</b><br>",
    #                                   "<extra></extra>")) %>% 
    add_lines( y=~ideal_test, name="Testing for 5% ",
               line=list(color="blue", width=1.2, dash="dot"), #yaxis = "y1",
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "<b>Ideal test: %{y:,f}</b><br>",
                                      "<extra></extra>")) %>% 
    add_lines( y=~cases_new, name="Daily cases", 
               line=list(color="black", width=1.3), yaxis = "y2",
               hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                      "<b>Total tests:  %{y:,f}</b>", "<br>",
                                      "<extra></extra>")
    ) %>% 
    layout( title = "Daily Cases and Testing",
            xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A", 
                          domain = c(0,0.975)),
            yaxis = list ( title = "Testing", tickformat = ",f", range = c(-15000, 370000)),
            yaxis2 = list( title= "Daily Cases", tickformat = ",f", range = c(-1500, 37000), 
                           side="right", overlaying="y", zeroline=F),
            shapes = events_generic[[1]], annotations = events_generic[[2]]
    )

plot_test_dailycases


# correlation plot - !!! require positive rate !!!
plot( df_test_dailycases$cases_new, df_test_dailycases$total)

perc5_line <- max(df_test_dailycases$total, na.rm=T)

scatterplot_test_dailycases <- df_test_dailycases %>% filter( complete.cases(.)) %>% 
    plot_ly( x=~cases_new, y=~total,
             type="scatter", mode="markers",
             name="Positive Tests",
             marker=list(size=10, symbol=100),
             text = ~paste0( format(date, "%e %b %Y"), "<br>",
                             dow, "<br><b>",
                             "Total tests:  ", format( total, big.mark=","), "<br>", 
                             "Daily cases:  ", format( cases_new, big.mark=","), "</b>" ),
             hoverinfo = "text"
    ) %>% 
    add_segments( x = 0, xend=perc5_line*1.1*0.05,
                  y = 0, yend=perc5_line*1.1,
                  name="5% Positive",
                  mode="lines", 
                  marker=list(size=0.1),
                  line=list(width=1.5, dash="dot")
    ) %>% 
    layout( title = "Daily Cases and Testing <br> ( lines shows min tests required for 5% positive rate )",
            xaxis = list( title = "Daily new cases", tickformat = ",f"),
            yaxis = list ( title = "Total tests", tickformat = ",f")
    )

scatterplot_test_dailycases    



#-------------------------------------------------------------------------------
# MYSEJAHTERA data
#-------------------------------------------------------------------------------
df_national_checkin <- read.csv( file.path( dir_path, "mysejahtera","checkin_malaysia.csv"), stringsAsFactors=F) %>% 
    mutate( checkins_per_id = checkins/unique_ind,
            checkins_per_loc = checkins/unique_loc,
            date = ymd(date),
            dow = wday( date, label=T, abbr=F)) %>% 
    full_join( df_dailycases %>% select(date, cases_new), by="date") %>% 
    arrange(date)

max_iter <- 20
colns <- paste0( "lagcheckins", seq(max_iter))
for( k in seq(max_iter)){
    df_national_checkin[,colns[k]] <- df_national_checkin$checkins %>% lag(k)
    
}



# prelim plot - note magnitude difference in y vals
plot( df_national_checkin$date, df_national_checkin$cases_new, type="l", col="blue")
plot( df_national_checkin$date, df_national_checkin$checkins, type="l", col="red" )

#-------------------------------------------------------------------------------
# compare checkins with daily cases
line_cols <- df_national_checkin %>% names %>% unique %>% length %>% glasbey

tsplot_checkin <- df_national_checkin %>% 
    plot_ly(type="scatter", mode="lines") %>% 
    add_lines( x=~date, y=~checkins, 
               line=list(color="blue", width=2),
               name="Checkins", 
               type="scattergl",
               hovertemplate = paste0("%{x|%e %b %Y %A}", "<br>",
                                      "<b>Checkins: %{y:,f}</b><br>",
                                      "<extra></extra>")) %>% 
    add_lines( x=~date, y=~cases_new, name="Daily Cases", 
               line=list(color="red", width=2),
               yaxis="y2",
               type="scatter",
               hovertemplate = ~paste0("%{x|%e %b %Y %A}", "<br>",
                                       "<b>Cases: %{y:,f}</b><br>",
                                       "<extra></extra>")) 
# for( k in seq(max_iter)){
#     tsplot_checkin <- tsplot_checkin %>% 
#                         add_lines( x=~date, y=df_national_checkin[,colns[k]], name=colns[k],
#                                    line=list(width=2, color=line_cols[k]),
#                                    type="scatter",
#                                    # yaxis="y2",
#                                    hovertemplate = ~paste0(format(date, "%e %b %Y %A"), "<br>",
#                                                              "<b> Lead checkins: %{y:,f}</b><br>",
#                                                              "<extra></extra>"))
# }
tsplot_checkin <- tsplot_checkin %>% 
    layout( title = "Daily MySejahtera checkins and Daily Cases [ National ]",
            xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A",
                          domain=c(0,.975),
                          showspikes = T, showline = T, 
                          spikemode = "across+toaxis", spikesnap = "data",
                          spikedash = "solid", spikecolor = "grey", spikethickness = 0.8 ),
            yaxis = list( title = "Daily Checkins", tickformat = ",f"),
            yaxis2 = list( title= "Daily Cases", tickformat = ",f", 
                           side="right", overlaying="y", zeroline=F),
            shapes = events_generic[[1]], annotations = events_generic[[2]],
            hovermode = "x"
    )
tsplot_checkin


#-------------------------------------------------------------------------------
# correlation of daily checkins vs daily cases
plot( df_national_checkin$checkins, df_national_checkin$cases_new)

marker_shape <- schema(F)$traces$scatter$attributes$marker$symbol$values %>% 
    grep("open$",.,value=T)

scatter_checkins_dailycases <- df_national_checkin %>% filter( complete.cases(.)) %>% 
    plot_ly(x=~checkins, y=~cases_new, 
            marker=list(size=10, symbol=100),
            name="Relation", type="scatter", mode="markers",
            hovertemplate = ~paste0(format(date, "%e %b %Y %A"), "<br>",
                                    "<b>Checkins: %{x:,f}</b><br>",
                                    "<b>  Cases: %{y:,f}</b><br>",
                                    "<extra></extra>")) 
# for( k in seq(max_iter)){
#     scatter_checkins_dailycases <- scatter_checkins_dailycases %>% 
#                                     add_markers( x=df_national_checkin[,colns[k]], y=~cases_new, name=colns[k],
#                                                  marker=list(size=10, symbol=marker_shape[k]),
#                                                  hovertemplate = ~paste0(format(date, "%e %b %Y %A"), "<br>",
#                                                                          "<b>Checkins: %{x:,f}</b><br>",
#                                                                          "<b>  Cases: %{y:,f}</b><br>",
#                                                                          "<extra></extra>"))
# }
scatter_checkins_dailycases <- scatter_checkins_dailycases %>% 
    layout( title = "Daily MysSejahtera checkins and Daily Cases [ National ]",
            xaxis = list( title = "Daily Checkins", tickformat = ",f"),
            yaxis = list( title = "Daily Cases", tickformat = ",f")
    )
scatter_checkins_dailycases


#-------------------------------------------------------------------------------
# correlation of daily checkins vs daily cases based on events
#   remove school reopens as no trend    
events_x_school <- events_gen(startDate="2020-12-01", exclude="2021-03-01")
df_event_x_school <- events_x_school$data

scatter_events_checkins_dailycases <- plot_ly( type="scatter", mode="markers")
for( i in seq(nrow(df_event_x_school))){
    startDate <- df_event_x_school$date[[i]]
    endDate <- ifelse( i == nrow(df_event_x_school), "2021-12-31", df_event_x_school$date[[i+1]])
    cat("\n", startDate, endDate, "\n\n")        
    
    df_event_period<- df_national_checkin %>% filter( complete.cases(.)) %>% filter( date >= startDate & date < endDate) 
    print( df_event_period %>% dim)
    
    scatter_events_checkins_dailycases <- scatter_events_checkins_dailycases %>% 
        add_markers( data=df_event_period, x=~checkins, y=~cases_new, 
                     marker=list(size=10, symbol=0),
                     name=df_event_x_school$label[[i]] %>% str_replace_all("<b>|</b>|<br>", " "),
                     hovertemplate = ~paste0(format(date, "%e %b %Y %A"), "<br>",
                                             "<b>Checkins: %{x:,f}</b><br>",
                                             "<b>  Cases: %{y:,f}</b><br>",
                                             "<extra></extra>")) 
}
scatter_events_checkins_dailycases <- scatter_events_checkins_dailycases %>% 
    layout( title = "Events - Daily MysSejahtera checkins and Daily Cases [ National ]",
            xaxis = list( title = "Daily Checkins", tickformat = ",f"),
            yaxis = list( title = "Daily Cases", tickformat = ",f")
    )
scatter_events_checkins_dailycases


#-------------------------------------------------------------------------------
# compare average checkins per id with daily cases
#    plot( df_national_checkin$date, df_national_checkin$checkins_per_id, type="l")
tsplot_checkin_per_id <- df_national_checkin %>% 
    plot_ly(x=~date, y=~checkins_per_id, 
            line=list(color="blue", width=1),
            name="Checkins per ID", type="scatter", mode="lines",
            hovertemplate = paste0("%{x|%e %b %Y %A}", "<br>",
                                   "<b>Checkins per ID: %{y:,f}</b><br>",
                                   "<extra></extra>")) %>% 
    add_lines( y=~cases_new, name="Daily Cases", 
               line=list(color="red", width=2),
               yaxis="y2",
               hovertemplate = ~paste0("%{x|%e %b %Y %A}", "<br>",
                                       "<b>Cases: %{y:,f}</b><br>",
                                       "<extra></extra>")) %>% 
    layout( title = "Daily MysSejahtera checkins per unique ID",
            xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A", 
                          domain = c( 0, .975)),
            yaxis = list( title = "Checkins per ID", range=c(1.3, 3.4)),
            yaxis2 = list( title= "Daily Cases", side="right",
                           overlaying="y", tickformat = ",f"),
            shapes = events_generic[[1]], annotations = events_generic[[2]]
    )
tsplot_checkin_per_id



#-------------------------------------------------------------------------------
# compare average checkins per id with daily cases
#    plot( df_national_checkin$date, df_national_checkin$checkins_per_id, type="l")
tsplot_checkin_per_loc <- df_national_checkin %>% 
    plot_ly(x=~date, y=~checkins_per_loc, 
            line=list(color="blue", width=1),
            name="Checkins per ID", type="scatter", mode="lines",
            hovertemplate = paste0("%{x|%e %b %Y %A}", "<br>",
                                   "<b>Checkins per location: %{y:,f}</b><br>",
                                   "<extra></extra>")) %>% 
    add_lines( y=~cases_new, name="Daily Cases", 
               line=list(color="red", width=2),
               yaxis="y2",
               hovertemplate = ~paste0("%{x|%e %b %Y %A}", "<br>",
                                       "<b>Cases: %{y:,f}</b><br>",
                                       "<extra></extra>")) %>% 
    layout( title = "Daily MySejahtera checkins per unique location",
            xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A", 
                          domain = c( 0, .975)),
            yaxis = list( title = "Checkins per Location"),
            yaxis2 = list( title= "Daily Cases", side="right",
                           overlaying="y", tickformat = ",f"),
            shapes = events_generic[[1]], annotations = events_generic[[2]]
    )
tsplot_checkin_per_loc


#-------------------------------------------------------------------------------
# correlation of checkins per id to cases
#   !!! does not take into account of density and land size
#   !!! 3 different behaviour modes
plot( df_national_checkin$checkins_per_id, df_national_checkin$cases_new)

scatter_events_avg_checkins_dailycases <- plot_ly( type="scatter", mode="markers")
for( i in seq(nrow(df_event_x_school))){
    startDate <- df_event_x_school$date[[i]]
    endDate <- ifelse( i == nrow(df_event_x_school), "2021-12-31", df_event_x_school$date[[i+1]])
    df_event_period<- df_national_checkin %>% filter( complete.cases(.)) %>% filter( date >= startDate & date < endDate) 
    scatter_events_avg_checkins_dailycases <- scatter_events_avg_checkins_dailycases %>% 
        add_markers( data=df_event_period, x=~checkins_per_id, y=~cases_new, 
                     marker=list(size=10, symbol=100),
                     name=df_event_x_school$label[[i]] %>% str_replace_all("<b>|</b>|<br>", " "),
                     hovertemplate = ~paste0(format(date, "%e %b %Y %A"), "<br>",
                                             "<b>Checkins: %{x:,f}</b><br>",
                                             "<b>  Cases: %{y:,f}</b><br>",
                                             "<extra></extra>")) 
}
scatter_events_avg_checkins_dailycases <- scatter_events_avg_checkins_dailycases %>% 
    layout( title = "Events - Daily MysSejahtera checkins per ID and Daily Cases [ National ]",
            xaxis = list( title = "Daily Checkins per ID", tickformat = ".2f"),
            yaxis = list( title = "Daily Cases", tickformat = ",f")
    )
scatter_events_avg_checkins_dailycases



#-------------------------------------------------------------------------------
# correlation of checkins per location to cases
#   !!! does not take into account of density and land size
#   !!! 3 different behaviour modes

scatter_events_avg_checkinsloc_dailycases <- plot_ly( type="scatter", mode="markers")
for( i in seq(nrow(df_event_x_school))){
    startDate <- df_event_x_school$date[[i]]
    endDate <- ifelse( i == nrow(df_event_x_school), "2021-12-31", df_event_x_school$date[[i+1]])
    df_event_period<- df_national_checkin %>% filter( complete.cases(.)) %>% filter( date >= startDate & date < endDate) 
    cat("\n", startDate, endDate)
    scatter_events_avg_checkinsloc_dailycases <- scatter_events_avg_checkinsloc_dailycases %>% 
        add_markers( data=df_event_period, x=~checkins_per_loc, y=~cases_new, 
                     marker=list(size=10, symbol=100),
                     name=df_event_x_school$label[[i]] %>% str_replace_all("<b>|</b>|<br>", " "),
                     hovertemplate = ~paste0(format(date, "%e %b %Y %A"), "<br>",
                                             "<b>Check-ins: %{x:,f}</b><br>",
                                             "<b>  Cases: %{y:,f}</b><br>",
                                             "<extra></extra>")) 
}
scatter_events_avg_checkinsloc_dailycases <- scatter_events_avg_checkinsloc_dailycases %>% 
    layout( title = "Events - Daily MysSejahtera check-ins per location and Daily Cases [ National ]",
            xaxis = list( title = "Daily Checkins per ID", tickformat = ".1f"),
            yaxis = list( title = "Daily Cases", tickformat = ",f")
    )
scatter_events_avg_checkinsloc_dailycases



#-------------------------------------------------------------------------------
# Time of day MySejahtera checkins 

#   data only from Dec 2020
t_df_tod_checkins <- read.csv( file.path( dir_path, "mysejahtera", "checkin_malaysia_time.csv"), stringsAsFactors=F) %>% 
                        mutate( date = ymd(date))
colnames(t_df_tod_checkins) <- seq( ymd_hm("2021-01-01 00:30"), ymd_hm("2021-01-02 00:00"), by="30 mins") %>%
                                format("%H%M") %>% 
                                paste0( "ending", .) %>% 
                                c( "date",.)
date_coln <- t_df_tod_checkins$date %>% format( "%Y-%m-%d")
df_tod_checkins <- t_df_tod_checkins %>% 
                        select( !contains("date")) %>%
                        data.table::transpose(.) %>% 
                        as.data.frame() %>% 
                        mutate( timestamps = seq( ymd_hm("2021-01-01 00:30"), ymd_hm("2021-01-02 00:00"), by="30 mins") ) %>% 
                        relocate(timestamps) 
colnames(df_tod_checkins) <- c( "timestamps", date_coln)


# add mean and stdev for the first X days
#   indicates changes in behaviour
cols_sdmean_calc <- ncol(df_tod_checkins) - 60
cat( paste0("Start date: ", names(df_tod_checkins)[2], 
            "\n  End date: ", names(df_tod_checkins)[cols_sdmean_calc]), 
            "\n\n")

df_tod_checkins <- df_tod_checkins %>% 
    mutate( mean = rowMeans(.[,2:cols_sdmean_calc]),
            sd = apply(.[,2:cols_sdmean_calc],1,sd),
            lowerband= mean - sd,
            upperband= mean + sd
    )

# # sanity check
# # sum matches national daily checkins 
# lapply(df_tod_checkins[, 2:ncol(df_tod_checkins)], sum) %>% unlist
# lapply(df_tod_checkins[, 2:ncol(df_tod_checkins)], function(x){ (x>0) %>% all}) %>% unlist
# lapply(df_tod_checkins[, 2:ncol(df_tod_checkins)], function(x){ x %>% is.integer %>% all}) %>% unlist


#-------------------------------------------------------------------------------
# plot day on day
line_cols <- df_tod_checkins %>% ncol %>% (brewer.pal( 9, "YlGnBu")[3:7] %>%  colorRampPalette(colors=.) )
date_cols <- colnames(df_tod_checkins) %>% grep("-",.,value=T)

plot_DoD <- df_tod_checkins %>% plot_ly( type="scatter", mode="lines")
for( i in seq_along(date_cols)){
    # cat(dates,"\t")
    plot_DoD <- plot_DoD %>% 
        add_lines( x=~timestamps, y=df_tod_checkins[, date_cols[i]], 
                   data=df_tod_checkins,
                   name=str_replace_all(date_cols[i],"_","-"),
                   line=list( dash="dot", width=1.2, color=line_cols[i]),
                   hovertemplate = paste0("<b>", str_replace_all(date_cols[i],"_","-"), "</b>   ",
                                          "Checkins:  %{y:,f}    ",
                                          " Time: %{x|%H:%M}",
                                          "<extra></extra>"))
}
plot_DoD <- plot_DoD %>% 
    add_lines( x=~timestamps, y=~mean, name="Average", 
               line=list(width=2.5, color="red")) %>% 
    add_lines( x=~timestamps, y=~upperband, name="Upper Stdev", line=list(width=3, color="darkorange")) %>% 
    add_lines( x=~timestamps, y=~lowerband, name="Lower Stdev", line=list(width=3, color="darkorange")) %>% 
    layout( title = "Day on Day National MySejahtera Checkins [ 30 minutes granularity ]",
            xaxis = list( title = "Time of Day", tickformat = "%H:%M"),
            yaxis = list ( title = "Checkins", tickformat = ",f")
    )
plot_DoD


# !!! SHOULD PLOT LAST YEAR COMPARED TO THIS YEAR

#-------------------------------------------------------------------------------
# changes in checkins at 30 mins interval over time
#   peoples behavior at different times of the day
#       1pm peaks on Friday - lowest on Monday

events_tod <- events_gen("2020-11-25")
pal_night <- brewer.pal( 9, "GnBu")[4:9] %>%  colorRampPalette(colors=.)     # GnBu and OrRd - 22gap 26gap
pal_day <- brewer.pal( 9, "OrRd")[2:9] %>%  colorRampPalette(colors=.)   
ls_color <- list( daycount = 0, nightcount = 0, day = pal_day(26), night = pal_night(22))


ls_cols <- names(t_df_tod_checkins %>% select( contains("ending")))

tsplot_tod <- plot_ly( type="scatter", mode="lines") 
for( k in seq_along(ls_cols)){
    tod <- ls_cols[k] %>% str_replace("ending","") %>% str_sub(1,2)
    tod <- ls_cols[k] %>% str_replace("ending","") %>% str_sub(3,4) %>% paste0(tod,":", .) %>%  hm()
    pick_color <- if( tod >= hm("20:00") | tod < hm("07:00")){
                        ls_color[["nightcount"]] <- ls_color$nightcount + 1
                        ls_color$night[ls_color$nightcount]
                    }else{ 
                        ls_color[["daycount"]] <- ls_color$daycount + 1
                        ls_color$day[ls_color$daycount]
                    }
    cat( "\n", ls_cols[k], tod, "\t", pick_color, ls_color$daycount, ls_color$nightcount)
    tsplot_tod <- tsplot_tod %>% 
                        add_lines( data=t_df_tod_checkins, 
                                   x=~date, y=t_df_tod_checkins[,ls_cols[k]],
                                   line=list( width=1, color=pick_color),
                                   name=str_replace(ls_cols[k], "ending","") %>% paste0( " hrs"),
                                   text=str_replace(ls_cols[k], "ending",""),
                                   hovertemplate = ~paste0("%{x|%e %b %Y %A} ",# "<br>",
                                                           "<b>%{text} hrs  ", #"<br>",
                                                           "Checkins: %{y:,f}</b>",
                                                           "<extra></extra>")
                        )
}
tsplot_tod <- tsplot_tod %>% 
                    layout( title = "National MySejahtera Checkins every 30 mins window across dates",
                            xaxis = list( title = "Date", tickformat = "%e %b %Y<br>%A"),
                            yaxis = list ( title = "Checkins for 30mins interval", tickformat = ",f"),
                            shapes = events_tod[[1]], annotations = events_tod[[2]]
                    )
                        
tsplot_tod

#-------------------------------------------------------------------------------
# day of week
t_df_tod_checkins <- t_df_tod_checkins %>% 
                        mutate( dow = wday(date, label=T, abbr=F))

gaga <- t_df_tod_checkins %>% 
            pivot_longer(cols=contains("ending")) %>% 
            mutate( interval = name %>% str_replace("ending",""),
                    tod = paste("2021-01-01", interval) %>% ymd_hm,
                    # dow = as.character(dow)
                    ) %>% 
            as.data.frame


tsplot_dow <- plot_ly( type="scatter", mode="markers") 
        
ls_cols <- names(t_df_tod_checkins %>% select( contains("ending")))
for( k in seq_along(ls_cols)[10:20]){
    # print(k)
    tsplot_dow <- tsplot_dow %>% 
                    add_markers( data=gaga %>% filter( interval == "0030", dow == "Monday"), 
                                 x=~tod, y=~value,
                                 marker=list(size=~value/10^4, symbol=100),
                                 name=~interval # str_replace(ls_cols[k], "ending","") %>% paste0( " hrs")
                                 # text=str_replace(ls_cols[k], "ending",""),
                                 # hovertemplate = ~paste0("%{x|%e %b %Y %A} ",# "<br>",
                                 #                         "<b>%{text} hrs  ", #"<br>",
                                 #                         "Checkins: %{y:,f}</b>",
                                 #                         "<extra></extra>")
                    )
}
tsplot_dow <- tsplot_dow %>% 
                layout( title = "National MySejahtera Checkins every 30 mins window across dates",
                        xaxis = list( title = "Day of week"),
                        yaxis = list ( title = "Checkins for 30mins interval")
                )

tsplot_dow



#-------------------------------------------------------------------------------
# boxplot of time of day - grouped according to day of week
#   !!! need to split based on event dates and day of week

boxplot_dow <- gaga %>% 
                plot_ly( type="box", x=~tod, y=~value, color=~dow
                         ) %>% 
                layout( title = "Time of day checkins based on Day of Week",
                        xaxis = list( title = "Time of Day", tickformat = "%H:%M"),
                        yaxis = list( title = "Checkins", tickformat = ",f" ),
                        boxmode = "group"
                       )
boxplot_dow



#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------




