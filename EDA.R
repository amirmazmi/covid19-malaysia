#-------------------------------------------------------------------------------
#
#   Exploratory data analysis for KKM data
#       Data: https://github.com/MoH-Malaysia/covid19-public
#
#       Author  : Amir Azmi
#       Created : 25 July 2021
#       Updated : 29 July 2021
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
df_events <- read.csv( "events.csv", stringsAsFactors=F) %>% 
                mutate( event = paste0( "<b>", str_replace_all( event, "\\s", "<br>"), "</b>"))
vert_line <- apply(df_events,1, function(x){ vline(x[1],"grey", 1, "dash")})
ann_vert_line <- apply(df_events,1, 
                       function(x){ 
                           text_labels( xloc=x[1], yloc=0.95, label=x[2], 
                                        anchor="left", showarrow=F, font=14)
                        })

# # generate y location data
# df_events %>% 
#     mutate( yloc=seq(0.65,0.9,0.031)[1:nrow(df_events)],
#             event = str_replace_all(event, "<br>"," ")) %>%
#     write.csv("events.csv", quote=F, row.names=F)

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
plot_dailycases <- df_dailycases %>% 
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
                            shapes = vert_line, annotations = ann_vert_line
                    )

plot_dailycases


# cumulative cases
plot_cumulative_dailycases <- gaga %>% 
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
                                        shapes = vert_line, annotations = ann_vert_line
                                )           
plot_cumulative_dailycases

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

plot_testing <- df_testing %>% 
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
                            shapes = vert_line, annotations = ann_vert_line
                    )

plot_testing


#-------------------------------------------------------------------------------
# combine testing and daily cases data 
df_test_dailycases <- df_testing %>% 
                        select( date, total, dow, ) %>% 
                        full_join( df_dailycases %>% select(date, cases_new), by="date") %>% 
                        mutate( ideal_test=cases_new/0.05)

# plot time series
plot_test_dailycases <- df_test_dailycases %>% 
                            plot_ly( x=~date, y=~cases_new, name="Daily cases", line=list(color="green"),
                                     type="scatter", mode="lines",
                                     hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                                            "<b>Total tests:  %{y:,f}</b>", "<br>",
                                                            "<extra></extra>")
                                     ) %>% 
                            add_lines( y=~total, name="Total test", line = list( color="blue"),
                                       hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                                              "<b>Cases: %{y:,f}</b><br>",
                                                              "<extra></extra>")) %>% 
                            add_lines( y=~ideal_test, name="Testing for 5% ", line=list(color="red"),
                                       hovertemplate = paste0("%{x|%e %b %Y<br>%A}", "<br>",
                                                              "<b>Ideal test: %{y:,f}</b><br>",
                                                              "<extra></extra>")) %>% 
                            layout( title = "Daily Cases and Testing",
                                    xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A"),
                                    yaxis = list ( title = "Daily new cases and Daily testing"),
                                    shapes = vert_line, annotations = ann_vert_line
                            )

plot_test_dailycases


# correlation plot - !!! require positive rate !!!
plot( df_test_dailycases$cases_new, df_test_dailycases$total)

perc5_line <- max(df_test_dailycases$total, na.rm=T)

scatterplot_test_dailycases <- df_test_dailycases %>% filter( completes.cases(.)) %>% 
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
# daily cases by state
df_dailystate <- read.csv( file.path( dir_path, "epidemic", "cases_state.csv"), stringsAsFactors=F) %>% 
                    mutate( date = ymd(date))

# generate palette based on num of states
pal <- brewer.pal( 12, "Paired") %>%  colorRampPalette(colors=.)
# line_cols <- df_dailystate$state %>% unique %>% length %>% pal %>% rev 
line_cols <- df_dailystate$state %>% unique %>% length %>% glasbey


tsplot_dailystate_cases <- df_dailystate %>% #group_by(state) %>% 
                        plot_ly(x=~date, y=~cases_new, split=~state,
                                type="scatter", mode="lines",
                                line = list(color = line_cols),  #, shape = "vh"),  
                                text = ~state,
                                hovertemplate = paste0("%{x|%e %b %Y %A}", "  ",
                                                       "<b>%{text} </b> ",
                                                       "<b>Cases: %{y:,f}</b>",
                                                       "<extra></extra>")
                                ) %>%  
                        layout( title = "Daily cases based on States",
                                xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A"),
                                yaxis = list ( title = "Daily cases"),
                                shapes = vert_line, annotations = ann_vert_line
                        )

tsplot_dailystate_cases


#-------------------------------------------------------------------------------
# state population data
df_popsize <- read.csv( file.path( dir_path, "static", "population.csv"), stringsAsFactors=F)
names( df_popsize) <- names( df_popsize) %>% str_replace("state", "STATE")


#-------------------------------------------------------------------------------
# daily cases as percentage of population
# !!! not adjusted for land area and localized density
df_dailystate[,"percent_pop"] <- df_dailystate %>% 
                                    apply( 1, function(x){ 
                                                100 * (( as.integer(x["cases_new"])/df_popsize[which(df_popsize$STATE == x["state"]),"pop"]) %>% round( 6)) 
                                        }) 

tsplot_dailystate_percentpop <- df_dailystate %>%
                            plot_ly(x=~date, y=~percent_pop, split=~state,
                                    type="scatter", mode="lines",
                                    line = list( #shape = "vh",
                                                colors=line_cols, width=1),
                                    text=~state,
                                    hovertemplate = paste0("%{x|%e %b %Y %A}", "  ",
                                                           "<b>%{text} </b><br>",
                                                           "<b>%{y:.3f} % of population</b>",
                                                           "<extra></extra>")
                                    ) %>% 
                            layout( title = "Daily cases as percent population based on States",
                                    xaxis = list( title = "Date"),
                                    yaxis = list ( title = "Percent of population ( % )"),
                                    shapes = vert_line, annotations = ann_vert_line
                            )


tsplot_dailystate_percentpop


#-------------------------------------------------------------------------------
# cumulative daily cases per state
df_dailystate <- df_dailystate %>% 
                    group_by(state) %>% 
                    mutate( cumulative = cumsum(cases_new)) %>% 
                    # mutate( state = state %>% tolower %>% str_replace_all("\\.","") %>% str_replace_all("\\h","_")) %>% 
                    as.data.frame

tsplot_dailystate_cumulative <- df_dailystate %>%
                                plot_ly(x=~date, y=~cumulative, split=~state,
                                        type="scatter", mode="lines",
                                        line = list(#shape = "vh",
                                                    color= line_cols),
                                        text=~state,
                                        hovertemplate = paste0("%{x|%e %b %Y %A}", "  ",
                                                               "<b>%{text} </b><br> ",
                                                               "<b>Cumulative Cases:  %{y:,f}</b>",
                                                               "<extra></extra>")
                                ) %>% 
                                layout( title = "Cumulative daily cases as based on States",
                                        xaxis = list( title = "Date"),
                                        yaxis = list ( title = "Cumulative cases"),
                                        shapes = vert_line, annotations = ann_vert_line
                                )

tsplot_dailystate_cumulative


#-------------------------------------------------------------------------------
# cumulative cases as percentage of population
df_dailystate[,"cum_percent_pop"] <- df_dailystate %>% 
                                    apply( 1, function(x){ 
                                        100 * (( as.integer(x["cumulative"])/df_popsize[which(df_popsize$STATE == x["state"]),"pop"]) %>% round( 6)) 
                                    }) 

tsplot_dailystate_cumpercentpop <- df_dailystate %>%
                                plot_ly(x=~date, y=~cum_percent_pop, split=~state,
                                        type="scatter", mode="lines",
                                        line = list(#shape = "vh",
                                                    color = line_cols),
                                        text=~state,
                                        hovertemplate = paste0("%{x|%e %b %Y %A}  ",
                                                               "<b>%{text}<br> ",
                                                               "Cumulative infections: %{y:.3f} % of population</b>",
                                                               "<extra></extra>")
                                ) %>% 
                                layout( title = "Cumulative daily cases as percent population based on States",
                                        xaxis = list( title = "Date"),
                                        yaxis = list ( title = "Cumulative percent of population ( % )"),
                                        shapes = vert_line, annotations = ann_vert_line
                                )

tsplot_dailystate_cumpercentpop


#-------------------------------------------------------------------------------
# MYSEJAHTERA data
#-------------------------------------------------------------------------------
df_national_checkin <- read.csv( file.path( dir_path, "mysejahtera","checkin_malaysia.csv"), stringsAsFactors=F) %>% 
                        mutate( checkins_per_id = checkins/unique_ind,
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
                            shapes = vert_line, annotations = ann_vert_line,
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
ls_event <- df_events %>% 
                filter( date >= "2020-12-07", str_detect(event, "School", negate=T) ) %>%
                apply( 1, function(x){ as.list(x) })# %>% select(date) %>% pull #%>% c(., "2021-12-31")
scatter_events_checkins_dailycases <- plot_ly( type="scatter", mode="markers")
for( i in seq_along(ls_event)){
    startDate <- ls_event[[i]]$date
    endDate <- ifelse( i == length(ls_event), "2021-12-31", ls_event[[i+1]]$date)
    cat("\n", startDate, endDate, "\n\n")        
    df_event_period<- df_national_checkin %>% filter( complete.cases(.)) %>% filter( date >= startDate & date < endDate) 
    print( df_event_period %>% dim)
    scatter_events_checkins_dailycases <- scatter_events_checkins_dailycases %>% 
                                            add_markers( data=df_event_period, x=~checkins, y=~cases_new, 
                                                         marker=list(size=10, symbol=100),
                                                         name=ls_event[[i]]$event %>% str_replace_all("<b>|</b>|<br>", " "),
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
                                shapes = vert_line, annotations = ann_vert_line
                        )
tsplot_checkin_per_id


#-------------------------------------------------------------------------------
# correlation of checkins per id to cases
#   !!! does not take into account of density and land size
#   !!! 3 different behaviour modes
plot( df_national_checkin$checkins_per_id, df_national_checkin$cases_new)

scatter_events_avg_checkins_dailycases <- plot_ly( type="scatter", mode="markers")
for( i in seq_along(ls_event)){
    startDate <- ls_event[[i]]$date
    endDate <- ifelse( i == length(ls_event), "2021-12-31", ls_event[[i+1]]$date)
    df_event_period<- df_national_checkin %>% filter( complete.cases(.)) %>% filter( date >= startDate & date < endDate) 
    scatter_events_avg_checkins_dailycases <- scatter_events_avg_checkins_dailycases %>% 
        add_markers( data=df_event_period, x=~checkins_per_id, y=~cases_new, 
                     marker=list(size=10, symbol=100),
                     name=ls_event[[i]]$event %>% str_replace_all("<b>|</b>|<br>", " "),
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
# checkins at state level
df_state_checkin <- read.csv( file.path( dir_path, "mysejahtera","checkin_state.csv"), stringsAsFactors=F) %>% 
                        mutate( state = ifelse( state == "W.P. KualaLumpur", "W.P. Kuala Lumpur", state),
                                checkins_per_id = checkins/unique_ind,
                                checkins_per_loc = checkins/unique_loc,
                                date = ymd(date),
                                dow = wday( date, label=T, abbr=F)) %>% 
                        full_join( df_dailystate, by=c("date", "state")) %>% 
                        arrange(date)

# generate palette based on num of states
pal <- brewer.pal( 12, "Paired") %>%  colorRampPalette(colors=.)
# line_cols <- df_state_checkin$state %>% unique %>% length %>% pal %>% rev %>% sample
line_cols <- df_state_checkin$state %>% unique %>% length %>% glasbey()


#-------------------------------------------------------------------------------
# time series - checkins vs daily cases
tsplot_state_checkin <- df_state_checkin %>% filter( complete.cases(.)) %>% 
                            plot_ly(x=~date, y=~checkins, color=~state, 
                                    colors=line_cols,
                                    name=~paste0("Checkins - ", state),
                                    type="scatter", mode="lines",
                                    line=list(width=1.5),
                                    hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                             "Checkins:  %{y}<br></b>",
                                                             # "     Cases:     ", format(cases_new, big.mark=","), "</b><br>",
                                                             "<extra></extra>")
                                    # line = list(shape = "vh")
                            ) %>% 
                            add_lines( y=~cases_new, color=~state,
                                       colors=line_cols,
                                       name=~paste0("Daily Cases - ", state),
                                       line=list(width=1.2, dash="dot"),
                                       yaxis="y2",
                                       hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                                # "Checkins: ", format(checkins_per_id, digits=6) ,"<br>",
                                                                "Cases:  %{y:,f}", "</b><br>",
                                                                "<extra></extra>")) %>%
                            layout( title = "Daily checkins based on States",
                                    xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A",
                                                  domain = c(0,.975)),
                                    yaxis = list ( title = "Daily Checkins", tickformat=",f"),
                                    yaxis2 = list( title= "Daily Cases", side="right", overlaying="y"),
                                    shapes = vert_line, annotations = ann_vert_line
                            )

tsplot_state_checkin


#-------------------------------------------------------------------------------
# time series - daily unique ID vs daily cases
tsplot_state_dailyID <- df_state_checkin %>% filter( complete.cases(.)) %>% 
                        plot_ly(x=~date, y=~unique_ind, color=~state, 
                                colors=line_cols,
                                name=~paste0("Checkins - ", state),
                                type="scatter", mode="lines",
                                line=list(width=1.5),
                                hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                         "Daily Unique ID:  %{y}<br></b>",
                                                         # "     Cases:     ", format(cases_new, big.mark=","), "</b><br>",
                                                         "<extra></extra>")
                                # line = list(shape = "vh")
                        ) %>% 
                        add_lines( y=~cases_new, color=~state,
                                   colors=line_cols,
                                   name=~paste0("Daily Cases - ", state),
                                   line=list(width=1.2, dash="dot"),
                                   yaxis="y2",
                                   hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                            # "Checkins: ", format(checkins_per_id, digits=6) ,"<br>",
                                                            "Cases:  %{y:,f}", "</b><br>",
                                                            "<extra></extra>")) %>%
                        layout( title = "Daily Unique ID vs Daily Cases <br> based on States",
                                xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A",
                                              domain = c(0,.975)),
                                yaxis = list ( title = "Daily Unique ID", tickformat=",f"),
                                yaxis2 = list( title= "Daily Cases", side="right", overlaying="y"),
                                margin = list( t = 70),
                                shapes = vert_line, annotations = ann_vert_line
                        )

tsplot_state_dailyID

#-------------------------------------------------------------------------------
# time series - daily unique locations vs daily cases
#   pattern of weekly cycles 
tsplot_state_dailyloc <- df_state_checkin %>% filter( complete.cases(.)) %>% 
                            plot_ly(x=~date, y=~unique_loc, color=~state, 
                                    colors=line_cols,
                                    name=~paste0("Checkins - ", state),
                                    type="scatter", mode="lines",
                                    line=list(width=1.5),
                                    hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                             "Daily Unique ID:  %{y}<br></b>",
                                                             # "     Cases:     ", format(cases_new, big.mark=","), "</b><br>",
                                                             "<extra></extra>")
                                    # line = list(shape = "vh")
                            ) %>% 
                            add_lines( y=~cases_new, color=~state,
                                       colors=line_cols,
                                       name=~paste0("Cases - ", state),
                                       line=list(width=1.2, dash="dot"),
                                       yaxis="y2",
                                       hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                                # "Checkins: ", format(checkins_per_id, digits=6) ,"<br>",
                                                                "Cases:  %{y:,f}", "</b><br>",
                                                                "<extra></extra>")) %>%
                            layout( title = "Daily Unique Locations vs Daily Cases <br> based on States",
                                    xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A",
                                                  domain = c(0,.975)),
                                    yaxis = list ( title = "Daily Unique Locations", tickformat=",f"),
                                    yaxis2 = list( title= "Daily Cases", side="right", overlaying="y"),
                                    margin = list( t = 70),
                                    shapes = vert_line, annotations = ann_vert_line
                            )
tsplot_state_dailyloc

#-------------------------------------------------------------------------------
# time series - average checkins per id vs daily cases
tsplot_avg_state_checkin <- df_state_checkin %>% filter( complete.cases(.)) %>% 
                        plot_ly(x=~date, y=~checkins_per_id, color=~state, 
                                colors=line_cols,
                                name=~paste0("Checkins - ", state),
                                type="scatter", mode="lines",
                                hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                       "Checkins:  %{y}<br></b>",
                                                       # "     Cases:     ", format(cases_new, big.mark=","), "</b><br>",
                                                       "<extra></extra>")
                                # line = list(shape = "vh")
                                ) %>% 
                        add_lines( y=~cases_new, color=~state,
                                   colors=line_cols,
                                   name=~paste0("Cases - ", state),
                                   line=list(width=1, dash="dot"),
                                   yaxis="y2",
                                   hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                            # "Checkins: ", format(checkins_per_id, digits=6) ,"<br>",
                                                            "Cases:  %{y:,f}", "</b><br>",
                                                            "<extra></extra>")) %>%
                        layout( title = "<b>Average</b> checkins per ID based on States",
                                xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A"),
                                yaxis = list ( title = "Checkins per ID"),
                                yaxis2 = list( title= "Daily Cases", side="right", overlaying="y")
                        )

tsplot_avg_state_checkin


#-------------------------------------------------------------------------------
# !!! WIP !!!
# time series - average checkins per LOCATION vs daily cases
tsplot_avg_state_checkin_loc <- df_state_checkin %>% filter( complete.cases(.)) %>% 
                                    plot_ly(x=~date, y=~checkins_per_loc, color=~state, 
                                            colors=line_cols,
                                            name=~paste0("Checkins - ", state),
                                            type="scatter", mode="lines",
                                            hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                                     "Checkins:  %{y}<br></b>",
                                                                     # "     Cases:     ", format(cases_new, big.mark=","), "</b><br>",
                                                                     "<extra></extra>")
                                            # line = list(shape = "vh")
                                    ) %>% 
                                    add_lines( y=~cases_new, color=~state,
                                               colors=line_cols,
                                               name=~paste0("Cases - ", state),
                                               line=list(width=1, dash="dot"),
                                               yaxis="y2",
                                               hovertemplate = ~paste0( state, "  %{x|%e %b %Y - %A}", "<br><b>",
                                                                        # "Checkins: ", format(checkins_per_id, digits=6) ,"<br>",
                                                                        "Cases:  %{y:,f}", "</b><br>",
                                                                        "<extra></extra>")) %>%
                                    layout( title = "<b>Average</b> checkins per location based on States",
                                            xaxis = list( title = "Date", tickformat = "%e-%b-%Y<br>%A",
                                                          domain = c(0,.975)),
                                            yaxis = list ( title = "Checkins per ID"),
                                            yaxis2 = list( title= "Daily Cases", side="right", overlaying="y")
                                    )
tsplot_avg_state_checkin_loc


#-------------------------------------------------------------------------------
marker_shape <- schema(F)$traces$scatter$attributes$marker$symbol$values %>% 
                    grep("open$",.,value=T) %>% 
                    head( df_state_checkin$state %>% unique %>% length)

# correlation of daily checkins to daily cases at state level
scatterplot_checkins_state <- df_state_checkin %>% filter( complete.cases(.)) %>% 
                                    plot_ly( x=~checkins, y=~cases_new,
                                             type="scatter", mode="markers",
                                             name=~state, symbol=~state, symbols=marker_shape,
                                             marker=list(size=10),
                                             text = ~paste0( "<b>", state,"</b>  ", format( date, "%e-%b-%Y  %A"), "<br><b>",
                                                             "Checkins:    ", format(checkins, digits=6, big.mark=","), "<br>", 
                                                             "Daily cases: ", format(cases_new, big.mark=","), "</b><br>"),
                                             hoverinfo = "text") %>% 
                                    layout( title = "Daily Checkins vs Daily Cases",
                                            xaxis = list( title = "Daily Checkins", tickformat=",f"),
                                            yaxis = list ( title = "Daily new cases", tickformat = ",f"),
                                            shapes = vert_line, annotations = ann_vert_line
                                    )

scatterplot_checkins_state

#-------------------------------------------------------------------------------
# correlation of daily cases and average checkins per id
#   !!! does not take into account of density and land size
scatterplot_avg_checkins_state <- df_state_checkin %>% filter( complete.cases(.)) %>% 
                                    plot_ly( x=~checkins_per_id, y=~cases_new, colors=line_cols,
                                             type="scatter", mode="markers",
                                             name=~state, symbol=~state, symbols=marker_shape,
                                             marker=list(size=10),
                                             text = ~paste0( "<b>", state,"</b>  ", format( date, "%e-%b-%Y  %A"), "<br><b>",
                                                             "Checkins:    ", format(checkins_per_id, digits=6), "<br>", 
                                                             "Daily cases: ", format(cases_new, big.mark=","), "</b><br>"),
                                             hoverinfo = "text") %>% 
                                    layout( title = "Average Checkins per ID vs Daily Cases",
                                            xaxis = list( title = "Checkins per ID"),
                                            yaxis = list ( title = "Daily new cases", tickformat = ",f", hoverformat=",f")
                                    )
scatterplot_avg_checkins_state

#-------------------------------------------------------------------------------
# correlation of daily cases and average checkins per location
#   !!! does not take into account of density and land size
scatterplot_avg_checkins_state_loc <- df_state_checkin %>% filter( complete.cases(.)) %>% 
                                        plot_ly( x=~checkins_per_loc, y=~cases_new, colors=line_cols,
                                                 type="scatter", mode="markers",
                                                 name=~state, symbol=~state, symbols=marker_shape,
                                                 marker=list(size=10),
                                                 text = ~paste0( "<b>", state,"</b>  ", format( date, "%e-%b-%Y  %A"), "<br><b>",
                                                                 "Checkins:    ", format(checkins_per_id, digits=6), "<br>", 
                                                                 "Daily cases: ", format(cases_new, big.mark=","), "</b><br>"),
                                                 hoverinfo = "text") %>% 
                                        layout( title = "Average Daily Checkins per Location vs Daily Cases",
                                                xaxis = list( title = "Checkins per Location", tickformat = ".2f"),
                                                yaxis = list ( title = "Daily new cases", tickformat = ",f", hoverformat=",f")
                                        )

scatterplot_avg_checkins_state_loc


#-------------------------------------------------------------------------------
#   !!! TEST !!!
# correlation of cumulative percent population infected and average checkins per location
#   !!! does not take into account of density and land size
scatterplot_avg_checkins_state_TEST <- df_state_checkin %>% filter( complete.cases(.)) %>% 
                                        plot_ly( x=~cum_percent_pop, y=~checkins_per_id, colors=line_cols,
                                                 type="scatter", mode="markers",
                                                 name=~state, symbol=~state, symbols=marker_shape,
                                                 marker=list(size=10),
                                                 text = ~paste0( "<b>", state,"</b>  ", format( date, "%e-%b-%Y  %A"), "<br><b>",
                                                                 "Checkins:    ", format(checkins_per_id, digits=6), "<br>", 
                                                                 "cum percent pop: ", format(cum_percent_pop, big.mark=","), "</b><br>"),
                                                 hoverinfo = "text") %>% 
                                        layout( title = "TEST",
                                                xaxis = list( title = "cum percent pop", tickformat = ".3f"),
                                                yaxis = list ( title = "checkins per id", tickformat = ".2f")
                                        )

scatterplot_avg_checkins_state_TEST



#-------------------------------------------------------------------------------
# state by state review of correlation between checks in and daily cases
#   some have similar pattern of negative correlatio
#   some have no change in behaviour
#   overall still expect 2 checkins per ID for every state

ls_state <- df_state_checkin %>% select(state) %>% unique %>% pull
targ_state <- ls_state[12]
df_ind_state_checkin <- df_state_checkin %>% filter( complete.cases(.), state == targ_state)

scatterplot_avg_checkins_state_indiv <- plot_ly( type="scatter", mode="markers")
for( i in seq_along(ls_event)){
    startDate <- ls_event[[i]]$date
    endDate <- ifelse( i == length(ls_event), "2021-12-31", ls_event[[i+1]]$date)
    df_event_period_ind_state<- df_ind_state_checkin %>% filter( complete.cases(.)) %>% filter( date >= startDate & date < endDate) 
    scatterplot_avg_checkins_state_indiv <- scatterplot_avg_checkins_state_indiv %>% 
                                        add_markers( x=~checkins_per_id, y=~cases_new, colors=line_cols,
                                                     data = df_event_period_ind_state,
                                                     name=ls_event[[i]]$event %>% str_replace_all("<br>|<b>|</b>", " "), 
                                                     marker=list(size=10, symbol=100),
                                                     text = ~paste0( "<b>", state,"</b>  ", format( date, "%e-%b-%Y  %A"), "<br><b>",
                                                                     "Checkins:    ", format(checkins_per_id, digits=6), "<br>", 
                                                                     "Daily cases: ", format(cases_new, big.mark=","), "</b><br>"),
                                                     hoverinfo = "text")
}
scatterplot_avg_checkins_state_indiv <- scatterplot_avg_checkins_state_indiv %>% 
                                    layout( title = paste0( "Average Checkins per ID vs Daily Cases <br>[ ", targ_state, " ]"),
                                            xaxis = list( title = "Checkins per ID"),
                                            yaxis = list ( title = "Daily new cases", tickformat = ",f", hoverformat=",f")
                                    )
scatterplot_avg_checkins_state_indiv


# !!!! lagged of checkins vs cases - scatterplot




#-------------------------------------------------------------------------------
# Time of day MySejahtera checkins 
#   data only from Dec 2020
df_tod_checkins <- read.csv( file.path( dir_path, "mysejahtera", "checkin_malaysia_time.csv"), stringsAsFactors=F) %>% t %>% as.data.frame 
colnames(df_tod_checkins) <- df_tod_checkins["date",] %>% lapply( function(x){ strftime(x, "%Y_%m_%d")})
df_tod_checkins <- df_tod_checkins[2:nrow(df_tod_checkins),] %>% 
                        # leave timestamps as time since cannot plot discrete values
                        mutate( timestamps = seq( ymd_hm("2021-01-01 00:00"), ymd_hm("2021-01-01 23:30"), by="30 mins") ) %>% 
                        relocate(timestamps) 
df_tod_checkins[,2:ncol(df_tod_checkins)] <- lapply(df_tod_checkins[,2:ncol(df_tod_checkins) ], type.convert)

# add mean and stdev for the first X days
#   indicates changes in behaviour
cols_sdmean_calc <- ncol(df_tod_checkins) - 60
cat( paste0("Start date: ", names(df_tod_checkins)[2], 
            "\n  End date: ", names(df_tod_checkins)[cols_sdmean_calc]))
                                    
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
pal <- brewer.pal( 9, "YlGnBu")[2:8] %>%  colorRampPalette(colors=.)
line_cols <- df_tod_checkins %>% ncol %>% pal   #colorRampPalette(colors=c("chartreuse", "green4"))(.)
date_cols <- colnames(df_tod_checkins) %>% grep("_",.,value=T)

plot_DoD <- plot_ly( type="scatter", mode="lines")
for( i in seq_along(date_cols)){
    # cat(dates,"\t")
    plot_DoD <- plot_DoD %>% 
                    add_lines( x=~timestamps, y=df_tod_checkins[, date_cols[i]], 
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


# !!! SHOULD PLOT LAST YEAR

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------




