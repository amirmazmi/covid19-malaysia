#-------------------------------------------------------------------------------
#
#   Exploratory data analysis for KKM data
#       Data: https://github.com/MoH-Malaysia/covid19-public
#       
#       STATE level data
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
# STATE level data
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
            shapes = events_generic[[1]], annotations = events_generic[[2]]
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
            shapes = events_generic[[1]], annotations = events_generic[[2]]
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
            shapes = events_generic[[1]], annotations = events_generic[[2]]
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
            shapes = events_generic[[1]], annotations = events_generic[[2]]
    )

tsplot_dailystate_cumpercentpop



#-------------------------------------------------------------------------------
# MYSEJAHTERA data
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
            shapes = events_generic[[1]], annotations = events_generic[[2]]
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
            shapes = events_generic[[1]], annotations = events_generic[[2]]
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
            shapes = events_generic[[1]], annotations = events_generic[[2]]
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
            shapes = events_generic[[1]], annotations = events_generic[[2]]
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
# 1. percent pop vs checkins per id
# 2. percent pop vs checkins per loc
# 3. cumulative perc pop vs checkins per id
# 4. cumulative perc pop vs checkins per loc
# shift checkins vs daily state cases


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


