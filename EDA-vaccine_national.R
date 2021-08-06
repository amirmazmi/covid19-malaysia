#-------------------------------------------------------------------------------
#
#   Exploratory data analysis for CITF data
#       Data: https://github.com/CITF-Malaysia/citf-public
#       
#       Author  : Amir Azmi
#       Created : 6 August 2021
#       Updated : 6 August 2021
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
dir_vaccine <- "repo citf-public"
dir_path <- "repo covid19-public"

#-------------------------------------------------------------------------------
# events data
events_generic <- events_gen(startDate="2021-02-15", exclude="2021-03-01")

#-------------------------------------------------------------------------------
# National Vaccine data
#-------------------------------------------------------------------------------
# population 
#   does not represent migrants (especially foreign)
df_pop <- read.csv( file.path( dir_vaccine, "static", "population.csv"),
                    stringsAsFactors=F)

# registrations
df_reg <- read.csv( file.path( dir_vaccine, "registration", "vaxreg_malaysia.csv"),
                    stringsAsFactors=F) %>% 
            select( -state) %>% 
            mutate( date = ymd(date))
names(df_reg) <- names(df_reg) %>% str_replace("total", "total_reg")

# deaths 
df_deaths <- read.csv( file.path(dir_path, "epidemic", "deaths_malaysia.csv"),
                       stringsAsFactors=T) %>% 
                mutate( date = ymd(date))

# cases
df_cases <- read.csv( file.path( dir_path, "epidemic","cases_malaysia.csv"),
                      stringsAsFactors=T) %>% 
                select( date, cases_new) %>% 
                mutate( date = ymd(date))


# vaccination
df_vax <- read.csv( file.path( dir_vaccine, "vaccination", "vax_malaysia.csv"),
                    stringsAsFactors=F) %>% 
            mutate( date = ymd(date),
                    total_MA14 = roll_meanr( total_daily, 14),
                    dose1_MA14 = roll_meanr( dose1_daily, 14),
                    dose2_MA14 = roll_meanr( dose2_daily, 14)) %>% 
            full_join( df_reg %>% select(date, total_reg), by="date") %>% 
            full_join( df_deaths, by="date") %>% 
            full_join( df_cases, by="date") %>% 
            mutate( dose1_pop_perc = (( dose1_cumul/df_pop$pop[1]) * 100) %>% round(2),
                    dose2_pop_perc = (( dose2_cumul/df_pop$pop[1]) * 100) %>% round(2),
                    total_reg_pop_perc = ((total_reg/df_pop$pop[1]) * 100) %>% round(2),
                    rem_dose1 = total_reg - dose1_cumul,
                    rem_dose2 = total_reg - dose2_cumul,
                    rem_total = total_reg*2 - total_cumul,
                    rem_dose1_pop_perc = (( rem_dose1/df_pop$pop[1]) * 100) %>% round(2),
                    rem_dose2_pop_perc = (( rem_dose2/df_pop$pop[1]) * 100) %>% round(2),
                    deaths_cumul = cumsum( deaths_new),
                    cases_cumul = cumsum( cases_new)
            ) %>% 
            arrange( date)
            # mutate( d1_cum = cumsum( dose1_daily),
            #         d2_cum = cumsum( dose2_daily),
            #         tot_cum = cumsum( total_daily),
            #         d1_match = d1_cum == dose1_cumul,
            #         d2_match = d2_cum == dose2_cumul,
            #         tot_match = tot_cum == total_cumul
            #         )





#-------------------------------------------------------------------------------
# plot vaccination daily
tsplot_vax <- df_vax %>% 
                plot_ly( type = "scatter", mode = "lines") %>% 
                add_lines( x=~date, y=~dose1_daily, name="Dose 1", 
                           hovertemplate = paste0("%{x|%e %b %Y  %A}", "<br>",
                                                "<b>Dose 1:  %{y:,f}</b>",
                                                "<extra></extra>"),
                           line = list( color="blue", width=1.5, dash="dot")) %>% 
                add_lines( x=~date, y=~dose1_MA14, name="Dose 1 (MA14)", 
                           hovertemplate = paste0("%{x|%e %b %Y  %A}", "<br>",
                                                  "<b>Moving Average:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="blue", width=2)) %>% 
                add_lines( x=~date, y=~dose2_daily, name="Dose 2", 
                           hovertemplate = paste0("%{x|%e %b %Y  %A}", "<br>",
                                                  "<b>Dose 2:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="green", width=1.5, dash="dot")) %>%
                add_lines( x=~date, y=~dose2_MA14, name="Dose 2 (MA14)", 
                           hovertemplate = paste0("%{x|%e %b %Y  %A}", "<br>",
                                                  "<b>Moving Average:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="green", width=2)) %>% 
                add_lines( x=~date, y=~total_daily, name="Total Daily Doses<br>Administered", 
                           hovertemplate = paste0("%{x|%e %b %Y  %A}", "<br>",
                                                  "<b>Total Daily Doses Administered:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="orange", width=1.4, dash="dot")) %>% 
                add_lines( x=~date, y=~total_MA14, name="Total Daily (MA14)", 
                           hovertemplate = paste0("%{x|%e %b %Y  %A}", "<br>",
                                                  "<b>Moving average:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="orange", width=2)) %>% 
                layout( title = "Daily Vaccinations",
                        xaxis = list( title = "<b>Date</b>", tickformat = "%e-%b-%Y<br>%A"),
                        yaxis = list ( title = "<b>Doses</b>", tickformat = ",f"),
                        shapes = events_generic[[1]], annotations = events_generic[[2]]
                )
tsplot_vax



#-------------------------------------------------------------------------------
# cumulative plot vaccination daily
cumplot_vax <- df_vax %>% 
                plot_ly( type = "scatter", mode = "lines") %>% 
                add_lines( x=~date, y=~total_reg, name="Registered for<br>Vaccination", 
                           hovertemplate = paste0("<b>Registered:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="red", width=2)) %>%
                add_lines( x=~date, y=~dose1_cumul, name="Dose 1", 
                           hovertemplate = paste0("<b>Dose 1:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="blue", width=1.5)) %>% 
                add_lines( x=~date, y=~rem_dose1, name="Remaining for<br>Dose 1", 
                           hovertemplate = paste0("<b>Remaining for Dose 1:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="blue", width=1.5, dash="dot")) %>% 
                add_lines( x=~date, y=~dose2_cumul, name="Dose 2", 
                           hovertemplate = paste0("<b>Dose 2:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="green", width=1.5)) %>% 
                add_lines( x=~date, y=~rem_dose2, name="Remaining for<br>Dose 2", 
                           hovertemplate = paste0("<b>Remaining for Dose 2:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="green", width=1.5, dash="dot")) %>% 
                add_lines( x=~date, y=~total_cumul, name="Total Doses", 
                           hovertemplate = paste0("<b>Total doses:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="orange", width=2)) %>%
                add_lines( x=~date, y=~rem_total, name="Remaining Total doses", 
                           hovertemplate = paste0("<b>Remaining total doses:  %{y:,f}</b>",
                                                  "<extra></extra>"),
                           line = list( color="orange", width=1.5, dash="dot")) %>% 
                layout( title = "Cumulative Vaccinations",
                        xaxis = list( title = "<b>Date</b>", tickformat = "%e-%b-%Y<br>%A",
                                      showspikes = T, showline = T, 
                                      spikemode = "across+toaxis", spikesnap = "data",
                                      spikedash = "solid", spikecolor = "grey", spikethickness = 0.8 ),
                        yaxis = list ( title = "<b>Doses</b>", tickformat = ",f"),
                        shapes = events_generic[[1]], annotations = events_generic[[2]],
                        hovermode = "x"
                )
cumplot_vax


#-------------------------------------------------------------------------------
# cumulative plot vaccination daily - % population
cumplot_perc_pop_vax <- df_vax %>% 
                        plot_ly( type = "scatter", mode = "lines") %>% 
                        add_lines( x=~date, y=~total_reg_pop_perc, name="Registered for<br>Vaccination", 
                                   hovertemplate = paste0("<b>Registered:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="red", width=2)) %>%
                        add_lines( x=~date, y=~dose1_pop_perc, name="Dose 1", 
                                   hovertemplate = paste0("<b>Dose 1:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="blue", width=1.5)) %>% 
                        add_lines( x=~date, y=~rem_dose1_pop_perc, name="Remaining<br>for Dose 1", 
                                   hovertemplate = paste0("<b>Remaining for Dose 1:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="blue", width=1.5, dash="dot")) %>% 
                        add_lines( x=~date, y=~dose2_pop_perc, name="Dose 2", 
                                   hovertemplate = paste0("<b>Dose 2:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="green", width=1.5)) %>%
                        add_lines( x=~date, y=~rem_dose2_pop_perc, name="Remaining<br>for Dose 2", 
                                   hovertemplate = paste0("<b>Remaining for Dose 2:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="green", width=1.5, dash="dot")) %>% 
                        layout( title = "Cumulative Vaccinations<br>as a Percentage of Population",
                                xaxis = list( title = "<b>Date</b>", tickformat = "%e-%b-%Y<br>%A",
                                              showspikes = T, showline = T, 
                                              spikemode = "across", spikesnap = "data",
                                              spikedash = "solid", spikecolor = "grey", spikethickness = 0.8),
                                yaxis = list ( title = "<b>Percent of Population  [ % ]</b>", tickformat = ",f%"),
                                shapes = events_generic[[1]], annotations = events_generic[[2]],
                                hovermode = "x"
                        )
cumplot_perc_pop_vax


#-------------------------------------------------------------------------------
# vaccination vs cases
#   time series plot
tsplot_vax_cases <- df_vax %>% 
                        plot_ly( type="scatter", mode="lines") %>% 
                        add_lines( x=~date, y=~deaths_new, name="Deaths",
                                   yaxis="y2",
                                   hovertemplate = paste0("<b>Deaths:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="red", width=2)) %>%
                        add_lines( x=~date, y=~dose1_daily, name="Dose 1",
                                   hovertemplate = paste0("<b>Dose 1:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="blue", width=2, dash="dot")) %>%
                        add_lines( x=~date, y=~dose2_daily, name="Dose 2",
                                   hovertemplate = paste0("<b>Dose 2:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="green", width=2, dash="dot")) %>%
                        layout( title = "Vaccination and deaths",
                                xaxis = list( title = "<b>Date</b>", tickformat = "%e-%b-%Y<br>%A",
                                              showspikes = T, showline = T, 
                                              spikemode = "across", spikesnap = "data",
                                              spikedash = "solid", spikecolor = "grey", spikethickness = 0.8),
                                yaxis = list ( title = "<b>Doses</b>", tickformat = ",f%"),
                                yaxis2 = list ( title = "<b>Deaths</b>", tickformat = ",f%",
                                                # range = c(-50, 500),
                                                side="right", overlaying="y", zeroline=F ),
                                shapes = events_generic[[1]], annotations = events_generic[[2]],
                                hovermode = "x" )
tsplot_vax_cases 


# correlation
scatter_vax_cases <- df_vax %>% select( date, dose1_daily, dose2_daily, cases_new) %>% 
                        filter( complete.cases(.)) %>% 
                        plot_ly( type="scatter", mode="markers") %>% 
                        add_markers( x=~dose1_daily, y=~cases_new, name="Dose 1",
                                     hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                            "<b>Dose 1:  %{x:,f}</b>", "<br>",
                                                            "<b>Cases:      %{y:,f}</b>",
                                                            "<extra></extra>"),
                                     marker = list( color="blue", size=10, symbol=100)) %>%
                        add_markers( x=~dose2_daily, y=~cases_new, name="Dose 2",
                                     hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                            "<b>Dose 2:  %{x:,f}</b>", "<br>",
                                                            "<b>Cases:      %{y:,f}</b>",
                                                            "<extra></extra>"),
                                     marker = list( color="green", symbol=100, size=10)) %>%
                        layout( title = "Vaccinations vs Cases",
                                xaxis = list( title = "<b>Daily Doses</b>", tickformat = ",f"),
                                yaxis = list( title = "<b>Daily Cases</b>", tickformat = ",f")
                        )
scatter_vax_cases


#   correlation - cumulative? 
#   not meaningful as the data is not really correlated despite the trend  
scatter_vax_cases <- df_vax %>% select( date, dose1_cumul, dose2_cumul, cases_cumul) %>% 
                        filter( complete.cases(.)) %>%
                        plot_ly( type="scatter", mode="markers") %>% 
                        add_markers( x=~dose1_cumul, y=~cases_cumul, name="Dose 1",
                                     hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                            "<b>Dose 1:  %{x:,f}</b>", "<br>",
                                                            "<b>Deaths:      %{y:,f}</b>",
                                                            "<extra></extra>"),
                                     marker = list( color="blue", size=10, symbol=100)) %>%
                        add_markers( x=~dose2_cumul, y=~cases_cumul, name="Dose 2",
                                     hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                            "<b>Dose 2:  %{x:,f}</b>", "<br>",
                                                            "<b>Deaths:      %{y:,f}</b>",
                                                            "<extra></extra>"),
                                     marker = list( color="green", symbol=100, size=10)) %>%
                        layout( title = "Cumulative Vaccinations vs Cumulative Cases",
                                xaxis = list( title = "<b>Cumulative Doses</b>", tickformat = ",f"),
                                yaxis = list( title = "<b>Cumulative Cases</b>", tickformat = ",f")
                        )
scatter_vax_cases



#-------------------------------------------------------------------------------
# vaccination vs deaths
#   time series plot - increaseing deaths even with increasing vaccinations
tsplot_vax_deaths <- df_vax %>% 
                        plot_ly( type="scatter", mode="lines") %>% 
                        add_lines( x=~date, y=~deaths_new, name="Deaths",
                                   yaxis="y2",
                                   hovertemplate = paste0("<b>Deaths:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="red", width=2)) %>%
                        add_lines( x=~date, y=~dose1_daily, name="Dose 1",
                                   hovertemplate = paste0("<b>Dose 1:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="blue", width=2, dash="dot")) %>%
                        add_lines( x=~date, y=~dose2_daily, name="Dose 2",
                                   hovertemplate = paste0("<b>Dose 2:  %{y:.2f} %</b>",
                                                          "<extra></extra>"),
                                   line = list( color="green", width=2, dash="dot")) %>%
                        layout( title = "Vaccination and deaths",
                                xaxis = list( title = "<b>Date</b>", tickformat = "%e-%b-%Y<br>%A",
                                              showspikes = T, showline = T, 
                                              spikemode = "across", spikesnap = "data",
                                              spikedash = "solid", spikecolor = "grey", spikethickness = 0.8),
                                yaxis = list ( title = "<b>Doses</b>", tickformat = ",f%"),
                                yaxis2 = list ( title = "<b>Deaths</b>", tickformat = ",f%",
                                                # range = c(-50, 500),
                                                side="right", overlaying="y", zeroline=F),
                                shapes = events_generic[[1]], annotations = events_generic[[2]],
                                hovermode = "x"
                        )
tsplot_vax_deaths


#   correlation 
scatter_vax_deaths <- df_vax %>% filter( complete.cases(.)) %>% arrange(date) %>% 
                        plot_ly( type="scatter", mode="markers") %>% 
                        add_markers( x=~dose1_daily, y=~deaths_new, name="Dose 1",
                                        hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                                "<b>Dose 1:  %{x:,f}</b>", "<br>",
                                                                "<b>Deaths:      %{y:,f}</b>",
                                                                "<extra></extra>"),
                                        marker = list( color="blue", size=10, symbol=100)) %>%
                        add_markers( x=~dose2_daily, y=~deaths_new, name="Dose 2",
                                     hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                            "<b>Dose 2:  %{x:,f}</b>", "<br>",
                                                            "<b>Deaths:      %{y:,f}</b>",
                                                            "<extra></extra>"),
                                     marker = list( color="green", symbol=100, size=10)) %>%
                        layout( title = "Vaccinations vs Deaths",
                                xaxis = list( title = "<b>Daily Doses</b>", tickformat = ",f"),
                                yaxis = list( title = "<b>Daily Deaths</b>", tickformat = ",f")
                        )
scatter_vax_deaths


# correlation - cumulative
#   not meaningful as the data is not really correlated despite the trend  
scatter_vax_deaths <- df_vax %>% select( date, dose1_cumul, dose2_cumul, deaths_cumul) %>% 
                        filter( complete.cases(.)) %>%
                        plot_ly( type="scatter", mode="markers") %>% 
                        add_markers( x=~dose1_cumul, y=~deaths_cumul, name="Dose 1",
                                     hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                            "<b>Dose 1:  %{x:,f}</b>", "<br>",
                                                            "<b>Deaths:      %{y:,f}</b>",
                                                            "<extra></extra>"),
                                     marker = list( color="blue", size=10, symbol=100)) %>%
                        add_markers( x=~dose2_cumul, y=~deaths_cumul, name="Dose 2",
                                     hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                            "<b>Dose 2:  %{x:,f}</b>", "<br>",
                                                            "<b>Deaths:      %{y:,f}</b>",
                                                            "<extra></extra>"),
                                     marker = list( color="green", symbol=100, size=10)) %>%
                        layout( title = "Cumulative Vaccinations vs Cumulative Deaths",
                                xaxis = list( title = "<b>Cumulative Doses</b>", tickformat = ",f",
                                              range = c(-10^5, 1.8*10^6) ),
                                yaxis = list( title = "<b>Cumulative Deaths</b>", tickformat = ",f",
                                              range = c(-500, 4.5*10^3) )
                        )
scatter_vax_deaths

#-------------------------------------------------------------------------------
# exponential mapping for size - TESTING
max_death <- df_vax$deaths_new %>% max( na.rm=T)

scatter_vax_death_cases <- df_vax %>% select( date, dose1_cumul, dose2_cumul, cases_cumul, deaths_new) %>% 
                            mutate( deaths_size = 10 + (deaths_new/70) ^ 3,
                                    deaths_size2 = 6 * exp( deaths_new / (max_death/3) )
                                                    # 65 * (deaths_new/max_death)
                            ) %>%   
                            filter( complete.cases(.)) %>%
                            plot_ly( type="scatter", mode="markers") %>% 
                            add_markers( x=~dose1_cumul, y=~cases_cumul, name="Dose 1",
                                         hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                                "Deaths: ", format( deaths_new, big.mark=","), "<br>",
                                                                "<b>Cases:  %{x:,f}</b>", "<br>",
                                                                "<b>Dose 1:      %{y:,f}</b>",
                                                                "<extra></extra>"),
                                         marker = list( color="blue", symbol=200, 
                                                        size=~deaths_size2, opacity=0.3,
                                                        line = list( color="blue"))
                            ) %>%
                            add_markers( x=~dose2_cumul, y=~cases_cumul, name="Dose 2",
                                         hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                                "Deaths: ", format( deaths_new, big.mark=","), "<br>",
                                                                "<b>Cases:  %{x:,f}</b>", "<br>",
                                                                "<b>Dose 2:      %{y:,f}</b>",
                                                                "<extra></extra>"),
                                         marker = list( color="green", symbol=200, 
                                                        size=~deaths_size2, opacity=0.3,
                                                        line = list( color="green"))
                            ) %>%
                            layout( title = "Cumulative Vaccinations vs Cumulative Cases<br>Deaths as circle size",
                                    yaxis = list( title = "<b>Cumulative Cases</b>", tickformat = ",f"),
                                    xaxis = list( title = "<b>Cumulative Doses</b>", tickformat = ",f")
                            )
scatter_vax_death_cases

#-------------------------------------------------------------------------------
# daily cases vs vaccinations ( death as size)
#   split by event
#   by virtue of increasing cases, the recent events are top-right
scatter_vax_death_cases_daily <- df_vax %>% 
                    select( date, dose1_daily, dose2_daily, total_daily, dose1_cumul,
                            cases_new, cases_cumul, deaths_new) %>% 
                    filter( complete.cases(.)) %>%
                    plot_ly( type="scatter", mode="markers") %>% 
                    add_markers( x=~cases_new, y=~dose1_daily, name="Dose 1",
                                 marker = list( color="blue", symbol=200, 
                                                size=~deaths_new/4, opacity=0.3,
                                                line = list( color="blue")),
                                 hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                        "<b>Cases:     %{x:,f}</b>", "<br>",
                                                        "<b>Dose 1:  %{y:,f}</b>",
                                                        "<extra></extra>") ) %>%
                    add_markers( x=~cases_new, y=~dose2_daily, name="Dose 2",
                                 mode="marker+lines",
                                 marker = list( color="green", symbol=200, 
                                                size=~deaths_new/4, opacity=0.3,
                                                line = list( color="green")),
                                 hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                        "<b>Cases:     %{x:,f}</b>", "<br>",
                                                        "<b>Dose 2:  %{y:,f}</b>",
                                                        "<extra></extra>")) %>%
                    add_markers( x=~cases_new, y=~total_daily, name="Total Dose",
                                 marker = list( color="orange", symbol=200, 
                                                size=~deaths_new/4, opacity=0.3,
                                                line = list( color="orange")),
                                 hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                        "<b>Cases:        %{x:,f}</b>", "<br>",
                                                        "<b>Total Dose: %{y:,f}</b>",
                                                        "<extra></extra>") ) %>%
                    layout( title = "Daily Vaccinations vs Daily Cases<br>Deaths as circle size",
                            xaxis = list( title = "<b>Daily Cases</b>", tickformat = ",f"),
                            yaxis = list( title = "<b>Daily Doses</b>", tickformat = ",f")
                    )
scatter_vax_death_cases_daily



# CUMULATIVE cases vs DAILY vaccinations
#   split by event
scatter_vax_death_cumulcases_daily <- df_vax %>% 
                    select( date, dose1_daily, dose2_daily, total_daily, dose1_cumul,
                            cases_new, cases_cumul, deaths_new) %>% 
                    filter( complete.cases(.)) %>%
                    plot_ly( type="scatter", mode="lines+markers") %>% 
                    add_markers( x=~cases_cumul, y=~dose1_daily, name="Dose 1",
                                 marker = list( color="blue", symbol=200, 
                                                size=~deaths_new/4, opacity=0.3,
                                                line = list( color="blue")),
                                 line = list( color="blue", width=0.2),
                                 hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                        "<b>Daily Dose 1:   %{y:,f}</b>", "<br>",
                                                        "<b>Daily Deaths:", str_pad( format( deaths_new, big.mark=","),13, "left") , "</b><br>",
                                                        "<b>Cumul. Cases: %{x:,f}</b>", "<br>",
                                                        "Daily Cases:", str_pad(format( cases_new, big.mark=","), 14, "left"), "<br>",
                                                        "<extra></extra>") ) %>%
                    add_markers( x=~cases_cumul, y=~dose2_daily, name="Dose 2",
                                 mode="marker+lines",
                                 marker = list( color="green", symbol=200, 
                                                size=~deaths_new/4, opacity=0.3,
                                                line = list( color="green")),
                                 line = list( color="green", width=0.2),
                                 hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                        "<b>Cases:     %{x:,f}</b>", "<br>",
                                                        "<b>Dose 2:  %{y:,f}</b>",
                                                        "<extra></extra>")) %>%
                    add_markers( x=~cases_cumul, y=~total_daily, name="Total Dose",
                                 marker = list( color="orange", symbol=200, 
                                                size=~deaths_new/4, opacity=0.3,
                                                line = list( color="orange")),
                                 line = list( color="orange", width=0.2),
                                 hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                        "<b>Cases:        %{x:,f}</b>", "<br>",
                                                        "<b>Total Dose: %{y:,f}</b>",
                                                        "<extra></extra>") ) %>%
                    layout( title = "Cumulative Vaccinations vs Cumulative Cases<br>Deaths as circle size",
                            xaxis = list( title = "<b>Cumulative Cases</b>", tickformat = ",f"),
                            yaxis = list( title = "<b>Daily Doses</b>", tickformat = ",f")
                    )
scatter_vax_death_cumulcases_daily



# cumulative cases vs cumulative vaccinations ( death as size)
#   split by event
scatter_vax_death_cases <- df_vax %>% select( date, dose1_cumul, dose2_cumul, cases_cumul, deaths_new) %>% 
                            filter( complete.cases(.)) %>%
                            plot_ly( type="scatter", mode="markers") %>% 
                            add_markers( x=~cases_cumul, y=~dose1_cumul, name="Dose 1",
                                         hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                                "<b>Cases:  %{x:,f}</b>", "<br>",
                                                                "<b>Dose 1:      %{y:,f}</b>",
                                                                "<extra></extra>"),
                                         marker = list( color="blue", symbol=200, 
                                                        size=~deaths_new/4, opacity=0.3,
                                                        line = list( color="blue"))
                                         ) %>%
                            add_markers( x=~cases_cumul, y=~dose2_cumul, name="Dose 2",
                                         hovertemplate=~paste0( format(date, "%e %b %Y  %A"), "<br>",
                                                                "<b>Cases:  %{x:,f}</b>", "<br>",
                                                                "<b>Dose 2:      %{y:,f}</b>",
                                                                "<extra></extra>"),
                                         marker = list( color="green", symbol=200, 
                                                        size=~deaths_new/4, opacity=0.3,
                                                        line = list( color="green"))
                                                        ) %>%
                            layout( title = "Cumulative Vaccinations vs Cumulative Cases<br>Deaths as circle size",
                                    xaxis = list( title = "<b>Cumulative Cases</b>", tickformat = ",f"),
                                    yaxis = list( title = "<b>Cumulative Doses</b>", tickformat = ",f")
                            )
scatter_vax_death_cases




#-------------------------------------------------------------------------------




