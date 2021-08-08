#-------------------------------------------------------------------------------
#
#   Functions for exploratory data analysis for KKM data
#       https://github.com/MoH-Malaysia/covid19-public
#
#       Author  : Amir Azmi
#       Created : 25 July 2021
#       Updated : 31 July 2021
#
#-------------------------------------------------------------------------------
cat( str_pad( "\n[+] funcs.R ", 40, "right", "-"),
     " loaded [", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "]")

#-------------------------------------------------------------------------------

# generate events shape and annotations for plotting 
#   filter based on given date
events_gen <- function(startDate = 0, exclude=list(), yloc = NA, anchor = "left", 
                       a_x = 10, a_y = -20, showarrow = F, arrowhead = 2, 
                       miny = 0.7, maxy = 0.95, font = 14, debug = F){
    # events data
    df_events <- read.csv( "events.csv", stringsAsFactors=F, comment.char="#") %>% 
                    mutate( label = paste0( "<b>", str_replace_all( event, "\\s", "<br>"), "</b>")) %>% 
                    filter( date >= startDate) %>% 
                    filter( !date %in% exclude)
    vert_line <- apply(df_events,1, function(x){ vline(x["date"],"grey", 1, "dash")})
    ann_vert_line <- apply(df_events,1, 
                           function(x){ 
                               text_labels( xloc=x["date"], yloc=ifelse( is.na(yloc), x["yloc"], NA ),
                                            label=x["label"],
                                            anchor=anchor, showarrow=showarrow, font=font,
                                            a_x = a_x, a_y = a_y, arrowhead = 2,
                                            miny = miny, maxy = maxy, debug = debug
                                            )
                           })
    return( list( shapes = vert_line, annotations = ann_vert_line, data = df_events))
}
# # generate y location data
# df_events %>% 
#     mutate( yloc=seq(0.65,0.9,0.031)[1:nrow(df_events)],
#             event = str_replace_all(event, "<br>"," ")) %>%
#     write.csv("events.csv", quote=F, row.names=F)


vline <- function(x = 0, color = "red", width = 2, dash="solid") {
    list(
        type = "line", 
        y0 = 0, 
        y1 = 1, 
        yref = "paper",
        x0 = x, 
        x1 = x, 
        line = list(color = color, width = width, dash = dash)
    )
}

text_labels <- function( xloc = NA, yloc = NA, x_ref = "x", y_ref = "paper", label, anchor = "right", 
                         a_x = 10, a_y = -20, showarrow = T, arrowhead = 2, 
                         miny = 0.7, maxy = 0.9, font = 12, debug = F){
    if( debug ) print( c(xloc,yloc))
    list(
        x = xloc,
        y = if( is.na(yloc)) seq(miny,maxy,0.025) else yloc, # %>% sample(10,replace=T)
        text = label,
        xref = x_ref,
        yref = y_ref,
        showarrow = showarrow,
        arrowhead = arrowhead,
        xanchor = anchor,
        ax = a_x,
        ay = a_y,
        font = list( size = font)
    )
}


hline <- function(y = 0, color = "blue", width = 2, dash="solid") {
    list(
        type = "line", 
        x0 = 0, 
        x1 = 1, 
        xref = "paper",
        y0 = y, 
        y1 = y, 
        line = list(color = color, width = width, dash = dash)
    )
}

# Bollinger Bands
bbands <- function( data_col, wind=11){
    result <- roll_meanr( data_col, n=wind)
    roll_stdev <- roll_sdr(data_col, n=wind) 
    
    result <- cbind.data.frame( result, roll_stdev)
    names(result) <- paste0( c("MA", "bband"),wind)
    
    result["upper"] <- result[1] + result[2] * 2
    result["lower"] <- result[1] - result[2] * 2
    
    colnames <- grep(names(result), pattern="bband",invert=T)
    
    return(result %>% select( all_of(colnames)) )
}


