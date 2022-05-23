#Function to create the plots present in the exploratory_analysis


#violinBoxPlot====
#This function creates a violin/box plot
#ARGUMENTS:
#   data        entry data
#   xvar        variable, as factor, to be used in the x axis, default is "Type"
#   zvar        varaible to be used in fill and colour var, if no input defaults to xvar
#   colours     colours to be used in by the zvar variables, in NA uses colours from rainbow()
#   legend      TRUE/FALSE if legend is used
#   ycut        value in y axis where to cut, if no value cut acording to quantcut value
#   quantcut    default value to cut if no ycut value is given
#   yvar        value to be used in the y axis, default is LoS (Time.stay colunmn)

violinBoxPlot <- function(data, xvar="Type", zvar=NA, colours=NA, legend=T, ycut=NULL, quantcut=0.99, yvar="Time.stay"){
  #Check if dplyr and ggplot are in use
  if (!require(dplyr)) {library(dplyr)}
  if (!require(ggplot2)) {library(ggplot2)}
  
  #Check if xvar is char
  stopifnot("xvar must be a character."=is.character(xvar))
  
  #Set up zvar, if empty, and check if is vhar
  if (is.na(zvar)) {
    zvar <- xvar
    legend <- F}
  stopifnot("zvar must be a character."=is.character(zvar))
  
  #if empty populate colours vector
  if (is.na(colours[1])){
    colours <- rainbow(length(levels(data[[zvar]])))
  } else {
    #Check if colours is correct length
    stopifnot("colours do not match set levels."= length(levels(data[[zvar]]))==length(colours))}
  
  #Check if legend is logical type
  stopifnot("'legend' must be type logical." = is.logical(legend))
  
  #Check and quantcut values
  stopifnot("quantcut must be numeric and between 0 and 1"=is.numeric(quantcut) & quantcut>=0 & quantcut<=1)

  #Cut at fix value in Y
  if (is.null(ycut)) {
    #standad scale is in the quantile 99%
    scale <- quantile(data[[yvar]],quantcut)[[1]]
  } else if (is.numeric(ycut) & ycut>0){
    scale <- ycut
  } else {
    stop("Invalid ycut value. Use positive numeric values!")
  }

  
  #ViolinBoxPlot====
  plot <- ggplot(data,aes(x=.data[[xvar]], y=.data[[yvar]], group = interaction(.data[[xvar]], .data[[zvar]]))) +
    geom_violin(aes(fill = .data[[zvar]], colour = .data[[zvar]]),lwd=0.1, position = position_dodge(width = 0.9)) +
    stat_boxplot(outlier.alpha = 0, coef = 0, color = "grey20", width = .3, alpha=0.6, size=0.6,position = position_dodge(width = 0.9)) +
    stat_boxplot(geom = "errorbar",           color = "grey20", width = .3, alpha=1,   size=0.6,position = position_dodge(width = 0.9)) +
    stat_summary(aes(x=.data[[xvar]],y=.data[[yvar]]),
                 fun = "mean", geom = "point", shape = 23, size = 3,lwd=0.6, fill = "white",position = position_dodge(width = 0.9)) +
    theme_bw() +
    theme(legend.title=element_blank(),
          #legend.position = c(0.94, 0.92),
          text = element_text(size = 12),
          plot.background = element_rect(color = "white"),
          legend.background = element_rect(fill = "transparent")) +
    scale_fill_manual(values = colours) +
    scale_colour_manual(values = colours) +
    labs(y="Lenght of stay(days)", x=NULL)+
    #coord_flip() +
    scale_y_continuous(expand = c(0,0), limits = c(0, scale)) 
  
  # remove legend
  if(legend == F){plot <- plot + theme(legend.position = "null")}
  
  #Return plot
  plot
}




#Linegraph====
#Function to creates linegraphs. If second variable(yvar2) is suplied is traced in the right y axis
#ARGUMENTS:
#   data        tible to be used
#   yvar        variable to be used in y axis (
#   yvar2       second y variable
#   colours     vector with colours to be used 
#   coef        coeficient to be multiply by the yvar2 values and adjust right y axis, default is 10^max(yvar) / 10^max(yvar2)
#   legend      if the legend is shown
#   xvar        standard os 'data', variable to be used in x axys

linePlot  <- function(data, xvar, yvar, yvar2 = NA, colours = c("#56B4E9","tomato1"), grapPerc = F, twoYaxis = F,coeff = NA, yaxis_name = NA){
  #Check if dplyr, ggplot and lubridate are in use
  if (!require(dplyr)) {library(dplyr)}
  if (!require(ggplot2)) {library(ggplot2)}
  if (!require(lubridate)) {library(lubridate)}
  
  #Check if yvar is char
  stopifnot("yvar must be a character."=is.character(yvar))
  
  if (is.na(yaxis_name)) {
    yaxis_name <- yvar
  }
  ymax <- yvar
  
  #Linegraph====
  plot <- ggplot(data,aes(x = .data[[xvar]]), size = 0.5, alpha = 0.8) +
    geom_line(aes(y = .data[[yvar]], group = 1, colour = yvar)) +
    
    labs(x=NULL, y= yaxis_name, title = NULL)  +
    theme_bw() +
    theme(legend.position = "none",
          plot.background = element_rect(color = "white"),
          text = element_text(size = 12),
          axis.text.x = element_text(angle = 30, vjust = 0.6, hjust=0.5))
  
    if (!is.na(yvar2)) {
      plot <- plot + geom_line(aes(y = .data[[yvar2]],group = 2, colour = yvar2)) +
        theme(legend.position = "top") +
        scale_color_manual(name = '',
                           values = c("yvar" = colours[1], "yvar2" = colours[2])
                           )+
        scale_color_discrete(name = '',
                             c("yvar" = yvar, "yvar2" = yvar2))
      
      if(max(data[[yvar]]) < max(data[[yvar2]])){ymax <- yvar2}
    }

  
  #If is a percentage
  if (grapPerc == T) {
    plot <- plot + scale_y_continuous(name=yaxis_name,
                                      labels=scales::percent,
                                      expand = c(0, 0),
                                      limits = c(0, max(data[[ymax]])*1.2),
                                      breaks = seq(0, max(data[[ymax]])*1.25, by= 0.05))
  }
  
  
  #if xvar is a date scale it
  #if (is.Date(as.Date(xvar[1], format = "%Y-%m"))) {plot <- plot + scale_x_date(date_breaks="1 month", date_labels="%Y-%m")}
  
  plot
}



#Two axis Linegraph====
#Function to creates linegraphs with two axis.
#ARGUMENTS:
#   data        tible to be used
#   yvar        variable to be used in the left  y axis
#   yvar2       variable to be used in the right y axis
#   colours     vector with colours to be used 
#   coef        coeficient to be multiply by the yvar2 values and adjust right y axis, default is 10^max(yvar) / 10^max(yvar2)
#   legend      if the legend is shown
#   xvar        standard os 'data', variable to be used in x axys
  
linePlot_2var <- function(data, xvar, yvar, yvar2 , colours = c("#56B4E9","tomato1"), grapPerc = F, coeff = NA, yaxis_name = NA){
  #Check if dplyr, ggplot and lubridate are in use
  if (!require(dplyr)) {library(dplyr)}
  if (!require(ggplot2)) {library(ggplot2)}
  if (!require(lubridate)) {library(lubridate)}

  #Checks 
  #Check if yvar is char
  stopifnot("yvar must be a character."=is.character(yvar))
  stopifnot("yvar2 must be a character."=is.character(yvar2))
  
  #set axis name
  if (is.na(yaxis_name)) {
    yaxis_name <- yvar
  }
  
  if (is.na(coeff)) {
    # coeff <- 10^(nchar(max(data[[yvar]]))) / 10^(nchar(max(data[[yvar2]])))
    coeff <- 10^(nchar(max(data[[yvar]])) - nchar(max(data[[yvar2]])))
  }
  
  #Linegraph====
  plot <- ggplot(data,aes(x = .data[[xvar]])) +
            geom_line(aes(y = .data[[yvar]]       , group = 1), colour = colours[1], size = 0.5, alpha = 0.8) +
            geom_line(aes(y = .data[[yvar2]]*coeff, group = 2), colour = colours[2], size = 0.5, alpha = 0.8) +
    #For seting the right y axis
    scale_y_continuous(name=yvar,
                       #expand = c(0, 0),
                       #limits = c(0,max(data[[yvar]])),
                       #breaks = seq(0, max(data[[yvar]])*1.10, by= 10^(nchar(max(data[[yvar]]))-1)),
                       sec.axis = sec_axis( trans=~./coeff,
                                            name=yvar2,
                                            #breaks=seq(0, max(data[[yvar2]])*1.10, by= 10^(nchar(max(data[[yvar2]]))-1))
                                                       )
                       ) +
  theme_bw() + 
  theme(axis.title.y = element_text(colour = colours[1], size=15),
        axis.title.y.right = element_text(colour = colours[2], size=15)) +
    
    labs(x=NULL, y= yaxis_name, title = NULL)  +
    
    theme(legend.position = "none",
          plot.background = element_rect(color = "white"),
          text = element_text(size = 12),
          axis.text.x = element_text(angle = 30, vjust = 0.6, hjust=0.5))
  
  
  #if xvar is a date scale it
  #if (is.Date(as.Date(xvar[1], format = "%Y-%m"))) {plot <- plot + scale_x_date(date_breaks="1 month", date_labels="%Y-%m")}
  
  
  #If is a percentage replace legend
  if (grapPerc == T) {
    plot <- plot + scale_y_continuous(name=yvar,
                                      labels=scales::percent,
                                      sec.axis = sec_axis(trans=~./coeff,
                                                          name=yvar2,
                                                          labels=scales::percent))

  }
  
  plot
}


#Histograms====
#Function taht creates a histogram, and respective table, from entry_data
#ARGUMENTS:
#   data          entry data
#   xvar          variable to be used in x axis
#   zvar          variable to be used as the colour variable
#   filter_var    vector with two elements, the column of the data to be filtered and the level(s) to filter
#   colours       vector with colours to be used, is NA colours will be assign acording to the number of levels in xvar
#   yvar_name     name to be given to the number of entries of the unfiltered data, will be used in the y axis if no filter occurs
#   plot_title    title to be used in the graph
#   plot_caption  caption of the percentages,default is "*Percentage of groups entries"

histPlot <- function(data,xvar,zvar, filter_var=NA, colours = NA, yvar_name = "Entries",
                     plot_title=NULL, plot_caption=paste("*Percentage of total entries by",zvar),
                     calc_Perc = T, Perc_func = NA){
  
  #Check if dplyr and ggplot are in use
  if (!require(dplyr)) {library(dplyr)}
  if (!require(ggplot2)) {library(ggplot2)}
  
  #Check if xvar and zvar are chars
  stopifnot("xvar must be a character."=is.character(xvar))
  stopifnot("zvar must be a character."=is.character(zvar))
  
  #Load default Perc_func if NA and needed
  if (calc_Perc == T & is.na(Perc_func)){
    source("functions/Perc_total_byZVar.R")
    Perc_func <- Perc_total_byType
  }
  
  #Check if any colours are provided
  if (is.na(colours[1])){
    colours <- rainbow(length(levels(data[[zvar]])))
  } else {
    #Check if colours is correct length
    stopifnot("colours do not match set levels."= length(levels(data[[zvar]]))==length(colours))}
  
    #TABLE======================================================================
      df_Vars <- as.data.frame(table(data[[xvar]],data[[zvar]]))
      colnames(df_Vars) <- c(xvar,zvar,yvar_name)
      #Only calculates if filter is suplied
      if (!is.na(filter_var[1])) {
        #teste filter_var
        #Check if xvar and zvar are chars
        stopifnot("filter_var[1] must be a character."=is.character(filter_var[1]))
        stopifnot("filter_var[2] must be a character."=is.character(filter_var[2]))
        #and if exist in the data
        stopifnot("filter_var[1] must be a column name."= any(filter_var[1] == colnames(data)))
        stopifnot("filter_var[2] must be a levels of the filter_var[1] column."=any(filter_var[2] == levels(data[[filter_var[1]]])))
        
        data_filt <- filter(data,data[[filter_var[1]]] == filter_var[2])
        df_Vars_filt <- as.data.frame(table(data_filt[[xvar]],data_filt[[zvar]]))
        colnames(df_Vars_filt) <- c(xvar,zvar,filter_var[2])
        # This merge chenges order, as such the 3 column will contain the y values, no mater if filtered or not
        df_Vars <- merge(x = df_Vars_filt, y = df_Vars, by = c(xvar,zvar))
      }
      
      #Only if calc_Perc = T
      if (calc_Perc == T) {
        #df_Vars$Perc <- df_Vars[[filter_var[2]]] / df_Vars[[yvar_name]]
        df_Vars$Perc <- Perc_func(df_Vars)    
      }
      
    #PLOT=======================================================================
      # The data is not filtered and no percentages are shown
      plot <- ggplot() +
        theme_bw() +
        labs(
          x = NULL,
          y = colnames(df_Vars[3]),
          title = plot_title,
          #caption = plot_caption
        ) +
        geom_bar(
          aes(x = df_Vars[[xvar]], y = df_Vars[[3]], fill = df_Vars[[zvar]]),
          stat = "identity",
          width = 0.90,
          position = "dodge"
        ) +
        theme(
          legend.title=element_blank(),
          #legend.position = c(0.96, 0.89),
          legend.position = "top",
          legend.background = element_rect(fill = "transparent"),
          axis.text.x = element_text(angle = 30, vjust = 0.6, hjust=0.5)) +
        scale_fill_manual(values = colours)+
        scale_y_continuous(expand = c(0,0), limits = c(0, max(df_Vars[[3]])*1.13), breaks=seq(0, max(df_Vars[[3]])*1.10, by=10^(nchar(max(df_Vars[[3]]))-1))) 
      
      #If filtering is applied check filter_var values, add percentages to plot and other estetical changes
      if (calc_Perc == T){
      #Check if xvar and zvar are chars
       plot <- plot + 
        geom_text(
          aes(
            label = paste0(round(df_Vars$Perc * 100, 2), "%*"),
            x = df_Vars[[xvar]],
            y = df_Vars[[3]],
            group = df_Vars[[zvar]]
          ),
          position = position_dodge(width = 0.9),
          vjust = 0.4,
          hjust = -0.05,
          size = 2.5,
          colour = "black",
          angle = 90
        ) +
         labs(caption = plot_caption) +
         #Larger gap in the top so that the geom_text is visible
        scale_y_continuous(expand = c(0,0), limits = c(0, max(df_Vars[[3]])*1.4), breaks=seq(0, max(df_Vars[[3]])*1.10, by=10^(nchar(max(df_Vars[[3]]))-1)))
      }
        
  
  histPlot_results <- list(table = df_Vars,
                           plot = plot)
  histPlot_results
}
