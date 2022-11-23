library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
# get percentage of white in each Urbanicity X
# get percentage of POC in each Urbanicity 
# get percetage of white incarcerated in each Urbanicity
  #get total white in jail
  # get total white in prison
# get percentage of POC incarcerated in each Urbanicity
  # get total POC in jail
  # get total POC in prison

#urbanacity order follows as documented in codebook

df <- get_data()
# Returns a dataframe that describes the percentage of the population that white
# people make up of each county type (urbanicity)
get_white_pop_county <- function() {
  total_white <- df %>% 
    filter(!is.na(urbanicity)) %>%
    group_by(urbanicity) %>%
    summarize(total_white_pop = sum(white_pop_15to64, na.rm=TRUE),
              total_pop = sum(total_pop, na.rm=TRUE)) %>%
    summarize(percent = total_white_pop / total_pop, .groups = "urbanicity")
  
  return(total_white)
}

# Returns a dataframe that describes the percentage of the population that POC 
# (AAPI, Black, Native, Latinx) make up of each county type (urbanicity)
get_poc_pop_county <- function() {
  total_poc <- df %>%
    filter(!is.na(urbanicity)) %>%
    group_by(urbanicity) %>%
    summarize(total_pop = sum(total_pop, na.rm=TRUE), 
              total_poc_pop = sum(aapi_pop_15to64, black_pop_15to64,
                                  latinx_pop_15to64, native_pop_15to64, 
                                  na.rm=TRUE)) %>%
    summarize(percent = total_poc_pop / total_pop, .groups = "urbanicity")
  
  return(total_poc)
}

# Returns a dataframe that describes the percentage of the incarcerated (jail 
# and prison) population that white pople make up in each county type
# (urbanicity)
get_white_incar_county <- function() {
  total_white_incar <- df %>%
    filter(!is.na(urbanicity)) %>%
    group_by(urbanicity) %>%
    summarize(total_white_pop = sum(white_jail_pop, white_prison_pop,
                                    na.rm=TRUE),
              total_incar_pop = sum(total_jail_pop, total_prison_pop, 
                                    na.rm=TRUE)) %>%
    summarize(percent = total_white_pop / total_incar_pop, .groups="urbanicity")
  
  return(total_white_incar)
}

# Returns a dataframe that describes the percentage of the incarcerated (jail
# and prison) population that POC (AAPI, Black, Native, Latinx) make up in each
# county type (urbanicity)
get_poc_incar_county <- function() {
  total_poc_incar <- df %>%
    filter(!is.na(urbanicity)) %>%
    group_by(urbanicity) %>%
    summarize(total_poc_pop = sum(aapi_jail_pop, aapi_prison_pop, 
                                  black_jail_pop, black_prison_pop, 
                                  latinx_jail_pop, latinx_prison_pop, 
                                  native_jail_pop, native_prison_pop, 
                                  other_race_jail_pop, other_race_prison_pop, 
                                  na.rm=TRUE),
              total_incar_pop = sum(total_jail_pop, total_prison_pop, 
                                    na.rm=TRUE)) %>%
    summarize(percent = total_poc_pop / total_incar_pop, .groups="urbanicity")
  
  return(total_poc_incar)
}

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# Returns a dataframe containing the jail population for each year
get_year_jail_pop <- function() {
  plot_df <- df %>%
    filter(!is.na(total_jail_pop), !is.na(year)) %>%
    select(total_jail_pop, year) 
  
  return(plot_df)   
}

# Returns the plot that describes the jail population of the US for each year
# in the form of a bar chart
plot_jail_pop_for_us <- function()  {
  plot <- ggplot(get_year_jail_pop()) + 
    geom_col(mapping = aes(x=year, y=total_jail_pop)) +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", 
         y = "Total Jail Population", 
         x = "Year",
         caption = "How the jail population has grown from 1970-2018.") +
    scale_y_continuous(labels=scales::comma)
  return(plot)   
} 


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Returns a dataframe that describes the jail population of each year of each 
# state in the given vector of state abbreviations 
get_jail_pop_by_states <- function(states) {
  plot_df <- df %>%
    filter(state %in% states, !is.na(total_jail_pop), !is.na(year), 
           !is.na(state)) %>%
    select(state, total_jail_pop, year) %>%
    group_by(year, state) %>%
    summarize(total_jail_pop = sum(total_jail_pop))
  return(plot_df)
}

# Returns a line chart that describes the jail population of each year of each
# state in the given vector of state abbreviations
plot_jail_pop_by_states <- function(states) {
  plot <- ggplot(get_jail_pop_by_states(states)) + 
    geom_line(mapping = aes(x = year, y = total_jail_pop, color = state)) +
    labs(title = "Increase of Jail Population by U.S. State (1970-2018)",
         x = "Year",
         y = "Total Jail Population",
         color = "State",
         caption = paste0("Differences between jail populations in Washington,", 
         " Texas, California, Georgia from 1970-2018.")) +
    scale_y_continuous(labels=scales::comma)
  
  return(plot)
}

# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ---- 
#----------------------------------------------------------------------------#
# Returns a dataframe the describes the percent of the population of each
# county type (urbanicity) that has been jailed each year
get_jail_pop_by_county_type <- function() {
  plot_df <- df %>% 
    filter(!is.na(urbanicity), !is.na(year)) %>%
    select(urbanicity, year, total_jail_pop, total_pop_15to64) %>%
    group_by(urbanicity, year) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm=TRUE),
              total_pop = sum(total_pop_15to64, na.rm=TRUE)) %>%
    filter(urbanicity == "rural" || urbanicity ==  "small/mid" ||
             urbanicity == "suburban" || urbanicity == "urban") %>%
    mutate(percent = total_jail_pop / total_pop)
  
  return(plot_df)
}

# Returns a line chart that describes the percent of the population of each
# county type (urbanicity) that has been jailed each year
plot_jail_pop_by_county_type <- function() {
  plot <- ggplot(get_jail_pop_by_county_type()) +
    geom_line(mapping = aes(x = year, y = percent, color = urbanicity)) +
    labs(title = paste0("Difference of Population Jailed Between County Types ", 
                        "(1970-2018)"),
         x = "Year", 
         y = "Pecentage of Population Jailed",
         color = "County Type",
         caption = paste0("Differences between county types of percentage of ", 
                          "population jailed from 1970-2018")) + 
    scale_y_continuous(labels=scales::percent)
  
  return(plot)
}

# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#

#(BOTH MAPS)
# showing percentage of Black people incarcerated
# vs percentage of Black people in each area

# Returns a dataframe that describes the percentage of the Black population of
# each state that is incarcerated (jail and prison). The dataframe is also
# contains information that allows it to be plotted into a map
get_black_incar_pop <- function() {
  plot_df <- df %>%
    filter(!is.na(state)) %>%
    select(state, black_jail_pop, black_prison_pop, total_jail_pop, 
           total_prison_pop) %>%
    group_by(state) %>%
    summarize(total_incar_black_pop = sum(black_jail_pop, black_prison_pop,
                                          na.rm=TRUE),
              total_incar_pop = sum(total_jail_pop, total_prison_pop, 
                                    na.rm=TRUE)) %>%
    mutate(percent = total_incar_black_pop / total_incar_pop)
  
  state_name_abb <- data.frame(
    state = state.abb,
    name = tolower(state.name)
  )
  
  plot_df <- plot_df %>% 
    right_join(state_name_abb, by="state") %>%
    rename(abb = state, state = name)
  
  state_shape <- map_data("state") %>%
    rename(state = region) %>%
    left_join(plot_df, by="state")
  
  return(state_shape)
}

# Returns a data frame that describes the percent of the population of each
# state that is Black. The dataframe also contains information that allows it
# to be plotted into a map.
get_black_total_pop <- function() {
  plot_df <- df %>%
    filter(!is.na(state)) %>%
    select(state, total_pop_15to64, black_pop_15to64) %>%
    group_by(state) %>%
    summarize(total_black_pop = sum(black_pop_15to64, na.rm=TRUE),
              total_pop = sum(total_pop_15to64, na.rm=TRUE)) %>%
    mutate(percent = total_black_pop / total_pop)
  
  state_name_abb <- data.frame(
    state = state.abb,
    name = tolower(state.name)
  )
  
  plot_df <- plot_df %>% 
    right_join(state_name_abb, by="state") %>%
    rename(abb = state, state = name)
  
  state_shape <- map_data("state") %>%
    rename(state = region) %>%
    left_join(plot_df, by="state")
  
  return(state_shape)
}

# Returns a map plot that describes the percent of the Black population that is
# incarcerated (jail and prison) in each state.
plot_black_incar_pop <- function() {
  plot_df <- get_black_incar_pop()
  
  # Taken from chapter 16 from the textbook in order for the map to look better
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(),        # remove axis lines
      axis.text = element_blank(),        # remove axis labels
      axis.ticks = element_blank(),       # remove axis ticks
      axis.title = element_blank(),       # remove axis titles
      plot.background = element_blank(),  # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank()      # remove border around plot
    )
  
  plot <- ggplot(plot_df) +
    geom_polygon(
      mapping = aes(x=long, y=lat, group=group, fill=percent),
      color = "white",
      size = .1
    ) +
    coord_map() +
    labs(fill = "Population Jailed",
         title = "Percent of Black Population Jailed",
         caption = paste0("Differences of percent of the Black population ", 
                          "jailed across the US")) +
    scale_fill_continuous(labels=scales::percent, low = "#66CCFF", 
                          high = "#000033") +
    blank_theme
  
  return(plot)
}

# Returns a map plot that describes the percentage of the population of each 
# state that is Black.
plot_black_pop <- function() {
  plot_df <- get_black_total_pop()
  
  # Taken from chapter 16 from the textbook in order for the map to look better 
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(),        # remove axis lines
      axis.text = element_blank(),        # remove axis labels
      axis.ticks = element_blank(),       # remove axis ticks
      axis.title = element_blank(),       # remove axis titles
      plot.background = element_blank(),  # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank()      # remove border around plot
    )
  
  plot <- ggplot(plot_df) +
    geom_polygon(
      mapping = aes(x=long, y=lat, group=group, fill=percent),
      color = "white",
      size = .1
    ) +
    coord_map() +
    labs(fill = "Population",
         title = "Concentrations of the Black Population",
         caption = "Percent of population that is Black across the US") +
    scale_fill_continuous(labels=scales::percent, low = "#66CCFF", 
                          high = "#000033") +
    blank_theme
  
  return(plot)
}
