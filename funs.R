#Functions

data_read_john <- function(){
  out <-read.csv(here::here("fiji_economic_game_master.csv"))
  }

data_clean_john <- function(df) {
  out <- df %>% 
    dplyr::mutate(church_dateR = church_date)%>% # create new column in case of SNAFU%
    dplyr::mutate(church_dateR = stringr::str_replace_all(church_dateR, "/10","/2010")) %>%
    dplyr::mutate(male =factor(ifelse(sex==1,"male","not_male"))) %>% # fix bad coding
    dplyr::mutate(id = factor(id) )%>% 
    dplyr::select(-church_at_risk) %>%  # we don't want this in our models -- if you can't attend it doesnt' count
    dplyr::mutate(church_attendF = factor(church_attend))%>% 
    dplyr::mutate(timeF = factor(time)) %>% 
    dplyr::mutate(church_event_noF = factor(church_event_no))%>%
    dplyr::mutate(church_dateF = factor(church_dateR),
                  kava_dateF = factor(kava_date),
                  reported_attendanceF = factor(reported_attendance),
                  church_donationF = factor(church_donation),
                  church_donationN = as.numeric(church_donationF),
                  church_donationC = scale(church_donationN,center=T,scale=F),
                  village_donationF = factor(village_donation),
                  village_donationN = as.numeric(village_donationF),
                  village_rankN = as.numeric(village_rank),
                  village_rankS = scale(village_rankN))%>%
    dplyr::mutate(church_dateR = church_date)%>% # create new column in case of SNAFU
    dplyr::mutate(church_dateR = stringr::str_replace_all(church_dateR, "/10","/2010")) # apply method
    arrange(id)
  return(out)
}


show_unique_id <- function(df,y){
  id =  paste0(y) # name of Id variable
  numb <- length(unique(df$id)) # count # of ids
  print(numb)
}


show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

show_correlations <- function(df,x,y) {
  df %>% dplyr::select(x,y)%>%
  correlation::correlation(partial = TRUE) %>% 
  summary()
}
