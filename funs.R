#Functions

read_johns_data <- function(){
  out <-read.csv(here::here("fiji_economic_game_master.csv"))
  }

johns_clean_data <- function(d) {
  out <- d %>% 
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
    arrange(id)
  return(out)
}
