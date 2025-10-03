library(rvest) 
library(RCurl) 
library(stringi)
library(tibble)

output <- tribble (~date, ~site, ~id, ~five_star, ~four_star, ~three_star, ~two_star, ~one_star, ~total, ~average)

timestamp <- Sys.time()

sites <- tribble (~siteName, ~url,
                  'Mayo Clinic', 'https://play.google.com/store/apps/details?id=com.mayoclinic.patient',
                  'Cleveland Clinic', 'https://play.google.com/store/apps/details?id=org.ccf.patientapp',
                  'MyMountSinai', 'https://play.google.com/store/apps/details?id=org.mountsinai.MyChart',
                  'MyChart', 'https://play.google.com/store/apps/details?id=epic.mychart.android',
                  'Johns Hopkins Medicine', 'https://play.google.com/store/apps/details?id=com.johnshopkins.ondemandvirtualcare',
                  'Penn Medicine', 'https://play.google.com/store/apps/details?id=org.secure.mypennmedicine.mypennmed',
                  'Cedars-Sinai', 'https://play.google.com/store/apps/details?id=com.cedars_sinai.mycslink',
                  'Houston Methodist', 'https://play.google.com/store/apps/details?id=org.houstonmethodist.methodistmobile',
                  'NYU Langone Health','https://play.google.com/store/apps/details?id=org.nyulmc.clinical.mychart',
                  'Stanford Health Care', 'https://play.google.com/store/apps/details?id=org.stanfordhealthcare.myhealth',
                  'Northwestern Medicine', 'https://play.google.com/store/apps/details?id=org.nm.mobile.myNM',
                  'New York - Presbyterian', 'https://play.google.com/store/apps/details?id=org.nyp.nyppatient.android',
                  'University of Michigan Health', 'https://play.google.com/store/apps/details?id=edu.umich.myuofmhealth', 
                  'Mass General Brigham, Inc', 'https://play.google.com/store/apps/details?id=org.partners.ppgmob',
                  'Vanderbilt University Medical Center', 'https://play.google.com/store/apps/details?id=com.ta.mhav',
                  'Kaiser Permanente', 'https://play.google.com/store/apps/details?id=org.kp.m',
                  'Primary Care On Demand Wis.', 'https://play.google.com/store/apps/details?id=org.mayoclinic.primarycareondemand'
                  
                  )

iterations <- nrow(sites)
strings <- stri_rand_strings(iterations, 30, pattern = "[A-Za-z0-9]")

#i <- 1

for(i in 1:iterations) {
  
  url <- sites$url[i]
  res <- GET(url, user_agent("Mozilla/5.0"))
  
  
  if (status_code(res) == 200) {
    webpage <- read_html(res)
    ratings <- webpage %>% html_elements(".wcB8se") %>% html_attr('title')
    ratings_5_star <- as.numeric(gsub(",", "", ratings[1])) 
    ratings_4_star <- as.numeric(gsub(",", "", ratings[2])) 
    ratings_3_star <- as.numeric(gsub(",", "", ratings[3]))
    ratings_2_star <- as.numeric(gsub(",", "", ratings[4]))
    ratings_1_star <- as.numeric(gsub(",", "", ratings[5]))
    total <- sum(ratings_5_star, ratings_4_star, ratings_3_star, ratings_2_star, ratings_1_star)
    
    average <- sum((ratings_5_star *5) + (ratings_4_star *4) + (ratings_3_star *3) + (ratings_2_star *2) + (ratings_1_star))/total
    
    output <- output %>% add_row (
      date = timestamp, 
      site = sites$siteName[i], 
      id = strings[i], 
      five_star = ratings_5_star, 
      four_star = ratings_4_star, 
      three_star = ratings_3_star, 
      two_star = ratings_2_star, 
      one_star = ratings_1_star, 
      total = total,
      average = average)

} else {
  print(paste("Failed to fetch:", status_code(res), " ", sites$sitename[i]))
}
}

write.table(output,paste0('android_ratings.csv'),append = TRUE, sep=',', col.names = FALSE)   

