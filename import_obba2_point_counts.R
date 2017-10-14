library(magrittr)
library(plyr)
library(dplyr)

data <-
  read.csv("DASL_OBBA2_PC_edited.csv",header=T,stringsAsFactors=F) %>%
  as.tbl

data %>%
  mutate(is_singing_row=
    rowSums(data[,c(
      'Time.1...25.m.',
      'Time.1..25.75.m.',
      'Time.1...75.m.',
      'Time.2...25.m.',
      'Time.2..25.75.m.',
      'Time.2...75.m.',
      'Time.3...25.m.',
      'Time.3..25.75.m.',
      'Time.3...75.m.',
      'Time.4...25.m.',
      'Time.4..25.75.m.',
      'Time.4...75.m.',
      'Time.5...25.m.',
      'Time.5..25.75.m.',
      'Time.5...75.m.'
      )])>0
    ) %>%
  
  group_by(BLOCK,POINT,Species) %>%
  mutate(agg_spp_comments=
           Bird.Count.Info_Comments[which(Bird.Count.Info_Comments != "")] %>%
           paste(sep="",collapse="; ")
         ) %>%

  summarize(
    A=recode(Species[1],CAGO="CANG",ETTI="TUTI",ROPI="Rock Pigeon (Feral Pigeon)",RPHE="RNEP",YWAR="YEWA"),
    B="",
    C="",
    D=sum(Non.singing.During.Count)+sum(Non.singing.Outside.Count)+sum(Flyovers)+sum(is_singing_row),
    E=agg_spp_comments[1],
    F=paste0("OBBA2 block ",BLOCK[1]," point count ",POINT[1],sep=""),
    G=Latitude[1],
    H=Longitude[1],
    I=Date[1],
    J=Time[1],
    K="OH",
    L="US",
    M="stationary",
    N=1L,
    O=6.25,
    P="y",
    Q="",
    R="",
    S=paste(collapse="",sep="",
      "6min15sec point counts. Large upload, data entry errors possible. Paper field notes available. Please contact me about anything unexpected. ",
      "Noise infererence=",
      Noise_Interference[1],
      ", Noise type=",
      if_else(Noise_Type[1]=="","N/A",Noise_Type[1]),
      ", Temp=",
      Temperature[1],
      "C, ",
      "Precip=",
      recode(Precipitation[1],`0`="none",`1`="haze/fog",`2`="drizzle/lt. rain",`3`="moderate/heavy rain/t-storms, "),
      ", Wind=Beaufort ",
      Wind[1],
      ", Sky=",
      recode(Cloud[1],`0`="clear",`1`="partly cloudy",`2`="mostly cloudy",`3`="overcast"),
      ", Road=",
      recode(Road[1],`1`="paved primary",`2`="paved secondary",`3`="gravel secondary",`4`="gravel/dirt primitive")
      )
  ) %>%
  
  ungroup %>%
  
  group_by(BLOCK, POINT) %>%
  
  select(-BLOCK, -POINT, -Species) %>%
  
  write.csv('obba2_PC_eB_upload.csv',row.names=F)

## This is "eBird Record Format".
## Delete the column names and split into <= 99 checklists per file before uploading.