library(tidyverse)
library(haven)
library(mice)
library(labelled)
library(phonics)
# WD ----------------------------------------------------------------------
getwd()
setwd("/home/arubhardwaj/Downloads/IWS SURVEY")


# Data Load ---------------------------------------------------------------


rajasthan <- read_csv("iws_rajasthan.csv") # import data in R
head(rajasthan)

rajasthan$sound_jati <- soundex(rajasthan$b7_text)
rajasthan$sound_jati %>% table()

rajasthan %>% filter(sound_jati=="V251") %>% View()

#---------------------------------------------------------
# function for max occutring
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#---------------------------------------------------------

rajasthan <- rajasthan %>% 
  group_by(sound_jati) %>% 
  mutate(modal_jati = Mode(b7_text)) 

rajasthan <- rajasthan %>% 
  group_by(sound_jati) %>% 
  mutate(modal_jati_code = Mode(b7)) 

rajasthan$modal_jati %>% unique()



# Split Categories --------------------------------------------------------

# hindu
kar_hindu <- rajasthan %>% filter(b4==1)
kar_hindu_sc <- kar_hindu %>% filter(b6==1)
kar_hindu_st <- kar_hindu %>% filter(b6==2)
kar_hindu_obc <- kar_hindu %>% filter(b6==3)
kar_hindu_gen <- kar_hindu %>% filter(b6==4)

### SC
kar_hindu_sc$hsc_jati_sound <- kar_hindu_sc$b7_text %>% soundex() # sound of jati

kar_hindu_sc <- kar_hindu_sc %>% 
  group_by(hsc_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_hindu_sc <- kar_hindu_sc %>% 
  group_by(hsc_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_hindu_sc %>% View()
kar_hindu_sc$b7 %>% table()
nrow(kar_hindu_sc)

kar_hindu_sc$modal_jati_code


code <- ifelse(kar_hindu_sc$b7==-888 |kar_hindu_sc$b7==-999,kar_hindu_sc$modal_jati_code,kar_hindu_sc$b7 )
kar_hindu_sc$new_code <- code
kar_hindu_sc %>% filter(b7 == -888) %>% View()


# remove columns
kar_hindu_sc <- ungroup(kar_hindu_sc)
kar_hindu_sc <- kar_hindu_sc %>% select(-c(hsc_jati_sound,modal_jati,modal_jati_code))


## obc
kar_hindu_obc$hobc_jati_sound <- kar_hindu_obc$b7_text %>% soundex() # sound of jati

kar_hindu_obc <- kar_hindu_obc %>% 
  group_by(hobc_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_hindu_obc <- kar_hindu_obc %>% 
  group_by(hobc_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_hindu_obc %>% View()
kar_hindu_obc$b7 %>% table()
nrow(kar_hindu_obc)

kar_hindu_obc$modal_jati_code


code <- ifelse(kar_hindu_obc$b7==-888 |kar_hindu_obc$b7==-999,kar_hindu_obc$modal_jati_code,kar_hindu_obc$b7 )
kar_hindu_obc$new_code <- code
kar_hindu_obc %>% filter(b7 == -888) %>% View()


# remove columns
kar_hindu_obc <- ungroup(kar_hindu_obc)
kar_hindu_obc <- kar_hindu_obc %>% select(-c(hobc_jati_sound,modal_jati,modal_jati_code))

kar_hindu_obc$hobc_jati_sound <- kar_hindu_obc$b7_text %>% soundex() # sound of jati

kar_hindu_obc <- kar_hindu_obc %>% 
  group_by(hobc_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_hindu_obc <- kar_hindu_obc %>% 
  group_by(hobc_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_hindu_obc %>% View()
kar_hindu_obc$b7 %>% table()
nrow(kar_hindu_obc)

kar_hindu_obc$modal_jati_code


code <- ifelse(kar_hindu_obc$b7==-888 |kar_hindu_obc$b7==-999,kar_hindu_obc$modal_jati_code,kar_hindu_obc$b7 )
kar_hindu_obc$new_code <- code
kar_hindu_obc %>% filter(b7 == -888) %>% View()


# remove columns
kar_hindu_obc <- ungroup(kar_hindu_obc)
kar_hindu_obc <- kar_hindu_obc %>% select(-c(hobc_jati_sound,modal_jati,modal_jati_code))

## gen
kar_hindu_gen$hgen_jati_sound <- kar_hindu_gen$b7_text %>% soundex() # sound of jati

kar_hindu_gen <- kar_hindu_gen %>% 
  group_by(hgen_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_hindu_gen <- kar_hindu_gen %>% 
  group_by(hgen_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_hindu_gen %>% View()
kar_hindu_gen$b7 %>% table()
nrow(kar_hindu_gen)

kar_hindu_gen$modal_jati_code


code <- ifelse(kar_hindu_gen$b7==-888 |kar_hindu_gen$b7==-999,kar_hindu_gen$modal_jati_code,kar_hindu_gen$b7 )
kar_hindu_gen$new_code <- code
kar_hindu_gen %>% filter(b7 == -888) %>% View()


# remove columns
kar_hindu_gen <- ungroup(kar_hindu_gen)
kar_hindu_gen <- kar_hindu_gen %>% select(-c(hgen_jati_sound,modal_jati,modal_jati_code))

kar_hindu_gen$hgen_jati_sound <- kar_hindu_gen$b7_text %>% soundex() # sound of jati

kar_hindu_gen <- kar_hindu_gen %>% 
  group_by(hgen_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_hindu_gen <- kar_hindu_gen %>% 
  group_by(hgen_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_hindu_gen %>% View()
kar_hindu_gen$b7 %>% table()
nrow(kar_hindu_gen)

kar_hindu_gen$modal_jati_code


code <- ifelse(kar_hindu_gen$b7==-888 |kar_hindu_gen$b7==-999,kar_hindu_gen$modal_jati_code,kar_hindu_gen$b7 )
kar_hindu_gen$new_code <- code
kar_hindu_gen %>% filter(b7 == -888) %>% View()


# remove columns
kar_hindu_gen <- ungroup(kar_hindu_gen)
kar_hindu_gen <- kar_hindu_gen %>% select(-c(hgen_jati_sound,modal_jati,modal_jati_code))



#3 ST
kar_hindu_st$hst_jati_sound <- kar_hindu_st$b7_text %>% soundex() # sound of jati

kar_hindu_st <- kar_hindu_st %>% 
  group_by(hst_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_hindu_st <- kar_hindu_st %>% 
  group_by(hst_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_hindu_st %>% View()
kar_hindu_st$b7 %>% table()
nrow(kar_hindu_st)

kar_hindu_st$modal_jati_code


code <- ifelse(kar_hindu_st$b7==-888 |kar_hindu_st$b7==-999,kar_hindu_st$modal_jati_code,kar_hindu_st$b7 )
kar_hindu_st$new_code <- code
kar_hindu_st %>% filter(b7 == -888) %>% View()


# remove columns
kar_hindu_st <- ungroup(kar_hindu_st)
kar_hindu_st <- kar_hindu_st %>% select(-c(hst_jati_sound,modal_jati,modal_jati_code))

kar_hindu_st$hst_jati_sound <- kar_hindu_st$b7_text %>% soundex() # sound of jati

kar_hindu_st <- kar_hindu_st %>% 
  group_by(hst_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_hindu_st <- kar_hindu_st %>% 
  group_by(hst_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_hindu_st %>% View()
kar_hindu_st$b7 %>% table()
nrow(kar_hindu_st)

kar_hindu_st$modal_jati_code


code <- ifelse(kar_hindu_st$b7==-888 |kar_hindu_st$b7==-999,kar_hindu_st$modal_jati_code,kar_hindu_st$b7 )
kar_hindu_st$new_code <- code
kar_hindu_st %>% filter(b7 == -888) %>% View()


# remove columns
kar_hindu_st <- ungroup(kar_hindu_st)
kar_hindu_st <- kar_hindu_st %>% select(-c(hst_jati_sound,modal_jati,modal_jati_code))



kh <- rbind(kar_hindu_gen,kar_hindu_obc,kar_hindu_sc,kar_hindu_st) 
kh$b7 %>% table()
kh$new_code %>% table()

write_csv(kh,"hindurajasthan.csv")


# Muslim ------------------------------------------------------------------


# muslim
kar_muslim <- rajasthan %>% filter(b4==2)
kar_muslim_sc <- kar_muslim %>% filter(b6==1)
kar_muslim_st <- kar_muslim %>% filter(b6==2)
kar_muslim_obc <- kar_muslim %>% filter(b6==3)
kar_muslim_gen <- kar_muslim %>% filter(b6==4)








### SC
kar_muslim_sc$hsc_jati_sound <- kar_muslim_sc$b7_text %>% soundex() # sound of jati

kar_muslim_sc <- kar_muslim_sc %>% 
  group_by(hsc_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_muslim_sc <- kar_muslim_sc %>% 
  group_by(hsc_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_muslim_sc %>% View()
kar_muslim_sc$b7 %>% table()
nrow(kar_muslim_sc)

kar_muslim_sc$modal_jati_code


code <- ifelse(kar_muslim_sc$b7==-888 |kar_muslim_sc$b7==-999,kar_muslim_sc$modal_jati_code,kar_muslim_sc$b7 )
kar_muslim_sc$new_code <- code
kar_muslim_sc %>% filter(b7 == -888) %>% View()


# remove columns
kar_muslim_sc <- ungroup(kar_muslim_sc)
kar_muslim_sc <- kar_muslim_sc %>% select(-c(hsc_jati_sound,modal_jati,modal_jati_code))


## obc
kar_muslim_obc$hobc_jati_sound <- kar_muslim_obc$b7_text %>% soundex() # sound of jati

kar_muslim_obc <- kar_muslim_obc %>% 
  group_by(hobc_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_muslim_obc <- kar_muslim_obc %>% 
  group_by(hobc_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_muslim_obc %>% View()
kar_muslim_obc$b7 %>% table()
nrow(kar_muslim_obc)

kar_muslim_obc$modal_jati_code


code <- ifelse(kar_muslim_obc$b7==-888 |kar_muslim_obc$b7==-999,kar_muslim_obc$modal_jati_code,kar_muslim_obc$b7 )
kar_muslim_obc$new_code <- code
kar_muslim_obc %>% filter(b7 == -888) %>% View()


# remove columns
kar_muslim_obc <- ungroup(kar_muslim_obc)
kar_muslim_obc <- kar_muslim_obc %>% select(-c(hobc_jati_sound,modal_jati,modal_jati_code))

kar_muslim_obc$hobc_jati_sound <- kar_muslim_obc$b7_text %>% soundex() # sound of jati

kar_muslim_obc <- kar_muslim_obc %>% 
  group_by(hobc_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_muslim_obc <- kar_muslim_obc %>% 
  group_by(hobc_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_muslim_obc %>% View()
kar_muslim_obc$b7 %>% table()
nrow(kar_muslim_obc)

kar_muslim_obc$modal_jati_code


code <- ifelse(kar_muslim_obc$b7==-888 |kar_muslim_obc$b7==-999,kar_muslim_obc$modal_jati_code,kar_muslim_obc$b7 )
kar_muslim_obc$new_code <- code
kar_muslim_obc %>% filter(b7 == -888) %>% View()


# remove columns
kar_muslim_obc <- ungroup(kar_muslim_obc)
kar_muslim_obc <- kar_muslim_obc %>% select(-c(hobc_jati_sound,modal_jati,modal_jati_code))

## gen
kar_muslim_gen$hgen_jati_sound <- kar_muslim_gen$b7_text %>% soundex() # sound of jati

kar_muslim_gen <- kar_muslim_gen %>% 
  group_by(hgen_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_muslim_gen <- kar_muslim_gen %>% 
  group_by(hgen_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_muslim_gen %>% View()
kar_muslim_gen$b7 %>% table()
nrow(kar_muslim_gen)

kar_muslim_gen$modal_jati_code


code <- ifelse(kar_muslim_gen$b7==-888 |kar_muslim_gen$b7==-999,kar_muslim_gen$modal_jati_code,kar_muslim_gen$b7 )
kar_muslim_gen$new_code <- code
kar_muslim_gen %>% filter(b7 == -888) %>% View()


# remove columns
kar_muslim_gen <- ungroup(kar_muslim_gen)
kar_muslim_gen <- kar_muslim_gen %>% select(-c(hgen_jati_sound,modal_jati,modal_jati_code))

kar_muslim_gen$hgen_jati_sound <- kar_muslim_gen$b7_text %>% soundex() # sound of jati

kar_muslim_gen <- kar_muslim_gen %>% 
  group_by(hgen_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_muslim_gen <- kar_muslim_gen %>% 
  group_by(hgen_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_muslim_gen %>% View()
kar_muslim_gen$b7 %>% table()
nrow(kar_muslim_gen)

kar_muslim_gen$modal_jati_code


code <- ifelse(kar_muslim_gen$b7==-888 |kar_muslim_gen$b7==-999,kar_muslim_gen$modal_jati_code,kar_muslim_gen$b7 )
kar_muslim_gen$new_code <- code
kar_muslim_gen %>% filter(b7 == -888) %>% View()


# remove columns
kar_muslim_gen <- ungroup(kar_muslim_gen)
kar_muslim_gen <- kar_muslim_gen %>% select(-c(hgen_jati_sound,modal_jati,modal_jati_code))



#3 ST
kar_muslim_st$hst_jati_sound <- kar_muslim_st$b7_text %>% soundex() # sound of jati

kar_muslim_st <- kar_muslim_st %>% 
  group_by(hst_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_muslim_st <- kar_muslim_st %>% 
  group_by(hst_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_muslim_st %>% View()
kar_muslim_st$b7 %>% table()
nrow(kar_muslim_st)

kar_muslim_st$modal_jati_code


code <- ifelse(kar_muslim_st$b7==-888 |kar_muslim_st$b7==-999,kar_muslim_st$modal_jati_code,kar_muslim_st$b7 )
kar_muslim_st$new_code <- code
kar_muslim_st %>% filter(b7 == -888) %>% View()


# remove columns
kar_muslim_st <- ungroup(kar_muslim_st)
kar_muslim_st <- kar_muslim_st %>% select(-c(hst_jati_sound,modal_jati,modal_jati_code))

kar_muslim_st$hst_jati_sound <- kar_muslim_st$b7_text %>% soundex() # sound of jati

kar_muslim_st <- kar_muslim_st %>% 
  group_by(hst_jati_sound) %>% 
  mutate(modal_jati = Mode(b7_text)) 

kar_muslim_st <- kar_muslim_st %>% 
  group_by(hst_jati_sound) %>% 
  mutate(modal_jati_code = Mode(b7)) 

kar_muslim_st %>% View()
kar_muslim_st$b7 %>% table()
nrow(kar_muslim_st)

kar_muslim_st$modal_jati_code


code <- ifelse(kar_muslim_st$b7==-888 |kar_muslim_st$b7==-999,kar_muslim_st$modal_jati_code,kar_muslim_st$b7 )
kar_muslim_st$new_code <- code
kar_muslim_st %>% filter(b7 == -888) %>% View()


# remove columns
kar_muslim_st <- ungroup(kar_muslim_st)
kar_muslim_st <- kar_muslim_st %>% select(-c(hst_jati_sound,modal_jati,modal_jati_code))



km <- rbind(kar_muslim_gen,kar_muslim_obc,kar_muslim_sc,kar_muslim_st) 
km$b7 %>% table()
km$new_code %>% table()

write_csv(kh,"muslimrajasthan.csv")



rajasthan_new <- rbind(km,kh)

write_csv(rajasthan_new,"rajasthan_new.csv")













jati_dict <- rajasthan %>% select(b7,b7_text)
jati_dict <- jati_dict %>% filter(b7 != -888) %>% filter(b7!=-999)

jati_dict <- unique(jati_dict)
write_csv(jati_dict,"rajasthan_jati_dict.csv")
View(jati_dict)
jati_dict







jati_dict <- rajasthan %>% select(b7,b7_text)
jati_dict <- jati_dict %>% filter(b7 == -888)

jati_dict <- unique(jati_dict)
write_csv(jati_dict,"rajasthan_missing_jati.csv")
View(jati_dict)
jati_dict






rajasthan %>% filter(b7==-888) %>% count(b6_text)

rajasthan  %>% count(b6_text)



rajasthan_new <- read_csv("rajasthan_new.csv")

rajasthan_new %>% filter(new_code==-888) %>% count(b6_text)

rajasthan_new  %>% count(b6_text)
