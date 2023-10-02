### week 3

### Derive armed conflict

conflict <- read.csv(here("original/conflictdata.csv"))
#Best = best estimate for total number of battle related deaths

#Write a new R script that derives the binary armed conflict variable that was used as the primary exposure in the paper

# (0 = no, <25 battle-related deaths; 1 = yes, ≥25 battle-related deaths)
conflict <- conflict %>%
  mutate(binaryconflict = case_when(best < 25 ~ 0,
                                    best >= 25 ~ 1))

write.csv(conflict, here("orignal/binaryconflict.csv"), row.names = FALSE)
