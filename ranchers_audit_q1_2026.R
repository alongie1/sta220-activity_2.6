#Audit of Ranchers Claims for Q1 of 2026

#Riley's working directory, can be changed as needed
#setwd("C:/Users/riley/Desktop/audit stuff/FINAL")

#Import

library(tidyverse)
claims_data = read_csv("DATA/2026_q1.csv")
#54 rows 4 columns expected from claims_codebook
dim(claims_data)
names(claims_data)
summary(claims_data)

#Clean

#"max_days" is the maximum days a claim can be open before being late. "late" tells if a claim is considered late or not
#max_days = 30 comes from claims_codebook, line 30, "A claim that takes more than 30 days is late."
max_days = 30
claims_data <- claims_data |> 
  mutate(late = days_to_close > max_days)

#Analyze

#"n" is the number of claims, "avg_days_open" is the average days from open to close for all closed claims
#"open_claims" is the number of claims currently open, "percent_late" is the percent of closed claims that were late
summary_table = claims_data |> 
  group_by(claim_type) |> 
  summarise(
    n = n(),
    open_claims = sum(days_to_close == -1),
    #Added back the number of open claims to the sum as each open claim gave a value of -1, when they should have no effect.
    #n() has the number of open claims subtracted from it because they are not included in the mean days or percent late
    avg_days_open = (sum(days_to_close) + sum(days_to_close == -1))/(n()-sum(days_to_close == -1)),
    percent_late = sum(late == TRUE) / (n() - sum(days_to_close == -1))
  )
summary_table

#Save
write_csv(claims_data, "2026_q1_audit.csv")

