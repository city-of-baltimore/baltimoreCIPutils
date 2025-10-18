## code to prepare `wd_agencies` dataset goes here

wd_agencies <- sharepointr::read_sharepoint(
  "https://bmore.sharepoint.com/:x:/r/sites/DOP-CIP/Shared%20Documents/Administration/Citywide%20CIP%20Contact%20List.xlsx?d=wf3beea2bfe1c4cffa2e061913eee4409&csf=1&web=1&e=RNXi9a",
  sheet = "Agency Reference"
)

usethis::use_data(wd_agencies, overwrite = TRUE)
