#!/usr/bin/env Rscript

# draft_summary_sheet_scrape.R

# Libraries ----

library(dplyr)
library(purrr)
library(readr)
library(tidyr)
library(readxl)
library(crayon)
library(stringr)
library(stringi)

# Custom Function ----

fuse_dfs <- function(df_1, df_2) {
  
  # ensure that df_1 fields to be fused are empty
  if (!all(is.na(df_1[, names(df_2)]))) {
    stop("The df_1 fields to be fused are not empty.")
  }
  
  # insert values from df_2 into df_1
  df_1[, names(df_2)] <- df_2
  
  # return df_1
  df_1
  
}

# # Test dfs
# (df_a <- tibble(id = c("abc123", "def456"), 
#                 a = rep(NA, 2), b = rep(NA, 2), c = rep(NA, 2)))
# (df_b <- tibble(a = rep("10", 2), b = rep("20", 2), c = rep("30", 2)))
# # Test call
# fuse_dfs(df_a, df_b)

# Setup ----

filenames_summ_sheets <- 
  list.files("./Double_Scored/",
             "^\\d{1,2}.\\d{1,2}.\\d{1,2}\\W+BH19STUD\\d{5}.+\\.xlsx$")
ids_summ_sheets <- str_extract(filenames_summ_sheets, "BH19STUD\\d{5}")

df_ids_filenames <- 
  tibble(id = ids_summ_sheets, 
         filename = filenames_summ_sheets)

sheets_std <-
  c("UMMAP Summary", "Evaluation (Pre)", "Toolbox (Pre)", 
    "Session 1", "Session 2", "Session 3", "Session 4", 
    "Evaluation (Post)", "Toolbox (Post)", "Outcome Summary")
df_sheets <- data.frame(matrix(data = list(NA), 
                               nrow = length(filenames_summ_sheets), 
                               ncol = length(sheets_std)))
names(df_sheets) <- sheets_std

df_ids_filenames_data <- dplyr::bind_cols(df_ids_filenames, df_sheets)
rm(df_ids_filenames); rm(df_sheets); gc()


# Loop to extract data from spreadsheets
for (id in df_ids_filenames_data$id) {
  cat(paste0("ID: ", id, "\n"))
  
  # Get row numer corresponding to STUD pt. ID
  row_num <- which(df_ids_filenames_data$id == id)
  cat(paste0("Row num: ", row_num, "\n"))
  
  # Get Excel filename corresponding to STUD pt. ID
  filename <- 
    df_ids_filenames_data[[which(df_ids_filenames_data$id == id), "filename"]]
  cat(paste0("Filename: ", filename, "\n"))
  
  sheets <-
    excel_sheets(
      path = paste0("./Double_Scored/", filename)
    )
  cat(paste0(sheets, "\n"))
  if (!identical(sheets_std, sheets)) {
    stop(
      paste0("\nThe names or order of the worksheets (tabs) in file\n'", 
             bold(red(filename)), "' are non-standard.\n",
             "Correct and/or reorder all worksheet names ",
             " to standard and re-run this script."))
  }
  
  ls_dfs <-
    suppressMessages(
      map(sheets,
          function(s) {
            read_excel(
              path = paste0("./Double_Scored/", filename),
              sheet = s, 
              # huge slowdown, but spreadsheets have data in header row!!!
              col_names = 
                as.character(1:ncol(
                  read_excel(
                    path = paste0("./Double_Scored/", filename),
                    sheet = s))),
              .name_repair = "unique"
            )
          })
    )
  names(ls_dfs) <- sheets
  
  for (df_name in names(ls_dfs)) {
    # cat(paste0(df_name, "\n"))
    df_ids_filenames_data[[row_num, df_name]] <- as_tibble(ls_dfs[[df_name]])
  }
  
  cat("\n")
}
rm(ls_dfs); gc()


# Meta Data ----

# Get meta dfs of REDCap project data structure

df_meta_datadict <- 
  read_csv("./data_structure/STUD_DataDictionary_2019-06-25.csv",
           col_types = cols(.default = col_character()))
df_meta_imptmplt <- 
  read_csv("./data_structure/STUD_ImportTemplate_2019-06-25.csv",
           col_types = cols(.default = col_character()))
df_meta_instdesg <- 
  read_csv("./data_structure/STUD_InstrumentDesignations_2019-06-25.csv",
           col_types = cols(.default = col_character()))


# Pre-Screen ----
# Pre-Screen event: df_prescreen

df_prescreen_id_ren <- 
  tibble(stud_id = ids_summ_sheets,
         redcap_event_name = rep("presceen_arm_1", 
                                 length(ids_summ_sheets)))
# get form names from instrument designations df
prescreen_form_names <-
  df_meta_instdesg %>% 
  filter(unique_event_name == "prescreen_arm_1") %>% 
  pull(form)
# get field names from data dictionary df
prescreen_field_names <-
  df_meta_datadict %>% 
  filter(`Form Name` %in% prescreen_form_names) %>% 
  pull(`Variable / Field Name`)
# build empty df to hold prescreen data
df_prescreen <-
  bind_cols(
    df_prescreen_id_ren,
    as_tibble(matrix(data = NA, 
                     nrow = length(filenames_summ_sheets), 
                     ncol = length(prescreen_field_names),
                     dimnames = list(NULL, prescreen_field_names)))
  )
rm(df_prescreen_id_ren)

# for loop to populate df_prescreen with data from spreadsheets
for (id in df_ids_filenames_data$id) {
  cat(paste0(id, "\n"))
  row_num <- which(df_prescreen$stud_id == id)
  # cat(paste0(row_num, "\n"))
  
  # age
  df_prescreen[[row_num, "age"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[1, 6]]
  # sex
  raw_sex <- df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[1, 8]]
  df_prescreen[[row_num, "sex"]] <- 
    case_when(
      raw_sex == "M" ~ "1",
      raw_sex == "F" ~ "2",
      TRUE ~ NA_character_
    )
  # race
  raw_race <- df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[2, 6]]
  df_prescreen[[row_num, "race"]] <-
    case_when(
      str_detect(raw_race, "[Ww]hite|[Cc]aucasian") ~ "1",
      str_detect(raw_race, "[Bb]lack|[Aa]frican [Aa]merican") ~ "2",
      str_detect(raw_race, "[Aa]merican [Ii]ndian|[Nn]ative [Aa]merican") ~ "3",
      str_detect(raw_race, "[Aa]sian") ~ "4",
      str_detect(raw_race, "[Nn]ative Hawaiian|Pacific [Ii]slander") ~ "5",
      str_detect(raw_race, "[Oo]ther|[Bb]i|[Mm]ulti|[Mm]ixed") ~ "50",
      TRUE ~ "99"
    )
  # hispanic
  raw_hispanic <- df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[2, 8]]
  df_prescreen[[row_num, "hispanic"]] <-
    case_when(
      str_detect(raw_hispanic, "^[Nn]on[\\W-?]([Hh]ispanic|[Ll]atin.?)") ~ "0",
      str_detect(raw_hispanic, "^[Hh]ispanic|[Ll]atin.?") ~ "1",
      TRUE ~ "9"
    )
  # educ
  raw_educ <- df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[2, 3]]
  df_prescreen[[row_num, "educ"]] <-
    case_when(
      str_detect(raw_educ, "^\\W*\\d+\\W*$") ~ str_trim(raw_educ),
      TRUE ~ NA_character_
    )
  # handedness
  raw_handedness <- 
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[1, 12]]
  df_prescreen[[row_num, "handedness"]] <-
    case_when(
      str_detect(raw_handedness, "^[Ll]") ~ "1",
      str_detect(raw_handedness, "^[Rr]") ~ "2",
      str_detect(raw_handedness, "^[Aa]mbi") ~ "3",
      TRUE ~ "9"
    )
}

# Baseline ----
# Baseline Event: df_baseline

df_baseline_id_ren <- 
  tibble(stud_id = ids_summ_sheets,
         redcap_event_name = rep("baseline_arm_1", 
                                 length(ids_summ_sheets)))
# get form names from instrument designations df
baseline_form_names <-
  df_meta_instdesg %>% 
  filter(unique_event_name == "baseline_arm_1") %>% 
  pull(form)
# get field names from data dictionary df
baseline_field_names <-
  df_meta_datadict %>% 
  filter(`Form Name` %in% baseline_form_names) %>% 
  pull(`Variable / Field Name`) %>% 
  # remove extraneous `stud_id` value
  str_remove(pattern = "^stud_id$") %>% stri_remove_empty()
# build empty df to hold prescreen data
df_baseline <-
  bind_cols(
    df_baseline_id_ren,
    as_tibble(matrix(data = NA, 
                     nrow = length(filenames_summ_sheets), 
                     ncol = length(baseline_field_names),
                     dimnames = list(NULL, baseline_field_names)))
  )
rm(df_baseline_id_ren)

# for loop to populate df_baseline with data from spreadsheets
for (id in df_ids_filenames_data$id) {
  cat(paste0(id, "\n"))
  row_num <- which(df_prescreen$stud_id == id)
  # cat(paste0(row_num, "\n"))
  
  # stud_date
  # ummap_id
  raw_ummap_id <- df_ids_filenames_data[[row_num, "UMMAP Summary"]][[2, 2]]
  df_baseline[[row_num, "ummap_id"]] <- 
    paste0("UM", strrep("0", 8-nchar(raw_ummap_id)), raw_ummap_id)
  # ummap_date
  # consent_date
  # consent_form_1, consent_form_2, consent_form_3
  
  # tb_date
  # tb_sessionnum
  
  # Toolbox dfs
  # Toolbox emotion battery
  df_tb_emo <- 
    df_ids_filenames_data[[row_num, "Toolbox (Pre)"]][2:21, 1:7]
  names(df_tb_emo) <-
    c("instrument", "raw", "theta", "tscore", "se", "instrbrk", "instrstat2")
  df_tb_emo <- df_tb_emo %>% 
    mutate(instrument = case_when(
      str_detect(instrument, "Positive Affect")           ~ "posaff",
      str_detect(instrument, "Life Satisfaction")         ~ "lifesat",
      str_detect(instrument, "Meaning and Purpose")       ~ "meanpurp",
      str_detect(instrument, "Emotonal Support")          ~ "emosupp",
      str_detect(instrument, "Instrumental Support")      ~ "instsupp",
      str_detect(instrument, "Friendship")                ~ "friend",
      str_detect(instrument, "Loneliness")                ~ "lonely",
      str_detect(instrument, "Perceived Rejection")       ~ "percrej",
      str_detect(instrument, "Perceived Hostility")       ~ "perchost",
      str_detect(instrument, "Self-Efficacy")             ~ "selfeff",
      str_detect(instrument, "Perceived Stress")          ~ "percstrs",
      str_detect(instrument, "Fear-Affect")               ~ "fearaff",
      str_detect(instrument, "Fear-Somatic Arousal")      ~ "fearsom",
      str_detect(instrument, "Sadness")                   ~ "sadness",
      str_detect(instrument, "Anger-Affect")              ~ "angaff",
      str_detect(instrument, "Anger-Hostility")           ~ "anghost",
      str_detect(instrument, "Anger-Physical Aggression") ~ "angphagr",
      str_detect(instrument, "Affect Summary")            ~ "negaff",
      str_detect(instrument, "Satisfaction Summary")      ~ "socsat",
      str_detect(instrument, "Well Being Summary")        ~ "psywelbe",
      TRUE ~ instrument)) %>% 
    mutate(
      raw        = as.integer(raw),
      theta      = round(as.numeric(theta), 3),
      tscore     = as.integer(tscore),
      se         = as.integer(se),
      instrbrk   = as.integer(instrbrk),
      instrstat2 = as.integer(instrstat2))
  df_tb_emo_long <- df_tb_emo %>% 
    gather(key = "measure", value = "value", -instrument) %>% 
    bind_cols(tibble(name = rep("tb_emo", nrow(.))), .) %>% 
    unite(col = "name_instrument_measure", 
          name, instrument, measure, sep = "_")
  df_tb_emo_wide <- 
    as_tibble(
      matrix(data = pull(df_tb_emo_long, value),
             ncol = nrow(df_tb_emo_long),
             dimnames = list(NULL, 
                             pull(df_tb_emo_long, name_instrument_measure))))
  # Insert df_tb_emo_wide values into df_baseline via fuse_dfs fxn
  df_baseline[row_num, ] <- fuse_dfs(df_baseline[row_num, ], df_tb_emo_wide)

  # Toolbox cognition battery  
  df_tb_cog <- 
    df_ids_filenames_data[[row_num, "Toolbox (Pre)"]][23:28, 1:11]
  names(df_tb_cog) <- 
    c("instrument", 
      "raw", "theta", "se", 
      "cs", "ucss", "acss", "npaa", "fcts",
      "instrbrk", "instrstat2")
  df_tb_cog <- df_tb_cog %>% 
    mutate(instrument = case_when(
      str_detect(instrument, "Flanker")                      ~ "flanker",
      str_detect(instrument, "List Sorting")                 ~ "listsort",
      str_detect(instrument, "Dimensional Change Card Sort") ~ "dccs",
      str_detect(instrument, "Pattern Comparison")           ~ "pattern",
      str_detect(instrument, "Picture Sequence")             ~ "picseq",
      str_detect(instrument, "Cognition Fluid")              ~ "cogfluid",
      TRUE ~ instrument)) %>% 
    mutate(
      raw = as.integer(raw),
      theta = round(as.numeric(theta), 2),
      se = round(as.numeric(se), 2),
      cs = round(as.numeric(cs), 2),
      ucss = as.integer(ucss),
      npaa = as.integer(npaa),
      fcts = as.integer(fcts),
      instrbrk = as.integer(instrbrk),
      instrstat2 = as.integer(instrstat2))
  # df_tb_cog
  # df_tb_cog %>% 
  #   gather(key = "measure", value = "value", -instrument)
  df_tb_cog_long <- df_tb_cog %>% 
    gather(key = "measure", value = "value", -instrument) %>% 
    bind_cols(tibble(name = rep("tb_cog", nrow(.))), .) %>% 
    unite(col = "name_instrument_measure",
          name, instrument, measure, sep = "_")
  df_tb_cog_wide <-
    as_tibble(
      matrix(data = pull(df_tb_cog_long, value),
             ncol = nrow(df_tb_cog_long),
             dimnames = list(NULL, 
                             pull(df_tb_cog_long, name_instrument_measure))))
  # Insert df_tb_cog_wide values into df_baseline via fuse_dfs fxn
  df_baseline[row_num, ] <- fuse_dfs(df_baseline[row_num, ], df_tb_cog_wide)
  
  # N-back
  # nback_sessiondate
  # nback_sessionnum
  # nback0_rawtargstimacc
  df_baseline[[row_num, "nback0_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[12, 8]] %>% 
    as.integer()
  # nback0_targstimacc
  df_baseline[[row_num, "nback0_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[13, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_rawfoilstimacc
  df_baseline[[row_num, "nback0_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[14, 8]] %>% 
    as.integer()
  # nback0_foilstimacc
  df_baseline[[row_num, "nback0_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[15, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_totalacc
  df_baseline[[row_num, "nback0_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[16, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_respavgrt
  df_baseline[[row_num, "nback0_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[17, 8]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback0_dprime
  df_baseline[[row_num, "nback0_dprime"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[18, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  # nback2_rawtargstimacc
  df_baseline[[row_num, "nback2_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[22, 8]] %>% 
    as.integer()
  # nback2_targstimacc
  df_baseline[[row_num, "nback2_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[23, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_rawfoilstimacc
  df_baseline[[row_num, "nback2_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[24, 8]] %>% 
    as.integer()
  # nback2_foilstimacc
  df_baseline[[row_num, "nback2_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[25, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_totalacc
  df_baseline[[row_num, "nback2_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[26, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_respavgrt
  df_baseline[[row_num, "nback2_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[27, 8]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback2_dprime
  df_baseline[[row_num, "nback2_dprime"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[28, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  
  # RBANS
  # rbans_sessiondate
  # rbans_sessionnum
  # rbans_examiner
  # rbans_imm_learn_raw
  df_baseline[[row_num, "rbans_imm_learn_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[7, 2]] %>% 
    as.integer()
  # rbans_imm_learn_z
  df_baseline[[row_num, "rbans_imm_learn_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[7, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_imm_story_raw
  df_baseline[[row_num, "rbans_imm_story_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[8, 2]] %>% 
    as.integer()
  # rbans_imm_story_z
  df_baseline[[row_num, "rbans_imm_story_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[8, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_imm_index
  df_baseline[[row_num, "rbans_imm_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[7, 4]] %>% 
    as.integer()
  # rbans_vis_figcp_raw
  df_baseline[[row_num, "rbans_vis_figcp_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[11, 2]] %>% 
    as.integer()
  # rbans_vis_figcp_z
  df_baseline[[row_num, "rbans_vis_figcp_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[11, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_vis_orien_raw
  df_baseline[[row_num, "rbans_vis_orien_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[12, 2]] %>% 
    as.integer()
  # rbans_vis_orien_z
  df_baseline[[row_num, "rbans_vis_orien_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[12, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_vis_index
  df_baseline[[row_num, "rbans_vis_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[11, 4]] %>% 
    as.integer()
  # rbans_lan_nam_raw
  df_baseline[[row_num, "rbans_lan_nam_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[15, 2]] %>% 
    as.integer()
  # rbans_lan_nam_z
  df_baseline[[row_num, "rbans_lan_nam_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[15, 3]] %>% 
    as.integer()
  # rbans_lan_flu_raw
  df_baseline[[row_num, "rbans_lan_flu_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[16, 2]] %>% 
    as.integer()
  # rbans_lan_flu_z
  df_baseline[[row_num, "rbans_lan_flu_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[16, 3]] %>% 
    as.integer()
  # rbans_lan_index
  df_baseline[[row_num, "rbans_lan_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[15, 4]] %>% 
    as.integer()
  # rbans_att_digsp_raw
  df_baseline[[row_num, "rbans_att_digsp_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[19, 2]] %>% 
    as.integer()
  # rbans_att_digsp_z
  df_baseline[[row_num, "rbans_att_digsp_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[19, 3]] %>% 
    as.integer()
  # rbans_att_cod_raw
  df_baseline[[row_num, "rbans_att_cod_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[20, 2]] %>% 
    as.integer()
  # rbans_att_cod_z
  df_baseline[[row_num, "rbans_att_cod_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[20, 3]] %>% 
    as.integer()
  # rbans_att_index
  df_baseline[[row_num, "rbans_att_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[19, 4]] %>% 
    as.integer()
  # rbans_del_lstrcl_raw
  df_baseline[[row_num, "rbans_del_lstrcl_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[23, 2]] %>% 
    as.integer()
  # rbans_del_lstrcl_z
  df_baseline[[row_num, "rbans_del_lstrcl_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[23, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_del_lstrcg_raw
  df_baseline[[row_num, "rbans_del_lstrcg_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[24, 2]] %>% 
    as.integer()
  # rbans_del_lstrcg_z
  df_baseline[[row_num, "rbans_del_lstrcg_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[24, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_del_strrcl_raw
  df_baseline[[row_num, "rbans_del_strrcl_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[25, 2]] %>% 
    as.integer()
  # rbans_del_strrcl_z
  df_baseline[[row_num, "rbans_del_strrcl_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[25, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_del_figrcl_raw
  df_baseline[[row_num, "rbans_del_figrcl_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[26, 2]] %>% 
    as.integer()
  # rbans_del_figrcl_z
  df_baseline[[row_num, "rbans_del_figrcl_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[26, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_del_index
  df_baseline[[row_num, "rbans_del_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[23, 4]] %>% 
    as.integer()
  # rbans_total_scale
  df_baseline[[row_num, "rbans_total_scale"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[28, 4]] %>% 
    as.integer()
  # rbans_effort_index
  df_baseline[[row_num, "rbans_effort_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[29, 4]] %>% 
    as.integer()
  
  # Paired Associates Task
  # prdasc_sessiondate
  # prdasc_sessionnum
  # prdasc_trn_raw_ttl_targ_stim_acc
  df_baseline[[row_num, "prdasc_trn_raw_ttl_targ_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[33, 4]] %>% 
    as.numeric() %>% round(digits = 2)
  # prdasc_trn_targ_stim_acc
  df_baseline[[row_num, "prdasc_trn_targ_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[34, 4]] %>% 
    as.numeric() %>% round(digits = 2)
  # prdasc_trn_targ_stim_acc_avg_rt_wo0
  df_baseline[[row_num, "prdasc_trn_targ_stim_acc_avg_rt_wo0"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[35, 4]] %>% 
    as.numeric() %>% round(digits = 2)
  # prdasc_tst_raw_targ_acc
  df_baseline[[row_num, "prdasc_tst_raw_targ_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[33, 8]] %>% 
    as.integer()
  # prdasc_tst_targ_stim_acc
  df_baseline[[row_num, "prdasc_tst_targ_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[34, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # prdasc_tst_raw_foil_stim_acc
  df_baseline[[row_num, "prdasc_tst_raw_foil_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[35, 8]] %>% 
    as.integer()
  # prdasc_tst_foil_stim_acc
  df_baseline[[row_num, "prdasc_tst_foil_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[36, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  # prdasc_tst_ttl_acc
  df_baseline[[row_num, "prdasc_tst_ttl_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[37, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  # prdasc_tst_resp_avg_rt_wo0
  df_baseline[[row_num, "prdasc_tst_resp_avg_rt_wo0"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Pre)"]][[38, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  # prdasc_tst_hifreq_hit_rate
  # prdasc_tst_hifreq_miss_rate
  # prdasc_tst_hifreq_false_alarm_rate
  # prdasc_tst_hifreq_crct_rej_rate
  # prdasc_tst_lofreq_hit_rate
  # prdasc_tst_lofreq_miss_rate
  # prdasc_tst_lofreq_false_alarm_rate
  # prdasc_tst_lofreq_crct_rej_rate
  
  # MMQ
  # mmq_sessiondate
  # mmq_sessionnum
  # mmq_feel_pleased
  # mmq_feel_wrong
  # mmq_feel_important
  # mmq_feel_serious
  # mmq_feel_other
  # mmq_feel_confidence
  # mmq_feel_unhappy
  # mmq_feel_notice
  # mmq_feel_nothard
  # mmq_feel_concerned
  # mmq_feel_downhill
  # mmq_feel_satisfied
  # mmq_feel_notupset
  # mmq_feel_worryforget
  # mmq_feel_embarrassed
  # mmq_feel_annoyed
  # mmq_feel_good
  # mmq_feel_worry
  # mmq_mistake_bill
  # mmq_mistake_daily
  # mmq_mistake_telephonelookup
  # mmq_mistake_justmet
  # mmq_mistake_leavebehind
  # mmq_mistake_appointment
  # mmq_mistake_abouttodo
  # mmq_mistake_errand
  # mmq_mistake_word
  # mmq_mistake_article
  # mmq_mistake_medication
  # mmq_mistake_nameknown
  # mmq_mistake_message
  # mmq_mistake_conversation
  # mmq_mistake_birthday
  # mmq_mistake_telephonefrequent
  # mmq_mistake_storyjoke
  # mmq_mistake_misplace
  # mmq_mistake_buy
  # mmq_mistake_recentconversation
  # mmq_strat_timeralarm
  # mmq_strat_askhelp
  # mmq_strat_rhyme
  # mmq_strat_image
  # mmq_strat_calendar
  # mmq_strat_alphabet
  # mmq_strat_organize
  # mmq_strat_outloud
  # mmq_strat_routine
  # mmq_strat_list
  # mmq_strat_elaborate
  # mmq_strat_prominentplace
  # mmq_strat_repeat
  # mmq_strat_story
  # mmq_strat_notebook
  # mmq_strat_acronym
  # mmq_strat_concentrate
  # mmq_strat_note
  # mmq_strat_retrace
  
}

baseline_field_names[str_detect(baseline_field_names, "mmq")] %>% 
  paste0(collapse = "\n") %>% cat()
df_ids_filenames_data[[row_num, "Evaluation (Pre)"]] %>% print(n = 100)


# Session 1 ----
# Session 1: df_session_1

df_session_1_id_ren <- 
  tibble(stud_id = ids_summ_sheets,
         redcap_event_name = rep("session_1_arm_1", 
                                 length(ids_summ_sheets)))
# get form names from instrument designations df
session_1_form_names <-
  df_meta_instdesg %>% 
  filter(unique_event_name == "session_1_arm_1") %>% 
  pull(form)
# get field names from data dictionary df
session_1_field_names <-
  df_meta_datadict %>% 
  filter(`Form Name` %in% session_1_form_names) %>% 
  pull(`Variable / Field Name`) %>% 
  # remove extraneous `stud_id` value
  str_remove(pattern = "^stud_id$") %>% stri_remove_empty()
# build empty df to hold prescreen data
df_session_1 <-
  bind_cols(
    df_session_1_id_ren,
    as_tibble(matrix(data = NA, 
                     nrow = length(filenames_summ_sheets), 
                     ncol = length(session_1_field_names),
                     dimnames = list(NULL, session_1_field_names)))
  )
rm(df_session_1_id_ren)

# for loop to populate df_baseline with data from spreadsheets
for (id in df_ids_filenames_data$id) {
  cat(paste0(id, "\n"))
  row_num <- which(df_prescreen$stud_id == id)
  # cat(paste0(row_num, "\n"))
  
  # nback_sessiondate
  # nback_sessionnum
  df_session_1[[row_num, "nback0_rawtargstimacc"]] <- 1L
  
  # nback0_rawtargstimacc
  df_session_1[[row_num, "nback0_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[5, 2]] %>% 
    as.integer()
  # nback0_targstimacc
  df_session_1[[row_num, "nback0_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[6, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_rawfoilstimacc
  df_session_1[[row_num, "nback0_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[7, 2]] %>% 
    as.integer()
  # nback0_foilstimacc
  df_session_1[[row_num, "nback0_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[8, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_totalacc
  df_session_1[[row_num, "nback0_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[9, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_respavgrt
  df_session_1[[row_num, "nback0_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[10, 2]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback0_dprime
  df_session_1[[row_num, "nback0_dprime"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[11, 2]] %>% 
    as.numeric() %>% round(digits = 6)
  
  # nback2_rawtargstimacc
  df_session_1[[row_num, "nback2_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[16, 2]] %>% 
    as.integer()
  # nback2_targstimacc
  df_session_1[[row_num, "nback2_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[17, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_rawfoilstimacc
  df_session_1[[row_num, "nback2_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[18, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_foilstimacc
  df_session_1[[row_num, "nback2_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[19, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_totalacc
  df_session_1[[row_num, "nback2_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[20, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_respavgrt
  df_session_1[[row_num, "nback2_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[21, 2]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback2_dprime
  df_session_1[[row_num, "nback2_dprime"]] <-
    df_ids_filenames_data[[row_num, "Session 1"]][[22, 2]] %>% 
    as.numeric() %>% round(digits = 6)
  
}

df_ids_filenames_data[[row_num, "Session 1"]] %>% print(n = 100)


# Session 2 ----
# Session 2: df_session_2

df_session_2_id_ren <- 
  tibble(stud_id = ids_summ_sheets,
         redcap_event_name = rep("session_2_arm_1", 
                                 length(ids_summ_sheets)))
# get form names from instrument designations df
session_2_form_names <-
  df_meta_instdesg %>% 
  filter(unique_event_name == "session_2_arm_1") %>% 
  pull(form)
# get field names from data dictionary df
session_2_field_names <-
  df_meta_datadict %>% 
  filter(`Form Name` %in% session_2_form_names) %>% 
  pull(`Variable / Field Name`) %>% 
  # remove extraneous `stud_id` value
  str_remove(pattern = "^stud_id$") %>% stri_remove_empty()
# build empty df to hold prescreen data
df_session_2 <-
  bind_cols(
    df_session_2_id_ren,
    as_tibble(matrix(data = NA, 
                     nrow = length(filenames_summ_sheets), 
                     ncol = length(session_2_field_names),
                     dimnames = list(NULL, session_2_field_names)))
  )
rm(df_session_2_id_ren)

# for loop to populate df_baseline with data from spreadsheets
for (id in df_ids_filenames_data$id) {
  cat(paste0(id, "\n"))
  row_num <- which(df_prescreen$stud_id == id)
  # cat(paste0(row_num, "\n"))
  
  # nback_sessiondate
  # nback_sessionnum
  df_session_2[[row_num, "nback0_rawtargstimacc"]] <- 1L
  
  # nback0_rawtargstimacc
  df_session_2[[row_num, "nback0_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[5, 2]] %>% 
    as.integer()
  # nback0_targstimacc
  df_session_2[[row_num, "nback0_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[6, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_rawfoilstimacc
  df_session_2[[row_num, "nback0_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[7, 2]] %>% 
    as.integer()
  # nback0_foilstimacc
  df_session_2[[row_num, "nback0_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[8, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_totalacc
  df_session_2[[row_num, "nback0_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[9, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_respavgrt
  df_session_2[[row_num, "nback0_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[10, 2]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback0_dprime
  df_session_2[[row_num, "nback0_dprime"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[11, 2]] %>% 
    as.numeric() %>% round(digits = 6)
  
  # nback2_rawtargstimacc
  df_session_2[[row_num, "nback2_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[16, 2]] %>% 
    as.integer()
  # nback2_targstimacc
  df_session_2[[row_num, "nback2_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[17, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_rawfoilstimacc
  df_session_2[[row_num, "nback2_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[18, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_foilstimacc
  df_session_2[[row_num, "nback2_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[19, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_totalacc
  df_session_2[[row_num, "nback2_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[20, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_respavgrt
  df_session_2[[row_num, "nback2_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[21, 2]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback2_dprime
  df_session_2[[row_num, "nback2_dprime"]] <-
    df_ids_filenames_data[[row_num, "Session 2"]][[22, 2]] %>% 
    as.numeric() %>% round(digits = 6)
  
}

df_ids_filenames_data[[row_num, "Session 2"]] %>% print(n = 100)


# Session 3 ----
# Session 3: df_session_3

df_session_3_id_ren <- 
  tibble(stud_id = ids_summ_sheets,
         redcap_event_name = rep("session_3_arm_1", 
                                 length(ids_summ_sheets)))
# get form names from instrument designations df
session_3_form_names <-
  df_meta_instdesg %>% 
  filter(unique_event_name == "session_3_arm_1") %>% 
  pull(form)
# get field names from data dictionary df
session_3_field_names <-
  df_meta_datadict %>% 
  filter(`Form Name` %in% session_3_form_names) %>% 
  pull(`Variable / Field Name`) %>% 
  # remove extraneous `stud_id` value
  str_remove(pattern = "^stud_id$") %>% stri_remove_empty()
# build empty df to hold prescreen data
df_session_3 <-
  bind_cols(
    df_session_3_id_ren,
    as_tibble(matrix(data = NA, 
                     nrow = length(filenames_summ_sheets), 
                     ncol = length(session_3_field_names),
                     dimnames = list(NULL, session_3_field_names)))
  )
rm(df_session_3_id_ren)

# for loop to populate df_baseline with data from spreadsheets
for (id in df_ids_filenames_data$id) {
  cat(paste0(id, "\n"))
  row_num <- which(df_prescreen$stud_id == id)
  # cat(paste0(row_num, "\n"))
  
  # nback_sessiondate
  # nback_sessionnum
  df_session_3[[row_num, "nback0_rawtargstimacc"]] <- 1L
  
  # nback0_rawtargstimacc
  df_session_3[[row_num, "nback0_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[5, 2]] %>% 
    as.integer()
  # nback0_targstimacc
  df_session_3[[row_num, "nback0_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[6, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_rawfoilstimacc
  df_session_3[[row_num, "nback0_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[7, 2]] %>% 
    as.integer()
  # nback0_foilstimacc
  df_session_3[[row_num, "nback0_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[8, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_totalacc
  df_session_3[[row_num, "nback0_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[9, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_respavgrt
  df_session_3[[row_num, "nback0_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[10, 2]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback0_dprime
  df_session_3[[row_num, "nback0_dprime"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[11, 2]] %>% 
    as.numeric() %>% round(digits = 6)
  
  # nback2_rawtargstimacc
  df_session_3[[row_num, "nback2_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[16, 2]] %>% 
    as.integer()
  # nback2_targstimacc
  df_session_3[[row_num, "nback2_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[17, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_rawfoilstimacc
  df_session_3[[row_num, "nback2_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[18, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_foilstimacc
  df_session_3[[row_num, "nback2_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[19, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_totalacc
  df_session_3[[row_num, "nback2_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[20, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_respavgrt
  df_session_3[[row_num, "nback2_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[21, 2]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback2_dprime
  df_session_3[[row_num, "nback2_dprime"]] <-
    df_ids_filenames_data[[row_num, "Session 3"]][[22, 2]] %>% 
    as.numeric() %>% round(digits = 6)
  
}

df_ids_filenames_data[[row_num, "Session 3"]] %>% print(n = 100)


# Session 4 ----
# Session 4: df_session_4

df_session_4_id_ren <- 
  tibble(stud_id = ids_summ_sheets,
         redcap_event_name = rep("session_4_arm_1", 
                                 length(ids_summ_sheets)))
# get form names from instrument designations df
session_4_form_names <-
  df_meta_instdesg %>% 
  filter(unique_event_name == "session_4_arm_1") %>% 
  pull(form)
# get field names from data dictionary df
session_4_field_names <-
  df_meta_datadict %>% 
  filter(`Form Name` %in% session_4_form_names) %>% 
  pull(`Variable / Field Name`) %>% 
  # remove extraneous `stud_id` value
  str_remove(pattern = "^stud_id$") %>% stri_remove_empty()
# build empty df to hold prescreen data
df_session_4 <-
  bind_cols(
    df_session_4_id_ren,
    as_tibble(matrix(data = NA, 
                     nrow = length(filenames_summ_sheets), 
                     ncol = length(session_4_field_names),
                     dimnames = list(NULL, session_4_field_names)))
  )
rm(df_session_4_id_ren)

# for loop to populate df_baseline with data from spreadsheets
for (id in df_ids_filenames_data$id) {
  cat(paste0(id, "\n"))
  row_num <- which(df_prescreen$stud_id == id)
  # cat(paste0(row_num, "\n"))
  
  # nback_sessiondate
  # nback_sessionnum
  df_session_4[[row_num, "nback0_rawtargstimacc"]] <- 1L
  
  # nback0_rawtargstimacc
  df_session_4[[row_num, "nback0_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[5, 2]] %>% 
    as.integer()
  # nback0_targstimacc
  df_session_4[[row_num, "nback0_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[6, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_rawfoilstimacc
  df_session_4[[row_num, "nback0_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[7, 2]] %>% 
    as.integer()
  # nback0_foilstimacc
  df_session_4[[row_num, "nback0_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[8, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_totalacc
  df_session_4[[row_num, "nback0_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[9, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_respavgrt
  df_session_4[[row_num, "nback0_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[10, 2]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback0_dprime
  df_session_4[[row_num, "nback0_dprime"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[11, 2]] %>% 
    as.numeric() %>% round(digits = 6)
  
  # nback2_rawtargstimacc
  df_session_4[[row_num, "nback2_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[16, 2]] %>% 
    as.integer()
  # nback2_targstimacc
  df_session_4[[row_num, "nback2_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[17, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_rawfoilstimacc
  df_session_4[[row_num, "nback2_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[18, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_foilstimacc
  df_session_4[[row_num, "nback2_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[19, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_totalacc
  df_session_4[[row_num, "nback2_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[20, 2]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_respavgrt
  df_session_4[[row_num, "nback2_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[21, 2]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback2_dprime
  df_session_4[[row_num, "nback2_dprime"]] <-
    df_ids_filenames_data[[row_num, "Session 4"]][[22, 2]] %>% 
    as.numeric() %>% round(digits = 6)
  
}

df_ids_filenames_data[[row_num, "Session 4"]] %>% print(n = 100)


# Session 5 / Post ----
# Session 5 / Post Event: df_session_5

df_session_5_id_ren <- 
  tibble(stud_id = ids_summ_sheets,
         redcap_event_name = rep("session_5_arm_1", 
                                 length(ids_summ_sheets)))
# get form names from instrument designations df
session_5_form_names <-
  df_meta_instdesg %>% 
  filter(unique_event_name == "session_5_arm_1") %>% 
  pull(form)
# get field names from data dictionary df
session_5_field_names <-
  df_meta_datadict %>% 
  filter(`Form Name` %in% session_5_form_names) %>% 
  pull(`Variable / Field Name`) %>% 
  # remove extraneous `stud_id` value
  str_remove(pattern = "^stud_id$") %>% stri_remove_empty()
# build empty df to hold prescreen data
df_session_5 <-
  bind_cols(
    df_session_5_id_ren,
    as_tibble(matrix(data = NA, 
                     nrow = length(filenames_summ_sheets), 
                     ncol = length(session_5_field_names),
                     dimnames = list(NULL, session_5_field_names)))
  )
rm(df_session_5_id_ren)

# for loop to populate df_session_5 with data from spreadsheets
for (id in df_ids_filenames_data$id) {
  cat(paste0(id, "\n"))
  row_num <- which(df_prescreen$stud_id == id)
  # cat(paste0(row_num, "\n"))
  
  # stud_date
  # ummap_id
  # raw_ummap_id <- df_ids_filenames_data[[row_num, "UMMAP Summary"]][[2, 2]]
  # df_session_5[[row_num, "ummap_id"]] <-
  #   paste0("UM", strrep("0", 8-nchar(raw_ummap_id)), raw_ummap_id)
  # ummap_date
  # consent_date
  # consent_form_1, consent_form_2, consent_form_3
  
  # tb_date
  # tb_sessionnum
  
  # Toolbox dfs
  # Toolbox emotion battery
  df_tb_emo <- 
    df_ids_filenames_data[[row_num, "Toolbox (Post)"]][2:21, 1:7]
  names(df_tb_emo) <-
    c("instrument", "raw", "theta", "tscore", "se", "instrbrk", "instrstat2")
  df_tb_emo <- df_tb_emo %>% 
    mutate(instrument = case_when(
      str_detect(instrument, "Positive Affect")           ~ "posaff",
      str_detect(instrument, "Life Satisfaction")         ~ "lifesat",
      str_detect(instrument, "Meaning and Purpose")       ~ "meanpurp",
      str_detect(instrument, "Emotonal Support")          ~ "emosupp",
      str_detect(instrument, "Instrumental Support")      ~ "instsupp",
      str_detect(instrument, "Friendship")                ~ "friend",
      str_detect(instrument, "Loneliness")                ~ "lonely",
      str_detect(instrument, "Perceived Rejection")       ~ "percrej",
      str_detect(instrument, "Perceived Hostility")       ~ "perchost",
      str_detect(instrument, "Self-Efficacy")             ~ "selfeff",
      str_detect(instrument, "Perceived Stress")          ~ "percstrs",
      str_detect(instrument, "Fear-Affect")               ~ "fearaff",
      str_detect(instrument, "Fear-Somatic Arousal")      ~ "fearsom",
      str_detect(instrument, "Sadness")                   ~ "sadness",
      str_detect(instrument, "Anger-Affect")              ~ "angaff",
      str_detect(instrument, "Anger-Hostility")           ~ "anghost",
      str_detect(instrument, "Anger-Physical Aggression") ~ "angphagr",
      str_detect(instrument, "Affect Summary")            ~ "negaff",
      str_detect(instrument, "Satisfaction Summary")      ~ "socsat",
      str_detect(instrument, "Well Being Summary")        ~ "psywelbe",
      TRUE ~ instrument)) %>% 
    mutate(
      raw        = as.integer(raw),
      theta      = round(as.numeric(theta), 3),
      tscore     = as.integer(tscore),
      se         = as.integer(se),
      instrbrk   = as.integer(instrbrk),
      instrstat2 = as.integer(instrstat2))
  df_tb_emo_long <- df_tb_emo %>% 
    gather(key = "measure", value = "value", -instrument) %>% 
    bind_cols(tibble(name = rep("tb_emo", nrow(.))), .) %>% 
    unite(col = "name_instrument_measure", 
          name, instrument, measure, sep = "_")
  df_tb_emo_wide <- 
    as_tibble(
      matrix(data = pull(df_tb_emo_long, value),
             ncol = nrow(df_tb_emo_long),
             dimnames = list(NULL, 
                             pull(df_tb_emo_long, name_instrument_measure))))
  # Insert df_tb_emo_wide values into df_session_5 via fuse_dfs fxn
  df_session_5[row_num, ] <- fuse_dfs(df_session_5[row_num, ], df_tb_emo_wide)
  
  # Toolbox cognition battery  
  df_tb_cog <- 
    df_ids_filenames_data[[row_num, "Toolbox (Post)"]][24:29, 1:11]
  names(df_tb_cog) <- 
    c("instrument", 
      "raw", "theta", "se", 
      "cs", "ucss", "acss", "npaa", "fcts",
      "instrbrk", "instrstat2")
  df_tb_cog <- df_tb_cog %>% 
    mutate(instrument = case_when(
      str_detect(instrument, "Flanker")                      ~ "flanker",
      str_detect(instrument, "List Sorting")                 ~ "listsort",
      str_detect(instrument, "Dimensional Change Card Sort") ~ "dccs",
      str_detect(instrument, "Pattern Comparison")           ~ "pattern",
      str_detect(instrument, "Picture Sequence")             ~ "picseq",
      str_detect(instrument, "Cognition Fluid")              ~ "cogfluid",
      TRUE ~ instrument)) %>% 
    mutate(
      raw = as.integer(raw),
      theta = round(as.numeric(theta), 2),
      se = round(as.numeric(se), 2),
      cs = round(as.numeric(cs), 2),
      ucss = as.integer(ucss),
      npaa = as.integer(npaa),
      fcts = as.integer(fcts),
      instrbrk = as.integer(instrbrk),
      instrstat2 = as.integer(instrstat2))
  # df_tb_cog
  # df_tb_cog %>% 
  #   gather(key = "measure", value = "value", -instrument)
  df_tb_cog_long <- df_tb_cog %>% 
    gather(key = "measure", value = "value", -instrument) %>% 
    bind_cols(tibble(name = rep("tb_cog", nrow(.))), .) %>% 
    unite(col = "name_instrument_measure",
          name, instrument, measure, sep = "_")
  df_tb_cog_wide <-
    as_tibble(
      matrix(data = pull(df_tb_cog_long, value),
             ncol = nrow(df_tb_cog_long),
             dimnames = list(NULL, 
                             pull(df_tb_cog_long, name_instrument_measure))))
  # Insert df_tb_cog_wide values into df_session_5 via fuse_dfs fxn
  df_session_5[row_num, ] <- fuse_dfs(df_session_5[row_num, ], df_tb_cog_wide)
  
  # N-back
  # nback_sessiondate
  # nback_sessionnum
  # nback0_rawtargstimacc
  df_session_5[[row_num, "nback0_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[12, 8]] %>% 
    as.integer()
  # nback0_targstimacc
  df_session_5[[row_num, "nback0_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[13, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_rawfoilstimacc
  df_session_5[[row_num, "nback0_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[14, 8]] %>% 
    as.integer()
  # nback0_foilstimacc
  df_session_5[[row_num, "nback0_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[15, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_totalacc
  df_session_5[[row_num, "nback0_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[16, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback0_respavgrt
  df_session_5[[row_num, "nback0_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[17, 8]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback0_dprime
  df_session_5[[row_num, "nback0_dprime"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[18, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  # nback2_rawtargstimacc
  df_session_5[[row_num, "nback2_rawtargstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[22, 8]] %>% 
    as.integer()
  # nback2_targstimacc
  df_session_5[[row_num, "nback2_targstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[23, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_rawfoilstimacc
  df_session_5[[row_num, "nback2_rawfoilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[24, 8]] %>% 
    as.integer()
  # nback2_foilstimacc
  df_session_5[[row_num, "nback2_foilstimacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[25, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_totalacc
  df_session_5[[row_num, "nback2_totalacc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[26, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # nback2_respavgrt
  df_session_5[[row_num, "nback2_respavgrt"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[27, 8]] %>% 
    as.numeric() %>% round(digits = 4)
  # nback2_dprime
  df_session_5[[row_num, "nback2_dprime"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[28, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  
  # RBANS
  # rbans_sessiondate
  # rbans_sessionnum
  # rbans_examiner
  # rbans_imm_learn_raw
  df_session_5[[row_num, "rbans_imm_learn_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[7, 2]] %>% 
    as.integer()
  # rbans_imm_learn_z
  df_session_5[[row_num, "rbans_imm_learn_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[7, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_imm_story_raw
  df_session_5[[row_num, "rbans_imm_story_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[8, 2]] %>% 
    as.integer()
  # rbans_imm_story_z
  df_session_5[[row_num, "rbans_imm_story_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[8, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_imm_index
  df_session_5[[row_num, "rbans_imm_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[7, 4]] %>% 
    as.integer()
  # rbans_vis_figcp_raw
  df_session_5[[row_num, "rbans_vis_figcp_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[11, 2]] %>% 
    as.integer()
  # rbans_vis_figcp_z
  df_session_5[[row_num, "rbans_vis_figcp_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[11, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_vis_orien_raw
  df_session_5[[row_num, "rbans_vis_orien_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[12, 2]] %>% 
    as.integer()
  # rbans_vis_orien_z
  df_session_5[[row_num, "rbans_vis_orien_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[12, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_vis_index
  df_session_5[[row_num, "rbans_vis_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[11, 4]] %>% 
    as.integer()
  # rbans_lan_nam_raw
  df_session_5[[row_num, "rbans_lan_nam_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[15, 2]] %>% 
    as.integer()
  # rbans_lan_nam_z
  df_session_5[[row_num, "rbans_lan_nam_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[15, 3]] %>% 
    as.integer()
  # rbans_lan_flu_raw
  df_session_5[[row_num, "rbans_lan_flu_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[16, 2]] %>% 
    as.integer()
  # rbans_lan_flu_z
  df_session_5[[row_num, "rbans_lan_flu_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[16, 3]] %>% 
    as.integer()
  # rbans_lan_index
  df_session_5[[row_num, "rbans_lan_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[15, 4]] %>% 
    as.integer()
  # rbans_att_digsp_raw
  df_session_5[[row_num, "rbans_att_digsp_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[19, 2]] %>% 
    as.integer()
  # rbans_att_digsp_z
  df_session_5[[row_num, "rbans_att_digsp_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[19, 3]] %>% 
    as.integer()
  # rbans_att_cod_raw
  df_session_5[[row_num, "rbans_att_cod_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[20, 2]] %>% 
    as.integer()
  # rbans_att_cod_z
  df_session_5[[row_num, "rbans_att_cod_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[20, 3]] %>% 
    as.integer()
  # rbans_att_index
  df_session_5[[row_num, "rbans_att_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[19, 4]] %>% 
    as.integer()
  # rbans_del_lstrcl_raw
  df_session_5[[row_num, "rbans_del_lstrcl_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[23, 2]] %>% 
    as.integer()
  # rbans_del_lstrcl_z
  df_session_5[[row_num, "rbans_del_lstrcl_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[23, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_del_lstrcg_raw
  df_session_5[[row_num, "rbans_del_lstrcg_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[24, 2]] %>% 
    as.integer()
  # rbans_del_lstrcg_z
  df_session_5[[row_num, "rbans_del_lstrcg_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[24, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_del_strrcl_raw
  df_session_5[[row_num, "rbans_del_strrcl_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[25, 2]] %>% 
    as.integer()
  # rbans_del_strrcl_z
  df_session_5[[row_num, "rbans_del_strrcl_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[25, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_del_figrcl_raw
  df_session_5[[row_num, "rbans_del_figrcl_raw"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[26, 2]] %>% 
    as.integer()
  # rbans_del_figrcl_z
  df_session_5[[row_num, "rbans_del_figrcl_z"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[26, 3]] %>% 
    as.numeric() %>% round(digits = 2)
  # rbans_del_index
  df_session_5[[row_num, "rbans_del_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[23, 4]] %>% 
    as.integer()
  # rbans_total_scale
  df_session_5[[row_num, "rbans_total_scale"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[28, 4]] %>% 
    as.integer()
  # rbans_effort_index
  df_session_5[[row_num, "rbans_effort_index"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[29, 4]] %>% 
    as.integer()
  
  # Paired Associates Task
  # prdasc_sessiondate
  # prdasc_sessionnum
  # prdasc_trn_raw_ttl_targ_stim_acc
  df_session_5[[row_num, "prdasc_trn_raw_ttl_targ_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[33, 4]] %>% 
    as.numeric() %>% round(digits = 2)
  # prdasc_trn_targ_stim_acc
  df_session_5[[row_num, "prdasc_trn_targ_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[34, 4]] %>% 
    as.numeric() %>% round(digits = 2)
  # prdasc_trn_targ_stim_acc_avg_rt_wo0
  df_session_5[[row_num, "prdasc_trn_targ_stim_acc_avg_rt_wo0"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[35, 4]] %>% 
    as.numeric() %>% round(digits = 2)
  # prdasc_tst_raw_targ_acc
  df_session_5[[row_num, "prdasc_tst_raw_targ_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[33, 8]] %>% 
    as.integer()
  # prdasc_tst_targ_stim_acc
  df_session_5[[row_num, "prdasc_tst_targ_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[34, 8]] %>% 
    as.numeric() %>% round(digits = 2)
  # prdasc_tst_raw_foil_stim_acc
  df_session_5[[row_num, "prdasc_tst_raw_foil_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[35, 8]] %>% 
    as.integer()
  # prdasc_tst_foil_stim_acc
  df_session_5[[row_num, "prdasc_tst_foil_stim_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[36, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  # prdasc_tst_ttl_acc
  df_session_5[[row_num, "prdasc_tst_ttl_acc"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[37, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  # prdasc_tst_resp_avg_rt_wo0
  df_session_5[[row_num, "prdasc_tst_resp_avg_rt_wo0"]] <-
    df_ids_filenames_data[[row_num, "Evaluation (Post)"]][[38, 8]] %>% 
    as.numeric() %>% round(digits = 6)
  # prdasc_tst_hifreq_hit_rate
  # prdasc_tst_hifreq_miss_rate
  # prdasc_tst_hifreq_false_alarm_rate
  # prdasc_tst_hifreq_crct_rej_rate
  # prdasc_tst_lofreq_hit_rate
  # prdasc_tst_lofreq_miss_rate
  # prdasc_tst_lofreq_false_alarm_rate
  # prdasc_tst_lofreq_crct_rej_rate
  
  # MMQ
  # mmq_sessiondate
  # mmq_sessionnum
  # mmq_feel_pleased
  # mmq_feel_wrong
  # mmq_feel_important
  # mmq_feel_serious
  # mmq_feel_other
  # mmq_feel_confidence
  # mmq_feel_unhappy
  # mmq_feel_notice
  # mmq_feel_nothard
  # mmq_feel_concerned
  # mmq_feel_downhill
  # mmq_feel_satisfied
  # mmq_feel_notupset
  # mmq_feel_worryforget
  # mmq_feel_embarrassed
  # mmq_feel_annoyed
  # mmq_feel_good
  # mmq_feel_worry
  # mmq_mistake_bill
  # mmq_mistake_daily
  # mmq_mistake_telephonelookup
  # mmq_mistake_justmet
  # mmq_mistake_leavebehind
  # mmq_mistake_appointment
  # mmq_mistake_abouttodo
  # mmq_mistake_errand
  # mmq_mistake_word
  # mmq_mistake_article
  # mmq_mistake_medication
  # mmq_mistake_nameknown
  # mmq_mistake_message
  # mmq_mistake_conversation
  # mmq_mistake_birthday
  # mmq_mistake_telephonefrequent
  # mmq_mistake_storyjoke
  # mmq_mistake_misplace
  # mmq_mistake_buy
  # mmq_mistake_recentconversation
  # mmq_strat_timeralarm
  # mmq_strat_askhelp
  # mmq_strat_rhyme
  # mmq_strat_image
  # mmq_strat_calendar
  # mmq_strat_alphabet
  # mmq_strat_organize
  # mmq_strat_outloud
  # mmq_strat_routine
  # mmq_strat_list
  # mmq_strat_elaborate
  # mmq_strat_prominentplace
  # mmq_strat_repeat
  # mmq_strat_story
  # mmq_strat_notebook
  # mmq_strat_acronym
  # mmq_strat_concentrate
  # mmq_strat_note
  # mmq_strat_retrace
  
}

session_5_field_names[str_detect(session_5_field_names, "mmq")] %>% 
  paste0(collapse = "\n") %>% cat()
df_ids_filenames_data[[row_num, "Evaluation (Post)"]] %>% print(n = 100)

# df_pet



# Loop to populate 

# Events 
#
### ### ### #
#
# Pre-Screen  - prescreen_arm_1
#   - demographics
#   - inclusion_exclusion_criteria
#
# Baseline    - baseline_arm_1
#   - header
#   - informed_consent
#   - nih_toolbox
#   - nback
#   - rbans
#   - paired_associates
#   - mmq_questionnaire
#   - psqi
#   - fmri
#   - pregnancy_test
#   - biospecimens_collection
#
# Session 1   - session_1_arm_1
# Session 2   - session_2_arm_1
# Session 3   - session_3_arm_1
# Session 4   - session_4_arm_1
# Session 5   - session_5_arm_1
# PET Session - pet_arm_1


# Row-bind all the event dfs together




