library(tidyverse)
library(dplyr)
#(a)
#問1 データの読み込み

semester_dummy_1　<- read_csv("cleaning/raw/semester_dummy/semester_data_1.csv")
head(semester_dummy_1)

semester_dummy_2　<- read_csv("cleaning/raw/semester_dummy/semester_data_2.csv")
head(semester_dummy_2)

#問2　列名の変更
colnames(semester_dummy_1) <- semester_dummy_1[1,]
colnames(semester_dummy_2) <- semester_dummy_1[1,]
semester_dummy_1 <- semester_dummy_1[-1,]

head(semester_dummy_1)

#問3　データの結合
semester_dummy_1$unitid <- as.integer(semester_dummy_1$unitid)
semester_dummy_1$semester <- as.integer(semester_dummy_1$semester)
semester_dummy_1$quarter <- as.integer(semester_dummy_1$quarter)
semester_dummy_1$year <- as.integer(semester_dummy_1$year)
semester_dummy_1$Y <- as.integer(semester_dummy_1$Y)



semester_dummy <- bind_rows(semester_dummy_1,semester_dummy_2)
View(semester_dummy)

#問4 Y列の削除
semester_data　= semester_dummy[,-6]
View(semester_data)

#問5 semester制が導入された年の列を作成
pre_semester_add_yearofsem <- semester_data |>
  group_by(unitid) |>
  mutate(yearofsem =　if_else(all(semester == 1　& quarter == 0), NA, 0)) |>
  ungroup()

View(pre_semester_add_yearofsem)

semester_add_yearofsem <- pre_semester_add_yearofsem |>
  group_by(unitid) |>
  mutate(yearofsem =　if_else(!all(semester == 1　& quarter == 0), min(year[semester == 1 & quarter == 0], na.rm = TRUE), NA)) |>
  ungroup()

View(semester_add_yearofsem)

#問6 semester制が導入された年の列を作成
semester_dummy_data <- semester_add_yearofsem |>
  group_by(unitid) |>
  mutate(after =if_else(year >= yearofsem, 1, 0))|>
  ungroup()
View(semester_dummy_data)
write_csv(semester_dummy_data, file = "cleaning/semester_data.csv")

#(b)
#問1 & 問5　読み込み　結合(2010年までのデータを使用することで代用)
library(openxlsx)
library(purrr)

files <- list.files(path = "cleaning/raw/outcome/", pattern = "\\.xlsx$", full.names = TRUE)

data <- map(files, read.xlsx)

#character型の変換
for (i in seq_along(data)) {
  data[[i]] <- map(data[[i]], as.integer)
}

all_data <- bind_rows(data)

head(all_data)

#問2 女子学生の4年卒業率に0.01をかけて、0から1のスケールに変更

all_data_w <- all_data |>
  mutate(
    women_gradrate_4yr = 0.01 * women_gradrate_4yr
  )

head(all_data_w$women_gradrate_4yr)

#問3 男女合計の4年卒業率と男子学生の4年卒業率を計算し、新たな列として追加
all_data_w_m <- all_data_w |>
  mutate(
    men_gradrate_4yr = m_4yrgrads / m_cohortsize,
    total_gradrate_4yr = tot4yrgrads / totcohortsize
  )

head(all_data_w_m$men_gradrate_4yr)
head(all_data_w_m$total_gradrate_4yr)

#問4 計算した卒業率を有効数字3桁に

all_data_w_m$men_gradrate_4yr <- round(all_data_w_m$men_gradrate_4yr, digits = 2)
all_data_w_m$total_gradrate_4yr <- round(all_data_w_m$total_gradrate_4yr, digits = 2)

head(all_data_w_m$men_gradrate_4yr)
head(all_data_w_m$total_gradrate_4yr)

#問5 1991年から2010年までのデータフレームに変形
graduate_data <- all_data_w_m |>
  filter(!year %in% c(2011, 2012, 2013, 2014, 2015, 2016))

View(graduate_data)
write_csv(graduate_data, file = "cleaning/graduate_data.csv")

#(C)
#問1 生データを読み込みなさい
covariates <- read.xlsx("cleaning/raw/covariates/covariates.xlsx")
head(covariates)

#問2 ’university_id’という列名を’unitid’に変更
covariates <- covariates |>
  rename(unitid　= university_id)

head(covariates)

#問3 ’unitid’に含まれる”aaaa”という文字を削除
covariates$unitid <- str_replace_all(covariates$unitid, "aaaa", "")
head(covariates)

#問4 ‘category’列に含まれる’instatetuition’, ‘costs’, ’faculty’, ’white_cohortsize’を別の列として追加
df_covariates <- covariates |>
  pivot_wider(
    names_from = category,
    values_from = value
  )
View(df_covariates)

#問5 yearが1991~2010までのデータを抽出

df_covariates_from1991to2010 <- df_covariates |>
  filter(!year %in% c(1987,1988,1989,1990,2011, 2012, 2013, 2014, 2015, 2016))
View(df_covariates_from1991to2010)

#問6 outcome_dataに含まれるunitidを特定し、covariatesに含まれるunitidをoutcomeデータに揃えなさい

df_covariates_modified<- df_covariates_from1991to2010　|>
  filter(unitid %in% graduate_data$unitid)
View(df_covariates_modified)

write_csv(covariates_modified, file = "cleaning/covariates_data.csv")

#(e)
#結合に用いる変数を考え、semester_data, covariates_data, gradrate_dataを適切に結合

#型変換
df_covariates_modified$unitid <- as.integer(df_covariates_modified$unitid)
df_covariates_modified$year <- as.integer(df_covariates_modified$year)
df_covariates_modified$instatetuition <- as.integer(df_covariates_modified$instatetuition)
df_covariates_modified$costs <- as.integer(df_covariates_modified$costs)
df_covariates_modified$faculty <- as.integer(df_covariates_modified$faculty)
df_covariates_modified$white_cohortsize <- as.integer(df_covariates_modified$white_cohortsize)
write_csv(df_covariates_modified, file = "cleaning/covariates_data.csv")

join_data<- semester_dummy_data

View(join_data)

graduate_join_data<- join_data |>
  left_join(graduate_data, join_by(unitid,year))
View(graduate_join_data)

covariates_join_data <- graduate_join_data |>
  left_join(df_covariates_modified, join_by(unitid,year))
View(covariates_join_data)

write_csv(covariates_join_data, file = "cleaning/join_data.csv")
