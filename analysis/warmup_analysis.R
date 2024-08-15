
#問1 「(d) Master Dataの作成」で作成したデータの、各列に含まれるNAの数を数えなさい。
data <- read.csv("analysis/join_data.csv")

na_count_data <- sapply(data, function(x) sum(is.na(x)))

na_count_data

#問2 問題背景などを知る上で役に立つ記述統計を作成しなさい
View(data)
#　Never_switchersとSwitchersをfilterしたデータを作成
never_switchers_data <- data |>
  filter(is.na(yearofsem))
View(never_switchers_data)

switchers_data <- data |>
  filter(!is.na(yearofsem))

View(switchers_data)
length(semester_data)


#以下は同じ作業を3種のデータに行えばよいので、Allに適用したもののみを記述
# semester calenderの算出
semester_data <- data$semester
average_semester <- sum(semester_data, na.rm = TRUE) / length(semester_data)
average_semester

std_semester <- sd(semester_data)
std_semester
# Four-year graduation rateの算出
#All
all_four_year_graduation_rate <- sum(data$tot4yrgrads) / sum(data$totcohortsize)
all_four_year_graduation_rate

std_four_year_graduation_rate <- sd(data$tot4yrgrads / data$totcohortsize)
std_four_year_graduation_rate
# Four-year women graduation rateの算出
all_four_year_women_graduation_rate <- sum(data$w_4yrgrads) / sum(data$w_cohortsize)
all_four_year_women_graduation_rate

std_four_year_women_graduation_rate <- sqrt(mean((data$womengradrate4yr - all_four_year_women_graduation_rate)^2, na.rm = TRUE))
std_four_year_women_graduation_rate

# Four-year men graduation rateの算出
all_four_year_men_graduation_rate <- sum(data$m_4yrgrads) / sum(data$m_cohortsize)
all_four_year_men_graduation_rate

std_four_year_men_graduation_rate <- sqrt(mean((data$womengradrate4yr - all_four_year_women_graduation_rate)^2, na.rm = TRUE))
std_four_year_men_graduation_rate

#UBM,SIX-YEAR系統は割愛(該当する変数がなさそうなので)

#Full-time-equivalent faculty
full_time_equivalent_faculty <- mean(data$faculty, na.rm = TRUE)
full_time_equivalent_faculty 

std_full_time_equivalent_faculty <- sd(data$faculty, na.rm = TRUE)
std_full_time_equivalent_faculty
#Cohort size
Cohort_size <- mean(data$totcohortsize, na.rm = TRUE)
Cohort_size
std_Cohort_size <- sd(data$totcohortsize, na.rm = TRUE)
std_Cohort_size
#In-state tuition
Instatetuition <- mean(data$instatetuition, na.rm = TRUE)
Instatetuition

std_Instatetuition <- sd(data$instatetuition, na.rm = TRUE)
std_Instatetuition
#問3(yearでgroupbyしたやつを計算してプロットすればok)
time_series_four_year_graduate_rate_graph <- data |>
  group_by(year) |>
  mutate(average_four_year_graduation_rate = sum(tot4yrgrads) /　sum(totcohortsize) ) |>
  ggplot() +
  geom_line(aes(x = year, y = average_four_year_graduation_rate)) +
  scale_x_continuous(breaks = seq(1990, 2010, by = 5),
                      labels = seq(1990, 2010, by = 5)) +
  scale_y_continuous(breaks = seq(0.25, 0.45, by = 0.05),
                     labels = seq(0.25, 0.45, by = 0.05),
                     limits = c(0.25, 0.45)) +
  labs(x = "year", y = "4-year_graduation_rate") +
  theme_minimal()
  
time_series_four_year_graduate_rate_graph

#問4　semester導入率を計算し、図で示しなさい
time_series_semester_rate_graph <- data |>
  group_by(year) |>
  summarize(
    total_count = n(),
    average_semester_rate = sum(semester) /　total_count ) |>
  ggplot() +
  geom_line(aes(x = year, y = average_semester_rate)) +
  scale_x_continuous(breaks = seq(1990, 2010, by = 5),
                     labels = seq(1990, 2010, by = 5)) +
  scale_y_continuous(breaks = seq(0.85, 1.0, by = 0.05),
                     labels = seq(0.85, 1.0, by = 0.05),
                     limits = c(0.8, 1.0)) +
  labs(x = "year", y = "4-year_graduation_rate") +
  theme_minimal()

time_series_semester_rate_graph

#問5　「4年卒業率」を縦軸にとった散布図を作成

depicting_scatter_plot <- function(data, row){
  graph <- data |>
  ggplot() +
  geom_point(aes_string(x = row, y = "gradrate4yr")) +
  labs(x = row, y = "4-year_graduation_rate") +
  theme_minimal()
  return(graph)
}

column_names <- colnames(data)
print(column_names)

#女子学生比率(間違えて"womengradrate4yr"をもう1列追加してたので、その値を使ってます)
depicting_scatter_plot(data, "womengradrate4yr")

#白人学生比率
depicting_scatter_plot(data, "per_white_cohort")

#学費
depicting_scatter_plot(data, "instatetuition")

#(b)
#回帰分析

fit = lm( formula = gradrate4yr ~ after, data = data )
summary( fit )
