library(dslabs)
data("gapminder")
head(gapminder)
View(gapminder)
str(gapminder)
library(dplyr)
library(ggplot2)

gapminder %>% 
  filter(year == 2015 & country %in% c("Sri Lanka","Turkey")) %>% 
  select(country, infant_mortality)
gapminder %>% 
  filter(year == 2015 & country %in% c("Poland","South Korea")) %>%
  select(country, infant_mortality)
gapminder %>% 
  filter(year == 2015 & country %in% c("Malaysia","Russia")) %>% 
  select(country, infant_mortality)
gapminder %>% 
  filter(year == 2015 & country %in% c("Pakistan","Vietnam")) %>% 
  select(country, infant_mortality)
gapminder %>% 
  filter(year == 2015 & country %in% c("Thailand","South Africa")) %>% 
  select(country, infant_mortality)

gp.reduced<-gapminder %>% 
  filter(year == 2015 ) %>% 
  select(country, starts_with("reg"), infant_mortality)
head(gp.reduced)


years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>% 
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year) 

gapminder %>% 
  filter(country == "United States") %>% 
  ggplot(aes(year,fertility)) +
  geom_line()

tidy_data <- gapminder %>% 
  filter(country %in% 
           c("South Korea", "Germany","United States")) %>%
  select(country, year, fertility)
head(tidy_data)
tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

library(tidyverse)
wide_data <- read_csv("DataSets/fertility-two-countries-example.csv")
head(wide_data)         
View(wide_data)
select(wide_data, country, `1960`:`1967`)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
new_tidy_data
class(new_tidy_data$year)
View(new_tidy_data)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

new_tidy_data <- wide_data %>%
  pivot_longer(-country,names_to = "year",values_to = "fertility")
class(new_tidy_data$year)


new_wide_data <- new_tidy_data %>% 
  spread(year, fertility)
new_wide_data
View(new_wide_data)
select(new_wide_data, country, `1960`:`1967`)

new_wide_data <- new_tidy_data %>% 
  pivot_wider(names_from=year, values_from=fertility)
select(new_wide_data, country, `1960`:`1967`)


billboard
View(billboard)
new_billboard <- billboard %>% 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  )
View(new_billboard)

new_billboard <-billboard %>% 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    names_prefix = "wk",
    names_ptypes = list(week = integer()),
    values_to = "rank",
    values_drop_na = TRUE,
  )
View(new_billboard)


who
View(who)
new_who<-who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = c("diagnosis", "gender", "age"), 
    names_pattern = "new_?(.*)_(.)(.*)",
    values_to = "count"
  )
View(new_who)

View(anscombe)
anscombe %>%
  pivot_longer(everything(),
               names_to = c(".value", "set"),
               names_pattern = "(.)(.)"
  )



raw_dat <- read_csv("DataSets/life-expectancy-and-fertility-two-countries-example.csv")
View(raw_dat)
select(raw_dat,1:5)
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
View(dat)
head(dat %>% separate(key, c("year", "variable_name"), "_"))

head(dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                      fill = "right"))

head(dat %>% separate(key, c("year", "variable_name"), extra = "merge"))

dat %>% 
  separate(key, c("year", "variable_name"), extra = "merge") %>%
  spread(variable_name, value) 

dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right")


co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
View(co2_wide)

co2_tidy<-co2_wide %>%
  gather(month, co2, -year, convert=TRUE)
View(co2_tidy)
class(co2_tidy$month)

co2_tidy %>%
  ggplot(aes(month, co2, col=year)) + geom_line()


admissions
View(admissions)
dat <- admissions %>% select(-applicants)
View(dat)
dat_tidy <- dat %>% spread(major,admitted)
View(dat_tidy)
tmp <- admissions %>% gather(key,value,`admitted`:`applicants`)
tmp
tmp <- tmp %>% unite(column_name,gender,key)
tmp
dat_tidy<-tmp %>% spread(column_name,value)
dat_tidy
admissions %>% 
  gather(key,value,`admitted`:`applicants`) %>%
  unite(column_name,gender,key) %>%
  spread(column_name,value)

admissions %>% 
  gather(key,value,`admitted`:`applicants`) %>%
  unite(column_name,gender,key) %>%
  spread(column_name,value) %>% 
  ggplot(aes(men_admitted/men_applicants,women_admitted/women_applicants,col=major))+geom_point()



family <- tribble(
  ~family,  ~dob_child1,  ~dob_child2, ~gender_child1, ~gender_child2,
  1L, "1998-11-26", "2000-01-29",             1L,             2L,
  2L, "1996-06-22",           NA,             2L,             NA,
  3L, "2002-07-11", "2004-04-05",             2L,             2L,
  4L, "2004-10-10", "2009-08-27",             1L,             1L,
  5L, "2000-12-05", "2005-02-28",             2L,             1L,
)
family
View(family)
family <- family %>% mutate_at(vars(starts_with("dob")), parse_date)
family
family %>% 
  pivot_longer(
    -family, 
    names_to = c(".value", "child"), 
    names_sep = "_", 
    values_drop_na = TRUE
  )



data(heights)
head(heights)
str(heights)
heights %>% 
  filter(sex == "Female") %>%
  summarize(
    average = mean(height),
    standard_deviation = sd(height)
  )

s <- heights %>%
  summarize(
    median = median(height),
    mad=mad(height),
    min=min(height),
    max=max(height))
s

View(murders)
s <- murders %>% 
  mutate(rate=total/population*100000) %>%
  summarize(mean(rate))
str(s)

s
s %>% .$rate

s <- murders %>% 
  summarize(rate=mean(total/population*100000)) %>%
  .$rate
s

s2<-murders %>% 
  summarize(total=sum(total)) %>%
  .$total
s2


tab.USA<-tab %>% 
  filter(year=="2015") %>% 
  select(-year) %>% 
  add_row(country = "USA", 
          continent = "Americas", 
          total = s2,
          murder_rate=s)

tab.USA %>% 
  group_by(continent) %>% 
  summarize(mean.rate=mean(murder_rate)) %>%     arrange(desc(mean.rate))

murders %>% arrange(population) %>% head()
murders %>% arrange(total) %>% head()
murders %>% mutate(rate=total/population*100000)%>%
  arrange(rate) %>% 
  head()
murders %>% mutate(rate=total/population*100000)%>%
  arrange(total,rate) %>% 
  head()

murders %>% mutate(rate=total/population*100000)%>%
  arrange(desc(rate)) %>%
  top_n(10)


install.packages("NHANES")
library(NHANES)
data(NHANES)
View(NHANES)


ref<-NHANES %>%
  filter(Gender=="female", AgeDecade==" 20-29") %>%
  summarize(average_BPSysAve=mean(BPSysAve, na.rm=TRUE))
ref

NHANES %>% 
  filter(Gender=="female" & AgeDecade==" 20-29") %>%
  select(BPSysAve) %>% filter(!is.na(BPSysAve)) %>% summarize(mean(BPSysAve))

ref_avg<-NHANES %>% 
  filter(Gender=="female" & AgeDecade==" 20-29") %>%
  select(BPSysAve) %>% 
  filter(!is.na(BPSysAve)) %>% 
  summarize(mean(BPSysAve)) %>% 
  pull()
ref_avg

min_max<-NHANES %>%
  filter(Gender=="female") %>%
  group_by(AgeDecade) %>%
  summarize(min(BPSysAve, na.rm=TRUE), max(BPSysAve, na.rm=TRUE))
min_max

mean_sd<-NHANES %>%
  filter(Gender=="female") %>%
  group_by(AgeDecade) %>%
  summarize(mean(BPSysAve, na.rm=TRUE), sd(BPSysAve, na.rm=TRUE))
mean_sd

mean_sd2<-NHANES %>%
    group_by(AgeDecade,Gender) %>%
  summarize(mean(BPSysAve, na.rm=TRUE), sd(BPSysAve, na.rm=TRUE))
mean_sd2

x <- NHANES %>%
  filter(Gender=="male", AgeDecade==" 40-49") %>%
  group_by(Race1) %>%
  summarize(mean(BPSysAve, na.rm=TRUE)) %>% arrange
x            



x <- c(-2, -1, 0, 1, 2)
case_when(x < 0 ~ "Negative", 
          x > 0 ~ "Positive", 
          TRUE ~ "Zero")

data(murders)
View(murders)
murders %>% 
  mutate(group = case_when(
    abb %in% c("ME", "NH", "VT", "MA", "RI", "CT") ~ "New England",
    abb %in% c("WA", "OR", "CA") ~ "West Coast",
    region == "South" ~ "South",
    TRUE ~ "other")) %>%
  group_by(group) %>%
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  arrange(rate)


install.packages("janitor")
library(janitor)
test_df <- as.data.frame(matrix(ncol = 6))
names(test_df) <- c("firstName", "ábc@!*", "% successful (2009)",
                    "REPEAT VALUE", "REPEAT VALUE", "")

test_df %>%
  clean_names()

make.names(names(test_df))

x <- c("big", "big", "small", "small", "small", NA)
tabyl(x)

mtcars %>%
  tabyl(gear, cyl) %>%
  adorn_totals("col") %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() %>%
  adorn_title()

mtcars %>% adorn_totals("col") %>% adorn_percentages("col") %>% head()


humans <- starwars %>%
  filter(species == "Human")

t1 <- humans %>%
  tabyl(eye_color)
t1
t1 %>%
  adorn_totals("row") %>%
  adorn_pct_formatting()

t2 <- humans %>%
  tabyl(gender, eye_color)
t2
t2 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

t3 <- humans %>%
  tabyl(eye_color, skin_color, gender)
t3

humans %>%
  tabyl(eye_color, skin_color, gender, show_missing_levels = FALSE) %>%
  adorn_totals("row") %>%
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title


humans %>%
  tabyl(gender, eye_color) %>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>% 
  adorn_pct_formatting(rounding = "half up", digits = 0) %>%
  adorn_ns() %>%
  adorn_title("combined") %>%
  knitr::kable()

mpg_by_cyl_and_am <- mtcars %>%
  group_by(cyl, am) %>%
  summarise(mpg = mean(mpg)) %>%
  spread(am, mpg)

mpg_by_cyl_and_am

mpg_by_cyl_and_am %>%
  adorn_rounding() %>%
  adorn_ns(
    ns = mtcars %>% # calculate the Ns on the fly by calling tabyl on the original data
      tabyl(cyl, am)
  ) %>%
  adorn_title("combined", row_name = "Cylinders", col_name = "Is Automatic")

get_dupes(mtcars, wt, cyl)

q <- data.frame(v1 = c(1, NA, 3),
                v2 = c(NA, NA, NA),
                v3 = c("a", NA, "b"))
q %>%
  remove_empty(c("rows", "cols"))

nums <- c(2.5, 3.5)
round(nums)
round_half_up(nums)

# Count factor levels in groups of high, medium, and low with top_levels()

f <- factor(
  c("strongly agree", "agree", "neutral", "neutral", "disagree", "strongly agree"),
  levels = c("strongly agree", "agree", "neutral", "disagree", "strongly disagree"))
top_levels(f)

top_levels(f, n = 1)
