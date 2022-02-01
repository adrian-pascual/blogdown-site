---
title: Skateboarding and Statistics
author: R package build
date: '2022-01-30'
slug: []
categories: []
tags: []
authors: []
description: ''
externalLink: ''
series: []
---


Although it might be hard to believe, 2021 was a fantastic year for the skateboarding industry. While [manufacturing issues](https://www.jenkemmag.com/home/2021/01/07/look-skateboard-drought-2020/) plagued the market during the early days of the pandemic, demand for skate products has been [skyrocketing](https://capitolweekly.net/ca-skateboarders-and-covid-19-demand-up-equipment-down/). Skateboarding's [Olympic debut](https://www.cbsnews.com/news/momiji-nishiya-japan-olympics-skateboarding-gold-medal/) only added to the sport's mainstream appeal. 

With competitive skateboarding reaching its apex, I contacted [The Boardr](https://theboardr.com/) (a company that organizes and scores skateboarding's top events) to see what data could be available from recent contests and their participants. Luckily, The Boardr was quick to respond with a surprisingly detailed data set! Below I use the coding language R to take a look at cleaning, exploring, and visualizing data on 3,000+ skate events from 2017 to May of 2020. 

## Cleaning the data 

First I started by taking a quick look at the raw file to understand how the data was formatted. 




```r
glimpse(import)
## Rows: 52,110
## Columns: 13
## $ ContestID          <int> 3329, 3329, 3329, 3329, 3329, 3329, 3329, 3329, 332…
## $ ContestName        <chr> "Red Bull Hart Lines Qualifiers ", "Red Bull Hart L…
## $ ContestDate        <chr> "2017-05-13T13:00:00", "2017-05-13T13:00:00", "2017…
## $ SkaterID           <int> 3497, 4130, 1393, 1549, 2455, 1933, 179, 947, 1222,…
## $ Place              <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, …
## $ Skater             <chr> "Alec Majerus", "Kelvin Hoefler", "Chase Webb", "To…
## $ ProfileLink        <chr> "https://theboardr.com/profile/3497/Alec_Majerus", …
## $ Mugshot            <chr> "https://theboardr.blob.core.windows.net/headshots/…
## $ AgeAtDateOfContest <int> 21, 24, 22, 28, 26, 23, 27, 31, 31, 32, 27, 27, 30,…
## $ CurrentAgeToday    <int> 24, 27, 25, 31, 29, 26, 30, 34, 34, 35, 30, 30, 33,…
## $ SkaterCountry      <chr> "United States", "Brazil", "United States", "Austra…
## $ PrizeMoney         <dbl> 0, 0, 0, 0, 0, 0, 0, 1000, 1000, 1000, 1000, 1000, …
## $ HighRunScore       <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
```

This data doesn't look so bad! First, I renamed the field names to be lower case and removed the fields "Profile Link" and "Mugshot". I standardized all the event titles to be proper case and removed their punctuation (a little easier to read). Some skaters were listed as being zero years old, I replaced their age with a "NULL" value. There were a couple folks with more that one "SkaterID", I picked one ID out of the two and removed the other row. 


```r
# Clean and rename fields -------------------------------------------------
raw <- import %>%  
  select(contest = ContestName,
         contest_id = ContestID,
         date = ContestDate,
         skater_id = SkaterID,
         skater = Skater,
         place = Place,
         age = AgeAtDateOfContest,
         current_age = CurrentAgeToday,
         prize = PrizeMoney,
         best_run = HighRunScore,
         skater_country = SkaterCountry) %>%
  mutate(contest = str_to_title(contest),
         contest = str_remove_all(contest, '[[:punct:]]'),
         prize = currency(prize, "$", digits = 0),
         date = ymd_hms(date),
         age = case_when(age == 0 ~ NA_integer_, T ~ age),
         current_age = case_when(current_age == 0 ~ NA_integer_, T ~ current_age)) %>% 
  # Remove skaters with multiple skater_ids 
  group_by(skater) %>% 
  mutate(unique_id = n_distinct(skater_id)) %>%
  filter(unique_id == 1) %>% 
  ungroup()
```


Let's take a look at the contest name format. 


```
## [1] " Tws Cut At Los Angeles Finals"                           
## [2] "18th Asian Games Japan Park Championships Mens Finals"    
## [3] "18th Asian Games Japan Park Championships Mens Qualifiers"
## [4] "18th Asian Games Japan Park Championships Womens Finals"  
## [5] "2017 Daughters Of Doom  Open"
```

Gender isn't included as a variable in the initial data set. I used REGEX to identify whether a contest was male or female by detecting strings like "Woman", "Man, "Male", etc in the contest title. Then I created a separate variable labeled "gender" that contained either "F" or "M" depending on the type of event. 


```r
# List of gender specific terms
s <- c('(Male','Men','Man','Boy', 'Hombre',
       'Female','Women','Woman','Girl','Ladies','Mujer','Femenino','Daughters)')

# New field with gender
df_gender <- raw %>%
  # Rename two contests that were mis-categorized by gender 
  mutate(contest = str_replace_all(contest, "Jsg  Jerusalem Skater Girls", "Jsg Jerusalem Skater"),
         contest = str_replace_all(contest, "King Of Street Mansfield Ladies", "Ladies King Of Street Mansfield")) %>% 
  extract(contest, into = 'gender',
          regex = regex(str_c(s, collapse = '|')),
          remove = F) %>%
  mutate(gender = str_replace_all(gender, '(Female|Women|Woman|Girl|Ladies|Mujer|Femenino|Daughters)', 'F'),
         gender = str_replace_all(gender, '(Male|Men|Man|Boy|Hombre)', 'M')) 

# Unique gender per skater
gender_na <- df_gender %>% 
  distinct(skater_id, gender) %>% 
  drop_na() 

# Replace NAs form unknown contests with values filled in earlier regex 
df_final <- left_join(df_gender, gender_na, by = "skater_id", suffix = c("_a", "_b")) %>% 
  mutate(gender_b = replace_na(gender_b, "M")) %>% 
  rename(gender = gender_b) %>% 
  select(-gender_a) %>% 
  group_by(skater_id) %>% 
  filter(n_distinct(gender) < 2)
```

The Boardr had also mentioned that the data includes BMX and skateboarding competitions. However, I'm only interested in the skating events. The BMX contests have the string "BMX" in their title, so I used REGEX to extract those events and place them in a separate data set. 


```r
# Data frame for BMX
bmx <- df_final %>% 
  filter(str_detect(contest, "(?i)bmx"))

# Data frame for skateboarding
skate <- df_final %>% 
  filter(str_detect(contest, "(?i)^((?!bmx).)*$"))
```

### Gender Pay Gap Analysis

After cleaning all the data, I was curious to know if men and women were compensated equally in competitive skating. Professional female skaters like Cara-Beth Burnside and Jen O’Brien have pushed for fair compensation in contests like the X Games [since at least 2005](https://www.nytimes.com/2006/07/26/sports/othersports/26skateboard.html). How much has changed in the past 15 years? First, I decide to take a quick look at the gender breakdown of all the skaters in the data set.



<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-8-1.png" width="672" />

Wow, contest skating is overwhelmingly male! No surprises here to veteran skaters or folks who follow the skate contests broadcast on television or the internet. What about the age differences between men and women?

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-9-1.png" width="672" />

Interestingly, women on average start competing two years earlier than men. This is a good sign for the growing popularity and diversity of skateboarding as a whole. However, when I looked at the distribution by gender of the total prize money awarded in a year, there were some disappointing results.


```
## # A tibble: 4 × 4
## # Groups:   year [4]
##    year F          M          `pay gap` 
##   <dbl> <formttbl> <formttbl> <formttbl>
## 1  2020 $5,000     $132,000   $127,000  
## 2  2019 $1,425,950 $1,810,225 $384,275  
## 3  2018 $1,228,100 $2,398,015 $1,169,915
## 4  2017 $457,275   $1,976,722 $1,519,447
```

In the earlier years of the data set, women were only earning 25% of the total prize money given to men. For 2020 in particular, I only had access to data from January to September. This lack of data coupled with the global COVID-19 pandemic made the prize money awarded in 2020 an unreliable representation of the gender pay gab as a whole. 

To investigate this disparity, I decided to examine the highest earning male and female skateboarders according to their all time winnings. 


```
## # A tibble: 5 × 2
##   skater          total_prize_money
##   <chr>           <formttbl>       
## 1 Sakura Yosozumi $238,500         
## 2 Pamela Rosa     $188,000         
## 3 Brighton Zeuner $177,775         
## 4 Jordyn Barratt  $163,500         
## 5 Lizzie Armanto  $163,050
```

```
## # A tibble: 5 × 2
##   skater         total_prize_money
##   <chr>          <formttbl>       
## 1 Nyjah Huston   $596,000         
## 2 Yuto Horigome  $271,250         
## 3 Tom Schaar     $270,500         
## 4 Pedro Barros   $260,500         
## 5 Kelvin Hoefler $235,750
```

The top earning male skateboarder in this data set, Nyjah Huston, made close to triple to total earnings of his female counterpart. 

### Skaters by Country 

From humble roots in 1960s California USA to center stage at the Olympics, skateboarding has attracted millions of practitioners around the world. The vast majority of skateboard competitors in this data set is American, although there could be some sample bias since the contest organizers at the Boardr who provided this data are based in the states.

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-13-1.png" width="672" />

While Brazil is fifth in terms total competitive skaters, Brazilians have earned more prize money than almost any other country. Moreover, Japan isn't even in the top five for the most competitive skaters, yet they they came in third in total earnings! 

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-14-1.png" width="672" />

### Final Thoughts

Having skateboarded myself for the past 12 years, I was thrilled to be able to get my hands on some data about a activity that has given me so much pleasure in life. Digging through this data, it's apparent that skating is  primarily male and American dominated in contests. That being said, as younger women join the skate scene and other countries find success ([only two out of nine Olympic medalists were American in 2021](https://www.nytimes.com/2021/08/04/sports/olympics/japan-skateboarding-gold.html)), there should be some optimism for developing a more diverse competitive environment. This won't happen without pros and the public demanding equitable compensation for the women, as well as sponsors recognizing the importance of supporting underrepresented skaters. 
