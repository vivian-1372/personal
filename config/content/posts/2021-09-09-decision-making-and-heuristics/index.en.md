---
title: Decision-making and Heuristics
author: Vivian Nguyen
date: '2021-09-09'
slug: []
categories:
  - Political Psychology
tags: []
draft: yes
---



# Introduction
This is the first Data Exploration task of Fall 2021! Welcome. This week, my classmates of Government 1372 (Political Psychology) and I worked with our in-class survey results to observe how susceptible we are to cognitive biases.


# Part 1: Cognitive Biases

You may have noticed that the questions on the survey you took during class last week were based on the Kahneman (2003) reading you did for this week. The goal for this set of questions is to examine those data to see if you and your classmates exhibit the same cognitive biases that Kahneman wrote about. The data you generated is described below.

**Data Details:**

* File Name: \texttt{bias\_data.csv}

* Source: These data are from the in-class survey you took last week. 

Variable Name         | Variable Description
--------------------- | --------------------------------------
\texttt{id}         | Unique ID for each respondent
\texttt{rare\_disease\_prog}  | From the rare disease problem, the program chosen by the respondent (either 'Program A' or 'Program B')
\texttt{rare\_disease\_cond}  | From the rare disease problem, the framing condition to which the respondent was assigned (either 'save' or 'die')
\texttt{linda}  | From the Linda problem, the option the respondent thought most probable, either "teller" or "teller and feminist"
\texttt{cab}  | From the cab problem, the respondent's estimate of the probability the car was blue
\texttt{gender}  | One of "man", "woman", "non-binary", or "other"
\texttt{year}  | Year at Harvard
\texttt{college\_stats}  | Indicator for whether or not the respondent has taken a college-level statistics course

Before you get started, make sure you replace "file_name_here_1.csv" with the name of the file. (Also, remember to make sure you have saved the .Rmd version of this file and the file with the data in the same folder.)

```r
# load the class-generated bias data
bias_data <- read_csv("bias_data.csv")
```


## Question 2
Now let's move on to the Linda problem. As we read in Kahneman (2003), answers to this problem tend to exhibit a pattern called a "conjunction fallacy" whereby respondents overrate the probability that Linda is a bank teller \textit{and} a feminist rather than just a bank teller. From probability theory, we know that the conjunction of two events A and B can't be more probable than either of the events occurring by itself; that is, `\(P(A) \ge P(A \wedge B)\)` and `\(P(B) \ge P(A \wedge B)\)`\footnote{The symbol `\(\wedge\)` is used in logical expressions to mean "AND". If there are two conditions, A and B, then `\(A \wedge B\)` is true only when both A and B are separately true. The expression `\(P(A) \ge P(A \wedge B)\)` is therefore interpreted as: "The probability A is true is greater than or equal to the probability that both A and B are true.}.

**What proportion of the class answered this question correctly? Why do you think people tend to choose the wrong option?**


```r
linda_correct <- bias_data %>%
  group_by(linda) %>%
  summarise(count = n(), prop = n() / 85)

linda_correct
```

```
## # A tibble: 2 × 3
##   linda               count  prop
##   <chr>               <int> <dbl>
## 1 teller                 60 0.706
## 2 teller and feminist    25 0.294
```
70.59% of the class answered this question correctly. While the majority of our class was able to choose the right answer, many people in general tend to choose the wrong option. This occurs because many people think that given information about someone, the information makes something more likely about them. When we know that Linda is interested in social activism, we think it is not surprising for her to be a feminist. Most people, however, are not aware of the probability principles of the Linda question -- or they simply ignore the principle because the seemingly logical connection between what we know about Linda and what could be true is too convincing in the moment.

## Question 3

**What attributes of the respondents do you think might affect how they answered the Linda problem and why? Using the data, see if your hypothesis is correct.**

```r
linda_cab <- bias_data %>% 
  group_by(linda) %>%
  summarise(count = n(), cab_avg_guess = mean(cab))
linda_cab
```

```
## # A tibble: 2 × 3
##   linda               count cab_avg_guess
##   <chr>               <int>         <dbl>
## 1 teller                 60        NA    
## 2 teller and feminist    25         0.685
```

```r
linda_cab_T <- bias_data %>% 
  group_by(ints = cut_width(cab, width = .10, boundary = 0)) %>%
  summarise(T = mean(linda == "teller"))
linda_cab_T
```

```
## # A tibble: 10 × 2
##    ints          T
##    <fct>     <dbl>
##  1 [0.1,0.2] 0.882
##  2 (0.2,0.3] 0.667
##  3 (0.3,0.4] 0.5  
##  4 (0.4,0.5] 1    
##  5 (0.5,0.6] 1    
##  6 (0.6,0.7] 0.636
##  7 (0.7,0.8] 0.677
##  8 (0.8,0.9] 0.167
##  9 (0.9,1]   0    
## 10 <NA>      1
```

```r
linda_cab_TaF <- bias_data %>% 
  group_by(ints = cut_width(cab, width = .10, boundary = 0)) %>%
  summarise(TaF = mean(linda == "teller and feminist"))
linda_cab_TaF
```

```
## # A tibble: 10 × 2
##    ints        TaF
##    <fct>     <dbl>
##  1 [0.1,0.2] 0.118
##  2 (0.2,0.3] 0.333
##  3 (0.3,0.4] 0.5  
##  4 (0.4,0.5] 0    
##  5 (0.5,0.6] 0    
##  6 (0.6,0.7] 0.364
##  7 (0.7,0.8] 0.323
##  8 (0.8,0.9] 0.833
##  9 (0.9,1]   1    
## 10 <NA>      0
```


```r
CrossTable(x = bias_data$linda, y = bias_data$college_stats, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
```

```
##    Cell Contents 
## |-------------------------|
## |                       N | 
## |           N / Col Total | 
## |-------------------------|
## 
## ============================================
##                        bias_data$college_stats
## bias_data$linda           No     Yes   Total
## --------------------------------------------
## teller                    19      41      60
##                        0.613   0.759        
## --------------------------------------------
## teller and feminist       12      13      25
##                        0.387   0.241        
## --------------------------------------------
## Total                     31      54      85
##                        0.365   0.635        
## ============================================
```

```r
ggplot(bias_data, aes(x = bias_data$cab, y = bias_data$linda)) +
    geom_vline(aes(xintercept = .413), col = "navy") + 
    geom_point()
```

```
## Warning: Removed 4 rows containing missing values (`geom_point()`).
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-3-1.png" width="672" />

```r
ggplot(data = bias_data, mapping = aes(x = bias_data$cab, fill = bias_data$linda)) + 
  geom_vline(aes(xintercept = .413), col = "navy") + 
  geom_bar()
```

```
## Warning: Removed 4 rows containing non-finite values (`stat_count()`).
```

<img src="{{< blogdown/postref >}}index.en_files/figure-html/unnamed-chunk-3-2.png" width="672" />

```r
CrossTable(x = bias_data$linda, y = bias_data$gender, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
```

```
##    Cell Contents 
## |-------------------------|
## |                       N | 
## |           N / Col Total | 
## |-------------------------|
## 
## =========================================================
##                        bias_data$gender
## bias_data$linda          Man   Non-binary   Woman   Total
## ---------------------------------------------------------
## teller                    36            1      23      60
##                        0.750        0.500   0.657        
## ---------------------------------------------------------
## teller and feminist       12            1      12      25
##                        0.250        0.500   0.343        
## ---------------------------------------------------------
## Total                     48            2      35      85
##                        0.565        0.024   0.412        
## =========================================================
```

```r
CrossTable(x = bias_data$linda, y = bias_data$year, prop.r = FALSE, prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE)
```

```
##    Cell Contents 
## |-------------------------|
## |                       N | 
## |           N / Col Total | 
## |-------------------------|
## 
## ============================================================
##                        bias_data$year
## bias_data$linda            1       2       3      4+   Total
## ------------------------------------------------------------
## teller                     2      16      29      13      60
##                        0.500   0.696   0.707   0.765        
## ------------------------------------------------------------
## teller and feminist        2       7      12       4      25
##                        0.500   0.304   0.293   0.235        
## ------------------------------------------------------------
## Total                      4      23      41      17      85
##                        0.047   0.271   0.482   0.200        
## ============================================================
```

College statistic education, age, responses to the cab question could correlate to students' answers to the Linda question. 

## Question 4: Data Science Question
Now we will take a look at the taxi cab problem. This problem, originally posed by Tversky and Kahneman in 1977, is intended to demonstrate what they call a "base rate fallacy". To refresh your memory, here is the text of the problem, as you saw it on the survey last week:

`\begin{quote}
  A cab was involved in a hit and run accident at night. Two cab companies, the Green and the Blue, operate in the city. 85% of the cabs in the city are Green and 15% are Blue.
  
  A witness identified the cab as Blue. The court tested the reliability of the witness under the same circumstances that existed on the night of the accident and concluded that the witness correctly identified each one of the two colours 80% of the time and failed 20% of the time.
  
  What is the probability that the cab involved in the accident was Blue rather than Green knowing that this witness identified it as Blue?
\end{quote}`

The most common answer to this problem is .8. This corresponds to the reliability of the witness, without regard for the base rate at which Blue cabs can be found relative to Green cabs. In other words, respondents tend to disregard the base rate when estimating the probability the cab was Blue.




**What is the true probability the cab was Blue? Visualize the distribution of the guesses in the class using a histogram. What was the most common guess in the class?**

p(blue car | blue ID) = p(blue ID | blue car) * p(blue car) / p(blue ID)

p(blue ID) = (p(blue ID | blue car) * p(blue car)) + (p(blue ID | green car) * p(green car)) = (0.8 * 0.15) + (0.2 * 0.85) 

p = 0.8 * 0.15 / 0.1 = ~0.413



For this week’s data exploration exercise, we focused on decision making and the role of heuristics. I personally found our investigation into the Linda question very interesting. We wanted to know, “What attributes of the respondents do you think might affect how they answered the Linda problem and why?” To explore this question, I first examined the crosstable of responses to the Linda question sorted by respondents who had taken a college statistics course versus those who had not. 

[]

From this table, we see that students with at least some formal statistics knowledge are more likely to answer the Linda question correctly. 75.9% of statistics students correctly answered that Linda was more likely to be only a teller, while only 61.3% of non-statistics students were able to do so. Despite the non-statistics-student group being much smaller than its counterpart group (31 versus 54 people), both groups contain a similar quantity of students who responded incorrectly – 12 and 13. From these proportions, we can see that respondents who have not taken a college-level statistics course are more likely to use the attribution substitution shortcut to form conclusions about who Linda is. Because the question provides that Linda is outspoken about issues of activism and seems liberal, a heuristic that leads observers to a statistical fallacy – believing that Linda must then be a feminist too – is readily available. Observers don’t actually know if Linda is a feminist, so they substitute the attribute in question with more known ones, such as her activism. However, if observers pay closer attention, their System 2 may remind them of the statistical principle that states “a conjunction of two events can’t be more probable than either of the events occurring by itself.” Of course, previous students of statistics are more likely to remember this principle before allowing their intuitive heuristics to decide Linda’s character for them. 

 

Understanding that students with some statistical background fare better when facing heuristics pitfalls, I wanted to explore the relationship, if any, between responses to the Linda question and responses to the blue cab question. How likely is it that the cab is actually blue? Is Linda a teller with feminist hobbies? These questions are hard to answer with certainty, so do respondents use heuristics to form conclusions about these questions? To look at how these questions’ responses interact, I plotted the Linda responses versus the blue cab responses. I also graphed a vertical line at the actual, statistically correct probability of the identified cab being a blue cab in navy blue, p = 0.41. 

[]

From this plot, I noticed that those who labeled Linda “teller and feminist” tended to believe that there was a high likelihood that the cab was actually blue. Some respondents who correctly answered the Linda question also thought that the cab was blue, with many responses concentrated over the p = 0.70 mark. However, the “teller” group had many more responses on the lower end of the cab probabilities than the “teller and feminist” group. This indicates to me that the former group perhaps had some intuition that the cab was less likely blue than the question initially would suggest. I then plotted the same data as a bar graph to get another visual.

[]

The majority of people who were correctly skeptical of the blue car ID are those at or near the navy vertical line. Those to the left of the vertical line also seem to have reason to believe that the likelihood of the car actually being blue is much closer to the base rate than 0.80, the most common response from our survey. As shown in the segmented bar graph, most of the respondents who were able to show correct or somewhat promising reasoning about the blue cab problem (those at or to the left of the vertical line shown at 0.41) are part of the “teller” group, the same students who answered the Linda question correctly. I believe there is a correlation between the ability of respondents to respond correctly to the Linda problem and the ability of those same respondents to answer the cab question correctly. Because both Linda and the cab are questions that require some statistical knowledge to figure out, the responses to these questions may be related in a way that is worth investigating. Though I do not know how to properly look at the correlation between these responses using R yet, I attempted to observe that using a crosstable of blue-cab-probability-responses versus linda-feminist-status-responses next. 

[]

Sorting the responses to the cab question into intervals of width 0.10, I observed how often respondents in each interval answered the cab question correctly (T = “teller”) versus incorrectly (TaF = “teller and feminist”). As hypothesized, respondents who were correctly suspicious of the blue ID of the car (so those who answered that the cab was only ~30-50% likely to be blue) were often the same group of people to know that Linda was more likely only a teller. The higher cab guesses, p = 0.8 and up, were more often wrong about Linda. Thus, I conclude that if a respondent ignores the base rate of blue cabs and responds with a higher probability (such as p = 0.8 or p = 0.85), then they are more likely to use attribute substitution to answer the Linda question than respondents who were able to statistically reason their way to the correct estimate for the cab color’s likelihood. 

 

Investigating the traits or behaviors of respondents who answer the Linda question correctly or incorrectly was very interesting to me as an initial data exercise. The data gathered from our class survey confirms, especially with the cab problem, that humans are not always able to use our System 2’s and logically reason through confusing questions. What makes respondents more likely to answer the Linda question correctly? After this investigation, I understand that college statistics background has some pull. I also understand now that the respondents’ ability to answer different challenging questions, such as the blue cab one, may indicate their ability to statistically reason and answer our Linda question of interest. Probability and statistics skills are not the most intuitive to most humans, and yet the ability to leverage them has shown to be significant in our class’ responses to seemingly simple survey questions.

---

**References**
[1] Kahneman (2003)