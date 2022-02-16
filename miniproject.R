######## Miniproject M/W Table 10

require(haven)
require(gtsummary)
require(tidyverse)
require(tidymodels)

setwd("D:/OneDrive - Johns Hopkins/Course/340.753.01 - Epidemiologic Methods 3/Lab/MiniProject/miniproject_stata")

data <- read_dta("data/project_c_copy-3.dta")


####### binary anemia
data <- data %>% 
    mutate(anemia = ifelse(hb < 13, 1, 0)) %>% 
    mutate(anemia = recode_factor(anemia, "0" = "no", "1" = "yes"))


####### Table 1
data %>%
    mutate(paids = recode_factor(paids, "0" = "no", "1" = "yes")) %>% 
    mutate(prior = recode_factor(prior, "0" = "no", "1" = "yes")) %>% 
    select(age:anemia) %>% 
    tbl_summary(by = "anemia",
                statistic = list(age ~ "{mean} ({sd})",
                                 bmi ~ "{mean} ({sd})",
                                 paids ~ "{n} ({p}%)",
                                 prior ~ "{n} ({p}%)"),
                digits = list(all_continuous() ~ 1,
                              paids ~ c(0, 1),
                              prior ~ c(0, 1)))


###### log-binomial regression

glm(cd4lt200 ~ anemia, data = data, family = binomial(link = "log")) %>% 
    tbl_regression(exponentiate = TRUE)

glm(cd4lt200 ~ anemia + age + bmi + paids + prior, data = data, family = binomial(link = "log")) %>% 
    tbl_regression(exponentiate = TRUE)   # fail to converge


require(sandwich)
require(lmtest)
model <- coeftest(glm(cd4lt200 ~ anemia + age + bmi + paids + prior, data = data, family = poisson(link = "log")),
         vcov = sandwich)
tidy(model) %>% mutate(estimate = exp(estimate)) %>% select(term, estimate, `std.error`:`p.value`)
exp(confint(model))
