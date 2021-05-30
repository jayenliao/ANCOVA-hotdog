# title: "統計諮詢：Mid-term Project 1"
# subtitle: "國立成功大學統計學系暨數據科學研究所"
# author: "陳溫茹（R26091040）、廖傑恩（RE6094028)、戴庭筠（R26091032）"

library(HH)
library(car)
library(dplyr)
data(hotdog)

df <- data.frame(table(hotdog[,'Type']))
colnames(df) <- c('Type', '數量')
library(knitr)
library(kableExtra)
kbl(df, booktabs = T, caption = "熱狗各型態數量表") %>%
  kable_styling(latex_options = "striped")

library(ggplot2)
qplot(x = Type, y = Sodium, geom = 'boxplot', data = hotdog) +
  ggtitle('Boxplots of Hot Dogs of Different Types') +
  theme_bw()

qplot(x = Calories, y = Sodium, col = Type, data = hotdog) +
  facet_wrap(. ~ Type) +
  ggtitle('Scatter Plots of Hot Dogs of Different Types') +
  theme_bw() +
  theme(legend.position = 'top')

sw_test <- shapiro.test(hotdog$Sodium)
round(sw_test$statistic, 4)
round(sw_test$p.value, 4)

bartlett_test <- bartlett.test(Sodium ~ Type, data = hotdog)
round(bartlett_test$statistic, 4)
round(bartlett_test$p.value, 4)

library(rstatix)
model_1 <- aov(Sodium ~ Type, data = hotdog)
model_1_tb <- anova_summary(model_1)
round(model_1_tb['F'][,1], 4)
round(model_1_tb$p, 4)

res1 <- residuals(aov(Sodium ~ Type, data = hotdog))
sw_test_res1 <- shapiro.test(res1)
round(sw_test_res1$statistic, 4)
round(sw_test_res1$p.value, 4)

model_interact <- aov(Sodium ~ Calories * Type, data = hotdog)
model_interact_tb <- anova_summary(model_interact)

data(col3x2, package = "HH")
ancovaplot(Sodium ~ Calories * Type, data = hotdog, col=col3x2)
round(model_interact_tb['F'][3,1])
round(model_interact_tb$p[3], 4)

ancovaplot(Sodium ~ Calories + Type, data = hotdog, col=col3x2)
lm__ <- lm(Sodium ~ Calories + Type, data = hotdog)
lm__$coefficients

library(tidyverse) 
umod <- lm(Sodium ~ Calories + Type, data = hotdog)
hotdog.new <- hotdog %>%
  mutate(Sodium.adj =  hotdog$Sodium - umod$coefficients[2]*(hotdog$Calories - mean(hotdog$Calories)))
sw_test_adj <- shapiro.test(hotdog.new$Sodium.adj)
round(sw_test_adj$statistic, 4)
round(sw_test_adj$p.value, 4)

leveneTest_ <- leveneTest(hotdog.new$Sodium.adj~ hotdog.new$Type)
round(leveneTest_[[2]][1], 4)
round(leveneTest_[[3]][1], 4)

ancovaplot(Sodium.adj~ Calories+Type, data=hotdog.new, col=col3x2)

model__ <- aov(Sodium.adj ~ Type, data = hotdog.new)
res <- residuals(model__)
sw_test_res <- shapiro.test(res)
round(sw_test_res$statistic, 4)
round(sw_test_res$p.value, 4)

library(multcomp)
hotdog.new$Type <- factor(hotdog.new$Type, levels = c("Poultry", "Meat", "Beef"), ordered = TRUE)
model__ <- aov(Sodium.adj ~ Type, data = hotdog.new)
fit.dunnett <- glht(model__, linfct=mcp(Type="Dunnett"), alternative="less")
confint(fit.dunnett)

sw_log <- shapiro.test(log(hotdog.new$Sodium.adj))
sw_log$statistic
sw_log$p.value

library(kableExtra)
hotdog.new <- hotdog.new %>% mutate(log.adj = log(Sodium.adj))
model2 <- aov(log.adj ~ Type, data=hotdog.new)

library(multcomp)
k <- rbind(c(0, 1, -1),
           c(1, 0, -1),
           c(1, -1, 0))
fit.gh <- glht(model2, linfct = mcp(Type = k), alternative="greater")
summary(fit.gh)
