## =============================设置环境=======================================
setwd("E:/我的工作学习/_（正在）_我的研究/a_研究2/4数据分析")

library(openxlsx)
library(psych)
library(car) #基本计算和方差分析
library(ez) #方差分析
library(ggplot2) #绘图
library(gghalves) #绘图：半小提琴图
library(cowplot) #绘图：白底模板
library(gridExtra) #绘图：组合图
library(pastecs) #描述性统计
library(lsr) #算Cohen d的
library(pwr)
library(lme4) #LMM
library(lmerTest) #LMM

## ============================信度分析====================================
# 打开数据
basedata = read.xlsx("实验数据表.xlsx", "基本情况与干扰变量")
names(basedata)
## 电子经验的量表信度，看raw_alpha
print(alpha(basedata[,c(13:15)]), digits = 3) #raw_alpha=0.848
## 空间策略的量表信度，看raw_alpha
print(alpha(basedata[,c(16:42)]), digits = 3) #raw_alpha=0.737

## =============================导入数据=======================================
# 打开数据
data = read.xlsx("实验数据表.xlsx", "数据汇总表")
data_1 = read.xlsx("实验数据表.xlsx", "分析回视角度") #删掉含空值的被试数据

#增加一个变量
data$mobile_navigation_usage_duration_proportion <- data$mobile_navigation_usage_duration/data$wayfinding_time*100

# 查看数据
head(data)
str(data)
View(data)
names(data)

## =========================检验正态性=====================================

# 导航绩效
by(data$wayfinding_time, list(data$locomotion_methods, data$OI_navigation_legibility,
                              data$age_group), shapiro.test) #大多数结果非正态
summary(powerTransform(data$wayfinding_time))
data$transformed_wayfinding_time <- log(data$wayfinding_time)
by(data$transformed_wayfinding_time, list(data$locomotion_methods, data$OI_navigation_legibility,
                                          data$age_group), shapiro.test) #结果正态

by(data$number_of_hesitation, list(data$locomotion_methods, data$OI_navigation_legibility,
                                   data$age_group), shapiro.test) #结果正态

by(data$number_of_turn_errors, list(data$locomotion_methods, data$OI_navigation_legibility,
                                    data$age_group), shapiro.test) #大多数结果非正态
summary(powerTransform(data$number_of_turn_errors)) #因为有0值所以无法转换，接下来将零值替换为一个很小的正数
data$number_of_turn_errors_copy <- data$number_of_turn_errors
data$number_of_turn_errors_copy <- ifelse(data$number_of_turn_errors_copy == 0, 1e-6, data$number_of_turn_errors_copy)
summary(powerTransform(data$number_of_turn_errors_copy))
data$transformed_number_of_turn_errors <- data$number_of_turn_errors_copy ^ 0.0662
by(data$transformed_number_of_turn_errors, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                data$age_group), shapiro.test) #结果非正态

# 导航交互
by(data$mobile_navigation_usage_number, list(data$locomotion_methods, data$OI_navigation_legibility,
                                             data$age_group), shapiro.test) #大多数结果非正态
summary(powerTransform(data$mobile_navigation_usage_number))
data$transformed_mobile_navigation_usage_number <- data$mobile_navigation_usage_number ^ 0.2
by(data$transformed_mobile_navigation_usage_number, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                         data$age_group), shapiro.test) #结果正态

by(data$mobile_navigation_usage_duration, list(data$locomotion_methods, data$OI_navigation_legibility,
                                               data$age_group), shapiro.test) #大多数结果正态
summary(powerTransform(data$mobile_navigation_usage_duration))
data$transformed_mobile_navigation_usage_duration <- data$mobile_navigation_usage_duration ^ 0.2
by(data$transformed_mobile_navigation_usage_duration, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                           data$age_group), shapiro.test) #结果正态

by(data$mobile_navigation_usage_number_in_outdoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                              data$age_group), shapiro.test) #大多数结果正态
summary(powerTransform(data$mobile_navigation_usage_number_in_outdoor_sapce))
data$transformed_mobile_navigation_usage_number_in_outdoor_sapce <- data$mobile_navigation_usage_number_in_outdoor_sapce ^ 0.2
by(data$transformed_mobile_navigation_usage_number_in_outdoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                                          data$age_group), shapiro.test) #结果正态

data$transformed_mobile_navigation_usage_duration_in_outdoor_sapce <- data$mobile_navigation_usage_duration_in_outdoor_sapce ^ 0.2
by(data$transformed_mobile_navigation_usage_duration_in_outdoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                                            data$age_group), shapiro.test) #结果正态

data$transformed_mobile_navigation_usage_duration_in_outdoor_sapce <- data$mobile_navigation_usage_duration_in_outdoor_sapce ^ 0.2
by(data$transformed_mobile_navigation_usage_duration_in_outdoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                                            data$age_group), shapiro.test) #结果正态

data$transformed_mobile_navigation_usage_number_in_OI_sapce <- data$mobile_navigation_usage_number_in_OI_sapce ^ 0.2
by(data$transformed_mobile_navigation_usage_duration_in_outdoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                                            data$age_group), shapiro.test) #结果正态

by(data$mobile_navigation_usage_duration_in_OI_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                           data$age_group), shapiro.test) #大多数结果正态
summary(powerTransform(data$mobile_navigation_usage_duration_in_OI_sapce)) #因为有0值所以无法转换，接下来将零值替换为一个很小的正数
data$mobile_navigation_usage_duration_in_OI_sapce_copy <- data$mobile_navigation_usage_duration_in_OI_sapce
data$mobile_navigation_usage_duration_in_OI_sapce_copy <- ifelse(data$mobile_navigation_usage_duration_in_OI_sapce == 0, 1e-6, data$mobile_navigation_usage_duration_in_OI_sapce)
summary(powerTransform(data$mobile_navigation_usage_duration_in_OI_sapce_copy))
data$transformed_mobile_navigation_usage_duration_in_OI_sapce <- data$mobile_navigation_usage_duration_in_OI_sapce_copy ^ 0.1606
by(data$transformed_mobile_navigation_usage_duration_in_OI_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                                       data$age_group), shapiro.test) #结果非正态

data$transformed_mobile_navigation_usage_number_in_indoor_sapce <- data$mobile_navigation_usage_number_in_indoor_sapce ^ 0.2
by(data$transformed_mobile_navigation_usage_number_in_indoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                                         data$age_group), shapiro.test) #结果正态

data$transformed_mobile_navigation_usage_duration_in_indoor_sapce <- data$mobile_navigation_usage_duration_in_indoor_sapce ^ 0.2
by(data$transformed_mobile_navigation_usage_duration_in_indoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                                           data$age_group), shapiro.test) #结果正态

by(data$mobile_navigation_usage_duration_in_deviated_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                                 data$age_group), shapiro.test) #大多数结果非正态
summary(powerTransform(data$mobile_navigation_usage_duration_in_deviated_sapce)) #因为有0值所以无法转换，接下来将零值替换为一个很小的正数
data$mobile_navigation_usage_duration_in_deviated_sapce_copy <- data$mobile_navigation_usage_duration_in_deviated_sapce
data$mobile_navigation_usage_duration_in_deviated_sapce_copy <- ifelse(data$mobile_navigation_usage_duration_in_deviated_sapce == 0, 1e-6, data$mobile_navigation_usage_duration_in_deviated_sapce)
summary(powerTransform(data$mobile_navigation_usage_duration_in_deviated_sapce_copy))
data$transformed_mobile_navigation_usage_duration_in_deviated_sapce <- data$mobile_navigation_usage_duration_in_deviated_sapce_copy ^ 0.0053
by(data$transformed_mobile_navigation_usage_duration_in_deviated_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                                             data$age_group), shapiro.test) #结果非正态

# 导航回视
by(data$regression_number, list(data$locomotion_methods, data$OI_navigation_legibility,
                                data$age_group), shapiro.test) #大多数结果非正态
summary(powerTransform(data$regression_number)) #因为有0值所以无法转换，接下来将零值替换为一个很小的正数
data$regression_number_copy <- data$regression_number
data$regression_number_copy <- ifelse(data$regression_number == 0, 1e-6, data$regression_number)
summary(powerTransform(data$regression_number_copy))
data$transformed_regression_number <- data$regression_number_copy ^ 0.4204
by(data$transformed_regression_number, list(data$locomotion_methods, data$OI_navigation_legibility,
                                            data$age_group), shapiro.test) #结果正态

by(data$average_regression_angle, list(data$locomotion_methods, data$OI_navigation_legibility,
                                       data$age_group), shapiro.test) #结果正态

by(data$regression_number_in_outdoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                data$age_group), shapiro.test) #大多数结果非正态
summary(powerTransform(data$regression_number_in_outdoor_sapce)) #因为有0值所以无法转换，接下来将零值替换为一个很小的正数
data$regression_number_in_outdoor_sapce_copy <- data$regression_number_in_outdoor_sapce
data$regression_number_in_outdoor_sapce_copy <- ifelse(data$regression_number_in_outdoor_sapce == 0, 1e-6, data$regression_number_in_outdoor_sapce)
summary(powerTransform(data$regression_number_in_outdoor_sapce_copy))
data$transformed_regression_number_in_outdoor_sapce <- data$regression_number_in_outdoor_sapce ^ 0.3287
by(data$transformed_regression_number_in_outdoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                            data$age_group), shapiro.test) #结果正态

by(data$average_regression_angle_in_outdoor_sapce, list(data$locomotion_methods, data$OI_navigation_legibility,
                                                 data$age_group), shapiro.test) #大多数结果非正态




# 认知地图
by(data$time_of_cognitive_map_drawing, list(data$locomotion_methods, data$OI_navigation_legibility,
                                            data$age_group), shapiro.test) #大多数结果正态

names(data)

## ============================相关性分析======================================

# 输出全样本所有变量的平均数和标准差（包含人口特征变量）
selected_data <- data[, c(4:6, 8:19, 21:22, 58, 31:32, 20, 41)]
means <- colMeans(selected_data)
std_devs <- apply(selected_data, 2, sd)
summary_table <- data.frame(Variable = colnames(selected_data),
                            Mean = round(means, 2),
                            Std.Dev = round(std_devs, 2))
print(summary_table)

# 输出老年人所有变量的平均数和标准差
filtered_data <- data[data$age_group == "1", c(4:6, 8:19, 21:22, 58, 31:32, 20, 41)]
means <- colMeans(filtered_data)
std_devs <- apply(filtered_data, 2, sd)
summary_table <- data.frame(Variable = colnames(filtered_data),
                            Mean = round(means, 2),
                            Std.Dev = round(std_devs, 2))
print(summary_table)

# 输出年轻人所有变量的平均数和标准差
filtered_data <- data[data$age_group == "0", c(4:6, 8:19, 21:22, 58, 31:32, 20, 41)]
means <- colMeans(filtered_data)
std_devs <- apply(filtered_data, 2, sd)
summary_table <- data.frame(Variable = colnames(filtered_data),
                            Mean = round(means, 2),
                            Std.Dev = round(std_devs, 2))
print(summary_table)

# 描述全样本变量之间的皮尔逊相关系数及相应p值
data_corr = corr.test(data[,c(4:6, 8:16, 58, 18:19, 61:62, 58, 73, 32, 20, 41)], method = "pearson")
data_corr = corr.test(data[data$age_group == "1", c(4:6, 8:16, 58, 18:19, 61:62, 58, 73, 32, 20, 41)], 
                      use = "pairwise", method = "pearson")
data_corr = corr.test(data[data$age_group == "0", c(4:6, 8:16, 58, 18:19, 61:62, 58, 73, 32, 20, 41)], 
                      use = "pairwise", method = "pearson")
print(data_corr)

# 输出相关系数矩阵的下三角部分，保留两位小数
lowerMat(data_corr$r, digits = 2)
# 输出相关系数显著性检验的p值
lowerMat(data_corr$p, digits = 4)


## ============================参试者特征=====================================

# 统计连续变量的平均值和方差就用相关性分析部分的代码就可以

# 统计名义变量的频次和比例
nominal_vars <- c("education_level")
nominal_stats <- lapply(nominal_vars, function(var) {
  counts <- table(data$age_group, data[[var]])
  freq <- prop.table(counts, margin = 1)
  list(counts = counts, freq = freq)
})

# 打印名义变量的频次和比例
for (i in seq_along(nominal_vars)) {
  cat("\nVariable:", nominal_vars[i], "\n")
  print(nominal_stats[[i]]$counts)
  cat("\nProportions:\n")
  print(nominal_stats[[i]]$freq)
  cat("\n")
}

## ==========================检验顺序效应=====================================

## 检验顺序效应

data$trail = factor(data$trail)

model <- manova(cbind(wayfinding_time, number_of_hesitation, number_of_turn_errors) 
                ~ locomotion_methods*OI_navigation_legibility 
                + Error(trail/(locomotion_methods*OI_navigation_legibility)), 
                data=subset(data, age_group == "0"))
summary(model) #p值不显著，表明不存在顺序效应

## =========================因子化准备=====================================

#原表
data$gender <-factor(data$gender,labels = c("Male","Female"))
data$age_group <-factor(data$age_group,labels = c("Younger adults","Older adults"))
data$education_level <-factor(data$education_level,labels = c("Primary school and below",
                                                        "Junior high school",
                                                        "Senior high school",
                                                        "Undergraduate or associate degree"))
data$locomotion_methods <-factor(data$locomotion_methods,labels = c("Discontinuous teleportation",
                                                           "Continuous teleportation"))
data$OI_navigation_legibility <-factor(data$OI_navigation_legibility,labels = c("Unenhanced legibility",
                                                              "Enhanced legibility"))
data$groups = factor(data$groups, labels = c("Continuous-Enhanced","Continuous-Unenhanced",
                                           "Discontinuous-Enhanced","Discontinuous-Unenhanced"))
data$participants = factor(data$participants)

#删掉回视角度空值被试数据
data_1$gender <-factor(data_1$gender,labels = c("Male","Female"))
data_1$age_group <-factor(data_1$age_group,labels = c("Younger adults","Older adults"))
data_1$education_level <-factor(data_1$education_level,labels = c("Primary school and below",
                                                                  "Junior high school",
                                                                  "Senior high school",
                                                                  "Undergraduate or associate degree"))
data_1$locomotion_methods <-factor(data_1$locomotion_methods,labels = c("Discontinuous teleportation",
                                                                        "Continuous teleportation"))
data_1$OI_navigation_legibility <-factor(data_1$OI_navigation_legibility,labels = c("Unenhanced legibility",
                                                                                    "Enhanced legibility"))
data_1$groups = factor(data_1$groups, labels = c("Continuous-Enhanced","Continuous-Unenhanced",
                                                 "Discontinuous-Enhanced","Discontinuous-Unenhanced"))
data_1$participants = factor(data_1$participants)

## =========================三因素方差分析=====================================

dataModel <- ezANOVA(data = data, 
                     dv = .(mobile_navigation_usage_duration_proportion), 
                     wid = .(participants), 
                     between = .(age_group), 
                     within = .(locomotion_methods, OI_navigation_legibility), 
                     between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                            allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                            possession_of_navigation,rotation_ability),
                     type = 3, 
                     detailed = TRUE)
dataModel

dataModel <- ezANOVA(data = data_1, 
                     dv = .(average_regression_angle), 
                     wid = .(participants), 
                     between = .(age_group), 
                     within = .(locomotion_methods, OI_navigation_legibility), 
                     between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                            allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                            possession_of_navigation,rotation_ability),
                     type = 3, 
                     detailed = TRUE)
dataModel

## =========================绘图（9个一级因变量）=====================================

## 定义一个计算标准误的函数
summarySE <- function(data=NULL, outcome, factor=NULL, na.rm=TRUE,
                      conf.interval=.95, .drop=TRUE) { 
  library(plyr)
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  datac <- ddply(data, factor, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 outcome
  )
  
  datac <- rename(datac, c("mean" = outcome))
  
  datac$se <- datac$sd / sqrt(datac$N) 
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

# 绘图
data_se <- summarySE(data=data, outcome = "wayfinding_time",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = wayfinding_time, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = wayfinding_time, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = wayfinding_time - se, ymax = wayfinding_time + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Wayfinding Time (s)") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 620)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = wayfinding_time, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = wayfinding_time - se, ymax = wayfinding_time + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Wayfinding Time (s)") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(80, 300)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("1-Wayfinding Time.png", plot = plot_combined, width=16, height=5, dpi=300)


data_se <- summarySE(data=data, outcome = "number_of_hesitation",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = number_of_hesitation, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = number_of_hesitation, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = number_of_hesitation - se, ymax = number_of_hesitation + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Number of hesitation") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 50)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = number_of_hesitation, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = number_of_hesitation - se, ymax = number_of_hesitation + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Number of hesitation") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(4, 20)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("2-Number of hesitation.png", plot = plot_combined, width=16, height=5, dpi=300)


data_se <- summarySE(data=data, outcome = "number_of_turn_errors",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = number_of_turn_errors, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = number_of_turn_errors, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = number_of_turn_errors - se, ymax = number_of_turn_errors + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Number of turn errors") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 8)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = number_of_turn_errors, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = number_of_turn_errors - se, ymax = number_of_turn_errors + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Number of turn errors") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 3)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("3-Number of turn errors.png", plot = plot_combined, width=16, height=5, dpi=300)


data_se <- summarySE(data=data, outcome = "mobile_navigation_usage_number",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = mobile_navigation_usage_number, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = mobile_navigation_usage_number, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number - se, ymax = mobile_navigation_usage_number + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = mobile_navigation_usage_number, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = mobile_navigation_usage_number - se, ymax = mobile_navigation_usage_number + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Mobile navigation usage number") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(15, 45)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("4-Mobile navigation usage number.png", plot = plot_combined, width=16, height=5, dpi=300)


data_se <- summarySE(data=data, outcome = "mobile_navigation_usage_duration",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = mobile_navigation_usage_duration, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = mobile_navigation_usage_duration, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration - se, ymax = mobile_navigation_usage_duration + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 280)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = mobile_navigation_usage_duration, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration - se, ymax = mobile_navigation_usage_duration + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Mobile navigation usage duration (s)") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(30, 130)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("5-Mobile navigation usage duration.png", plot = plot_combined, width=16, height=5, dpi=300)

data_se <- summarySE(data=data, outcome = "mobile_navigation_usage_duration_proportion",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = mobile_navigation_usage_duration_proportion, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = mobile_navigation_usage_duration_proportion, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_proportion - se, ymax = mobile_navigation_usage_duration_proportion + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Mobile navigation usage duration proportion (%)") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 120)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = mobile_navigation_usage_duration_proportion, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_proportion - se, ymax = mobile_navigation_usage_duration_proportion + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Mobile navigation usage duration proportion (%)") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(30, 60)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("5-1-Mobile navigation usage duration proportion.png", plot = plot_combined, width=16, height=5, dpi=300)

data_se <- summarySE(data=data, outcome = "regression_number",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = regression_number, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = regression_number, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number - se, ymax = regression_number + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Regression number") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(0, 40)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = regression_number, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = regression_number - se, ymax = regression_number + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Regression number") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(4, 13)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("6-Regression number.png", plot = plot_combined, width=16, height=5, dpi=300)


data_se <- summarySE(data=data, outcome = "average_regression_angle",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = average_regression_angle, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = average_regression_angle, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle - se, ymax = average_regression_angle + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(45, 130)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = average_regression_angle, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = average_regression_angle - se, ymax = average_regression_angle + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Average regression angle (°)") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(65, 95)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("7-Average regression angle.png", plot = plot_combined, width=16, height=5, dpi=300)


data_se <- summarySE(data=data, outcome = "time_of_cognitive_map_drawing",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = time_of_cognitive_map_drawing, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = time_of_cognitive_map_drawing, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = time_of_cognitive_map_drawing - se, ymax = time_of_cognitive_map_drawing + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Time of cognitive map drawing (s)") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(20,500)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = time_of_cognitive_map_drawing, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = time_of_cognitive_map_drawing - se, ymax = time_of_cognitive_map_drawing + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Time of cognitive map drawing (s)") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(140, 220)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("8-Time of cognitive map drawing.png", plot = plot_combined, width=16, height=5, dpi=300)


data_se <- summarySE(data=data, outcome = "accuracy_of_cognitive_map_drawing",
                     factor = c("locomotion_methods","OI_navigation_legibility","age_group")) 
head(data_se)
plot1 <- ggplot(data_se, aes(x = locomotion_methods, y = accuracy_of_cognitive_map_drawing, fill = OI_navigation_legibility)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  gghalves::geom_half_violin(data = data, aes(y = accuracy_of_cognitive_map_drawing, fill = OI_navigation_legibility),
                             position = position_dodge(width = 1), side = "r", colour = NA, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing - se, ymax = accuracy_of_cognitive_map_drawing + se),
                colour = "black", width = 0.2, position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  facet_wrap(~age_group) +
  labs(x = "Locomotion methods", y = "Accuracy of cognitive map drawing (%)") +
  scale_fill_manual(values = c("#81A88D", "#a6bddb"),
                    guide_legend(title = "OI navigation legibility", override.aes = list(alpha = 0.3)),
                    labels = c("Unenhanced legibility", "Enhanced legibility")) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(20,119)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot2 <- ggplot(data_se, aes(x = locomotion_methods, y = accuracy_of_cognitive_map_drawing, colour = OI_navigation_legibility, group = OI_navigation_legibility)) + 
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing - se, ymax = accuracy_of_cognitive_map_drawing + se), colour = "black", width = 0.2, position = position_dodge(0.3)) +
  geom_line(position = position_dodge(0.3), size = 1, aes(linetype = OI_navigation_legibility)) +
  geom_point(position = position_dodge(0.3), size = 3) +
  labs(x = "Locomotion methods", y = "Accuracy of cognitive map drawing (%)") +
  scale_color_manual(values = c("Unenhanced legibility" = "#81A88D", "Enhanced legibility" = "#a6bddb"),
                     name = "OI navigation legibility") +
  scale_linetype_manual(values = c("Unenhanced legibility" = "solid", "Enhanced legibility" = "dashed"),
                        name = "OI navigation legibility") +
  facet_wrap(~age_group) +
  theme_cowplot() +
  theme(text = element_text(size = 14)) +
  coord_cartesian(ylim = c(30, 100)) +
  scale_x_discrete(labels = c("Discontinuous\n teleportation", "Continuous\n teleportation")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol=2)
ggsave("9-Accuracy of cognitive map drawing.png", plot = plot_combined, width=16, height=5, dpi=300)

## =========================绘图（三类二级因变量）=====================================

# ---------------------------DU场景的导航交互——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_outdoor_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 25)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_outdoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_outdoor_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 65)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(DU)-(outdoor_space).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_OI_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 2)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_OI_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_OI_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 8)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(DU)-(OI_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_indoor_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_indoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_indoor_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 45)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(DU)-(indoor_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_deviated_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_deviated_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 6)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_deviated_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_deviated_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(DU)-(deviated_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)

# ---------------------------DE场景的导航交互——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_outdoor_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 25)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_outdoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_outdoor_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 65)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(DE)-(outdoor_space).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_OI_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 2.5)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_OI_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_OI_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 8)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(DE)-(OI_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_indoor_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_indoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_indoor_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 45)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(DE)-(indoor_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_deviated_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_deviated_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 6)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_deviated_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_deviated_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(DE)-(deviated_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)

# ---------------------------CU场景的导航交互——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_outdoor_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 25)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_outdoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_outdoor_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 65)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(CU)-(outdoor_space).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_OI_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 2.5)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_OI_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_OI_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 8)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(CU)-(OI_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_indoor_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_indoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_indoor_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 45)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(CU)-(indoor_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_deviated_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_deviated_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 6)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_deviated_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_deviated_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(CU)-(deviated_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)

# ---------------------------CE场景的导航交互——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_outdoor_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 25)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_outdoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_outdoor_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 65)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(CE)-(outdoor_space).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_OI_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 2.5)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_OI_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_OI_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 8)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(CE)-(OI_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_indoor_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_indoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_indoor_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 45)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(CE)-(indoor_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_number_in_deviated_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_number_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_number_in_deviated_sapce - se, 
                    ymax = mobile_navigation_usage_number_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 6)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "mobile_navigation_usage_duration_in_deviated_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = mobile_navigation_usage_duration_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = mobile_navigation_usage_duration_in_deviated_sapce - se, 
                    ymax = mobile_navigation_usage_duration_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Mobile navigation usage duration (s)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 15)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Mobile_navigation_usage)-(CE)-(deviated_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)

# ---------------------------DU场景的导航回视——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_outdoor_sapce - se, 
                    ymax = regression_number_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 5)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_outdoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_outdoor_sapce - se, 
                    ymax = average_regression_angle_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 90)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(DU)-(outdoor_space).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_OI_sapce - se, 
                    ymax = regression_number_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 2)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_OI_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_OI_sapce - se, 
                    ymax = average_regression_angle_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 90)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(DU)-(OI_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_indoor_sapce - se, 
                    ymax = regression_number_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 4)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_indoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_indoor_sapce - se, 
                    ymax = average_regression_angle_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 85)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(DU)-(indoor_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_deviated_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_deviated_sapce - se, 
                    ymax = regression_number_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 3)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_deviated_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_deviated_sapce - se, 
                    ymax = average_regression_angle_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 130)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(DU)-(deviated_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)

# ---------------------------DE场景的导航回视——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_outdoor_sapce - se, 
                    ymax = regression_number_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 5)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_outdoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_outdoor_sapce - se, 
                    ymax = average_regression_angle_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 90)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(DE)-(outdoor_space).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_OI_sapce - se, 
                    ymax = regression_number_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 2)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_OI_sapce",
                     factor = c("age_group"))
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_OI_sapce - se, 
                    ymax = average_regression_angle_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 90)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(DE)-(OI_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_indoor_sapce - se, 
                    ymax = regression_number_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 4)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_indoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_indoor_sapce - se, 
                    ymax = average_regression_angle_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 85)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(DE)-(indoor_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_deviated_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_deviated_sapce - se, 
                    ymax = regression_number_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 3)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_deviated_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_deviated_sapce - se, 
                    ymax = average_regression_angle_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 130)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(DE)-(deviated_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)

# ---------------------------CU场景的导航回视——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_outdoor_sapce - se, 
                    ymax = regression_number_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 5)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_outdoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_outdoor_sapce - se, 
                    ymax = average_regression_angle_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 90)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(CU)-(outdoor_space).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_OI_sapce - se, 
                    ymax = regression_number_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 2)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_OI_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_OI_sapce - se, 
                    ymax = average_regression_angle_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 90)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(CU)-(OI_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_indoor_sapce - se, 
                    ymax = regression_number_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 4)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_indoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_indoor_sapce - se, 
                    ymax = average_regression_angle_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 85)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(CU)-(indoor_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_deviated_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_deviated_sapce - se, 
                    ymax = regression_number_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 3)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_deviated_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_deviated_sapce - se, 
                    ymax = average_regression_angle_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 130)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(CU)-(deviated_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)

# ---------------------------CE场景的导航回视——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_outdoor_sapce - se, 
                    ymax = regression_number_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 5)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_outdoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_outdoor_sapce - se, 
                    ymax = average_regression_angle_in_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 90)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(CE)-(outdoor_space).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_OI_sapce - se, 
                    ymax = regression_number_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 2)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_OI_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_OI_sapce - se, 
                    ymax = average_regression_angle_in_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 90)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(CE)-(OI_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_indoor_sapce - se, 
                    ymax = regression_number_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 4)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_indoor_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_indoor_sapce - se, 
                    ymax = average_regression_angle_in_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 85)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(CE)-(indoor_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "regression_number_in_deviated_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = regression_number_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = regression_number_in_deviated_sapce - se, 
                    ymax = regression_number_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Regression number") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 3)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "average_regression_angle_in_deviated_sapce",
                     factor = c("age_group")) 
plot2 <- ggplot(data_se, aes(x = age_group, y = average_regression_angle_in_deviated_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = average_regression_angle_in_deviated_sapce - se, 
                    ymax = average_regression_angle_in_deviated_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Average regression angle (°)") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 130)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
plot_combined <- grid.arrange(plot1, plot2, ncol = 2)
ggsave("10-(Regression)-(CE)-(deviated_sapce).png", plot = plot_combined, width = 10, height = 8, dpi = 300)

# ---------------------------DU场景的认知地图——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_outdoor_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(DU)-(outdoor_space).png", plot = plot1, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_OI_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(DU)-(OI_sapce).png", plot = plot1, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_indoor_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(DU)-(indoor_sapce).png", plot = plot1, width = 10, height = 8, dpi = 300)

# ---------------------------DE场景的认知地图——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_outdoor_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(DE)-(outdoor_space).png", plot = plot1, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_OI_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(DE)-(OI_sapce).png", plot = plot1, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Discontinuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_indoor_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(DE)-(indoor_sapce).png", plot = plot1, width = 10, height = 8, dpi = 300)

# ---------------------------CU场景的认知地图——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_outdoor_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(CU)-(outdoor_space).png", plot = plot1, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_OI_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(CU)-(OI_sapce).png", plot = plot1, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Unenhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_indoor_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(CU)-(indoor_sapce).png", plot = plot1, width = 10, height = 8, dpi = 300)

# ---------------------------CE场景的认知地图——--------------------------------------------

sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_outdoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_outdoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_outdoor_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_outdoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(CE)-(outdoor_space).png", plot = plot1, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_OI_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_OI_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_OI_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_OI_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(CE)-(OI_sapce).png", plot = plot1, width = 10, height = 8, dpi = 300)



sub_data <- subset(data, locomotion_methods == "Continuous teleportation" & 
                     OI_navigation_legibility == "Enhanced legibility")
data_se <- summarySE(data = sub_data, outcome = "accuracy_of_cognitive_map_drawing_for_indoor_sapce",
                     factor = c("age_group")) 
plot1 <- ggplot(data_se, aes(x = age_group, y = accuracy_of_cognitive_map_drawing_for_indoor_sapce, fill = age_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1), width = 0.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = accuracy_of_cognitive_map_drawing_for_indoor_sapce - se, 
                    ymax = accuracy_of_cognitive_map_drawing_for_indoor_sapce + se),
                position = position_dodge(width = 1), colour = "black", width = 0.2) +
  geom_point(position = position_dodge(1), size = 3, alpha = 0.5) +
  labs(x = "", y = "Accuracy of cognitive map drawing") +
  scale_fill_manual(values = c("Younger adults" = "#A08887", "Older adults" = "#60656C"), # 设置颜色映射
                    guide_legend(title = "Age Group", override.aes = list(alpha = 0.3)), # 设置图例标题和透明度
                    labels = c("Younger adults", "Older adults")) + # 设置图例标签
  theme_cowplot() +
  theme(text = element_text(size = 28),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24)) +
  coord_cartesian(ylim = c(0, 100)) +
  guides(fill = guide_legend(title = "Age Group", override.aes = list(alpha = 0.3))) + # 添加年龄组图例
  guides(fill = "none") + # 移除图例
  scale_x_discrete(labels = c("Young", "Old")) # 修改x轴标签
ggsave("10-(Cognitive map)-(CE)-(indoor_sapce).png", plot = plot1, width = 10, height = 8, dpi = 300)

## =========================两因素方差分析=====================================

# --------------------------1：wayfinding_time------------------------------------------

## 年龄差异

by(data$wayfinding_time, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(transformed_wayfinding_time ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(transformed_wayfinding_time ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(transformed_wayfinding_time),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$transformed_wayfinding_time, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$wayfinding_time, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$wayfinding_time, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "wayfinding_time",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data, age_group == "Older adults"), #重复测量方差分析
             dv = .(transformed_wayfinding_time),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Older adults")
pairwise.t.test(Older_data$transformed_wayfinding_time, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$wayfinding_time, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$wayfinding_time, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "wayfinding_time",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

# --------------------------2：number_of_hesitation------------------------------------------

## 年龄差异

by(data$number_of_hesitation, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(number_of_hesitation ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(number_of_hesitation ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(number_of_hesitation),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$number_of_hesitation, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$number_of_hesitation, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$number_of_hesitation, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "number_of_hesitation",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data, age_group == "Older adults"), #重复测量方差分析
             dv = .(number_of_hesitation),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Older adults")
pairwise.t.test(Older_data$number_of_hesitation, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$number_of_hesitation, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$number_of_hesitation, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "number_of_hesitation",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

# --------------------------3：number_of_turn_errors------------------------------------------

## 年龄差异

by(data$number_of_turn_errors, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(number_of_turn_errors ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(number_of_turn_errors ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(number_of_turn_errors),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$number_of_turn_errors, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$number_of_turn_errors, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$number_of_turn_errors, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "number_of_turn_errors",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data, age_group == "Older adults"), #重复测量方差分析
             dv = .(number_of_turn_errors),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Older adults")
pairwise.t.test(Older_data$number_of_turn_errors, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$number_of_turn_errors, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$number_of_turn_errors, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "number_of_turn_errors",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

# --------------------------4：mobile_navigation_usage_number------------------------------------------

## 年龄差异

by(data$mobile_navigation_usage_number, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(transformed_mobile_navigation_usage_number ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(transformed_mobile_navigation_usage_number ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(transformed_mobile_navigation_usage_number),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$transformed_mobile_navigation_usage_number, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$mobile_navigation_usage_number, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$mobile_navigation_usage_number, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "mobile_navigation_usage_number",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data, age_group == "Older adults"), #重复测量方差分析
             dv = .(transformed_mobile_navigation_usage_number),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Older adults")
pairwise.t.test(Older_data$transformed_mobile_navigation_usage_number, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$mobile_navigation_usage_number, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$mobile_navigation_usage_number, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "mobile_navigation_usage_number",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

# --------------------------5：mobile_navigation_usage_duration------------------------------------------

## 年龄差异

by(data$mobile_navigation_usage_duration, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(transformed_mobile_navigation_usage_duration ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(transformed_mobile_navigation_usage_duration ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(mobile_navigation_usage_duration),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$transformed_mobile_navigation_usage_duration, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$mobile_navigation_usage_duration, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$mobile_navigation_usage_duration, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "mobile_navigation_usage_duration",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data, age_group == "Older adults"), #重复测量方差分析
             dv = .(mobile_navigation_usage_duration),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Older adults")
pairwise.t.test(Older_data$transformed_mobile_navigation_usage_duration, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$mobile_navigation_usage_duration, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$mobile_navigation_usage_duration, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "mobile_navigation_usage_duration",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

# --------------------------6：regression_number------------------------------------------

## 年龄差异

by(data$regression_number, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(transformed_regression_number ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(transformed_regression_number ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(transformed_regression_number),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$transformed_regression_number, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$regression_number, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$regression_number, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "regression_number",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data, age_group == "Older adults"), #重复测量方差分析
             dv = .(transformed_regression_number_in_outdoor_sapce),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Older adults")
pairwise.t.test(Older_data$transformed_regression_number, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$regression_number, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$regression_number, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "regression_number",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

# --------------------------7：average_regression_angle------------------------------------------

## 年龄差异

by(data$average_regression_angle, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(average_regression_angle ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(average_regression_angle ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(average_regression_angle),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$transformed_average_regression_angle, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$average_regression_angle, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$average_regression_angle, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "average_regression_angle",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data_1, age_group == "Older adults"), #重复测量方差分析
             dv = .(average_regression_angle),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Older_data$transformed_average_regression_angle, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$average_regression_angle, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$average_regression_angle, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "average_regression_angle",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

# --------------------------8：time_of_cognitive_map_drawing------------------------------------------

## 年龄差异

by(data$time_of_cognitive_map_drawing, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(time_of_cognitive_map_drawing ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(time_of_cognitive_map_drawing ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(time_of_cognitive_map_drawing),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$time_of_cognitive_map_drawing, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$time_of_cognitive_map_drawing, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$time_of_cognitive_map_drawing, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "time_of_cognitive_map_drawing",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data, age_group == "Older adults"), #重复测量方差分析
             dv = .(time_of_cognitive_map_drawing),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Older adults")
pairwise.t.test(Older_data$time_of_cognitive_map_drawing, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$time_of_cognitive_map_drawing, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$time_of_cognitive_map_drawing, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "time_of_cognitive_map_drawing",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

# --------------------------9：accuracy_of_cognitive_map_drawing------------------------------------------

## 年龄差异

by(data$accuracy_of_cognitive_map_drawing, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(accuracy_of_cognitive_map_drawing ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(accuracy_of_cognitive_map_drawing ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(accuracy_of_cognitive_map_drawing),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$accuracy_of_cognitive_map_drawing, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$accuracy_of_cognitive_map_drawing, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$accuracy_of_cognitive_map_drawing_for_indoor_sapce, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "accuracy_of_cognitive_map_drawing",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data, age_group == "Older adults"), #重复测量方差分析
             dv = .(accuracy_of_cognitive_map_drawing),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Older adults")
pairwise.t.test(Older_data$accuracy_of_cognitive_map_drawing, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$accuracy_of_cognitive_map_drawing, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$accuracy_of_cognitive_map_drawing, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "accuracy_of_cognitive_map_drawing",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

# --------------------------10：mobile_navigation_usage_duration_proportion------------------------------------------

data$mobile_navigation_usage_duration_proportion <- data$mobile_navigation_usage_duration/data$wayfinding_time*100

## 年龄差异
by(data$mobile_navigation_usage_duration_proportion, list(data$age_group), 
   stat.desc, basic = FALSE)
t.test(mobile_navigation_usage_duration_proportion ~ age_group, data=data, var.equal=TRUE) ##若有转换后数据使用转换数据
cohensD(mobile_navigation_usage_duration_proportion ~ age_group,data = data) # t-test的effect size

## 年轻人的分析
m <- ezANOVA(data = subset(data, age_group == "Younger adults"), #重复测量方差分析
             dv = .(mobile_navigation_usage_duration_proportion),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Younger_data <- subset(data, age_group == "Younger adults")
pairwise.t.test(Younger_data$mobile_navigation_usage_duration_proportion, Younger_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Younger_data$mobile_navigation_usage_duration_proportion, list(Younger_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Younger_data$mobile_navigation_usage_duration_proportion, list(Younger_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Younger_data, outcome = "mobile_navigation_usage_duration_proportion",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计

## 老年人的分析
m <- ezANOVA(data = subset(data, age_group == "Older adults"), #重复测量方差分析
             dv = .(mobile_navigation_usage_duration_proportion),  ##若有转换后数据使用转换数据
             wid = .(participants), 
             within = .(locomotion_methods, OI_navigation_legibility), 
             between_covariates = .(gender,age,education_level,digital_experience,egocentric_scale,
                                    allocentric_scale,cardinal_directions_scale,use_of_navigation,
                                    possession_of_navigation,rotation_ability),
             type = 3, detailed = T)
m$ANOVA #分析结果
Older_data <- subset(data, age_group == "Older adults")
pairwise.t.test(Older_data$mobile_navigation_usage_duration_proportion, Older_data$groups, 
                paired = TRUE, p.adjust.method = "bonferroni") # post hoc
by(Older_data$mobile_navigation_usage_duration_proportion, list(Older_data$locomotion_methods), 
   stat.desc, basic = FALSE) #locomotion_methods主效应的描述统计
by(Older_data$mobile_navigation_usage_duration_proportion, list(Older_data$OI_navigation_legibility), 
   stat.desc, basic = FALSE) #OI_navigation_legibility主效应的描述统计
data_se <- summarySE(data=Older_data, outcome = "mobile_navigation_usage_duration_proportion",
                     factor = c("locomotion_methods","OI_navigation_legibility")) 
head(data_se) #两个变量交互效应的描述统计


## =========================线性模型Generalized Linear Model (lm)=====================================

# --------------------------1：不加因变量的------------------------------------------

model <- lm(wayfinding_time ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
              gender + education_level + digital_experience + egocentric_scale + 
              allocentric_scale + cardinal_directions_scale + use_of_navigation + 
              possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(number_of_hesitation ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(number_of_turn_errors ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(mobile_navigation_usage_number ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(mobile_navigation_usage_duration ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(mobile_navigation_usage_duration_proportion ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(regression_number ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(average_regression_angle ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age 
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(time_of_cognitive_map_drawing ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(accuracy_of_cognitive_map_drawing ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age 
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

# --------------------------2：加因变量的------------------------------------------

model <- lm(accuracy_of_cognitive_map_drawing ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
               mobile_navigation_usage_number + mobile_navigation_usage_duration +
               regression_number + average_regression_angle +
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)

model <- lm(regression_number ~ locomotion_methods + OI_navigation_legibility + age_group + #去掉age
               mobile_navigation_usage_number + mobile_navigation_usage_duration +
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, data = data)
summary(model)


names(data)


model <- glm(认知地图在转接点是否存在方向误差 ~ locomotion_methods + OI_navigation_legibility + age_group +
               mobile_navigation_usage_number + mobile_navigation_usage_duration +
               regression_number + average_regression_angle +
               gender + education_level + digital_experience + egocentric_scale + 
               allocentric_scale + cardinal_directions_scale + use_of_navigation + 
               possession_of_navigation + rotation_ability, 
             data = data, family = "binomial")

summary(model)



# --------------------------计算power------------------------------------------

#计算独立样本t test的power
pwr.t.test(n = 48, d = 1.629, sig.level = 0.001, power = NULL, type = "two.sample")

