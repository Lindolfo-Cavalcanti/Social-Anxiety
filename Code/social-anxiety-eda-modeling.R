library(tidyverse)
library(ggpubr)
library(caret)
use('skimr', 'skim')
use('psych', 'describe')
use('corrplot', 'corrplot')
use('car', 'leveneTest')
use('report', 'report')

# Read in data
df <- read.csv("Data/enhanced_anxiety_dataset.csv")

# EDA

df = df |> mutate(across(where(is.character), as.factor))

glimpse(df)
skim(df)

df.num = df |> select(where(is.numeric))

df.num |> describe()

cor_matrix <- cor(df.num)

# Custom color palette (blue to red)
col <- colorRampPalette(c(
  "#0571b0",
  "#92c5de",
  "#f7f7f7",
  "#f4a582",
  "#ca0020"
))(200)

corrplot(
  cor_matrix,
  method = "circle", # Show circles
  type = "lower", # Lower triangle only
  diag = FALSE, # Hide diagonal
  tl.col = "black", # Text label color
  tl.cex = 0.8, # Text label size
  tl.srt = 45, # Text label rotation
  col = col, # Color gradient
  bg = "white", # Background color
  addCoef.col = "black", # Coefficient text color
  number.cex = 0.6, # Coefficient text size
  number.font = 1, # Coefficient font (1=plain)
  mar = c(0, 0, 1, 0), # Margins
  cl.pos = "n", # Color legend on right
  cl.ratio = 0.2, # Color legend width ratio
  cl.cex = 0.7 # Color legend text size
)

# High with anxiety: sleep hours(-0.49), Caffeine intake (0.35), stress level (0.67), therapy sessions (0.52)

# Compute correlation matrix
cor_mat <- df.num |>
  select(
    Anxiety.Level..1.10.,
    Stress.Level..1.10.,
    Sleep.Hours,
    Caffeine.Intake..mg.day.,
    Therapy.Sessions..per.month.
  )

cor(cor_mat) %>%
  as.data.frame() %>%
  mutate(across(everything(), ~ round(., 2))) %>%
  View()

df$Anxiety.Category[df$Anxiety.Level..1.10. <= 3] = "Low"
df$Anxiety.Category[
  df$Anxiety.Level..1.10. >= 4 & df$Anxiety.Level..1.10. <= 6
] = "Medium"
df$Anxiety.Category[df$Anxiety.Level..1.10. >= 7] = "High"
df$Anxiety.Category = as.factor(df$Anxiety.Category)

df |>
  group_by(Anxiety.Category, Gender) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>
  view()

# No difference in Gender

df |>
  group_by(Anxiety.Category) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>
  view()

# Difference only in High anxiety group (Alcohol Consuption, Physical Activity, Hearth Rate, Breathing Rate, Sweating Level, Diet Quality and maybe Age)

# I think a model to identify only High anxiety group would be better than one to differentiate Low and Medium anxiety groups.

# Looking for diferences between ocuppations in anxiety group using barplots

count.of.anxity.in.occupation = df |>
  count(Anxiety.Category, Occupation)


count.of.anxity.in.occupation |>
  ggplot(aes(x = Anxiety.Category, y = n, fill = Occupation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribuição por Categoria de Ansiedade e Ocupação",
    x = "Categoria de Ansiedade",
    y = "Frequência",
    fill = "Ocupação"
  ) +
  theme_minimal()

count.of.anxity.in.occupation |>
  ggplot(aes(x = Anxiety.Category, y = n, fill = Anxiety.Category)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Occupation) +
  labs(
    title = "Distribuição por Categoria de Ansiedade (um gráfico por ocupação)",
    x = "Categoria de Ansiedade",
    y = "Frequência"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# High Anxiety level doesn't differenciate between occupations, in Medium and Low anxiety levels it does probably. The ideal would be to test if there is a difference between occupations in Low and Medium anxiety levels with statistical tests, but it would be too much work.
# Occupations that Medium is more frequent then low and a bit higher in high anxiety: Doctor, Engineer, Scientist, Lawyer, Student, Freelancer

# Smoking, Family History of Anxiety, Dizziness, Medication, Major Evente in life.

vars_to_plot <- c(
  "Smoking",
  "Family.History.of.Anxiety",
  "Dizziness",
  "Medication",
  "Recent.Major.Life.Event"
)

df_long <- df |>
  pivot_longer(
    cols = all_of(vars_to_plot),
    names_to = "Variable",
    values_to = "Value"
  )

df_counts <- df_long |>
  count(Variable, Value, Anxiety.Category)

# Plotar gráfico com facetas para cada variável
ggplot(df_counts, aes(x = Anxiety.Category, y = n, fill = Value)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable) +
  labs(
    title = "Distribuição da Ansiedade por Variáveis Explicativas",
    x = "Categoria de Ansiedade",
    y = "Frequência",
    fill = "Valor da Variável"
  ) +
  theme_minimal()

# All factor variables (not Gender and Occupation) show differences between high levels of anxiety and lower ones

# In to distinguish low and medium levels, Family history is relevant.

# Age?

# categories: 18 to 25 = young
#             26 to 30 = young adults
#             31 to 59 = adults
#             > 60 = elders

df <- df |>
  mutate(
    age.category = case_when(
      Age >= 18 & Age <= 25 ~ "young",
      Age >= 26 & Age <= 30 ~ "young adults",
      Age >= 31 & Age <= 59 ~ "adults",
      Age >= 60 ~ "elders",
      TRUE ~ NA_character_ # para lidar com valores fora do intervalo ou NA
    )
  )

df |>
  group_by(age.category, Anxiety.Category) |>
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) |>
  view()

df |>
  count(age.category, Anxiety.Category) |>
  ggplot(aes(x = Anxiety.Category, y = n, fill = age.category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribuição da Ansiedade por Idade",
    x = "Categoria de Ansiedade",
    y = "Frequência",
    fill = "Idade"
  ) +
  theme_minimal()

# I'm kind of confused about age variable.

# T-tests for variables with YES/NO answers

# Variace test

car::leveneTest(Anxiety.Level..1.10. ~ Smoking, data = df) |> print()
car::leveneTest(Anxiety.Level..1.10. ~ Family.History.of.Anxiety, data = df)
car::leveneTest(Anxiety.Level..1.10. ~ Dizziness, data = df)
car::leveneTest(Anxiety.Level..1.10. ~ Medication, data = df)
car::leveneTest(Anxiety.Level..1.10. ~ Recent.Major.Life.Event, data = df)

# T-tests

t.result.smoking <- t.test(df$Anxiety.Level..1.10. ~ df$Smoking)
t.result.family.history <- t.test(
  df$Anxiety.Level..1.10. ~ df$Family.History.of.Anxiety
)
t.result.dizziness <- t.test(df$Anxiety.Level..1.10. ~ df$Dizziness)
t.result.medication <- t.test(df$Anxiety.Level..1.10. ~ df$Medication)
t.result.recent.major.life.event <- t.test(
  df$Anxiety.Level..1.10. ~ df$Recent.Major.Life.Event
)

t.result.smoking |> report::report() |> print()
t.result.family.history |> report::report() |> print()
t.result.dizziness |> report::report() |> print()
t.result.medication |> report::report() |> print()
t.result.recent.major.life.event |> report::report() |> print()

wilcox.result.smoking <- wilcox.test(
  df$Anxiety.Level..1.10. ~ df$Smoking
)
wilcox.result.family.history <- wilcox.test(
  df$Anxiety.Level..1.10. ~ df$Family.History.of.Anxiety
)

wilcox.result.dizziness <- wilcox.test(
  df$Anxiety.Level..1.10. ~ df$Dizziness
)

wilcox.result.medication <- wilcox.test(
  df$Anxiety.Level..1.10. ~ df$Medication
)

wilcox.result.recent.major.life.event <- wilcox.test(
  df$Anxiety.Level..1.10. ~ df$Recent.Major.Life.Event
)

wilcox.result.smoking |> report::report()
wilcox.result.family.history |> report::report()
wilcox.result.dizziness |> report::report()
wilcox.result.medication |> report::report()
wilcox.result.recent.major.life.event |> report::report()

# Testing diference in age categories

ggplot(df, aes(sample = Anxiety.Level..1.10.)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~age.category)

car::leveneTest(Anxiety.Level..1.10. ~ age.category, data = df)

kruskal.test(Anxiety.Level..1.10. ~ age.category, data = df)

pairwise.wilcox.test(
  df$Anxiety.Level..1.10.,
  df$age.category,
  p.adjust.method = "bonferroni"
)

library(ggpubr)

ggplot(df, aes(x = age.category, y = Anxiety.Level..1.10.)) +
  geom_boxplot() +
  stat_compare_means(
    comparisons = list(
      c("adults", "elders"),
      c("adults", "young"),
      c("young adults", "elders"),
      c("young adults", "young")
    ),
    method = "wilcox.test",
    label = "p.signif"
  ) +
  labs(x = "Age Group", y = "Anxiety Level (1-10)")

# Categories for Ocucupations

table(df$Occupation)

# I can make two kinds of categories: Area (health, art, education) or High and lower probability of anxiety. In both I'd add other as a unique category
# I'll make the second one

df <- df |>
  mutate(
    risk.occupation = case_when(
      Occupation == 'Doctor' |
        Occupation == 'Engineer' |
        Occupation == 'Scientist' |
        Occupation == 'Lawyer' |
        Occupation == 'Student' |
        Occupation == 'Freelancer' ~
        'Yes',
      TRUE ~ 'No'
    )
  )

df <- df |>
  mutate(
    risk.occupation = ifelse(Occupation == 'Other', 'Other', risk.occupation)
  )

df$risk.occupation = as.factor(df$risk.occupation)

table(df$risk.occupation)

df |>
  count(risk.occupation, Anxiety.Category) |>
  ggplot(aes(x = Anxiety.Category, y = n, fill = risk.occupation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribuição da Ansiedade por Categoria de Profissão",
    x = "Categoria de Ansiedade",
    y = "Frequência",
    fill = "Profissão"
  ) +
  theme_minimal()

# Testing diference in risk.occupation

ggplot(df, aes(sample = Anxiety.Level..1.10.)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~risk.occupation)

car::leveneTest(Anxiety.Level..1.10. ~ risk.occupation, data = df)

kruskal.test(Anxiety.Level..1.10. ~ risk.occupation, data = df)

pairwise.wilcox.test(
  df$Anxiety.Level..1.10.,
  df$risk.occupation,
  p.adjust.method = "bonferroni"
)

ggplot(df, aes(x = risk.occupation, y = Anxiety.Level..1.10.)) +
  geom_boxplot(
    fill = "lightblue",          # Softer fill color
    outlier.shape = NA,          # Hide outliers (optional)
    width = 0.6,                 # Adjust box width
    alpha = 0.7                  # Slight transparency
  ) +
  stat_compare_means(
    comparisons = list(
      c("Yes", "No"),
      c("Yes", "Other"),
      c("No", "Other")
    ),
    method = "wilcox.test",
    label = "p.signif",
    step.increase = 0.1,         # Space between brackets
    tip.length = 0.01,           # Length of comparison lines
    size = 4,                    # Asterisk size
    vjust = 0.5                  # Vertical adjustment
  ) +
  labs(
    x = "Occupation Risk Group", 
    y = "Anxiety Level (1-10)",
    title = "Anxiety Levels by Occupation Risk Category"
  ) +
  theme_minimal() +              # Clean background
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11))


# Variables to be included in the General Model: risk.occupation,  Sleep.Hours, Physical.Activity..hrs.week., Caffeine.Intake..mg.day., Alcohol.Consumption..drinks.week., Smoking, Family.History.of.Anxiety, Stress.Level..1.10., Heart.Rate..bpm., Breathing.Rate..breaths.min., Sweating.Level..1.5., Diet.Quality..1.10., Medication, Recent.Major.Life.Event, Dizziness, Therapy.Sessions..per.month., age.categoty,

# Variables to be included in the reduced General Model: risk.occupation , Sleep.Hours, Caffeine.Intake..mg.day., Stress.Level..1.10., Therapy.Sessions..per.month., Family.History.of.Anxiety

# Variables to be incluided in the High Anxiety Model: Smoking, Family.History.of.Anxiety, Dizziness, Medication, Recent.Major.Life.Event, Sleep.Hours, Caffeine.Intake..mg.day., Stress.Level..1.10., Therapy.Sessions..per.month., Alcohol.Consumption..drinks.week., Physical.Activity..hrs.week., Heart.Rate..bpm., Breathing.Rate..breaths.min. , Sweating.Level..1.5. and Diet.Quality..1.10., age.categoty

# Models
