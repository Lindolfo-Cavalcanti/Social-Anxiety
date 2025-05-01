library(tidyverse)
library(skimr)
library(corrplot)
library(psych)
library(dlookr)

# Read in data
df <- read.csv("Data/enhanced_anxiety_dataset.csv")

# EDA

df = df |> mutate(across(where(is.character), as.factor))

glimpse(df)
skim(df)

df.num = df |> select(where(is.numeric))

df.num |> describe()

corrplot(
  cor(df.num),
  method = "number",
  main = "Correlation matrix of numeric variables",
  col = c("#FF0000", "#FFFFFF", "#008000"),
  cl.lim = c(-1, 1),
  number.cex = .8,
  mar = c(1, 1, 1, 1),
  type = "lower",
  addCoef.col = "#FF0000",
  outline = FALSE,
  bg = "white",
  tl.pos = "lt",
  tl.cex = 0.7,
  tl.srt = 45,
  tl.col = "#008000"
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
  ) |>
  cor(use = "complete.obs") |>
  round(2)

# Remove upper triangle
cor_mat[upper.tri(cor_mat)] <- NA
cor_mat

corPlot(
  cor_mat,
  method = "number",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
)

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
# Occupations that Medium is more frequent then low: Doctor, Nurse, Scientist, Lawyer

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

# Elders are less likely to have high Anxiety level?

# I'm kind of confused about age variable.

# Variables to be included in the General Model: Occupation,  Sleep.Hours, Physical.Activity..hrs.week., Caffeine.Intake..mg.day., Alcohol.Consumption..drinks.week., Smoking, Family.History.of.Anxiety, Stress.Level..1.10., Heart.Rate..bpm., Breathing.Rate..breaths.min., Sweating.Level..1.5., Diet.Quality..1.10., Medication, Recent.Major.Life.Event, Dizziness, Therapy.Sessions..per.month., age.categoty,

# Variables to be included in the reduced General Model: Occupation , Sleep.Hours, Caffeine.Intake..mg.day., Stress.Level..1.10., Therapy.Sessions..per.month., Family.History.of.Anxiety

# Variables to be incluided in the High Anxiety Model: Smoking, Family.History.of.Anxiety, Dizziness, Medication, Recent.Major.Life.Event, Sleep.Hours, Caffeine.Intake..mg.day., Stress.Level..1.10., Therapy.Sessions..per.month., Alcohol.Consumption..drinks.week., Physical.Activity..hrs.week., Heart.Rate..bpm., Breathing.Rate..breaths.min. , Sweating.Level..1.5. and Diet.Quality..1.10., age.categoty

# Models
