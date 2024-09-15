options(scipen = 999)

# ggplot to display median salary
recent.grads %>%
  ggplot(aes(x = Median)) +
  geom_histogram()

# Median salary by major
table(recent.grads$Major_category)

# Median salary by major category
recent.grads %>%
  mutate(Major_category = fct_reorder(Major_category, Median)) %>%
  ggplot(aes(x = Major_category, y = Median,
             fill = Major_category)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(labels = dollar_format()) +
  expand_limits(y = 0) +
  theme(legend.position = "none") 


# What majors earn the most?
recent.grads %>%
  arrange(desc(Median)) %>%
  select(Major, Major_category, Median, P25th, P75th) %>%
  view()

# Top 20 salaries by major
recent.grads %>%
  arrange(desc(Median)) %>%
  select(Major, Major_category, Median, P25th, P75th) %>%
  head(20) %>%
  mutate(Major = str_to_title(Major),
         Major = fct_reorder(Major, Median)) %>%
  ggplot(aes(x = Major, y = Median,
             color = Major_category)) +
  geom_point() +
  geom_errorbar(aes(ymin = P25th, ymax = P75th)) +
  expand_limits(y = 0) +
  coord_flip()

# Lowest 20 salaries by major
recent.grads %>%
  arrange(desc(Median)) %>%
  select(Major, Major_category, Median, P25th, P75th) %>%
  tail(20) %>%
  mutate(Major = str_to_title(Major),
         Major = fct_reorder(Major, Median)) %>%
  ggplot(aes(x = Major, y = Median,
             color = Major_category)) +
  geom_point() +
  geom_errorbar(aes(ymin = P25th, ymax = P75th)) +
  expand_limits(y = 0) +
  coord_flip()

# We can see the highest paid majors are STEM and business
# The top 6 are all engineering
# The lowest paid majors are education, psychology, social work, arts, and humanities


# sub data frame
majors_processed <- recent.grads %>%
  arrange(desc(Median)) %>%
  select(Major, Major_category, Median, P25th, P75th, Sample_size) %>%
  mutate(Major = str_to_title(Major),
         Major = fct_reorder(Major, Median))

majors_processed %>%
  ggplot(aes(Sample_size, Median)) +
  geom_point() +
  geom_text(aes(label = Major), check_overlap = TRUE, 
            vjust = -.05, hjust = -.05, size = 3) +
  scale_x_log10(limits = c(1, 15000)) +
  scale_y_continuous(labels = dollar_format())

# Top 20 majors with at least 30 graduates
majors_processed %>%
  filter(Sample_size >= 30) %>%
  head(20) %>%
  ggplot(aes(x = Major, y = Median,
             color = Major_category)) +
  geom_point() +
  geom_errorbar(aes(ymin = P25th, ymax = P75th)) +
  expand_limits(y = 0) +
  coord_flip() +
  labs(
    title = "What are the Highest Earning Majors?",
    subtitle = "Top 20 majors with at least 30 graduates surveyed.
          Bars represent the 25th to 75th percentile",
    x = "",
    y = "Median Salary of Graduates")

# Lowest earning majors with at least 30 graduates
majors_processed %>%
  filter(Sample_size >= 30) %>%
  tail(20) %>%
  ggplot(aes(x = Major, y = Median,
             color = Major_category)) +
  geom_point() +
  geom_errorbar(aes(ymin = P25th, ymax = P75th)) +
  expand_limits(y = 0) +
  coord_flip() +
  labs(
    title = "What are the Lowest Earning Majors?",
    subtitle = "Bottom 20 majors with at least 30 graduates surveyed.
          Bars represent the 25th to 75th percentile",
    x = "",
    y = "Median Salary of Graduates")

# What are the most common majors and categories
# Use count()

recent.grads %>%
  count(Major_category) %>%
  arrange(desc(n))

# Number of graduates from each category
recent.grads %>%
  count(Major_category,
        wt = Total) %>%
  arrange(desc(n))

# Most popular major categories
recent.grads %>%
  count(Major_category, wt = Total, sort = TRUE) %>%
  mutate(Major_category = fct_reorder(Major_category, n)) %>%
  ggplot(aes(Major_category, n, fill = Major_category)) +
  geom_col() +
  coord_flip() +
  expand_limits(y = 0) +
  labs(
    x = "",
    y = "Number of Graduates")

#Most common majors
recent.grads %>%
  count(Major, wt = Total, sort = TRUE) %>%
  mutate(Major = fct_reorder(Major, n)) %>%
  head(20) %>%
  ggplot(aes(Major, n, fill = Major)) +
  geom_col() +
  coord_flip() +
  expand_limits(y = 0) +
  labs(
    x = "",
    y = "Number of Graduates")

gender_2 <- recent.grads %>%
  arrange(desc(Total)) %>%
  head(26) %>%
  mutate(Major = fct_reorder(Major, Total)) %>%
  gather(key = Gender, value = Number, Men, Women)

# Most common majors by gender
ggplot(data = gender_2, 
       aes(x = Major, y = Number, fill = Gender)) +
  geom_col() +
  scale_y_continuous(labels = comma_format()) +
  coord_flip()

by_major_category <- recent.grads %>%
  filter(!is.na(Total)) %>%
  group_by(Major_category) %>%
  summarize(
    Men = sum(Men),
    Women = sum(Women),
    Total = sum(Total),
    MedianSalary = median(Median),
    MedianSalaryWT = sum(Median * Sample_size) / sum(Sample_size)) %>%
  mutate(ShareWomen = Women / Total)

# Median salaries and percentage of women in that major
by_major_category %>%
  ggplot(aes(ShareWomen, MedianSalaryWT)) +
  geom_point()

by_major_category %>%
  ggplot(aes(ShareWomen, MedianSalaryWT)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_text_repel(aes(label = Major_category), force = .2) +
  expand_limits(y = 0)

gender_2 %>%
  select(Major, Total, ShareWomen, Sample_size, Median) %>%
  lm(Median ~ ShareWomen, data = ., weights = Sample_size) %>%
  summary()

gender_2 %>%
  ggplot(aes(x = ShareWomen, y = Median)) +
  geom_point() +
  geom_smooth(method = "lm") +
  expand_limits(y = 0)

gender_2 %>%
  select(Major, Major_category, Total, ShareWomen, Sample_size, Median) %>%
  add_count(Major_category) %>%
  filter(n >= 10) %>%
  nest(-Major_category) %>%
  mutate(model = map(
    data, ~lm(Median ~ ShareWomen, data = ., weights = Sample_size)),
    tidied = map(model, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "ShareWomen") %>%
  arrange(estimate) %>%
  mutate(fdr = p.adjust(p.value, method = "fdr"))

# Interactive Graph
g <- gender_2 %>%
  mutate(
    Major_category = fct_lump(Major_category, 4)) %>%
  ggplot(aes(x = ShareWomen, y = Median,
             color = Major_category,
             size = Sample_size,
             label = Major)) +
  geom_point() +
  geom_smooth(aes(group = 1), method = "lm") +
  expand_limits(y = 0) +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = dollar_format())

plotly::ggplotly(g)

# Median unemployment rate by major category
gender_2 %>%
  group_by(Major_category) %>%
  summarize(
    median_unemployment_rate = median(Unemployment_rate) * 100) %>%
  arrange(median_unemployment_rate)
  
 
  









