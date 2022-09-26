data <- eventdataR::hospital

bench::mark(
  all = data %>% filter_activity_presence(activities = c("ordertarief", "aanname laboratoriumonderzoek", "calcium", "glucose"), method = "all"),
  none = data %>% filter_activity_presence(activities = c("ordertarief", "aanname laboratoriumonderzoek", "calcium", "glucose"), method = "none"),
  one_of = data %>% filter_activity_presence(activities = c("ordertarief", "aanname laboratoriumonderzoek", "calcium", "glucose"), method = "one_of"),
  exact = data %>% filter_activity_presence(activities = c("ordertarief", "aanname laboratoriumonderzoek", "calcium", "glucose"), method = "exact"),
  only = data %>% filter_activity_presence(activities = c("ordertarief", "aanname laboratoriumonderzoek", "calcium", "glucose"), method = "only"),
  iterations = 10,
  check = FALSE
)