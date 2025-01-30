tb <- read_csv("muscle_cola_wqq.csv")
tb

glimpse(tb)

if (!all(c("wtp", "quantity", "quantity_at_fraction") %in% names(tb))) {
  stop("The dataset must contain 'wtp', 'QQquantity', and 'quantity_at_fraction' columns.")
}

fraction <- .5
tb

# Add computed price at fraction
tb <- tb %>% mutate(price_at_fraction = wtp * fraction)

tb_long <- tb |> 
    pivot_longer(
      cols = c(wtp, price_at_fraction),
      names_to = "price_type",
      values_to = "price"
    ) %>%
    pivot_longer(
      cols = c(quantity, quantity_at_fraction),
      names_to = "quantity_type",
      values_to = "quantity"
    ) %>%
    filter(
      # Keep only aligned rows: wtp with quantity and price_at_fraction with quantity_at_fraction
      (price_type == "wtp" & quantity_type == "quantity") |
      (price_type == "price_at_fraction" & quantity_type == "quantity_at_fraction")
    ) |> 
#select(price, quantity) %>%
  group_by(price) %>%
  summarise(sum_quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(price)) %>%
  mutate(quantity = cumsum(sum_quantity))  # Apply cumulative sum



tb <- tb_long
print(n = 42, tb)

lin_model <- lm(quantity ~ price, data = tb)
exp_model <- lm(log(quantity) ~ price, data = tb)

# Sigmoid model
sig_model <- tryCatch(
  nls(quantity ~ SSlogis(price, Asym, xmid, scal), data = tb),
  error = function(e) NULL
)


