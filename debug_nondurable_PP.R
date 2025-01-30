tb <- read_csv("muscle_cola_wq.csv")
tb <- read_csv("25+Donut+WTP+Q_January+27,+2025_10.16.csv")

tb
glimpse(tb)

tb_wide <- tb
tb <- tb_wide |> 
  pivot_longer(cols = P0:P3,
               names_prefix = "P",
               names_to = "price",
               values_to = "quantity") |> 
  mutate(
    price = as.numeric(price),   # Ensure price is numeric
    quantity = as.numeric(quantity)  # Ensure quantity is numeric
  ) |> 
  relocate(c(price, quantity)) |> 
  filter(!is.na(price) & !is.na(quantity))  # Remove rows with NA values

tb |> select(c(price, quantity))

tbq <- tb |> 
      group_by(price) |> 
      summarise(quantity = sum(quantity, na.rm = TRUE), .groups = "drop")

tbq

SSlogis(tb$price, Asym, xmid, scal)
sig_model <- nls(quantity ~ SSlogis(price, Asym, xmid, scal), data = tb)

summary(tbq)
plot(tbq$price, tbq$quantity, main = "Data for Sigmoid Fit", xlab = "Price (WTP)", ylab = "Quantity")


sig_model <- nls(
  quantity ~ SSlogis(price, Asym, xmid, scal),
  data = tbq,
  start = list(Asym = max(tbq$quantity, na.rm = T), 
               xmid = mean(tbq$price, na.rm = T), 
               scal = diff(range(tbq$price, na.rm = T)) / 5)
)

sig_model <- nls(
  quantity ~ SSlogis(price, Asym, xmid, scal),
  data = tbq,
  start = list(Asym = max(tbq$quantity, na.rm = TRUE), 
               xmid = mean(tbq$price, na.rm = TRUE), 
               scal = diff(range(tbq$price, na.rm = TRUE)) / 5),
  algorithm = "port",
  lower = c(Asym = 0, xmid = min(tbq$price), scal = 0.01),
  upper = c(Asym = Inf, xmid = max(tbq$price), scal = Inf)
)

all(diff(tbq$quantity) >= 0) || all(diff(tbq$quantity) <= 0) # TRUE means monotonic
