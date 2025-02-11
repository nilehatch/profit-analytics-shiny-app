tb <- read_csv("muscle_cola_all.csv")
tb
glimpse(tb)

tb_quantity <- tb |> 
  group_by(wtp) |> 
  summarise(count = n(), .groups = 'drop') |> 
  arrange(desc(wtp)) |> 
  mutate(quantity = cumsum(count))

print(n = 32, tb_quantity)

glimpse(tb)
glimpse(tb_quantity)
summary(tb_quantity)

lin_model <- lm(quantity ~ wtp, data = tb_quantity)
exp_model <- lm(log(quantity) ~ wtp, data = tb_quantity)
sig_model <- nls(quantity ~ SSlogis(wtp, Asym, xmid, scal), data = tb_quantity)

# Calculate pseudo-R2 for sigmoid
y_observed <- tb_quantity$quantity
y_predicted <- predict(sig_model)
ss_residual <- sum((y_observed - y_predicted)^2)
ss_total <- sum((y_observed - mean(y_observed))^2)
pseudo_r2 <- 1 - (ss_residual / ss_total)

# Create demand functions
fQ_lin <- function(P) coef(lin_model)[1] + coef(lin_model)[2] * P
fQ_exp <- function(P) exp(coef(exp_model)[1] + coef(exp_model)[2] * P)
fQ_sig <- function(P) {
  coef(sig_model)[1] / (1 + exp((coef(sig_model)[2] - P) / coef(sig_model)[3]))
}
  
demand_function <- fQ_lin
price_value <- 2

ggplot(tb_quantity, aes(x = wtp, y = quantity)) +
  geom_function(fun = demand_function, color = "royalblue", linewidth = 2) +
  geom_point() +
  annotate("segment", x = price_value, xend = price_value, y = 0, yend = fQ_lin(price_value), linetype = "dashed", color = "royalblue") +
  annotate("segment", x = 0, xend = price_value, y = fQ_lin(price_value), yend = fQ_lin(price_value), linetype = "dashed", color = "royalblue") +
  annotate("point", x = price_value, y = fQ_lin(price_value), color = "royalblue", fill = "white", shape = 21, size = 4) +
  labs(title = paste("Plot"), x = "Price (WTP)", y = "Quantity") +
  scale_x_continuous(limits = c(0, 1.25*max(tb_quantity$wtp, na.rm = T)), labels = scales::dollar_format()) + 
  scale_y_continuous(limits = c(0, max(tb_quantity$quantity, na.rm = TRUE)), labels = scales::comma) +
  annotate("text", x = max(tb_quantity$wtp, na.rm = T) * 0.95, y = max(tb_quantity$quantity, na.rm = T) * 0.95,
           label = paste0("Price: $", price_value, "\nQuantity: ", round(fQ_lin(price_value), 2)),
           hjust = 1, vjust = 1, color = "royalblue", fontface = 2, size = 5) +
#  annotate("text",           x = max(tb_quantity$wtp) * 0.95,  # Place in the lower-right corner           y = max(tb_quantity$quantity) * 0.5, #Place in the lower-right corner           label = as.expression(fQ_lin),           hjust = 1, vjust = 1, color = "black", fontface = 2, size = 7, parse = TRUE           ) +
  theme_minimal()


max(tb_quantity$wtp, na.rm = T)
