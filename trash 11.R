femininity <- tibble(femininity = rep(quantile(h$femininity, 0.9), 92),
                     min_pressure_std = rep(mean(h$min_pressure_std), 92),
                     damage_norm_std = seq(-1.0, 1.0, length.out = 92))

masculinity <- tibble(femininity = rep(quantile(h$femininity, 0.1), 92),
                      min_pressure_std = rep(mean(h$min_pressure_std), 92),
                      damage_norm_std = seq(-1.0, 1.0, length.out = 92))

fem <- fitted(h11.8, newdata = femininity) %>% cbind(femininity)

masc <- fitted(h11.8, newdata = masculinity) %>% cbind(masculinity)

ggplot(data = fem, mapping = aes(x = damage_norm_std, y = Estimate)) +
  geom_line(color = "red") + geom_ribbon(mapping = aes(x = damage_norm_std,
                                                       ymin = Q2.5,
                                                       ymax = Q97.5),
                                         fill = "red", alpha = 0.1) +
  geom_line(data = masc, mapping = aes(x = damage_norm_std, y = Estimate),
            color = "blue") + geom_ribbon(data = masc, mapping = aes(x = damage_norm_std,
                                                                     ymin = Q2.5,
                                                                     ymax = Q97.5),
                                          fill = "blue", alpha = 0.1)

ggplot(data = masc, mapping = aes(x = damage_norm_std, y = Estimate)) +
  geom_line(color = "blue") + geom_ribbon(mapping = aes(x = damage_norm_std,
                                                        ymin = Q2.5,
                                                        ymax = Q97.5),
                                          fill = "blue", alpha = 0.1)
