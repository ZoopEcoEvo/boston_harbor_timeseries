SCoPE: Seasonality in Copepod Physiology and Ecology
================
2026-03-10

- [Boston Harbor Seasonality](#boston-harbor-seasonality)
- [CTmax Data](#ctmax-data)
- [Body Size](#body-size)
- [Respiration Rate TPCs](#respiration-rate-tpcs)

## Boston Harbor Seasonality

Boston Harbor exhibits the typical seasonality expected for a temperate
coastal system - waters typically drop to \<5°C in the winter and then
rise to ~20°C during the summers.

``` r
bharb_temps %>% 
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  ggplot(aes(x = month, y = mean_temp, group = year, colour = year)) + 
  geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = c(1:12)) + 
  scale_colour_viridis_c(option = "G") + 
  labs(x = "Month", 
       y = "Mean Temp. (°C)") + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

Temperatures in both March and August (near the minimum and maximum
temperatures, respectively) have been rising steadily since at least the
1960s.

``` r
bharb_temps %>% 
  mutate(month = lubridate::month(date),
         year = lubridate::year(date)) %>% 
  filter(month %in% c(3, 8)) %>% 
  mutate(month = if_else(month == 3, "March", "August"),
         month = fct_relevel(month, "March", "August")) %>% 
  ggplot(aes(x = year, y = mean_temp)) + 
  facet_wrap(.~month, scales = "free_y") + 
  geom_line() + 
  geom_smooth() + 
  labs(x = "Year", 
       y = "Mean Temp. (°C)") + 
  theme_matt_facets()
```

<img src="../Figures/markdown/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

## CTmax Data

``` r

trait_data %>% 
  mutate(date = as_date(collection_datetime)) %>% 
  ggplot(aes(x = date, y = ctmax, colour = species)) + 
  geom_point(position = position_dodge(width = 0.1)) + 
  theme_matt() + 
  theme(legend.position = "right")
```

<img src="../Figures/markdown/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

``` r

sample_sizes = trait_data %>% 
  count(species)

trait_data %>% 
ggplot(aes(x = species, y = ctmax)) + 
  geom_boxplot() + 
  geom_point() + 
  geom_text(data = sample_sizes, aes(y = min(trait_data$ctmax) - 1, label = n)) + 
  theme_matt_facets() + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = 0.5))
```

<img src="../Figures/markdown/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

``` r

trait_data %>% 
  group_by(species, sex) %>% 
  summarise(mean_ctmax = mean(ctmax, na.rm = T), 
            ctmax_se = sd(ctmax) / sqrt(n())) %>% 
ggplot(aes(x = sex, y = mean_ctmax, group = species)) + 
  facet_wrap(species~.) + 
  geom_line(linewidth = 1) + 
  geom_errorbar(aes(ymin = mean_ctmax - ctmax_se, ymax = mean_ctmax + ctmax_se),
                width = 0.2, linewidth = 1) + 
  geom_point(size = 2) + 
  theme_matt_facets()
```

<img src="../Figures/markdown/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

``` r
trait_data %>% 
  ggplot(aes(x = hours_in_lab, y = ctmax, group = collection_datetime)) + 
  facet_wrap(species~.) + 
  geom_point() + 
  geom_smooth(method = "lm",
              se = F) + 
  theme_matt_facets()
```

<img src="../Figures/markdown/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

## Body Size

``` r

size_sample_sizes = trait_data %>% 
  drop_na(size) %>% 
  count(species)

trait_data %>% 
  drop_na(size) %>% 
  mutate(species = fct_reorder(species, size, median, .desc = T)) %>% 
ggplot(aes(x = species, y = size)) + 
  geom_boxplot() + 
  geom_point() + 
  geom_text(data = size_sample_sizes, aes(y = min(trait_data$size, na.rm = T) - 0.1, label = n)) + 
  theme_matt_facets() + 
  theme(axis.text.x = element_text(angle = 300, hjust = 0, vjust = 0.5))
```

<img src="../Figures/markdown/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
trait_data %>% 
  group_by(species, sex) %>% 
  summarise(mean_size = mean(size, na.rm = T), 
            size_se = sd(size, na.rm = T) / sqrt(n())) %>% 
ggplot(aes(x = sex, y = mean_size, group = species)) + 
  facet_wrap(species~.) + 
  geom_line(linewidth = 1) + 
  geom_errorbar(aes(ymin = mean_size - size_se, ymax = mean_size + size_se),
                width = 0.2, linewidth = 1) + 
  geom_point(size = 2) + 
  theme_matt_facets()
```

<img src="../Figures/markdown/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r

high_abund = trait_data %>% 
  count(species, sex) %>% 
  filter(n > 3)

trait_data %>% 
  filter(sex != "j", species %in% unique(high_abund$species)) %>% 
ggplot(aes(x = size, y = ctmax, colour = sex)) + 
  facet_wrap(species~., scales = "free") + 
  geom_point() + 
  geom_smooth(method = "lm")
```

<img src="../Figures/markdown/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

``` r

pseudocal = trait_data %>% 
  filter(species == "Pseudocalanus sp.")

ggplot(pseudocal, aes(x = size, y = ctmax)) + 
  geom_point(size = 4)
```

<img src="../Figures/markdown/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

## Respiration Rate TPCs

``` r
tpc_rates %>%
  ggplot(aes(x = temp, y = rate, colour = treatment)) +
  facet_wrap(treatment~., nrow = 2) +
  geom_hline(yintercept = 0, colour = "grey") + 
  geom_point() + 
  geom_smooth(se = F) + 
  theme_bw()
```

<img src="../Figures/markdown/unnamed-chunk-11-1.png" style="display: block; margin: auto;" />

When estimating TPCs, we will use just rate estimates with an R^2 of
greater than 0.7. We will also censor negative respiration rate
estimates to zero.

``` r
subset_rates = tpc_rates %>%
  filter(rsq >0.7) %>%
  mutate(rate = if_else(rate < 0, 0, rate))

tpc_rates %>%
  filter(rsq >0.7) %>%
  mutate(rate = if_else(rate < 0, 0, rate)) %>%
  group_by(treatment, temp) %>%  
  summarise(rate = mean(rate)) %>% 
  ggplot(aes(x = temp, y = rate, colour = treatment)) +
  facet_wrap(treatment~., nrow = 2) +
  geom_point(data = subset_rates, aes(x = temp, y = rate), 
             alpha = 0.3) + 
  geom_smooth(data = subset_rates, aes(x = temp, y = rate), se = F) + 
  geom_point(size = 3) + 
  theme_bw() + 
  theme(legend.position = "none")
```

<img src="../Figures/markdown/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r
library(rTPC)
library(nls.multstart)
library(broom)

tpc_params = data.frame()
pred_data = data.frame()
species_list = unique(subset_rates$treatment)

for(s in species_list){

d = subset_rates %>% 
  filter(treatment == s) 

# choose model
mod = 'sharpschoolhigh_1981'

# get start vals
start_vals <- get_start_vals(
  d$temp,
  d$rate,
  model_name = 'sharpeschoolhigh_1981'
)

# get limits
low_lims <- get_lower_lims(d$temp, d$rate, model_name = 'sharpeschoolhigh_1981')

upper_lims <- get_upper_lims(
  d$temp,
  d$rate,
  model_name = 'sharpeschoolhigh_1981'
)

# fit model
fit <- nls_multstart(
  rate ~ sharpeschoolhigh_1981(temp = temp, r_tref, e, eh, th, tref = 15),
  data = d,
  iter = 500,
  start_lower = start_vals - 10,
  start_upper = start_vals + 10,
  lower = low_lims,
  upper = upper_lims,
  supp_errors = 'Y',
  lhstype = 'random'
)

# calculate additional traits
sp_params = calc_params(fit) %>%
  # round for easy viewing
  mutate_all(round, 2) %>% 
  mutate(species = s)

tpc_params = bind_rows(tpc_params, sp_params)

# predict new data
new_data <- data.frame(temp = seq(min(subset_rates$temp), max(subset_rates$temp) + 5, 0.25))
preds <- augment(fit, newdata = new_data) %>% 
  mutate(species = s)

pred_data = bind_rows(pred_data, preds)

}

resp_ctmax_comp = trait_data %>% 
  ungroup() %>% 
  mutate(species = as.character(species)) %>% 
  filter(species %in% c("Acartia hudsonica", "Pseudocalanus sp.")) %>% 
  select(species, sex, ctmax) %>% 
  filter(sex == "f")
  

# plot data and model fit
ggplot(subset_rates, aes(temp, rate, colour = treatment)) +
  geom_point(alpha = 0.5) +
  geom_line(data = pred_data, 
            aes(temp, .fitted, colour = species), 
            linewidth = 2) +
  geom_boxplot(data = resp_ctmax_comp, 
               aes(x = ctmax, y = 0.1, fill = species), 
               colour = "black", width = 0.05) + 
  theme_bw(base_size = 12) +
  labs(
    x = 'Temperature (ºC)',
    y = 'Metabolic rate',
    colour = "Species"
  )  +
  guides(fill = "none") + 
  theme_matt() + 
  theme(legend.position = "bottom")
```

<img src="../Figures/markdown/tpc-fits-1.png" style="display: block; margin: auto;" />
