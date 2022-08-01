exp <- read_csv('../data/exp_df.csv.bz2')
votes <- read_csv('../data/votes_df.csv.bz2')
replies <- read_csv('../data/reply_df.csv.bz2')

df <- exp %>%
  left_join(votes %>% dplyr::select(-time_stamp)) %>%
  left_join(replies %>% dplyr::select(-time_stamp)) %>%
  mutate(identified = 1 - anonymous,
         score = ifelse(is.na(score), 0, score),
         num = ifelse(is.na(num), 0, num),
         up = as.numeric(score == 1),
         dn = as.numeric(score == -1),
         rp = as.numeric(num > 0),
         ds = as.Date(as.POSIXct(first_view, origin = '1970-01-01')))

outcome.levels <- c('Up-vote', 'Down-vote', 'Reply')

by_viewer <- df %>%
  group_by(user_id) %>%
  summarise(anon = sum(anonymous),
            up = sum(up),
            dn = sum(dn),
            rp = sum(rp),
            iden = sum(identified)) %>%
  mutate(exp_iden = anon * 19,
         excess = iden - exp_iden)

pval <- Vectorize(function(x, y) {
    binom.test(x, x + y, p = 0.05)$p.value
  })

by_viewer_with_stats <- by_viewer %>%
  ungroup() %>%
  mutate(n = anon + iden,
         x = anon,
         z = 1.96,
         nt = n + z^2,
         pt = (1/nt) * (x + z^2 / 2),
         intv = z * sqrt(pt * (1 - pt) / nt),
         se = sqrt(pt * (1 - pt) / nt),
         zs = (pt - 0.05) / se,
         pval = pval(anon, iden),
         lcl = pt - intv,
         ucl = pt + intv, 
         sig = if_else(lcl > 0.05 | ucl < 0.05, 'yes', 'no')) %>%
  arrange(-excess) %>%
  mutate(excess_rk = 1:n())


goodness <- by_viewer_with_stats %>%
  mutate(plt51 = pnorm(0.051, mean = pt, sd = se),
         plt50 = pnorm(0.050, mean = pt, sd = se),
         plt49 = pnorm(0.049, mean = pt, sd = se),
         pbad  = round(1 - plt50, 1),
         pgood = round(plt51 - plt49, 1)
         ) %>%
  dplyr::select(user_id, plt50, pgood, pt, nt, se, zs, anon, iden)

df_clean <- df %>%
  inner_join(goodness %>% filter(plt50 < 0.9) %>% select(user_id))

