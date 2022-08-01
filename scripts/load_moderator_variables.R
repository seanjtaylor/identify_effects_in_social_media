# construct moderator variables
cat('Using the_lag={the_lag}' %>% glue::glue())

# Step 1: a user-day rollup
user_actions <- df %>%
  group_by(comment_id) %>%
  summarise(author_id = first(author_id),
            ds = min(ds),
            up = sum(up),
            dn = sum(dn),
            exp = n()) %>%
  group_by(author_id, ds) %>%
  summarise(comments = n(),
            up = sum(up),
            dn = sum(dn),
            exp = sum(exp))


# Step 2: cartesian product of users x days
# arrange in chronological order and compute rolling sums + lags
unique_commenters <- with(user_actions, unique(author_id))
unique_days <- with(user_actions, unique(ds))

user_daily <- expand_grid(author_id = unique_commenters, ds = unique_days) %>%
  left_join(user_actions) %>%
  mutate(comments = coalesce(comments, 0L),
         up = coalesce(up, 0),
         dn = coalesce(dn, 0),
         exp = coalesce(exp, 0L)) %>%
  group_by(author_id) %>%
  arrange(ds) %>%
  mutate(c_roll = roll_sumr(comments, the_lag),
         up_roll = roll_sumr(up, the_lag),
         dn_roll = roll_sumr(dn, the_lag),
         exp_roll = roll_sumr(exp, the_lag),
         production = lag(c_roll),
         reputation = lag((up_roll - dn_roll + 1) / (c_roll + 1)))


# Helper function: create a pairwise roll-up for a dyad
compute_reciprocity_df <- function(data, the_lag = 7) {
  group_by(data, ds) %>%
  summarise(up = sum(score == 1),
            dn = sum(score == -1),
            exp = n(), .groups = "drop") %>%
  right_join(tibble(ds = unique_days), by = 'ds') %>%
  mutate(up = coalesce(up, 0L),
         dn = coalesce(dn, 0L),
         exp = coalesce(exp, 0L)) %>%
  arrange(ds) %>%
  mutate(up_roll = coalesce(roll_sumr(up, the_lag), 0L),
         dn_roll = coalesce(roll_sumr(dn, the_lag), 0L),
         exp_roll = coalesce(roll_sumr(exp, the_lag), 0L),
         reciprocity = lag((up_roll - dn_roll) / (exp_roll + 1)))
}

# Step 3: find pairs with at least 500 interactions
# this does filter some low-volume pairs out
pre_aggregated_pairs <- df %>%
  group_by(user_id, author_id) %>%
  mutate(n = n()) %>%
  filter(n >= 500)

# Step 4: compute reciprocity measure on each pair

user_author_pairs <- pre_aggregated_pairs %>%
  do({compute_reciprocity_df(., the_lag = the_lag) }) %>%
  ungroup() %>%
  filter(!is.na(reciprocity)) %>%
  select(user_id = author_id, author_id = user_id, ds, reciprocity)

# Step 5: make a joined data frame with all measures

df_with_covariates <- df_clean %>%
  inner_join(user_daily %>% dplyr::select(author_id, ds, reputation, production)) %>%
  left_join(user_author_pairs)