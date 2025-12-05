library(dplyr)
library(tidyverse)
library(stringr)
library(lubridate)
library(tidyr)
library(igraph)
library(RSiena)
library(tibble)
library(ggplot2)
library(ggraph)

setwd("/Users/huijia/Desktop/Dissertation/code")

tweets <- readRDS("/Users/huijia/Desktop/Dissertation/code/twts_corpus_sample.rds")

view(head(tweets))
unique(tweets$party_value)
sum(tweets$reftweet_username %in% tweets$username)

# Helper
clean_handle <- function(x) {
  x %>%
    str_trim() %>%
    str_remove_all("^@") %>%
    str_to_lower() %>%
    str_remove_all("[^a-z0-9_]")
}

# Define MP list ----
mp_list <- tweets %>%
  filter(!is.na(party_value), party_value != "") %>%
  transmute(handle = clean_handle(username)) %>%
  distinct() %>%
  pull(handle)
length(mp_list) # there are 551 MPs in this dataset

# Look at tweets that include specific words
write_filtered_tweets <- function(data, pattern, file, ignore_case = TRUE) {
  keep <- grepl(pattern, data$tweet, ignore.case = ignore_case) &
    data$username %in% mp_list &
    data$reftweet_username %in% mp_list
  
  sub <- data[keep, ]
  
  lines <- paste(as.character(sub$date), "-", sub$tweet)
  writeLines(lines, file)
  invisible(sub)  
}
write_filtered_tweets(tweets, "Boris Johnson", "boris_johnson.txt")
write_filtered_tweets(tweets, "Theresa May", "may.txt")
write_filtered_tweets(tweets, "shit", "shit.txt")

#-------------------------------------------------------------------------------------------------------------
# Deal with edges---------------------------------------------------------------------------------------------

## --- Retweet edges (MP -> MP only, exclude self-edges) ----
retweet_edges <- tweets %>%
  transmute(
    date   = as.Date(date),
    source = clean_handle(username),
    target = clean_handle(reftweet_username),
    type   = "retweet"
  ) %>%
  filter(
    !is.na(source), source != "",
    !is.na(target), target != "",
    source != target,
    source %in% mp_list,
    target %in% mp_list
  ) %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  distinct(date, year_month, source, target, type)

head(retweet_edges)
nrow(retweet_edges)

## --- Mention edges (MP -> MP only, exclude self-edges) ----
mention_edges <- tweets %>%
  filter(!is.na(ments), nchar(ments) > 0) %>%
  transmute(
    date    = as.Date(date),
    source  = clean_handle(username),
    mentions = ments
  ) %>%
  separate_rows(mentions, sep = ",\\s*") %>%
  mutate(
    target = clean_handle(str_remove_all(mentions, "@")),
    type   = "mention"
  ) %>%
  filter(
    !is.na(source), source != "",
    !is.na(target), target != "",
    source != target,
    source %in% mp_list,
    target %in% mp_list
  ) %>%
  mutate(year_month = format(date, "%Y-%m")) %>%
  distinct(date, year_month, source, target, type)

head(mention_edges)
nrow(mention_edges)

#-------------------------------------------------------------------------------------------------------------
# Deal with Parties-------------------------------------------------------------------------------------------

### --- Check for Party switchers
party_switchers <- tweets %>%
  mutate(username = clean_handle(username)) %>%
  distinct(username, party_value) %>%
  add_count(username, name = "n_parties") %>%
  filter(n_parties > 1)

party_switchers # shows none party switchers

### --- List of ALL MPs with their party affiliation
mp_party_lookup <- tweets %>%
  mutate(username = clean_handle(username)) %>%
  filter(!is.na(party_value), party_value != "") %>%
  distinct(username, party = party_value)

head(mp_party_lookup)
nrow(mp_party_lookup) # correspond to length(mp_list)

### --- All MPs that appear in the network (retweets OR mentions)
mp_names_in_network <- bind_rows(
  retweet_edges %>% select(source, target),
  mention_edges %>% select(source, target)
) %>%
  pivot_longer(c(source, target), values_to = "username") %>%
  distinct(username)

mp_in_network <- mp_names_in_network %>% left_join(mp_party_lookup, by = "username")

head(mp_in_network)
nrow(mp_in_network) # 2 MPs not in both network -- 549 active MPs/2 inactive




### --- Party counts of all MPs
party_counts <- mp_party_lookup %>% count(party, sort = TRUE, name = "n_mps")

party_counts
sum(party_counts$n_mps) 

############ Covariate List -- Labour(Co-op) merge with Labour
covariates <- mp_party_lookup %>%
  mutate(party = if_else(party == "Labour (Co-op)", "Labour", party))

head(covariates)
count(covariates, party, sort = TRUE)
nrow(covariates)

### Bar plot of party affiliatinos
party_counts_1 <- count(covariates, party, sort = TRUE)
party_counts_1
sum(party_counts_1$n)

ggplot(party_counts_1, aes(x = reorder(party, n), y = n, fill = party)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  geom_text(aes(label = n), 
            hjust = -0.2,            
            size = 4) +              
  coord_flip() +
  labs(
    title = "Figure1 - Number of MPs by Party",
    x = "Party",
    y = "Number of MPs"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.y = element_blank()
  ) +
  scale_fill_manual(values = c(
    "Conservative" = "#0087DC",
    "Labour" = "#E4003B",
    "Scottish National Party" = "#FFF95D",
    "Liberal Democrat" = "#FAA61A",
    "Democratic Unionist Party" = "#D46A4C",
    "Plaid Cymru" = "#008142",
    "Green Party" = "#6AB023"
  )) +
  expand_limits(y = max(party_counts_1$n) * 1.1)  

#-------------------------------------------------------------------------------------------------------------
# Time periods(waves) Segmentation----------------------------------------------------------------------------

####### Define periods ########
periods_df <- tibble::tribble(
  ~wave,     ~start,                ~end,
  "wave1",   as.Date("2017-11-20"), as.Date("2017-12-31"),
  "wave2",   as.Date("2018-06-25"), as.Date("2018-08-05"),
  "wave3",   as.Date("2019-01-07"), as.Date("2019-02-17"),
  "wave4",   as.Date("2019-07-08"), as.Date("2019-08-18")
)

#-------------------------------------------------------------------------------------------------------------
# Descriptives------------------------------------------------------------------------------------------------

####### --- 1) Add wave label to each edge
tag_waves <- function(df, periods_df) {
  pd <- periods_df                    
  out <- df
  out$wave <- NA_character_
  for (i in seq_len(nrow(pd))) {
    mask <- out$date >= pd$start[i] & out$date <= pd$end[i]
    out$wave[mask] <- pd$wave[i]
  }
  filter(out, !is.na(wave))
}
retweet_edges_inwave <- tag_waves(retweet_edges, periods_df)
mention_edges_inwave <- tag_waves(mention_edges, periods_df)

head(retweet_edges_inwave)
head(mention_edges_inwave)
count(retweet_edges_inwave, wave)
count(mention_edges_inwave, wave)

####### --- 2) Attach party labels; flag within vs cross-party ties
attach_parties <- function(df, mp) {
  out <- df %>%
    left_join(mp,  by = c("source" = "username")) %>% rename(source_party = party) %>%
    left_join(mp,  by = c("target" = "username")) %>% rename(target_party = party) %>%
    mutate(
      same_party     = as.integer(!is.na(source_party) & source_party == target_party),
      tie_party_type = if_else(same_party == 1L, "within", "cross")
    )
}
retweet_party <- attach_parties(retweet_edges_inwave, covariates)
mention_party <- attach_parties(mention_edges_inwave, covariates)

head(retweet_party)
head(mention_party)
count(retweet_party, tie_party_type)
count(mention_party, tie_party_type)

####### --- 3) Build wave-specific igraphs (constant actor set)
all_actors <- unique(covariates$username)

make_graphs <- function(df_party) {
  df_party %>%
    split(.$wave) %>%
    imap(function(dat, w) {
      g <- graph_from_data_frame(
        dat %>% distinct(source, target) %>%
          select(source, target),
        directed = TRUE,
        vertices = tibble::tibble(name = all_actors)
      )
      list(wave = w, g = g)
    })
}

retweet_graphs <- make_graphs(retweet_party)
mention_graphs <- make_graphs(mention_party)


# ========= A) Core metrics (per wave × layer) --------------------
graph_metrics <- function(gl) {
  g <- gl$g
  g_act  <- induced_subgraph(g, vids = V(g)[degree(g, mode = "all") > 0])
  comp   <- components(as_undirected(g, mode = "collapse"))
  data.frame(
    wave            = gl$wave,
    nodes_active    = gorder(g_act),
    edges           = gsize(g),
    density_full    = edge_density(g,     loops = FALSE),
    density_active  = edge_density(g_act, loops = FALSE),
    reciprocity     = tryCatch(reciprocity(g_act), error = function(e) NA_real_),
    transitivity    = transitivity(g_act, type = "global", isolates = "zero"),
    lcc_share       = max(comp$csize) / gorder(g),
    n_components    = comp$no
  )
}

retweet_metrics <- bind_rows(lapply(retweet_graphs,  graph_metrics)) %>% mutate(layer = "retweet")
mention_metrics <- bind_rows(lapply(mention_graphs, graph_metrics)) %>% mutate(layer = "mention")

network_metrics <- bind_rows(retweet_metrics, mention_metrics) %>%
  relocate(layer, .before = wave) %>% arrange(layer, wave)


# ========= B) Within vs cross-party mix + E–I index ----------------------
within_cross_summary <- function(df_party, layer_name) {
  df_party %>%
    count(wave, tie_party_type, name = "n_edges") %>%
    group_by(wave) %>%
    mutate(prop = n_edges / sum(n_edges)) %>%
    ungroup() %>%
    mutate(layer = layer_name) %>%
    relocate(layer)
}
retweet_mix <- within_cross_summary(retweet_party,  "retweet")
mention_mix <- within_cross_summary(mention_party,  "mention")

party_mix <- bind_rows(retweet_mix, mention_mix) %>% arrange(layer, wave, tie_party_type)

party_mix_ei <- party_mix %>%
  select(layer, wave, tie_party_type, n_edges) %>%
  tidyr::pivot_wider(names_from = tie_party_type, values_from = n_edges, values_fill = 0) %>%
  mutate(EI_index = (cross - within) / pmax(cross + within, 1L))  


# ========= C) Turnover between waves (Jaccard) ---------------------------
# helper to edge set per wave
edge_sets <- function(df_party) {
  df_party %>%
    distinct(wave, source, target) %>%
    mutate(edge = paste(source, target, sep = "|")) %>%
    group_by(wave) %>%
    summarise(edges = list(edge), .groups = "drop")
}

jaccard_by_layer <- function(df_party, layer_name) {
  es <- edge_sets(df_party)
  waves <- es$wave
  if (length(waves) < 2) return(tibble(layer = layer_name, wave_from = character(), wave_to = character(), jaccard = numeric()))
  purrr::map2_dfr(seq_len(length(waves)-1), seq_len(length(waves)-1)+1, function(i, j) {
    e1 <- es$edges[[i]]; e2 <- es$edges[[j]]
    jac <- length(intersect(e1, e2)) / max(length(union(e1, e2)), 1L)
    tibble(layer = layer_name, wave_from = waves[i], wave_to = waves[j], jaccard = jac)
  })
}

turnover <- bind_rows(
  jaccard_by_layer(retweet_party, "retweet"),
  jaccard_by_layer(mention_party, "mention")
)
turnover

#-------------------------------------------------------------------------------------------------------------
# Prepare for Gephi visualization-----------------------------------------------------------------------------

waves <- sort(unique(retweet_edges_inwave$wave))

for (w in waves) {
  ## ---------- RETWEET, wave w ----------
  e_rt <- retweet_edges_inwave %>%
    filter(wave == w) %>%
    distinct(source, target)          
  
  v_rt <- tibble::tibble(
    name = union(e_rt$source, e_rt$target)
  ) %>%
    left_join(covariates, by = c("name" = "username"))  
  
  g_rt <- graph_from_data_frame(
    d        = e_rt,
    directed = TRUE,
    vertices = v_rt
  )
  
  write_graph(
    g_rt,
    file   = paste0("retweet_wave", w, ".graphml"),
    format = "graphml"
  )
  
  ## ---------- MENTION, wave w ----------
  e_m <- mention_edges_inwave %>%
    filter(wave == w) %>%
    distinct(source, target)
  
  v_m <- tibble::tibble(
    name = union(e_m$source, e_m$target)
  ) %>%
    left_join(covariates, by = c("name" = "username"))
  
  g_m <- graph_from_data_frame(
    d        = e_m,
    directed = TRUE,
    vertices = v_m
  )
  
  write_graph(
    g_m,
    file   = paste0("mention_wave", w, ".graphml"),
    format = "graphml"
  )
}

### ========= Top3 indegree of each wave/layer
top3_in_each_men_wave <- mention_edges_inwave %>%
  group_by(wave, target) %>%
  summarise(in_deg = n(), .groups = "drop") %>%
  group_by(wave) %>%
  slice_max(in_deg, n = 3, with_ties = FALSE) %>%
  arrange(wave, desc(in_deg))

top3_in_each_men_wave

top3_in_each_rt_wave <- retweet_edges_inwave %>%
  group_by(wave, target) %>%
  summarise(in_deg = n(), .groups = "drop") %>%
  group_by(wave) %>%
  slice_max(in_deg, n = 3, with_ties = FALSE) %>%
  arrange(wave, desc(in_deg))

top3_in_each_rt_wave

#-------------------------------------------------------------------------------------------------------------
# SOAM data preparation---------------------------------------------------------------------------------------

###--- Helper function to convert edgelists to siena net
# Convert a list of wave edgelists to an n x n x T binary array for RSiena
convert_edgelists_to_siena <- function(edgelist_list, nodes, directed = TRUE) {
  stopifnot(is.list(edgelist_list), length(edgelist_list) >= 2)
  stopifnot(is.vector(nodes), length(nodes) >= 2)
  
  all_nodes <- as.character(nodes)              
  n  <- length(all_nodes)
  TT <- length(edgelist_list)
  wnames <- names(edgelist_list)
  if (is.null(wnames)) wnames <- paste0("wave", seq_len(TT))
  
  # Precompute name -> index map for speed
  idx <- setNames(seq_along(all_nodes), all_nodes)
  
  adj_mats <- lapply(edgelist_list, function(df) {
    M <- matrix(0, n, n, dimnames = list(all_nodes, all_nodes))
    if (nrow(df) > 0) {
      # keep only edges whose endpoints are in the fixed actor set
      df2 <- df[, c("source", "target")]
      ok  <- df2$source %in% all_nodes & df2$target %in% all_nodes & df2$source != df2$target
      if (any(ok)) {
        from <- idx[df2$source[ok]]
        to   <- idx[df2$target[ok]]
        M[cbind(from, to)] <- 1
        if (!directed) M[cbind(to, from)] <- 1
      }
    }
    diag(M) <- 0
    M
  })
  
  arr <- array(unlist(adj_mats, use.names = FALSE),
               dim = c(n, n, TT),
               dimnames = list(all_nodes, all_nodes, wnames))
  storage.mode(arr) <- "double"
  arr
}


######## --- Base Preparation --- ########

# Constant actor set
all_nodes <- covariates$username
save(covariates, file = "Covariates_list")

# ---- Mentions layer
mention_edges_list <- mention_edges_inwave %>%
  filter(source %in% all_nodes, target %in% all_nodes) %>%
  split(.$wave) %>%
  lapply(\(d) d %>% select(source, target) %>% distinct())

mention_net <- convert_edgelists_to_siena(
  mention_edges_list,
  nodes = all_nodes,
  directed = TRUE
)
save(mention_net, file = "mention_net")


# ---- Retweets layer
retweet_edges_list <- retweet_edges_inwave %>%
  filter(source %in% all_nodes, target %in% all_nodes) %>%
  split(.$wave) %>%
  lapply(\(d) d %>% select(source, target) %>% distinct())

retweet_net <- convert_edgelists_to_siena(
  retweet_edges_list,
  nodes = all_nodes,
  directed = TRUE
)
save(retweet_net, file = "retweet_net")












