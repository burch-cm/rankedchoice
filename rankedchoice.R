library(dplyr)
library(purrr)

drop_candidate <- function(x, candidate) {
  x[!(x %in% candidate)]
}

count_votes <- function(vote_list, dropped_candidates = NA) {
  vote_list %>%
    modify(drop_candidate, dropped_candidates) %>%
    map_chr(pluck(1)) %>%
    table() %>%
    as_tibble() %>%
    rename(candidate = ".", votes = n)
}

find_winner <- function(vote_list, dropped_candidates = NA) {
  stopifnot(is.list(vote_list))
  majority_count <- length(vote_list)/2
  if (is.na("dropped_candidates")) dropped_candidates <- vector(mode = "character")
  # count the votes
  vote_count <- vote_list %>% 
    count_votes(dropped_candidates)
  # if someone has the majority, stop and return totals
  if (vote_count[which.max(vote_count$votes), ]$votes >= majority_count) {
    max_votes <- vote_count[which.max(vote_count$votes), ]$votes
    return(arrange(vote_count, desc(votes)))
  } else {
    # otherwise, drop lowest person and recount
    fewest_votes <- vote_count[which.min(vote_count$votes), ]$candidate
    dropped_candidates <- append(dropped_candidates, 
                                 fewest_votes)
    message("Dropping ", fewest_votes, " and recalculating.")
    find_winner(vote_list, dropped_candidates)
  }
}

#####
candidates <- c("Alice", "Bob", "Charlie", "David", "Ellen", "Frank", "George")
polling    <- c(0.25, 0.20, 0.15, 0.08, 0.15, 0.12, 0.05)

vote_list1 <- replicate(100, sample(candidates, 
                                    length(candidates), 
                                    replace = FALSE,
                                    prob = polling),
                        simplify = FALSE)

vote_list2 <- list(
  a1 = c("Alice", "Bob"),
  a2 = c("Alice", "Bob"),
  a3 = c("Bob", "Alice"),
  a4 = c("Bob", "Alice")
)

vote_list3 <- list(
  c("Alice", "Outsider", "Bob", "Dave"),
  c("Alice", "Outsider", "Bob", "Dave"),
  c("Alice", "Outsider", "Bob", "Dave"),
  c("Bob", "Outsider", "Alice", "Dave"),
  c("Bob", "Outsider", "Alice", "Dave"),
  c("Bob", "Outsider", "Alice", "Dave"),
  c("Outsider", "Alice", "Bob", "Dave"),
  c("Outsider", "Bob", "Alice", "Dave"),
  c("Dave", "Outsider", "Alice", "Bob")
)

alice_heavy   <- c(.80, .05, .15)
bob_heavy     <- c(.05, .80, .15)
protest_heavy <- c(.15, .05, .80)
  
vote_list4 <- c(
  replicate(100, sample(c("Alice", "Bob", "Protest"), 
                        size = 3,
                        replace = FALSE,
                        prob = alice_heavy), simplify = FALSE),
  replicate(100, sample(c("Alice", "Bob", "Protest"), 
                        size = 3,
                        replace = FALSE,
                        prob = bob_heavy), simplify = FALSE),
  replicate(50, sample(c("Alice", "Bob", "Protest"), 
                        size = 3,
                        replace = FALSE,
                        prob = protest_heavy), simplify = FALSE)
)

#####

find_winner(vote_list1)
find_winner(vote_list2)
find_winner(vote_list3)
find_winner(vote_list4)
