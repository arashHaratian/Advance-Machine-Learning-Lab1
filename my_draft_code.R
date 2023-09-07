library(bnlearn)
library(gRain)
library(purrr)

data("asia")

?asia



# question1 ---------------------------------------------------------------

# diff resets
hc_network_1 <- hc(asia, score = "bde", restart = 100)
plot(hc_network_1)
hc_network_2 <- hc(asia, score = "bde", restart = 1)
plot(hc_network_2)
all.equal(hc_network_1, hc_network_2)
score(hc_network_1, asia, type = "bde")
score(hc_network_2, asia, type = "bde")


# diff start
hc_network_1 <- hc(asia, score = "bde", start = random.graph(names(asia), method = "melancon"))
plot(hc_network_1)
hc_network_2 <- hc(asia, score = "bde", start = random.graph(names(asia), method = "ic-dag"))
plot(hc_network_2)
all.equal(hc_network_1, hc_network_2)

score(hc_network_1, asia, type = "bde")
score(hc_network_2, asia, type = "bde")


# diff scores
hc_network_1 <- hc(asia, restart = 100)
plot(hc_network_1)
hc_network_2 <- hc(asia, score = "bde", restart = 100)
plot(hc_network_2)
hc_network_3 <- hc(asia, score = "aic", restart = 100)
plot(hc_network_3)
all.equal(hc_network_1, hc_network_2, hc_network_3)

score(hc_network_1, asia, type = "bde")
score(hc_network_2, asia, type = "bde")
score(hc_network_3, asia, type = "bde")


# checking different numbers of arrow to be changed in each iterations
hc_network_1 <- hc(asia, score = "bde", restart = 100, perturb = 3)
plot(hc_network_1)
hc_network_2 <- hc(asia, score = "bde", restart = 100, perturb = 7)
plot(hc_network_2)
all.equal(hc_network_1, hc_network_2)

score(hc_network_1, asia, type = "bde")
score(hc_network_2, asia, type = "bde")


# different imaginary sample size for BDeu algorithm
hc_network_1 <- hc(asia, score = "bde", iss = 2, restart = 100)
plot(hc_network_1)
hc_network_2 <- hc(asia, score = "bde", iss = 2, restart = 100)
plot(hc_network_2)
all.equal(hc_network_1, hc_network_2)

score(hc_network_1, asia, type = "bde")
score(hc_network_2, asia, type = "bde")



graphviz.compare(hc_network_1, hc_network_2)



# The networks will differ if we change the regularization or ....




# question2 ---------------------------------------------------------------

set.seed(7)
training_idx <- sample(5000, nrow(asia) * 0.8)
asia_training <- asia[training_idx, ]
asia_test <- asia[-training_idx, ]


bn_dag <- hc(asia_training, score = "bde", iss = 2, restart = 100)
plot(bn_dag)

param_tables <- bn.fit(bn_dag, asia_training, method = "mle")



### approx inference

target_node_index <- 2
evidence_vector <-
  map_chr(
    list_transpose(as.list(asia_test)),
    ~ paste(
      "(",
      names(asia_test)[-target_node_index],
      " == '",
      map_chr(.x[-target_node_index], as.character),
      "')",
      sep = "",
      collapse = " & "
    )
  )
evnet_vector <-
  map_chr(
    list_transpose(as.list(asia_test)),
    ~ paste(
      "(",
      names(asia_test)[target_node_index],
      " == '",
      map_chr(.x[target_node_index], as.character),
      "')",
      sep = "",
      collapse = " & "
    )
  )

cmds <- paste("cpquery(param_tables, ", evnet_vector, ", ",  evidence_vector, ")", sep = "")
probs <- map_dbl(cmds, ~eval(parse(text = .x)) )

y_hats <- ifelse(probs > 0.5, "no", "yes")
table(y_hats, asia_test_s0$S)





### exact inference


asia_test_s0 <- asia_test_s0[, -2]

bn_grain <- param_tables %>% as.grain() %>% compile()

bn_grain_evidences <- map_dbl(1:nrow(asia_test_s0),
                          ~ setEvidence(bn_grain,
                                        nodes = names(asia_test_s0),
                                        states = as.matrix(asia_test_s0)[.x,]) %>%
                            querygrain(nodes = "S") %>% pluck(1, "no")) 


bn_grain_evidences




dag <- model2network("[A][S][T|A][L|S][B|S][D|B:E][E|T:L][X|E]")
plot(dag)
param_tables <- bn.fit(dag, asia_training, method = "mle")



# question3 ---------------------------------------------------------------

mb(bn_dag, "S")
