# probabilistic linkage
library(reclin)
library(dplyr)


# this is an extended version of this https://github.com/djvanderlaan/reclin

#data sets with artificial data
data("linkexample1", "linkexample2")

print(linkexample1)
print(linkexample2)



#----step 1 - generate pairs----

# note that there are
nrow(linkexample1)*nrow(linkexample2) 
# of possible pairs

# assume we are sure that the postal code is already cleaned and standardized
# pair_blocking will generate a data frame with indices of elements from compared data frame that 
# match EXCATLY on a given variables
p <- pair_blocking(linkexample1, linkexample2, blocking_var = "postcode", large = FALSE)
  print(p) #

  nrow(p) # space of possible pairs with blocking is smaller than without blocking
  
  
  
#----step 2 - compare pairs----
  
#We can now compare the records on their linkage keys
  p <- compare_pairs(p, by = c("lastname", "firstname", "address", "sex"))
  print(p)

#e use the jaro_winkler similarity score to compare all fields
  p <- compare_pairs(p, by = c("lastname", "firstname", "address", "sex"),
                     default_comparator = jaro_winkler(0.9), overwrite = TRUE)
  # as default_comparator you can also use identical() , lcs() oraz jaccard()
  print(p)
  
#----step 3 - score pairs----
  #The next step in the process, is to determine which pairs of records belong to the same entity and which do not. 
  #calculate the sum of the comparison vectors
  p <- score_simsum(p, var = "simsum")
  print(p)
  # The disadvantage of score_simsum is that it doesn't take into account that the amount of information in agreement
  #or disagreement on a variable depends on the variable. For example, agreement on sex doesn't tell us much: when our
  #data sets contain 50% men an 50% women, there is a 50% chance that two random records agree on sex. On the other
  #hand the probability that two random records agree on last name is much lower. 
  #Therefore, agreement on last name makes it much more likely that the two records belong to the same entity.
  
  #This is what the probabilistic linkage framework initially formalised by Fellegi and Sunter tries to do.
  #The function problink_em uses an EM-algorithm to estimate the so called m- and u-probabilities for each
  #of the linkage variables. 
  m <- problink_em(p)
  print(m)
  
  # These m- and u-probabilities can be used to score the pairs:
  p <- score_problink(p, model = m, var = "weight")
  print(p)
  #The higher the weight the more likely the two pairs belong to the same entity/are a match..
  
  
# ----step 4 - select pairs----
  #The final step is to select the pairs that are considered to belong to the same entities. 
  #The simplest method is to select all pairs above a certain threshold
  
  p <- select_threshold(p, "weight", var = "threshold", threshold = 8)
  print(p)

  
  # The select functions add a (logical) variable to the data set indicating whether a pairs is selected or not.
  #The add_from_x function adds variables from the original x so from the first one. As was mentioned before the two data sets are stored in p.
  p <- add_from_x(p, id_x = "id")
  print(p)
  
  #The add_from_y function adds variables from the original x so from the second one
  p <- add_from_y(p, id_y = "id")
  p$true <- p$id_x == p$id_y
  table(as.data.frame(p[c("true", "threshold")])) 
  
  
  #We see that three of the four matches that should have been found 
  #have indeed been found (the recall is 3/4) and we have one false link (sensitivity is 1/4).
  
  
  
  #other metods
  #Using a threshold, does not take into account the fact that often we know that one record from the first data set
  #can be linked to at most one record from the second data set and vice versa. If we make the threshold low enough
  #we have more links than records in either data set. reclin contains two functions that force one-to-one linkage.
 
  #one-to-one linkage
  #fast
  #it selects pairs starting from the highest score;
  #pairs are only selected when each of the records in a pair have not been selected previously
  p <- select_greedy(p, "weight", var = "greedy", threshold = 0)
  table(as.data.frame(p[c("true", "greedy")]))
  
  #slower,but can lead to better results
  #it tries to optimise to total
  #score of the selected records under the restriction that each record can be selected only once
  p <- select_n_to_m(p, "weight", var = "ntom", threshold = 0)
  table(as.data.frame(p[c("true", "ntom")]))
  
  
  #The final, last step
  #The real final step is to create the linked data set. We now know which pairs are to be linked,
  #but we still have to actually link them.
  #link does that (the optional arguments all_x and all_y control the type of linkage)
  linked_data_set <- link(p)
  print(linked_data_set)
  
  

  #The functions have been designed to be usable with pipe operators,
  #so the entire linkage process could be written as
  linked_data_set <- pair_blocking(linkexample1, linkexample2, "postcode") %>%
    compare_pairs(by = c("lastname", "firstname", "address", "sex"),
                  default_comparator = jaro_winkler(0.9)) %>%
    score_problink(var = "weight") %>%
    select_n_to_m("weight", var = "ntom", threshold = 0) %>%
    link()

  
  
#----Task----
# load following data sets
  library(readxl)
  setwd('//vmfrze01/DSM/Szkolenia prowadzone przez DSM/Madryt 6-7 czerwca 2023 INE/Text string - script and data')
  Survey_rzeszow <- read_excel("Survey_rzeszow.xlsx", 
                               col_types = c("text", "text", "text", 
                                             "text", "text", "text", "text", "text", 
                                             "text", "text", "text", "numeric","numeric"))
  Webscraping_rzeszow <- read_excel("Webscraping_rzeszow.xlsx", 
                                    col_types = c("numeric", "numeric", "text", 
                                                  "text", "text", "text", "text", "text", 
                                                  "text", "numeric", "numeric"))

# find pairs in these data sets using name, address and postal code
# consider two approaches: postal code as a blocking variable and not
# tip: clean address and postal Code replacing 'undefined' and word "NA" with NA
  
  
  
  
  
  
  # DONT LOOK BELOW! ANSWERS PROVIDED!
  
  
  
  # I REALLY MEAN DONT LOOK BELOW!!!
  
  
  
  # YOU HAVE BEEN WARNED. ANSWER BELOW.
 
  
  
  linked_data_set <- pair_blocking(Survey_rzeszow, Webscraping_rzeszow) %>%
    compare_pairs(by = c("name", "address","postalCode"),
                  default_comparator = jaro_winkler(0.9)) %>%
    score_problink(var = "weight") %>%
    select_n_to_m("weight", var = "ntom", threshold = 0) %>%
    link()
  
  # Does not work? the problem is with score_problink function
  # within it is tabulating all matching patterns 
  pairs <- pair_blocking(linkexample1, linkexample2, "postcode") %>%
    compare_pairs(by = c("lastname", "firstname", "address", "sex"),
                  default_comparator = jaro_winkler(0.9))
  tabulate_patterns(pairs) # these patterns are used in model of m and u
  
  # in our case there are very few non-zero size patterns
  pairs2 <- pair_blocking(Survey_rzeszow, Webscraping_rzeszow) %>%
    compare_pairs(by = c("name", "address","postalCode"),
                  default_comparator = jaro_winkler(0.9))
  tabulate_patterns(pairs2) 
  # it is because we set one threshold 0.9 for all variables. It is good for postal code, but not for other variables
  # hence we can set three comparators with different thresholds 
  
  pairs2 <- pair_blocking(Survey_rzeszow, Webscraping_rzeszow)
  pairs2 <- compare_pairs(pairs2,
                by = c("name", "address","postalCode"),
                comparators = list(jaro_winkler(0.4),jaro_winkler(0.4),jaro_winkler(0.9)))
  pairs2 # results look the same but matching rule is different
  
  tabulate_patterns(pairs2) # now it looks much better
  
  linked_data_set <- pairs2 %>%
    score_problink(var = "weight") %>%
    select_n_to_m("weight", var = "ntom", threshold = 0) %>%
    link()
  
  pairs2 %>%
    score_problink(var = "weight") %>%
    select_greedy("weight", var = "ntom", threshold = 0) %>%
    link()
  