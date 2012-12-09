
generateTree <- function(number_of_nodes){
  # the following is the dimension of the word used in the original article
  word_dimension <- 2 * number_of_nodes
  # making the universe from which we're going to extract the L set
  universe <- 1:word_dimension
  sample <- sample(universe, size=number_of_nodes)
  w = rep(0, word_dimension)
  for (i in 1:word_dimension) {
    w[i] <- ifelse(any(sample == i), 1, -1)
  }
  
  list(word=w, cumsum=cumsum(w), split=split.word(w), phi=phi(w))
}

split.word <- function(w){
   if(length(w) == 0)
     return(list(u=c(), v=c()))
  
  u_index_set <- 1:match(0, cumsum(w))
  list(u=w[u_index_set], v=w[-u_index_set])
}

phi <- function(w){
  if(length(w) == 0)
    return(w)
  
  split <- split.word(w)
  print(split)
  
  
  if(all(cumsum(split$u) > -1)){
    return (c(split$u, phi(split$v)))
  }
  else{
    t = split$u[-c(1, length(split$u))]
    return (c(1, phi(split$v), -1, -t))
  }
}