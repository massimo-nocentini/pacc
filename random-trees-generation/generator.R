main <- function(){
  
  number_of_nodes <- seq(from=3, to=10, by=1) 
  number_of_trees <- seq(from=100, to=100100, by=5000)
  
  matrix <- matrix(nrow = length(number_of_trees), 
                   ncol = length(number_of_nodes), 
                   #byrow=TRUE,
                   dimnames = list(number_of_trees,
                                   number_of_nodes))
  
  for(tree_index in 1:length(number_of_trees)){
    for(node_index in 1:length(number_of_nodes)){
      
      if(node_index > tree_index)
        next
      
      # to fix: now simulation returns a list
      data <- simulation(number_of_trees[tree_index],
                         number_of_nodes[node_index])
      chi_square_test <- chisq.test(data$hits)
      matrix[tree_index, node_index] <- chi_square_test$p.value
    }
  }
  matrix
}

simulation <- function(number_of_trees, nodes_in_each_tree){
  datas <- data.frame()
  
  keys <- c()
  hits <- c()
  words <- c()
  for(i in 1:number_of_trees){
    generated_tree <- generate.tree(nodes_in_each_tree)
    bracket_sequence <- generated_tree$as_brackets
    if (any(keys == bracket_sequence)){
      index = match(bracket_sequence, keys)
      hits[index] <- hits[index] + 1
    }
    else{
      keys <- c(keys, bracket_sequence)
      hits <- c(hits, 1)
      words <- c(words, paste(generated_tree$phi, collapse=""))
    }
  }
  datas <- data.frame(keys, words, hits)
  datas <- datas[order(datas$keys),]
  filename <- paste(format(Sys.time(), 
                           "%a%b%d-%H-%M-%S-%Y"), 
                    ".csv",
                    sep="")  
  
  command <- paste ("ocamlrun treesUtility.ocaml.bytecode", 
                     filename)
  system(command)
  
  write.table(datas, file=filename, sep=",")
  return(list(datas=datas, filename=filename))
}

generate.tree <- function(number_of_nodes){
  #number_of_nodes <- number_of_nodes - 1
  
  # the following is the dimension of the word used in the original article
  word_dimension <- 2 * number_of_nodes
  # making the universe from which we're going to extract the L set
  universe <- 1:word_dimension
  sample <- sample(universe, size=number_of_nodes)
  w = rep(0, word_dimension)
  for (i in 1:word_dimension) {
    w[i] <- ifelse(any(sample == i), 1, -1)
  }
  
  #phi=c(1,phi(w),-1) # this is for adjustment
  phi=phi(w)
  list(word=w, phi=phi, as_brackets = brackets_of_word(phi))
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
  
  if(all(cumsum(split$u) > -1)){
    return (c(split$u, phi(split$v)))
  }
  else{
    t = split$u[-c(1, length(split$u))]
    return (c(1, phi(split$v), -1, -t))
  }
}

brackets_of_word <- function(word){
  brackets <- ''
  for (i in 1:length(word)){
    
    brackets <- paste(brackets, 
                      ifelse(word[i] == 1,
                             '(', ')'), collapse='')
  }
  brackets
}