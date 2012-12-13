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

repeated_simulation <- function(number_of_trees, 
                                nodes_in_each_tree, 
                                repeated_sampling_dimension){
  
#   curve(dnorm(x, mean = 500, sd = 1.118), 495, 505, xlab = expression(bar(Y)), 
#         y = "")
  leaves_mean = rep(repeated_sampling_dimension)
  height_mean = rep(repeated_sampling_dimension)
  leaves_var = rep(repeated_sampling_dimension)
  height_var = rep(repeated_sampling_dimension)
  theoretical_mean_of_leaves=rep(repeated_sampling_dimension)
  theoretical_mean_of_height=rep(repeated_sampling_dimension)
  theoretical_var_of_leaves=rep(repeated_sampling_dimension)
  theoretical_var_of_height=rep(repeated_sampling_dimension)
  chi.sq.statistics = rep(repeated_sampling_dimension)
  
  for (i in 1:repeated_sampling_dimension) {
    system("echo")
    system(paste("echo Starting ", i, "-th generation", sep=""))
    sim <- simulation(number_of_trees, nodes_in_each_tree)
    leaves_mean[i] = sim$sampling_mean_of_leaves
    height_mean[i] = sim$sampling_mean_of_height
    leaves_var[i] = sim$sampling_var_of_leaves
    height_var[i] = sim$sampling_var_of_height
    theoretical_mean_of_leaves[i] = sim$theoretical_mean_of_leaves
    theoretical_mean_of_height[i] = sim$theoretical_mean_of_height
    theoretical_var_of_leaves[i] = sim$theoretical_var_of_leaves
    theoretical_var_of_height[i] = sim$theoretical_var_of_height
    chi.sq.statistics[i] = sim$chi.square.obs.statistic
  }
  
  #print(theoretical_mean_of_leaves)
  postscript("repeated-sampling-leaves-mean.ps", horizontal = FALSE)

  plot(density(leaves_mean), 
       ylab="leaves mean density distribution", 
       col="blue")  

#   lines(dt(x,df=sim$freedom.degree),
#         theoretical_mean_of_leaves[1]-5,
#         theoretical_mean_of_leaves[1]+5,
#         y = "")
  dev.off()     
  
  postscript("repeated-sampling-height-mean.ps", horizontal = FALSE)
  plot(density(height_mean), 
       ylab="height mean density distribution", 
       col="red")  
  dev.off()     
  
  postscript("repeated-sampling-chi-squared-quar-mean.ps", horizontal = FALSE)
  plot(density(chi.sq.statistics), 
       ylab="chi-squared density distribution", 
       col="green")  
  dev.off()     
  
  postscript("repeated-sampling-leaves-var.ps", horizontal = FALSE)
  plot(density(leaves_var), 
       ylab="leaves var density distribution", 
       col="grey")  
  dev.off()    
  
  postscript("repeated-sampling-height-var.ps", horizontal = FALSE)
  plot(density(height_var), 
       ylab="height var density distribution", 
       col="brown")  
  dev.off()    
}

timed_simulation <- function(number_of_trees, nodes_in_each_tree){
  system.time(simulation(number_of_trees, nodes_in_each_tree))
}
  
simulation <- function(number_of_trees, nodes_in_each_tree, render_svg=FALSE){
  datas <- data.frame()
  
  keys <- c()
  hits <- c()
  words <- c()
  system(paste ("echo Generating ",
                number_of_trees,
                " trees at random, each with ",
                nodes_in_each_tree,
                " nodes..."))
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
  system("echo done")
  datas <- data.frame(keys, words, hits)
  datas <- datas[order(datas$keys),]
  
  filename_without_extension <- format(Sys.time(), 
                                       "%a%b%d-%H-%M-%S-%Y")
  
  filename <- paste(filename_without_extension, 
                    ".csv",
                    sep="")  
  write.table(datas, file=filename, sep=",")
  
  command <- paste ("./treesUtility.ocaml.bytecode", 
                     filename, nodes_in_each_tree)
  system(command)
  
  if(render_svg){
    system("echo Rendering trees...")
    system(paste ("dot -Tsvg ",
                  filename_without_extension, 
                  ".dot > ",
                  filename_without_extension,
                  ".svg",
                  sep=""))
    
    system("echo done")
  }
  
  datas <- read.csv(file=paste(filename_without_extension, 
                               "-augmented.csv",
                               sep="")
                    ,head=TRUE,
                    sep=",")
  
  datas$sampling_leaves <- datas$leaves * datas$hits  
  datas$sampling_height <- datas$height * datas$hits
  
  system("echo Cleaning mess files...")
  system("rm *.dot")
  system("rm *.csv")
  system("echo done")
  
  return(make_interesting_report(datas, 
                                 number_of_trees, 
                                 nodes_in_each_tree))
}

make_interesting_report <- function(datas,
                                    number_of_trees, 
                                    nodes_in_each_tree){
  chi_square_test <- chisq.test(datas$hits)
  
  theoretical_mean_of_leaves <- mean(datas$leaves)
  theoretical_mean_of_height <- mean(datas$height)
  theoretical_var_of_leaves <- var(datas$leaves)
  theoretical_var_of_height <- var(datas$height)  
  
  
  sampling_mean_of_leaves <- sum(datas$sampling_leaves)/sum(datas$hits)
  sampling_mean_of_height <- sum(datas$sampling_height)/sum(datas$hits)
  sampling_var_of_leaves <- sum((datas$leaves - 
                                   sampling_mean_of_leaves)^2)/(length(datas$hits)-1)
  sampling_var_of_height <- sum((datas$height - 
                                   sampling_mean_of_height)^2)/(length(datas$hits)-1)
  
  list(chi.square.obs.statistic = sqrt(chi_square_test$statistic),
       p.value = chi_square_test$p.value,
       freedom.degree = chi_square_test$parameter,
       theoretical_mean_of_leaves=theoretical_mean_of_leaves,
       theoretical_mean_of_height=theoretical_mean_of_height,
       theoretical_var_of_leaves=theoretical_var_of_leaves,
       theoretical_var_of_height=theoretical_var_of_height,
       sampling_mean_of_leaves=sampling_mean_of_leaves,
       sampling_mean_of_height=sampling_mean_of_height,
       sampling_var_of_leaves=sampling_var_of_leaves,
       sampling_var_of_height=sampling_var_of_height)
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