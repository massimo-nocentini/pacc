
draw.chi.square.curve <- function(){
  postscript("chi-squared-theo-curve-4nodes-13fd.ps",
             horizontal = FALSE)  
  curve(dchisq(x, 13),
        from=-3,
        to=45,        
        col="blue",
        ylab="Chi-squared with 13 freedom degrees")
  dev.off()                             
}

main <- function(){
  
  number_of_nodes <- c(4, 5, 6, 8) 
  number_of_trees <- c(1000, 2000, 5000, 10000, 20000, 50000)

  dimension.matrix=rbind(number_of_nodes,
    count.trees.with.vector.of.nodes(number_of_nodes))

  categories.atleast.elements.count.matrix=matrix(nrow =
                           length(number_of_trees), ncol =
                           length(number_of_nodes), dimnames =
                           list(number_of_trees, number_of_nodes))
  
  p.value.matrix <- matrix(nrow = length(number_of_trees), 
                           ncol = length(number_of_nodes), 
                           dimnames = list(number_of_trees,
                             number_of_nodes))

  v.observed.matrix <- matrix(nrow = length(number_of_trees), 
                              ncol = length(number_of_nodes), 
                              dimnames = list(number_of_trees,
                                number_of_nodes))

  X.squared.matrix <- matrix(nrow = length(number_of_trees), 
                              ncol = length(number_of_nodes), 
                              dimnames = list(number_of_trees,
                                number_of_nodes))
  
  for(tree_index in 1:length(number_of_trees)){
    for(node_index in 1:length(number_of_nodes)){
      
      simulation.data <-
        simulation(number_of_trees[tree_index],
                   number_of_nodes[node_index],
                   enable.computation.on.trees=FALSE)
      
      p.value.matrix[tree_index, node_index] <-
        simulation.data$p.value
      
      v.observed.matrix[tree_index, node_index] <-
        simulation.data$v.observed

      X.squared.matrix[tree_index, node_index] <-
        simulation.data$chi.square.obs.statistic

      categories.atleast.elements.count.matrix[tree_index, node_index] <-
        number_of_trees[tree_index] /
        count.of.trees.with.specified.nodes(
          number_of_nodes[node_index])
    }
  }

  print.latex.table(dimension.matrix)
  print.latex.table(p.value.matrix)
  print.latex.table(v.observed.matrix)
  print.latex.table(X.squared.matrix)
  print.latex.table(categories.atleast.elements.count.matrix)  
  
  list(
    dimension.matrix=dimension.matrix,
    p.value.matrix=p.value.matrix,
    v.observed.matrix=v.observed.matrix,
    X.squared.matrix=X.squared.matrix,
    categories.atleast.elements.count.matrix=
    categories.atleast.elements.count.matrix)  
}

produce.leaves.height.frame <- function(){
  nodes <- 3:10
  dimensions <- c(100,200,300,1000, 10000, 10000, 50000, 100000)
  ## nodes <- 3:6
  ## dimensions <- c(100,200,300,1000)

  rows <- length(nodes)
  theo.mean.leaves <- rep(0, rows)
  theo.mean.height <- rep(0, rows)
  emp.mean.leaves <- rep(0, rows)
  emp.mean.height <- rep(0, rows)
  frame <- data.frame(nodes,
                      dimensions,
                      theo.mean.leaves,
                      theo.mean.height,
                      emp.mean.leaves,
                      emp.mean.height)
  for(i in 1:length(dimensions)){
    sim <- simulation(dimensions[i],
                      nodes[i])
    frame$theo.mean.leaves[i] <- sim$theoretical.mean.leaves
    frame$theo.mean.height[i] <- sim$theoretical.mean.height
    frame$emp.mean.leaves[i] <-  sim$sampling.mean.leaves
    frame$emp.mean.height[i] <-  sim$sampling.mean.height
    
  }
  print.latex.table(frame)
  frame
}

print.latex.table <- function(matrix){
  library(xtable)
  tex.code <- xtable(matrix)
  print(tex.code)
  tex.code  
}

repeated_simulation <- function(number_of_trees, 
                                nodes_in_each_tree, 
                                repeated_sampling_dimension){
  
  leaves_mean = rep(repeated_sampling_dimension)
  height_mean = rep(repeated_sampling_dimension)
  leaves_var = rep(repeated_sampling_dimension)
  height_var = rep(repeated_sampling_dimension)
  theoretical_mean_of_leaves=rep(repeated_sampling_dimension)
  theoretical_mean_of_height=rep(repeated_sampling_dimension)
  theoretical_var_of_leaves=rep(repeated_sampling_dimension)
  theoretical_var_of_height=rep(repeated_sampling_dimension)
  chi.sq.statistics = rep(repeated_sampling_dimension)
  test.mean.leaves = rep(repeated_sampling_dimension)
  test.mean.height = rep(repeated_sampling_dimension)
  test.var.leaves = rep(repeated_sampling_dimension)
  test.var.height = rep(repeated_sampling_dimension)

  ## the freedom degrees vector should contains all the same value
  freedom.degrees = rep(repeated_sampling_dimension)
  
  for (i in 1:repeated_sampling_dimension) {
    system("echo")
    system(paste("echo Starting ", i, "-th generation", sep=""))
    sim <- simulation(number_of_trees, nodes_in_each_tree)
    freedom.degrees[i] <- sim$freedom.degree
    leaves_mean[i] = sim$sampling.mean.leaves
    height_mean[i] = sim$sampling.mean.height
    leaves_var[i] = sim$sampling.var.leaves
    height_var[i] = sim$sampling.var.height
    theoretical_mean_of_leaves[i] = sim$theoretical.mean.leaves
    theoretical_mean_of_height[i] = sim$theoretical.mean.height
    theoretical_var_of_leaves[i] = sim$theoretical.var.leaves
    theoretical_var_of_height[i] = sim$theoretical.var.height
    chi.sq.statistics[i] = sim$chi.square.obs.statistic
    test.mean.leaves[i] = sim$test.mean.leaves 
    test.mean.height[i] = sim$test.mean.height
    test.var.leaves[i] = sim$test.var.leaves
    test.var.height[i] = sim$test.var.height
  }
  
  test.var.leaves <- test.var.leaves/sqrt(var(leaves_var))
  test.var.height <- test.var.height/sqrt(var(height_var))
  
  postscript("repeated-sampling-leaves-mean.ps", horizontal = FALSE)  
  curve(dnorm(x),
        from=-3,
        to=3,
        lty=2,
        col="red",
        ylab="standardized leaves mean distribution")
  
  lines(density(test.mean.leaves, from=-3, to=3), 
        col="blue")  
  dev.off()     
  
  postscript("repeated-sampling-height-mean.ps", horizontal = FALSE)
  curve(dnorm(x),
        from=-3,
        to=3,
        lty=2,
        col="red",
        ylab="standardized heights mean distribution")
  
  lines(density(test.mean.height, from=-3, to=3), 
        col="blue")  
  dev.off()     
  
  postscript("repeated-sampling-chi-squared-quar-mean.ps", horizontal = FALSE)
  plot(density(chi.sq.statistics), 
       ylab="chi-squared density distribution", 
       col="green")  
  dev.off()     
  
  postscript("repeated-sampling-leaves-var.ps", horizontal = FALSE)
  ## curve(dcauchy(x, mean(theoretical_var_of_leaves)),
  ##       lty=2,
  ##       col="red",
  ##       ylab="standardized leaves var distribution")
  
  plot(density(test.var.leaves),
       ylab="standardized leaves var distribution",
       col="grey")  
  dev.off()    
  
  postscript("repeated-sampling-height-var.ps", horizontal = FALSE)
  plot(density(test.var.height),
       ylab="standardized height var distribution",
       col="brown")  
  
  dev.off()    
}

timed_simulation <- function(number_of_trees, nodes_in_each_tree){
  system.time(simulation(number_of_trees, nodes_in_each_tree))
}

simulation <- function(number_of_trees, 
                       nodes_in_each_tree, 
                       render_svg=FALSE,
                       remove_cvs_files=TRUE,
                       enable.computation.on.trees=TRUE){
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

  datas$sampling_leaves <- 0
  datas$sampling_height <- 0
  datas$leaves <- 0
  datas$height <- 0
  if(enable.computation.on.trees){
    
    command <- paste ("./treesUtility.ocaml.bytecode", 
                      filename, nodes_in_each_tree)
    system(command)
    
    datas <- read.csv(file=paste(filename_without_extension, 
                        "-augmented.csv",
                        sep="")
                      ,head=TRUE,
                      sep=",")
    
    datas$sampling_leaves <- datas$leaves * datas$hits  
    datas$sampling_height <- datas$height * datas$hits
    
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
  }
  
  if (remove_cvs_files){
    system("echo Cleaning mess files...")
    system("rm *.dot")
    system("rm *.csv")
    system("echo done")
  }

  return(make_interesting_report(datas, 
                                 number_of_trees, 
                                 nodes_in_each_tree))
}

count.trees.with.vector.of.nodes <-
  function(vector.of.nodes.dimension){
    
  result <- rep(0, length(vector.of.nodes.dimension))
  for(i in 1:length(vector.of.nodes.dimension)){
    result[i] <- count.of.trees.with.specified.nodes(
                   vector.of.nodes.dimension[i])
  }
  result
}

count.of.trees.with.specified.nodes <- function(nodes.in.each.tree){
  (length(combn(2*nodes.in.each.tree,
                nodes.in.each.tree))/nodes.in.each.tree)/
                  (nodes.in.each.tree+1)
}

make_interesting_report <- function(datas,
                                    number.of.trees, 
                                    nodes.in.each.tree){
  chi_square_test <- chisq.test(datas$hits)
  
  number.of.possible.trees <- count.of.trees.with.specified.nodes(
                                nodes.in.each.tree)

  v.observed <- ((number.of.possible.trees / number.of.trees) *
                 sum(datas$hits^2))-number.of.trees

  ## the following formula is another way to compute the observed
  ## statistic, equivalent to the one written above (the following is
  ## kept from the lecture notes LucidiSimulazione1213.pdf)
  ## v.observed <- sum((datas$hits - number.of.trees /
  ##                    number.of.possible.trees)^2 / (number.of.trees /
  ##                    number.of.possible.trees))
  
  theoretical.mean.leaves <- mean(datas$leaves)
  theoretical.mean.height <- mean(datas$height)
  theoretical.var.leaves <- var(datas$leaves)
  theoretical.var.height <- var(datas$height)  
  
  sampling.mean.leaves <- sum(datas$sampling_leaves)/sum(datas$hits)
  sampling.mean.height <- sum(datas$sampling_height)/sum(datas$hits)
  sampling.var.leaves <- sum((datas$leaves - 
                              sampling.mean.leaves)^2)/(length(datas$hits)-1)
  sampling.var.height <- sum((datas$height - 
                              sampling.mean.height)^2)/(length(datas$hits)-1)

  test.mean.leaves <- (sampling.mean.leaves - theoretical.mean.leaves)/
    sqrt (theoretical.var.leaves) * sqrt (sum (datas$hits))

  test.mean.height <- (sampling.mean.height - theoretical.mean.height)/
    sqrt (theoretical.var.height) * sqrt (sum (datas$hits))

  ## for the following computation misses a factor, namely the square
  ## root of the variance of the variances, to divide all the
  ## expression
  test.var.leaves <- (sampling.var.leaves - theoretical.var.leaves)*
    sqrt (sum (datas$hits))

  test.var.height <- (sampling.var.height - theoretical.var.height)*
    sqrt (sum (datas$hits))
  
  list(
    v.observed=v.observed,
    chi.square.obs.statistic = chi_square_test$statistic,
    p.value = chi_square_test$p.value,
    freedom.degree = chi_square_test$parameter,
    theoretical.mean.leaves=theoretical.mean.leaves,
    theoretical.mean.height=theoretical.mean.height,
    theoretical.var.leaves=theoretical.var.leaves,
    theoretical.var.height=theoretical.var.height,
    sampling.mean.leaves=sampling.mean.leaves,
    sampling.mean.height=sampling.mean.height,
    sampling.var.leaves=sampling.var.leaves,
    sampling.var.height=sampling.var.height,
    test.mean.leaves=test.mean.leaves,
    test.mean.height=test.mean.height,
    test.var.leaves=test.var.leaves,
    test.var.height=test.var.height)
}

generate.tree <- function(number_of_nodes){

  word_dimension <- 2 * number_of_nodes

  ## making the universe from which we're going to extract the L set
  universe <- 1:word_dimension
  sample <- sample(universe, size=number_of_nodes)
  w = rep(0, word_dimension)
  for (i in 1:word_dimension) {
    w[i] <- ifelse(any(sample == i), 1, -1)
  }
  
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
