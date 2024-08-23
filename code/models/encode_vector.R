library(circular)

encode_vector = function(stimulus, feature_len, kappa) {
  # Converts stimulus (=2 feature positions) 
  # to concatenated von Mises distributions, length normalised
  step_size = 360/feature_len
  radians = circular(seq(0, 359, by = step_size)/180 * pi)
  
  # stimulus = radians[ceiling((stimulus+0.1)/step_size)] # snap to closest tracked angle
  stimulus = circular(stimulus/180 * pi)
  vector = c(sapply(stimulus, function(x) dvonmises(radians, mu = x, kappa = kappa)))
  
  # normalise by length
  vector = vector/sqrt(sum(vector^2))
  # vector = vector/sum(vector)
  return(vector)
}
