# helper function
# Function that scales a vector such that sum = 1  
frequency.a = function(x,...)
{
  x = x/sum(x,...)
}

# Function that generates a multinomial probabilities for a number of classes.
