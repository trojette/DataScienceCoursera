flip = function(z, min, max) {
  set <- c()
  up <- 0
  down <- 0
  
  while(z >= min && z <= max) {
    set <- c(set, z)
    # print(z)
    coin <- rbinom(1, 1, 0.5)
    z <- if(coin == 1) { up <- up + 1 ; z + 1 ; } 
                  else { down <- down + 1 ; z - 1 }
  }  

  bias <- 100 * (up-down) / (up+down)
  
  plot(c(1:eval(up+down)), set, xlab = "coups", ylab = "z",
       col = "black", type = "s")
  title(sprintf("%d flips, %d up / %d down, bias = %s %.2f %%",
                up + down, up, down, if(sign(bias) > 0) "+" else "",
                bias))
  
  # Sys.sleep(.09)
}

flip(10, 0, 20)