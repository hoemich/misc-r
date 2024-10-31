Usage:

```
library(ggplot2)
source("interval_scaling.R")

my_scale <- create_interval_scales(c(-10,-5,-2,2,5,10), c(1,1,0.3,1,0.3,1,1), "my_scale")

ggplot() +
  geom_function(fun=rlang::as_function(~.x^2)) +
  scale_x_continuous(transform=my_scale, limits = c(-15,15), breaks=c(-15,-10,-6,-2,0,2,6,10,15)) +
  scale_y_continuous(transform=my_scale)
```
