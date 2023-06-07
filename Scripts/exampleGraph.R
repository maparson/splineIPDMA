#--------------------------------#
# Example graph script           #
#--------------------------------#

# Load packages
source("./Scripts/packages.R")

# Create example data
datExample <- tibble(
  x = 1:100
) 

# Categorization
datCateg <- tibble(
  x = (1:10000)/10000
) %>%
  mutate(
    f2 = as.numeric(x>33.3/100 & x <66.6/100),
    f3 = as.numeric(x>= 66.6/100)
  ) %>%
  pivot_longer(cols = starts_with("f"),
               names_to = "grp") %>%
  mutate(Method = "Categorization")

# FP
datFP <- tibble(
  x = (1:10000)/10000
) %>%
  mutate(
    f1 = x^(1/2),
    f2 = x^1,
    f3 = x^3
  ) %>%
  pivot_longer(cols = starts_with("f"),
               names_to = "grp")%>%
  mutate(Method = "FP")

# Splines
datSpline <- tibble(
  x = (1:10000)/10000
) %>%
  mutate(
    f1 = ns(x, df = 3)[,1],
    f2 = ns(x, df = 3)[,2],
    f3 = ns(x, df = 3)[,3],
  ) %>%
  pivot_longer(cols = starts_with("f"),
               names_to = "grp") %>%
  mutate(Method = "Spline")

ggexample<-
  ggplot(mapping = aes(x = x, y = value, group = grp, colour = Method))+
  geom_line(data = datSpline, aes(), size = 1.1) +
  geom_line(data = datFP, aes(), size = 1.1) +
  geom_line(data = datCateg, aes(), size = 1.1) +
  ylab(expression(f[m](x))) + 
  theme_minimal(base_size = 16)
  

ggsave(ggexample, filename = "ExampleGraph.png",  device='png', dpi=700)



