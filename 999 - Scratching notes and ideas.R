# Scratching file

library(magrittr)

# This is where I play around with ideas before committing them to functions and discrete code files

# First I want a function for Hex to RGB. This is so that I can then
# go from RGB to XYZ tristimulus

# Turns out this is in base R

# Sample Hex value: #bab598 has RGB (186,181,152) and XYZ (42.44, 45.75, 36.30)
colour = "#bab598"
vec_RGB <- col2rgb(col = colour, alpha = FALSE)

# Now from rgb to XYZ, which would be a great thing
# For this we need to know that WCAG uses the sRGB working space
# which uses the D65 reference white.

# The matrix transformations are fairly standard. These are from
# either ASTM E308-01 or Wzszecki & Stiles Colour Science)

# So I propose a series of functions to convert RGB to XYZ giving known 
# RGB working spaces and reference white. Each working space has a defined
# reference white, but the function should be able to handle conversion 
# between references, using the Bradford method: rather than calculating 
# it will simply use the matrix of conversion.

# This will be supported by a series of tables holding information
# on the primaries.

# This can be done later. For the meantime simply working out the contrast 
# ratio between two RGB colours

## Relative Luminance
# We will simply assume that we are in the sRGB space for ease, as this
# is what is used in the WCAG, in which case luminance

# Get Hex
colour = "#bab598"
# Hex to RGB
temp_RGB <- col2rgb(col = colour, alpha = FALSE)/255
vec_RGB <- data.frame(V = c(temp_RGB[1], temp_RGB[2], temp_RGB[3])) %>% 
  dplyr::mutate(V_L = dplyr::case_when(
    V <= 0.04045 ~ V/12.92,
    TRUE ~ ((V+0.055)/1.055)^2.4
  ))
L = 0.2126*vec_RGB[1,2]+0.7152*vec_RGB[2,2]+0.0722*vec_RGB[3,2]

# In a function
fn_HexToLuminance <- function(hex){
  
  temp_RGB <- col2rgb(col = hex, alpha = FALSE)/255
  vec_RGB <- data.frame(V = c(temp_RGB[1], temp_RGB[2], temp_RGB[3])) %>% 
    dplyr::mutate(V_L = dplyr::case_when(
      V <= 0.04045 ~ V/12.92,
      TRUE ~ ((V+0.055)/1.055)^2.4
    ))
  L = 0.2126*vec_RGB[1,2]+0.7152*vec_RGB[2,2]+0.0722*vec_RGB[3,2]
  
  return(L)
}

fn_HexToLuminance("#bab598")

# Now to test aspect ratio for two hex colours
fn_ContrastRatio <- function(hex1, hex2){
  La = fn_HexToLuminance(hex1)
  Lb = fn_HexToLuminance(hex2)
  
  L1 = max(La,Lb)
  L2 = min(La,Lb)
  
  CR = (L1 + 0.05)/(L2 + 0.05)
  
  return(CR)
}

# Test!

fn_ContrastRatio("#bab598","#deface")
# Correct compared to WCAG website

# Now the reverse ... if I have a reference colour, what other colours pass the
# colour contrast threshold? And then can I define a set of colours that tile
# space with these minimum distances?

# What if we could populate points within the RGB space, such that the 
# edge weight joining the points is the contrast ratio and never is 
# lower than 4.6 (the limit + 0.1

fn_HexToLuminance("#bab598")
fn_HexToLuminance("#deface")



## Investigating that first transformation from RGB_s to RGB

xy <- data.frame(x = seq(0.001,1,0.001)) %>% 
  dplyr::mutate(y = dplyr::case_when(
    x <= 0.04045 ~ x/12.92,
    TRUE ~ ((x+0.055)/1.055)^2.4
  ))

ggplot2::ggplot(data = xy,
                ggplot2::aes(x = x, y = y)) +
  ggplot2::geom_line()

# That's a pretty damn smooth response curve, so perhaps it can be 
# approximated. Let's try power regression
lm1 <- lm(log(xy$y) ~ log(xy$x))
summary(lm1)

z = exp(1.729695*log(xy$x)-0.298431)
plot(xy$x, xy$y)
lines(xy$x, z, col = "blue", lty = 2)
# Nope, not a good model at all

# Let's try just using the same power curve for all values of x
xyz <- data.frame(x = seq(0.001,1,0.001)) %>% 
  dplyr::mutate(y = dplyr::case_when(
    x <= 0.04045 ~ x/12.92,
    TRUE ~ ((x+0.055)/1.055)^2.4
  ),
  z = ((x+0.055)/1.055)^2.4)

ggplot2::ggplot(data = xyz) +
  ggplot2::geom_line(ggplot2::aes(x, y), colour = "red") +
  ggplot2::geom_line(ggplot2::aes(x, z), colour = "blue") +
  ggplot2::scale_x_continuous(limits = c(0,0.05)) +
  ggplot2::scale_y_continuous(limits = c(0,0.05))

# Utterly tiny result difference