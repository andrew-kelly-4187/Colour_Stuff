# Scratching file

# This is where I play around with ideas before committing them to functions and discrete code files

# First I need a function for Hex to RGB. This is so that I can then
# go from RGB to XYZ tristimulus

# Turns out this is in base R

# Sample Hex value: #bab598 has RGB (186,181,152) and XYZ (42.44, 45.75, 36.30)
colour = "#bab598"
vec_RGB <- col2rgb(col = colour, alpha = FALSE)

# Now from rgb to XYZ
# For this we need to know that WCAG uses the sRGB working space
# which uses the D65 reference white.

# The matrix transformations are fairly standard. These are from
# either ASTM E308-01 or Wzszecki & Stiles Colour Science)

# So I propose a series of functions to convert RGB to XYZ giving known 
# RGB working spaces and reference white

