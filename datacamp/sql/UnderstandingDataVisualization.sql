--UNDERSTANDING DATA VISUALIZATION
--HISTOGRAMS plot a single continuous variable
/*
When looking at a distribution:
  1. Look at the modality (bimodal, trimodal, etc)
  2. Look at the skewness (one-sided heavy tails) 
  3. Look at the kurtosis (ocurrence of outliers)
*/
/*
BOXPLOTS
When trying to visualize the distribution of different continuous variable.
You can plot all histograms in a single plot, use a panel of histograms, 
but it is better to use boxplots, they show a box of the IQR, 
also the median and the whiskers 1.5 IQR.
*/
/*
SCATTER PLOTS
They are used when there are 2 continuous vaviables and want to visualize
the relationship between the variables.
Specially, pay attention to correlation. 
Then try to use transformations to account for the scale of the values.

LINE PLOT
When there is a causal connection or correlation in the x axis 
(such as time or space) and we only have a single record for each x VALUE
then it is better to use lineplot.
It is common to have different series of observations from different categories
So it is better to plot each category line separately with different color or typeline.

BAR PLOTS
They are used when plotting counts or categorical variables.
When there are a lot of categories, put them in the y axis and order them by count.

DOT PLOTS
They are used in the same situations as BAR PLOTS, but they can be scaled.
Also several metrics can be shown at once
*/
/*
HIGHER DIMENSIONS
When dealing with more than 2 variables. wecould use a 3D scatterplot
Other ways of drawing 3rd 4th and nth dimensions are:
  * color
  * size
  * transparency
  * shape
Colorspaces: Hue-Chroma-Luminance
Hue: is the color
Chroma is the intensity of grayness
Luminance is the intesity of white
For categories: Vary Hue
For ordered variables: Vary Luminance

Divergent scale: Use opposite colors with a neural in the middle.

PLOTTING MANY VARIABLES AT ONCE
PAIRPLOTS. Plots 2 variables at once in a panel containing all variables
(this applies to categorical and numerical vairables)
CORRELATION HEATMAPS. It is useful for several numerical variables to show relationships.

PARALLEL COORDINATE PLOT. 
*/
/*
Polar coordinates.
PIE PLOTS. It is usually denoted to show proportions. Since they are harder to interpret it is almost never a good idea.
IF DATA HAS SOME NATURAL CIRCULARITY, like time of the day POLAR COORDINATES are acceptable.
*/
/*
DEALING WITH AXES
Always use true scales. And set the reference level plot correctly.
SENSORY OVERLOAD
If there are too many things in a plotm the information can be overwhelming
and the audience can get zero insights from the plot.
*/