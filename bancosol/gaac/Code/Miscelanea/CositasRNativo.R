df1 <- data.frame(id=c(1:5),saldo=c(200,100,300,500,100))
df2 <- data.frame(id=c(2:6),cast=c(100,200,0,500,200))
df1
df2
merge(df1,df2,by="id")
merge(df1,df2,by="id",all=TRUE)
merge(df1,df2,by="id",all.x=TRUE)
merge(df1,df2,by="id",all.y=TRUE)
merge(df1,df2,by=NULL) #producto cartesiano

library(dplyr)
df <- df1%>% inner_join(df2,by="id")
df <- df1%>% full_join(df2,by="id")
df <- df1%>% left_join(df2,by="id")
df <- df1%>% right_join(df2,by="id")
df <- df1%>% semi_join(df2,by="id") #only df1 columns, considering matches with df2
df <- df1%>% anti_join(df2,by="id") #only df1 rows not present in df2, es decir, df1-df2

library(ggplot2)
#The building blocks of ggplot graphics are:
#data, aesthetic mappings, geometric objects, statistical transformations,
#scales, coordinate systems, position adjustments, faceting
#We can specify different parts of the plot and combine them with +
#Data: we use ggplot(data = <DATA>) to bind the plot to a specific data frame
#Geometric objects are the actual marks we put on the plot, at least one
# geoms examples: points, lines, boxplot
#Aesthetic mapping means something observable in the plot
#An aesthetic is a mapping between a visual cue and a variable
# aes examples: position, color, fill, shape, linetype, size, alpha, group (for separating geom_line)
#NOTES:
#***You should see help pages to see which aes each geom accepts
#***Fixed visual cues (meaning no mapping required) are set outside aes
#Aesthetic only states that variables should be mapped to visual cues
#but they do not tell how should happen
#Scales describes the visual cues that should be used in the plot
#scales examples: position, color, fill, size, shape, linetype
# they are modified as: scales_<aesthetics>_<type>
# scale args: name, limits, breaks, labels
#Faceting is a name for small multiples in ggplot2
#Facets create separate graphs for subsets of data
#facet_wrap(): subsets as levels of a single grouping var
#facet_grid(): subsets as the crossing of two variables
#labs() is used to name title, and axis
#Finally theme() is used to customize  components of the graph
# theme examples:text, axis.text.x = element_text(colour,size,angle,hjust,vjust)
#If you want to save store the ggplot in an object and then use ggsave()
