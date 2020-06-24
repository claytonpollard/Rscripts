data = read.csv("data.csv")

max_y <- 200                                                             ### MAXIMUM Y VALUE (LEFT)
interval_left <- 20                                                      ### LEFT INTERVAL
interval_right <- 10# %                                                  ### RIGHT INTERVAL

percentage_inverval <- max_y*interval_right/100

suppressPackageStartupMessages(library(dplyr))

data <- arrange(data, desc(freq)) %>%
  mutate(
    cumsum = cumsum(freq),
    rel_freq = round(freq / sum(freq), 3),
    cum_freq = cumsum(rel_freq),
    cumsum_graph = max_y*cumsum/max(cumsum)
  )
data

#library(tikzDevice)                                                      ### TO EXPORT TIKZ FILE FOR USE IN LATEX
#tikz('chart.tex')                                                        ### NOTE IT DOES *NOT* LIKE USING "%' IN LINE 24
                                                                          ### I SUGGEST WRITING IN 'PERCENT' AND DO A FIND AND REPLACE IN THE .TEX FILE TO '\%'

def_par <- par() 

par(mar=c(5,5,3,5))                                                       ### IF ADDING HEADER CHANGE THE 3 TO A 5 (changes margin)

pc = barplot(data$freq,
             width = 2,
             space = 0.25,
             axes = F,
             ylim = c(0, max_y),
             names.arg = data$category,
#            col="#69b3a2"                                                ### remove # and add HEX colour for the bars if you like
             )

grid(nx=NA, ny=50, col='grey40')

lines(pc, data$cumsum_graph, type = "b", pch = 20)

box(col = "grey50")

axis(side = 2, at=seq(0,max_y,interval_left), las = 1)

axis(side = 4, at=seq(0,max_y,percentage_inverval), labels =  paste(seq(0,100,interval_right),"%",sep=""), las = 1)

mtext("Reasons for arriving late", side = 1, line = 3)                    ### X LABEL
mtext("Number of late arrivals", side = 2, line = 3)                      ### Y (left) LABEL
mtext("Cumulative Percentage", side = 4, line = 3)                        ### Y (right) LABEL
#mtext("Header", side = 3, line = 2,font=2,cex=2)                         ### HEADER LABEL - Remove # at beginning of line


par(def_par)