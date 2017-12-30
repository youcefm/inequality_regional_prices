# Inequality Measurement and regional prices
library(data.table)
library(googleVis)
library(wordcloud)
library(reldist)
dt <- data.table(read.csv("~/Desktop/Work/VincentGeloso/RegionalInequality.csv"))

# Population stats by state
dt_pop <- dt[, list(pop = sum(wtsupp)), by = "statefip"]
dt_pop[, `:=`(pop_pct = 100 * pop/sum(pop))]

library(Hmisc)
smpl_n <- nrow(dt)
pop_n <- sum(dt$wtsupp)
probs <- seq(0.1, 0.9, 0.1)
z = 1.96 # 1.96 is for 95% confidence interval
r_lower <- ( smpl_n*probs - z*sqrt(smpl_n*probs*(1-probs)) )/smpl_n
r_upper <- ( smpl_n*probs + z*sqrt(smpl_n*probs*(1-probs)) )/smpl_n

# Uncorrected Income deciles in US distribution
inc_decile <- as.integer(wtd.quantile(dt$inctot
                           ,probs = seq(0.1, 0.9, 0.1)
                           ,weights = dt$wtsupp))

inc_decile_lb <- as.integer(wtd.quantile(dt$inctot
                                 ,probs = r_lower
                                 ,weights = dt$wtsupp))

inc_decile_ub <-as.integer(wtd.quantile(dt$inctot
                                 ,probs = r_upper
                                 ,weights = dt$wtsupp))

# Income deciles corrected for State price levels
incRPP_decile <- as.integer(wtd.quantile(dt$inctotRPP
                           ,probs = seq(0.1, 0.9, 0.1)
                           ,weights = dt$wtsupp))

incRPP_decile_lb <- as.integer(wtd.quantile(dt$inctotRPP
                              ,probs = r_lower
                              ,weights = dt$wtsupp))

incRPP_decile_ub <- as.integer(wtd.quantile(dt$inctotRPP
                              ,probs = r_upper
                              ,weights = dt$wtsupp))

# Income Decile Difference Table (with uncertainty range)
dt_inc_decile_diff <- data.table(avg_diff = (incRPP_decile - inc_decile)/inc_decile
                                 , uncertainty_diff1 = (incRPP_decile_ub - inc_decile_lb)/inc_decile_lb
                                 , uncertainty_diff2 = (incRPP_decile_lb - inc_decile_ub)/inc_decile_ub )


# GINI FOR WHOLE COUNTRY
gini_1 <- gini(dt$inctot, weights = dt$wtsupp)
gini_2 <- gini(dt$inctotRPP, weights = dt$wtsupp)

# Between state gini

dt_state <- dt[, list(avg_inc = weighted.mean(inctot, wtsupp), 
                          avg_incRPP = weighted.mean(inctotRPP, wtsupp)), 
                   by = "statefip"]

gini_1 <- gini(dt_state$avg_inc)
gini_2 <- gini(dt_state$avg_incRPP)

dt_plot <- data.table(labels = c("between state gini index before price correction", "between state gini index after price correction"),
                      values = c(gini_1, gini_2))

plot(gvisColumnChart(dt_plot, xvar = "labels", 
                     yvar = "values", 
                     options = list(vAxis = "{format:'#.##%', minValue: 0}",
                                    title = "Title")
))

# Bottom 10% by state
dt[, `:=`(nb_bottom = bottom_ten * wtsupp)]
dt_bottom_state <- data.table(dt[, list(nb_bottom = sum(nb_bottom)), by = "statefip"])
dt_bottom_state[, `:=`(pct_bottom = 100 * nb_bottom/sum(nb_bottom))]

# Bottom 10% by state correcting for Regional Prices
dt[, `:=`(nb_bottomRPP = bottomRPP_ten * wtsupp)]
dt_bottomRPP_state <- data.table(dt[, list(nb_bottomRPP = sum(nb_bottomRPP)), by = "statefip"]) 
dt_bottomRPP_state[, `:=`(pct_bottomRPP = 100 * nb_bottomRPP/ sum(nb_bottomRPP))]

# Top 10% by state
dt[, `:=`(nb_top = top_ten * wtsupp)]
dt_top_state <- data.table(dt[, list(nb_top = sum(nb_top)), 
                              by = "statefip"])
dt_top_state[, `:=`(pct_top = 100 * nb_top/sum(nb_top))]

# Top 10% by state correcting for Regional Prices
dt[, `:=`(nb_topRPP = topRPP_ten * wtsupp)]
dt_topRPP_state <- data.table(dt[, list(nb_topRPP = sum(nb_topRPP)), by = "statefip"]) 
dt_topRPP_state[, `:=`(pct_topRPP = 100 * nb_topRPP/ sum(nb_topRPP))]

#Format tables
# Bottom
dt_diff_bottom <- merge(dt_bottom_state, dt_bottomRPP_state, by = "statefip")
dt_diff_bottom <- merge(dt_diff_bottom, dt_pop, by = "statefip")
dt_diff_bottom[, `:=`(ratio_bottom = pct_bottom/pop_pct, 
                      ratio_bottomRPP = pct_bottomRPP/pop_pct)]
dt_diff_bottom[, `:=`(diff_ratio_b10 = (ratio_bottomRPP - ratio_bottom)/ratio_bottom)]


# dt_diff_bottom[ statefip %in% c("Connecticut", "Maine", 
#                                 "Massachusetts", "New Hampshire", 
#                                 "Rhode Island", "Vermont"), `:=`(region = "New England")]
# dt_diff_bottom[ statefip %in% c("Alabama", "Arkansas", "Florida", 
#                                 "Georgia", "Kentucky", "Louisiana", 
#                                 "Mississippi", "Missouri", "North Carolina", "South Carolina", 
#                                 "Tennessee", "Virginia", "West Virginia"), `:=`(region = "South")]
# dt_diff_bottom[ statefip %in% c("Delaware", "Maryland", 
#                                 "New Jersey", "New York", "Pennsylvania"), 
#                 `:=`(region = "Midlle Atlantic")]
# dt_diff_bottom[ statefip %in% c("Illinois", "Indiana", "Iowa", "Kansas", 
#                                 "Michigan", "Minnesota", "Nebraska", "North Dakota", 
#                                 "Ohio", "South Dakota", "Wisconsin"), `:=`(region = "Midwest")]
# dt_diff_bottom[ statefip %in% c("Arizona", "New Mexico", "Oklahoma", "Texas"), 
#                 `:=`(region = "Southwest")]
# dt_diff_bottom[ statefip %in% c("Alaska", "California", "Colorado", 
#                                 "Hawaii", "Idaho", "Montana", "Nevada", 
#                                 "Oregon", "Utah", "Washington", "Wyoming"), `:=`(region = "West")]
# dt_diff_bottom[ ,`:=`(ratio_bottom = ratio_bottom -1, ratio_bottomRPP = ratio_bottomRPP -1)]

# Top
dt_diff_top <- merge(dt_top_state, dt_topRPP_state, by = "statefip")
dt_diff_top <- merge(dt_diff_top, dt_pop, by = "statefip")
dt_diff_top[, `:=`(ratio_top = pct_top/pop_pct, 
                      ratio_topRPP = pct_topRPP/pop_pct)]
dt_diff_top[, `:=`(diff_ratio_t10 = (ratio_topRPP - ratio_top)/ratio_top)]

dt_merge <- merge(dt_diff_top, dt_diff_bottom, by = "statefip")

plot(gvisLineChart(dt_merge, xvar = "statefip", yvar = c("diff_ratio_b10", "diff_ratio_t10") 
                   ,option = list(height = 600, 
                                  chartArea = "{width : '70%', height :'75%'}", 
                                  vAxis = "{format: '#.##%'}")))

# Plots

# Main result
# Over/Under representation of population 
# in bottom 10% of US distribution by state
wanted_region <- "Midlle Atlantic"
for(instance in unique(dt_diff_bottom$region)){
  plot(gvisColumnChart(dt_diff_bottom[region == instance], xvar = "statefip",
                       yvar = c("ratio_bottom", "ratio_bottomRPP"),
                       options = list(vAxis = "{format:'#.##%'}",
                                      title = paste("Deviation of share of bottom 10% relative 
                                      to population share, for region", instance))))
}
plot(gvisColumnChart(dt_diff_bottom, xvar = "statefip",
                     yvar = c("ratio_bottom", "ratio_bottomRPP"),
                     options = list(vAxis = "{format:'#.##%'}",
                                    title = "Deviation of share of bottom 10% relative 
                                    to population share")))


# states with a correction of at least 5%
plot(gvisColumnChart(dt_diff_bottom[diff_ratio >= 0.05 ], xvar = "statefip", 
                   yvar = "diff_ratio", 
                   options = list(vAxis = "{format:'#.##%'}",
                                  title = "States with signficantly more people in bottom 10%")
                   ))

# States with a correction of at least -5%
plot(gvisColumnChart(dt_diff_bottom[diff_ratio <= -0.05 ], xvar = "statefip", 
                     yvar = "diff_ratio", 
                     options = list(vAxis = "{format:'#.##%'}",
                                    title = "States with signficantly more people in bottom 10%")
))

# All states
plot(gvisColumnChart(dt_diff_bottom, xvar = "statefip", 
                     yvar = "diff_ratio", 
                     options = list(vAxis = "{format:'#.##%'}, 
                                    {title : 'Change in proportion of bottom 10 percent'}",
                                    title = "Impact of price correction on distribution of bottom 10%", 
                                    xAxis = "{title : 'State'}",
                                    chartArea = "{width:'80%', hight:'80%'}")
))


# Word cloud based on difference
increase_list <- dt_diff_bottom[diff_ratio>0, 
                                list(statefip, diff_ratio, 
                                     size = diff_ratio/sum(diff_ratio))]
layout(matrix(c(1, 2), nrow=2), heights=c(1, 10))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "States where price correction increases share of bottom 10%")
wordcloud(increase_list$statefip,
          increase_list$size,
          scale=c(2,0.5), 
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))
layout(matrix(c(1, 2), nrow=2), heights=c(1, 10))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "States where price correction increases share of bottom 10%")
wordcloud(increase_list$statefip,
          increase_list$size,
          scale=c(2,0.5), 
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))

layout(matrix(c(1, 2), nrow=2), heights=c(1, 10))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "States where price correction decreases share of bottom 10%")
wordcloud(decrease_list$statefip,
          decrease_list$size,
          scale=c(2,0.5), 
          max.words=100, 
          random.order=FALSE, 
          rot.per=0.35, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Dark2"))

