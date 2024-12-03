library(tidyverse)
library(broom)
summary(combined_data)
write.csv(combined_data, "Downloads/EPLdata", row.names = FALSE)
uniq <- length(unique(combined_data$Referee))
uniq
min(combined_data$Time)

combined_data$timeofday <- ifelse(as.numeric(substr(combined_data$Time, 1, 2)) < 17, 
                       "Afternoon", 
                       "Evening")
library(moments)
skewness(combined_data$HST)
hist(combined_data$HST, main = "Histogram", xlab = "Home Shots on Target")

test <- read.csv("Downloads/EPLdata")
which(names(combined_data) == "AR")
try <- combined_data[, 1:24]
try$timeofday <- combined_data$timeofday
write.csv(x, "Downloads/JAMAMA.csv", row.names = FALSE)
rows_w_Amoss <- try[try$Referee == "A Moss", ]
rows_w_dCoote <- try[try$Referee == "D Coote", ]
cootestudy <- try[try$Referee == "D Coote" & (try$HomeTeam == "Chelsea" | try$AwayTeam == "Liverpool"), ]

x <- read.csv(file = 'Downloads/JAMOVI.csv')
x$B365CA <- combined_data$B365CA

model <- lm(TBP ~ -1+Referee, x)
summary(model)


model_summary <- tidy(model, conf.int = TRUE)
model_summary$term
model_summary <- model_summary[model_summary$term != "(Intercept)", ]

ggplot(model_summary, aes(x = reorder(term, estimate), y = estimate)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  coord_flip() + 
  labs(
    x = "Referees",
    y = "Coefficient (Impact on Total Booking Points)",
    title = "Referee Impact on Total Booking Points"
  ) +
  theme_linedraw()




x$ExpectedR <- apply(x[, c("B365CH", "B365CD", "B365CA")], 1, function(row) {

  min_col <- names(row)[which.min(row)]
  
  substr(min_col, nchar(min_col), nchar(min_col))
})

x$POS <- ifelse(x$ExpectedR == x$FTR, "Y", "N")


bothRED <- x[x$HR > 0 & x$AR > 0, ]

x$ERO <- pmin(x$B365CH, x$B365CA)

united <- x[(x$HomeTeam == "Man United" | x$AwayTeam == "Man United") & x$POS == "N", ]
filt_uni <- united[
  ((united$HomeTeam == "Man United" & united$ExpectedR == "H") & united$POS == "N") |
    ((united$AwayTeam == "Man United" & united$ExpectedR == "A") & united$POS == "N"),
]

