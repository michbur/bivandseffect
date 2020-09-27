library(bib2df)
library(ggplot2)
download.file(url = "https://journal.r-project.org/archive/RJournal.bib",
              destfile = "rjournal.bib")
arts <- bib2df("rjournal.bib")
arts[["YEAR"]]

find_words <- function(word, df)
  df[grep(pattern = word, x = df[["TITLE"]], ignore.case = TRUE), 
     c("TITLE", "YEAR")]

spatial_articles <- unique(rbind(find_words("spatial", arts),
                                 find_words("geo", arts),
                                 find_words("gis", arts),
                                 find_words("area", arts)))


png("bivands_effect.png", height = 480, width = 480)
ggplot(spatial_articles, aes(x = YEAR)) +
  geom_bar() +
  scale_x_continuous("Year") +
  scale_y_continuous("Number of spatial articles") +
  theme_bw(base_size = 16)
dev.off()
