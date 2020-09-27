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

spatial_articles[["editor"]] <- spatial_articles[["YEAR"]] == 2017

png("bivands_effect.png", height = 480, width = 480)
ggplot(spatial_articles, aes(x = YEAR, fill = editor)) +
  geom_bar() +
  scale_x_continuous("Year") +
  scale_y_continuous("Number of spatial articles") +
  scale_fill_manual("Roger is editing R Journal", values = c("darkgrey", "red")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")
dev.off()
