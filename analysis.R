library(bib2df)
library(ggplot2)
library(dplyr)

download.file(url = "https://journal.r-project.org/archive/RJournal.bib",
              destfile = "rjournal.bib")
arts <- bib2df("rjournal.bib")

find_words <- function(word, df)
  df[grep(pattern = word, x = df[["TITLE"]], ignore.case = TRUE), 
     c("TITLE", "YEAR")]

spatial_articles <- unique(rbind(find_words("spatial", arts),
                                 find_words("geo", arts),
                                 find_words("gis", arts),
                                 find_words("area", arts)))

prop_df <- inner_join(group_by(arts, YEAR) %>% 
             summarise(n_articles = length(YEAR)),
           group_by(spatial_articles, YEAR) %>% 
             summarise(n_spatial = length(YEAR))) %>% 
  mutate(prop = n_spatial/n_articles,
         editor = YEAR == 2017,
         lab = paste0(n_spatial, "/", n_articles)) %>% 
  filter(YEAR != 2020)


png("bivands_effect.png", height = 480, width = 480)
ggplot(prop_df, aes(x = YEAR, y = prop, fill = editor, label = lab)) +
  geom_col() +
  geom_label(show.legend = FALSE) +
  scale_x_continuous("Year") +
  scale_y_continuous("Fraction of spatial articles") +
  scale_fill_manual("Roger is editing R Journal", values = c("darkgrey", "red")) +
  theme_bw(base_size = 16) +
  theme(legend.position = "bottom")
dev.off()
