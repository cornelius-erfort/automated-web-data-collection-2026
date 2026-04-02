library(httr)
library(tidyverse)
library(rvest)
library(pbapply)
library(xml2)

# Download Wikipedia page
myhtml <- read_html("https://en.wikipedia.org/wiki/List_of_political_scientists")

# Select the relevant nodes
myelements <- html_elements(myhtml, ".mw-heading2+ ul li > a:nth-child(1)")

# Remove last element "Political theorist"
myelements <- myelements[-length(myelements)]

# Remove links that do not point to wikipedia.org
myelements <- myelements[str_detect(html_attr(myelements, "href"), "^\\/wiki")]

# Remove duplicates
myelements <- myelements[!duplicated(html_attr(myelements, "href"))]

# Make dataframe
scientists <- data.frame(link = str_c("https://en.wikipedia.org", html_attr(myelements, "href")),
                         name = html_text(myelements)
                         )

scientists$filename <- str_c("wikipedia/", basename(scientists$link) %>% str_remove_all("\\%"))

# Make folder
dir.create("wikipedia")

# Download individual pages
pbsapply(scientists$link, function (x) {
  
  if(!file.exists(str_c("wikipedia/", basename(x)  %>% 
                        str_remove_all("\\%")))) GET(x) %>% 
    content %>% write_html(file = str_c("wikipedia/", basename(x) %>% str_remove_all("\\%")))
  
}
  )

# Check whether I downloaded all HTMLs
dim(scientists) # Dimensions of my dataframe
list.files("wikipedia") %>% length # Number of downloaded files

# Go through all HTML pages and get a list of links
read_html(scientists$filename[1]) %>% html_nodes("#mw-content-text a") %>% html_attr("href") %>% str_subset("^\\/wiki") %>% str_c("https://en.wikipedia.org", .) %>% list

# Count the number of links to other political scientists on each page
scientists$mentions <- pbsapply(scientists$filename, FUN = function (x) {
  all_links <- read_html(x) %>% html_nodes("p a") %>% html_attr("href") %>% str_subset("^\\/wiki") %>% str_c("https://en.wikipedia.org", .) %>% list
  all_links <- all_links[[1]]
  all_links[all_links %in% scientists$link] %>% length
  })


table(scientists$mentions)





############## NETWORK

# Install and load required packages
# install.packages(c("tidygraph", "ggraph", "igraph"))
library(tidygraph)
library(ggraph)
library(igraph)

# Subset only to pages with many mentions
scientists$mentions[scientists$mentions > 2] %>% length
famous_scientists <- filter(scientists, mentions > 2)
famous_scientists$id <- 1:nrow(famous_scientists) # Add id var

# Create edges
edges <- data.frame(from = famous_scientists$id)
edges$to <- pbsapply(edges$from, FUN = function (x) {
  all_links <- (read_html(famous_scientists$link[famous_scientists$id == x]) %>% 
                html_nodes("#mw-content-text a") %>% 
                html_attr("href") %>% 
                str_subset("^\\/wiki") %>% 
                str_c("https://en.wikipedia.org", .) %>% 
                list)[[1]]
  famous_scientists$id[famous_scientists$link %in% all_links]
}) 
edges <- edges %>% unnest(cols = c("to")) %>% as.data.frame()

# Create graph object
graph <- tbl_graph(nodes = famous_scientists, edges = edges, directed = FALSE)

# Add community detection
graph <- graph %>%
  mutate(community = as.factor(group_louvain()))

# Create the visualization
ggraph(graph, layout = "fr") + 
  geom_edge_link(alpha = 0.2) +
  geom_node_point(aes(color = community, size = mentions), alpha = 0.8) +
  geom_node_text(aes(label = name), 
                 size = 3, 
                 repel = TRUE,
                 max.overlaps = 20) +
  scale_size_continuous(range = c(2, 8)) +
  theme_graph() +
  theme(legend.position = "none") +
  labs(title = "Network of Political Scientists on Wikipedia",
       subtitle = "Nodes sized by number of mentions, colored by community")

# Transform NYT responses into a single dataframe
nyt_df <- nytsearch %>%
  lapply(content) %>%  # Extract content from each response
  lapply(function(x) x$response$docs) %>%  # Get the docs from each response
  bind_rows() %>%  # Combine all docs into one dataframe
  select(  # Select relevant columns
    headline = headline.main,
    abstract,
    pub_date,
    web_url,
    snippet,
    lead_paragraph,
    section_name,
    subsection_name,
    word_count
  ) %>%
  mutate(  # Clean up the data
    pub_date = as.Date(pub_date),
    word_count = as.numeric(word_count)
  )

# View the resulting dataframe
View(nyt_df)

# Optional: Save to CSV
write_csv(nyt_df, "nyt_articles.csv")
