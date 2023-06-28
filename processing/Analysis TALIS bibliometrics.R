# Analysis TALIS ---------------------------------------------------------------

## 1. Gráficos -----------------------------------------------------------------
### Fig 1 ----------------------------------------------------------------------
# Number of documents over time
freq_doc_py <- with(talis, table(PY)) %>%
  as.data.frame()
freq_doc_py$PY <- as.numeric(as.character(freq_doc_py$PY))
freq_doc_py$Freq <- as.numeric(freq_doc_py$Freq)

freq_doc_py %>%
  ggplot(aes(x = PY, y = Freq, color = "#440154FF")) + 
  theme_bw(base_size = 15) +
  theme(legend.position = "none",
        legend.key.size = unit(1,"cm")) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = "#440154FF") +
  scale_x_continuous(breaks = seq(2009, 2022, 1), limits = c(2009,2022)) +
  scale_y_continuous(breaks = seq(0, 70, 10), limits = c(0,70)) +
  labs(x = "Publication year", y = "Number of documents",
       caption = paste0("n = ", nrow(talis), " documents"))

### Fig 2 -----------------------------------------------------------------------
# Number of documents over time by index
freq_doc_ind_py <- with(talis, table(IND, PY)) %>%
  as.data.frame()
freq_doc_ind_py$PY <- as.numeric(as.character(freq_doc_ind_py$PY))
freq_doc_ind_py$Freq <- as.numeric(freq_doc_ind_py$Freq)

freq_doc_ind_py %>%
  ggplot(aes(x = PY, y = Freq, group = IND)) + 
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1,"cm")) +
  geom_line(aes(color = IND), linewidth = 1) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(2009, 2022, 1), limits = c(2009,2022)) +
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0,40)) +
  labs(x = "Publication year", y = "Number of documents", colour = "Indexation database",
       caption = paste0("n = ", nrow(talis), " documents"))

### Fig 3 -----------------------------------------------------------------------
# Percentage of documents over time by index
freq_doc_ind_py_prop = freq_doc_ind_py %>%
  group_by(PY) %>%
  reframe(IND = IND,
          Freq = Freq,
          total_year = sum(Freq),
          perc_year = round((Freq/total_year)*100,1),
          perc_year = if_else(is.na(perc_year), 0, perc_year)
  )

freq_doc_ind_py_prop %>%
  ggplot(aes(x = PY, y = perc_year, group = IND)) + 
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1,"cm")) +
  geom_line(aes(color = IND), linewidth = 1) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(2009, 2022, 1), limits = c(2009,2022)) +
  scale_y_continuous(breaks = seq(0, 100, 25), limits = c(0,100),
                     labels = function(x) paste0(x, "%")) +
  labs(x = "Publication year", y = "Percentage of documents", colour = "Indexation database",
       caption = paste0("n = ", nrow(talis), " documents"))

### Fig 4 -----------------------------------------------------------------------
# Number of documents published in sources
freq_doc_so <- with(talis, table(SO)) %>%
  as.data.frame()
freq_doc_so$Freq <- as.numeric(as.character(freq_doc_so$Freq))
freq_doc_so <- freq_doc_so %>% arrange(desc(Freq))

filter(freq_doc_so, Freq >= 3) %>%
  ggplot(aes(x = reorder(SO, Freq), y = Freq)) + 
  theme_bw(base_size = 15) +
  theme(legend.position = "bottom",
        legend.key.size = unit(1,"cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  geom_bar(stat = "identity", fill = "#440154FF") +
  scale_y_continuous(breaks = seq(0, 15, 3), limits = c(0,15)) +
  labs(x = "Journal", y = "Number of documents published",
       caption = paste0("n = ", nrow(talis), " documents.", " n = ", nrow(freq_doc_so), " journals. Top 30 journals are shown."))


### Fig 5 -----------------------------------------------------------------------
# Most productive countries
n_co <- talis %>% separate_rows(AU_CO, sep = ";") %>%
  count(AU_CO) %>%
  arrange(desc(n))

n_co$AU_CO <- str_to_title(n_co$AU_CO)
n_co$AU_CO <- if_else(n_co$AU_CO == "Usa", "United States", n_co$AU_CO)

ggplot(data = n_co, aes(x = reorder(AU_CO, n), y = n, fill = if_else(AU_CO %in% c("Colombia", "Chile", "Argentina", "Brazil", "Mexico"), "Latin America", "Other"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Latin America" = "#FDE725FF", "Other" = "#440154FF")) +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 125, by = 25), limits = c(0,130)) +
  labs(x = "Author country", y = "Number of authors' country occurrences", fill = "Region",
       caption = paste0("n = ", sum(n_co$n), " countries."))

### Fig 6 ----------------------------------------------------------------------
# Most common disciplines
n_wc <- talis %>% separate_rows(WC, sep = "; ") %>%
  count(WC) %>%
  arrange(desc(n)) %>%
  filter(!is.na(WC))

n_wc$WC <- str_to_title(n_wc$WC)

ggplot(data = n_wc, aes(x = reorder(WC, n), y = n)) +
  geom_bar(stat = "identity", fill = "#3B528BFF") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Subject", y = "Number of subject's ocurrences",
       caption = paste0("n = ", sum(n_wc$n), " subjects. Only Web of Science documents."))

n_wc <- n_wc %>%
  reframe(WC = WC,
          n = n,
          total = sum(n),
          perc = round((n/total)*100,1))

ggplot(data = n_wc, aes(x = reorder(WC, n), y = perc)) +
  geom_bar(stat = "identity", fill = "#3B528BFF") +
  theme_bw(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100),
                     labels = function(x) paste0(x, "%")) +
  labs(x = "Subject", y = "Percentage of subject's ocurrences",
       caption = paste0("n = ", sum(n_wc$n), " subjects. Only Web of Science documents"))

### Fig 7 -----------------------------------------------------------------------
# Country collaboration network
net_talis_colab <- biblioNetwork(talis, analysis = "collaboration",
                                 network = "countries", sep = ";")

# Convert to igraph object and remove self-loops
net_talis_colab_ig <- simplify(graph_from_adjacency_matrix(as.matrix(net_talis_colab), mode = "undirected"))

# Set node labels to title case
V(net_talis_colab_ig)$name <- str_to_title(V(net_talis_colab_ig)$name)
V(net_talis_colab_ig)$name[V(net_talis_colab_ig)$name == "Usa"] <- "USA"
V(net_talis_colab_ig)$name[V(net_talis_colab_ig)$name == "United Kingdom"] <- "UK"
V(net_talis_colab_ig)$name[V(net_talis_colab_ig)$name == "U Arab Emirates"] <- "UA Emirates"

# Define color palette
colpal <- ifelse(V(net_talis_colab_ig)$name %in% c("Colombia", "Chile", "Argentina", "Brazil", "Mexico"), "#FDE725", "#44015480")

# Set up edge width and node size
edge_width <- degree(net_talis_colab_ig, mode = "in")
edge_width <- rescale(edge_width, to = c(2, 6), from = c(0, 12))
node_size <- degree(net_talis_colab_ig, mode = "in")
node_size <- rescale(node_size, to = c(3, 9), from = c(0, 12))

# Plot network with custom colors, edge width, and node size
layout <- layout_with_fr(net_talis_colab_ig)
plot(net_talis_colab_ig, layout = layout, 
     vertex.label.dist = 0.7, vertex.label.cex = 0.7, 
     vertex.size = node_size, vertex.label.color = "black", 
     vertex.color = colpal, edge.color = "#525252")

### Fig 8 ----------------------------------------------------------------------
# Keywords network
net_talis_key <- biblioNetwork(talis, analysis = "co-occurrences", network = "keywords", sep = ";")
net_key <- networkPlot(net_talis_key, normalize = "association", weighted = T, n = 30, Title = "Keyword co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)

### Fig 9 -----------------------------------------------------------------------
# Centralities
t1 <- data.frame(
  degree_cen = degree(net_talis_colab_ig),
  bet_cen = betweenness(net_talis_colab_ig, directed = FALSE),
  clos_cen = closeness(net_talis_colab_ig),
  ei_cen = evcent(net_talis_colab_ig)$vector,
  har_cen = harmonic_centrality(net_talis_colab_ig, mode = "out"),
  katz_cen = katzcent(net_talis_colab_ig, vids = V(net_talis_colab_ig), alpha = 0.1)
)

t1 <- bind_cols(rownames(t1), t1)
t1 <- rename(t1, "Country" = "...1")
rownames(t1) <- NULL
t1 <- filter(t1, degree_cen != 0 & clos_cen != 1)
t1 <- arrange(t1, -degree_cen, -bet_cen, -clos_cen, -ei_cen, -har_cen, -katz_cen)

t1_long <- gather(t1, key = "centrality", value = "value", -Country)

t1_long$centrality <- factor(t1_long$centrality,
                             levels = c("degree_cen", "bet_cen", "clos_cen", "ei_cen", "har_cen", "katz_cen"),
                             labels = c("Degree", "Betweeness", "Closeness", "Eigenvector", "Harmonic", "Katz"),
                             ordered = TRUE)

# Add a new column with the rank of each country within each centrality group
t1_long <- t1_long %>%
  group_by(centrality) %>%
  mutate(rank = dense_rank(desc(value))) %>%
  ungroup()

# Reorder the levels of the Country factor based on the rank within each centrality group
t1_long$Country <- factor(t1_long$Country, levels = unique(t1_long$Country[order(t1_long$rank)]))

# Create the plot with the reordered countries within each facet
ggplot(t1_long, aes(x = Country, y = value, fill = if_else(Country %in% c("Colombia", "Chile", "Argentina", "Brazil", "Mexico"), "Latin America", "Other"))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Latin America" = "#FDE725FF", "Other" = "#440154FF")) +
  facet_wrap(~centrality, scales = "free_y",
             ncol = 2, nrow = 3) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs(fill = "Region", y = "Value", x = "Country")

## 2. Modelos ------------------------------------------------------------------
# 2.1. Predecir lenguaje de publicación según país primer autor ----------------
m1 <- glm(LA_REC ~ LATAM_AU1_CO, data = talis, family = "binomial")
round(exp(m1[["coefficients"]][["LATAM_AU1_CO"]])*100,1)

# Crear un nuevo data frame con valores de LATAM_AU1_CO para predecir
new_data <- data.frame(LATAM_AU1_CO = seq(min(talis$LATAM_AU1_CO), max(talis$LATAM_AU1_CO), length.out = 100))

# Obtener los valores predichos
predicted <- predict(m1, newdata = new_data, type = "response")

# Crea un data frame con los valores predichos
df_predicted <- data.frame(LATAM_AU1_CO = new_data$LATAM_AU1_CO, predicted = predicted)

# Graficar los valores predichos con ggplot2
ggplot(df_predicted, aes(x = LATAM_AU1_CO, y = predicted)) +
  geom_line() +
  scale_x_continuous(breaks = c(0,1), limits = c(0,1)) +
  scale_y_continuous(breaks = c(seq(.7,.9, .1)), limits = c(.7,.9)) +
  theme_bw() +
  labs(x = "Latin American authorship", y = "Predicted probability")

# 3. Ejercicios ----------------------------------------------------------------

## 3.1. Ejercicio 1 -------------------------------------------------------------
# Ajusta un modelo de regresión logística para predecir la indexación (variable WOS)
# según el país primer autor
# Obtén los resultados del modelo con la función summary()
# Obtén los valores predichos
# Gráfica los valores predichos
# Usa la función BIC() para comparar el ajuste del m1 con el m2: ¿cuál modelo ajusta mejor?

## 3.2. Ejercicio 2 ------------------------------------------------------------
# Retoma el código de la Figura 2
# Grafica la productividad según indexación recodificada en WoS y Scopus
# ¿Notas algún cambio en la tendencia?
# Recuerda hacer un gráfico del número de documentos y otro de porcentaje en el tiempo