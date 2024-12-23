#============================================================================
# Scatter
castGabo <- readRDS('C:/!bso/castigos/bdcCastProc_feb2023.rds') # Este archivo lo arma el gabo
castGabo_clean <- readRDS('C:/!bso/castigos/Castigos_v2.rds') # Este archivo lo arma el gabo

febCast <- castGabo_clean %>% 
  dplyr::filter(monDate == 'feb. 2023') %>% 
  select(Sucursal, NOMBRE_AGENCIA, AgenciaDes, saldoCast) %>% 
  group_by(Sucursal, NOMBRE_AGENCIA) %>% 
  summarise_all(sum, na.rm = T)
sum(febCast$saldoCast, na.rm = T)

feb_act <- readRDS('D:/!bso/girCartera/rds_v3/ec_Feb2023.rds') %>% 
  dplyr::filter(ESTADO != 'CASTIGADA') %>% 
  dplyr::filter(MODULO != 131) %>% 
  group_by(AGENCIA) %>% 
  summarise(saldous = sum(saldous, na.rm = T)) %>% 
  left_join(nameAG, by = 'AGENCIA') %>% 
  select(-AGENCIA)
sum(feb_act$saldous, na.rm = T)

gph <- feb_act %>% 
  left_join(febCast, by = 'NOMBRE_AGENCIA') %>% 
  arrange(Sucursal, NOMBRE_AGENCIA) %>% 
  group_by(Sucursal, NOMBRE_AGENCIA) %>% 
  mutate(n = max(row_number())) %>% 
  mutate(saldous = ifelse(row_number() > 1, 0, saldous)) %>% 
  dplyr::filter(!str_detect(NOMBRE_AGENCIA, 'Norm')) %>% 
  mutate(ratio = saldoCast/saldous) %>% 
  dplyr::filter(ratio < 1)

sum(gph$saldoCast, na.rm = T)
sum(gph$saldous, na.rm = T)

ggplot(gph, aes(x = saldoCast, y = saldous/1000, color = Sucursal)) + 
  geom_point(aes(size = ratio)) + 
  theme_minimal() + scale_color_manual(values = paleta(14)) +
  scale_x_continuous(label = comma) + 
  geom_vline(xintercept = mean(gph$saldoCast)) + 
  geom_hline(yintercept = mean(gph$saldous/1000))

# clustering
clust <- gph %>% 
  ungroup() %>% 
  select(NOMBRE_AGENCIA, ratio)

# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- gph[,7]
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

k3 <- kmeans(gph[,7], 4, nstart = 20)
clusters <- as.data.frame(k3$cluster) %>% 
  dplyr::rename(Grupo = `k3$cluster`) %>% 
  bind_cols(gph) %>% 
  mutate(label = paste0(Sucursal, ', ', NOMBRE_AGENCIA, ': ',
                        as.character(round(ratio*100, 1)), '%')) %>% 
  mutate(label = ifelse(ratio > 0.034, label, NA))

ggplot(clusters, aes(x = saldoCast, y = saldous/1000, 
                     color = as.factor(Grupo))) + 
  geom_point(size = 3) + 
  theme_minimal() + 
  scale_x_continuous(label = comma, breaks = seq(0, 1500000, 250000), 
                     name = 'Saldo Castigado (USD)') + 
  scale_y_continuous(label = comma, name = 'Saldo (M USD)') +
  geom_label_repel(aes(label = label, color = factor(Grupo)),
                   position = 'dodge', size = 2.5, show.legend = F) + 
  theme(legend.position = 'bottom') + 
  guides(color=guide_legend(title="Grupo")) +
  scale_color_manual(values = paleta(5)) 
ggsave('C:/!bso/castigos/scatter_feb23.png')