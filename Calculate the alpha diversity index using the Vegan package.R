library(vegan)
library(picante)
zqdata <- read.csv("", row.names = 1, head = T, fileEncoding = "GBK")
zqdata <- t(zqdata)
zqdata <- zqdata[,-1]

## Number
AB <- rowSums(zqdata)

##  Richness 
richness <- rowSums(zqdata > 0)

# 2.5 Margalef （D）
S <- specnumber(zqdata)
Margalef <- (S - 1) / log(rowSums(zqdata))

## Shannon
# Shannon 
shannon_index <- diversity(zqdata, index = "shannon", base = exp(1))

# Shannon 
shannon_diversity <- exp(1)^shannon_index

# Shannon （Pielou ） 
pielou_JSW <- shannon_index / log(richness, exp(1))

## Simpson
# Gini-Simpson
gini_simpson_index <- diversity(zqdata, index = "simpson")

# 经典 Simpson 
simpson_index <- 1 - gini_simpson_index

# Invsimpson （Gini-Simpson ）
invsimpson_index <- 1 / gini_simpson_index

# Simpson 
simpson_diversity <- 1 / (1 - gini_simpson_index)

# Simpson （equitability ） 以Simpson 指数为基础的Pielou 均匀度指数（JSI）
equitability_JSI <- 1 / (richness * (1 - gini_simpson_index))

## Chao1 & ACE
# Chao1 
chao1 <- estimateR(zqdata)[2, ]

# ACE 
ace <- estimateR(zqdata)[4, ]

## goods_coverage 
goods_coverage <- 1 - rowSums(zqdata == 1) / rowSums(zqdata)


DATA <- cbind(richness,  shannon_index, gini_simpson_index, pielou_JSW)
write.csv(DATA, "")


alpha <- read.csv("",  head = T, fileEncoding = "GBK")
alpha <- alpha[c(1:9),]
group <- read.csv("", row.names = 1, head = T, fileEncoding = "GBK")
alpha <- cbind(alpha,group)
alpha$Group1 <- factor(alpha$Group1)
model <- lm(pielou_JSW ~ Type, data = alpha)
Anova(model)
out <- LSD.test(model, "Type", p.adj = "bonferroni")
out$group
plot(out)
library(ggplot2)
p <- ggplot(alpha, aes(x = Type, y = pielou_JSW, color = Type)) +
  geom_boxplot(width = 0.35) +
  # geom_point(size = 3, position = position_jitter(width = 0.15), alpha = .5) +
  theme_bw() +
  theme(
    text = element_text(size = 25),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.background = element_rect(fill = "transparent"),
    legend.text = element_text(size = 13)
  ) 
annotate("text", x = 1, y = 0.4, label = "p = 0.017", hjust = 0, vjust = 1, size = 7.5, color = "black")
p
library(eoffice)
topptx(filename = "", height = 6, width = 12)
ggsave(plot = p, "", height = 5, width = 9, dpi = 600)
