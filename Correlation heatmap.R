
a=plot(1:5, 1:5, type = "n",ann = F, bty = "n", xaxt = "n", yaxt ="n")
text(3,3, "三维热图",cex=3)


dat_heat <- read_excel("Health_heatmap.xlsx")

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1
library(ComplexHeatmap)
cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[2:n,2:n]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "healthy heatmap")
library(ggcorrplot)
ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())

dat_heat <- read_excel("heatmap.xlsx")

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1
library(ComplexHeatmap)
cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[2:n,2:n]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "Covid_heatmap")
library(ggcorrplot)
ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())



dat_heat <- read_excel("m_heatmap.xlsx")

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1
library(ComplexHeatmap)
cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[2:n,2:n]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "myocarditis heatmap")
library(ggcorrplot)
ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())






dat_heat <- gg0
#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1

ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())


cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[1:n-1,1:n-1]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "healthy heatmap")


dat_heat <-gg1

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1

ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())


cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[1:n-1,1:n-1]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "Covid19_heatmap")




dat_heat <- gg2

#cancer patient
testdata1 <- do.call(rbind,dat_heat)
testdata1 <- t(testdata1)
testdata1 <- as.data.frame(testdata1)
testdata1 <- as.data.frame(lapply(testdata1,as.numeric))
testdata1

ggcorrplot(cor_data1,outline.color="white",
           type="lower",colors = c("#06DDF9", "#F7F7F7", "#E46726"),
           lab = TRUE,lab_size = 3, tl.cex = 10, ggtheme = ggplot2::theme_void())


cor_data1 <- abs(cor(testdata1, use = 'pairwise.complete.obs',method="kendall"))
n <- length(colnames(cor_data1))
mat  <- cor_data1[1:n-1,1:n-1]
a <- 1:n
for (ra in a){mat[ra,ra]=0}
mat
Heatmap3D(mat, name = "mat", column_title = "myocarditis heatmap")

