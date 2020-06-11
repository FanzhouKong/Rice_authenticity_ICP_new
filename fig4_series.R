library("ggpubr")
library(ggplot2)
relief_result <- read.csv("~/Documents/GitHub/Rice_authenticity_ICP_new/updated_results/relief_result_python_with_constant.csv")
tiff("~/Desktop/GFSC/Projects/Rice Authentication Project/publication_restart/manuscript_almost_done/figures/figure_ready/fig4a.tiff", res=300, compression = "rle", height=4, width=3.33, units="in")


ggbarplot(relief_result, x = "feature", y = "score",
          fill = "constant",               # change fill color by cyl
          color = "white",            # Set bar border colors to white
          # palette = "jco",            # jco journal color palett. see ?ggpar
          sort.val = "asc",          # Sort the value in dscending order
          sort.by.groups = FALSE,     # Don't sort inside each group
          x.text.angle = 0,          # Rotate vertically x axis texts
          rotate=TRUE,
          ggtheme = theme_pubr()   
) + theme(legend.position = "none", text = element_text(size = 5, face = "bold"))+labs(y="Relative importance", x = "Features")
dev.off()

training_results<- read.csv("~/Documents/GitHub/Rice_authenticity_ICP_new/updated_results/training_result.csv")
# training_results$Number.of.features<- as.factor(training_results$Number.of.features)
tiff("~/Desktop/GFSC/Projects/Rice Authentication Project/publication_restart/manuscript_almost_done/figures/figure_ready/fig4b.tiff", res=300, compression = "rle", height=4, width=3.33, units="in")
# ggplot(training_results, aes(x=Number.of.features, y=Accuracy, color=Algorithm,fill=Algorithm, shape = Algorithm)) + geom_line() +geom_point(size=4)
ggplot(training_results, aes(x=Number.of.features, 
                             y=Accuracy, 
                             color=Algorithm,
                             fill=Algorithm, 
                             shape = Algorithm)
       ) +geom_line() +geom_point(size=4)+theme_pubr()+theme(text = element_text(size = 6, face = "bold"),
                                                             legend.position = "bottom"
                                                )+labs(y="Accuracy (%)", x = "Number of features")
dev.off()
