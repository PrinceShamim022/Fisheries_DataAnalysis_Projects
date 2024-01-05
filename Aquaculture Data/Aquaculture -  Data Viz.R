##############################################################################################################################
########################## BOX PLOT - Water Quality Parameters ####################################################################################################

# Load Libraries
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

nf <- read.csv("WQ.csv")
unique(nf$Treatment)
colnames(nf)
library(tidyverse)


nf$Treatment <- ordered(nf$Treatment, levels = c("Commercial", "Control","T1 (5%)", "T2 (10%)", "T3 (15%)"))

bp <- ggplot(nf, aes(x = Treatment, y = TAN, fill = Treatment)) + 
  geom_boxplot(alpha = 0.6, show.legend = F)+
  #  labels = c("Commercial", "Control","T1 (5%)", "T2 (10%)", "T3 (15%)") +
  labs(title="",x= "Treatment", y = "Concentration (TAN)") +
  coord_flip() +
  theme_classic()

bp


ggsave("bp.png",
       width = 10,
       height = 8,
       units = "cm",
       dpi = 300)

tiff('bp.tiff', units="in", width=10, height=6, res=300, compression = 'lzw')
bp
dev.off()



gp <- ggplot(nf, aes(x = Treatment, y = SRP, fill = Treatment)) + 
  geom_boxplot(alpha = 0.6, show.legend = F)+
  labs(title="",x="Treatment", y = " Concentration (SRP)") +
  coord_flip() +
  theme_classic()

gp


ggsave("gp.png",
       width = 10,
       height = 8,
       units = "cm",
       dpi = 300)


tiff('gp.tiff', units="in", width=10, height=6, res=300, compression = 'lzw')
gp
dev.off()






hp <- ggplot(nf, aes(x = Treatment, y= NO.N, fill = Treatment)) + 
  geom_boxplot(alpha = 0.6, show.legend = F)+
  labs(title="", x = "Treatment", y = "Concentration (NO2-N)") +
  coord_flip() +
  theme_classic()

hp

ggsave("hp.png",
       width = 10,
       height = 8,
       units = "cm",
       dpi = 300)


tiff('hp.tiff', units="in", width=10, height=6, res=300, compression = 'lzw')hp
dev.off()



library(ggpubr) #load in library for multi-panel figures
#put all three plots together into one multipanel plot
multi_plot <- ggarrange(bp,hp,gp, #plots that are going to be included in this multipanel figure
                        labels = c("A", "B", "C"), #labels given each panel 
                        ncol = 3, nrow = 1, #adjust plot space 
                        common.legend = T) #does the plot have a common legend
#add titles and labels to the multi-panel graph
multi_plot <- annotate_figure(multi_plot,
                              top = text_grob("", color = "black", face = "bold", size = 11))
multi_plot


##############################################################################################################################
########################## Line PLOT - Bacterial Challenges ####################################################################################################

# Load Libraries
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(tidyverse) 

df <- read.csv("Bacteria.csv")
str(df)
colnames(df)
unique(df$Treatment)
unique(df$Day)

df$Day <- ordered(df$Day, levels = c("Day1", "Day2",  "Day3",  "Day4", "Day5",  
                                     "Day6",  "Day7",  "Day8", "Day9",  "Day10"))

df$Treatment <- ordered(df$Treatment, levels = c("Algae treatment", "Control", "Commercial"))

library(tidyverse)
df %>%
  #  filter(Treatment != "Commercial" &
  #           Treatment != "Algae treatment") %>% 
  ggplot(aes(Day, Cumulative.Mortality, color = Treatment)) +
  geom_point(aes(color=Treatment)) +
  geom_line(aes(color=Treatment))+
  labs(title = "") +
  theme_minimal()


p <- ggplot(df, aes(x = Day, y = Cumulative.Mortality, group = Treatment)) +
  geom_line(aes(color = Treatment), size = 2, alpha = 0.7)+
  geom_point(aes(color = Treatment),stat = "identity", size = 4, alpha = 0.6) +
  scale_color_manual(values =
                       c("Algae treatment" = "darkgreen",
                         "Control" = "blue",
                         "Commercial" = "red"))+
  labs(title = "", x = "", y = "Cumulative Mortality (%)") +
  theme_classic()
p


tiff('p.tiff', units="in", width=10, height=6, res=300, compression = 'lzw')
p
dev.off()


##############################################################################################################################
########################## Stacked Bar PLOT - Proximate Composition ####################################################################################################

# Load Libraries
library(readxl)
library(ggplot2)
library(RColorBrewer)
library(tidyverse)

df <- read_excel("PLC.xlsx")
glimpse(df)
colnames(df)

# 1. Load required R packages
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(rstatix))


# 3. Statistical tests
res.stats <- df %>%
  group_by(Treatment) %>%
  t_test(Value ~ Composition) %>%
  adjust_pvalue() %>%
  add_significance()

res.stats

# 4. Create a stacked bar plot, add "mean_se" error bars
p <- ggbarplot(
  df, x = "Treatment", y = "Value", add = "mean_se",
  fill = "Composition", palette = "Set2"
)
# theme(axis.text.x = element_text(angle = 90))


##############################################################################################################################
########################## Density PLOT - Proximate Survival Rate ####################################################################################################

library(readxl)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
display.brewer.all(colorblindFriendly=TRUE)

df <- read_excel("Growth.xlsx")
tibble(df)
glimpse(df)
colnames(df)
unique(df$Treatment)

#df$Treatment <- ordered(df$Treatment, levels = c("Treatment 1", "Treatment 2", 
#                                                 "Treatment 3", "Control", 
#                                                 "Commercial"))

n <- df %>%
  ggplot(aes(x = SurvivalRate,
             fill = Treatment))+
  geom_density(alpha = 0.6)+
  theme(plot.title=element_text(size=20)) +
  labs(title = "",
       x = "Survival Rate (%)",
       y = "Density",
       fill = "Treatment")+
  theme_classic()

n

ggsave("n.png",
       width = 10,
       height = 8,
       units = "cm",
       dpi = 300)


tiff('n.tiff', units="in", width=10, 
     height=6, res=300, 
     compression = 'lzw')
n
dev.off()


##############################################################################################################################
############## Dodge Error Bar PLOT - Hematological Parameters ####################################################################################################

library(tidyverse)
library(readxl)

#View(df)

# Load modules
library(ggplot2)
library(RColorBrewer)


df <- read_excel("Hemp.xlsx")
glimpse(df)
colnames(df)

unique(df$RBC)
unique(df$WBC)
unique(df$Monocytes)



data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}



df2 <- data_summary(df, varname="RBC",
                    groupnames=c("Treatment"))
#df2$dose=as.factor(df2$dose)
head(df2)



library(ggplot2)
p <- ggplot(df2, aes(x = Treatment, y=RBC, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=RBC-sd, ymax=RBC+sd), width=.2,
                position=position_dodge(.9)) +
  theme(legend.position = "none")

print(p)



colnames(df)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}



df2 <- data_summary(df, varname="Hb",
                    groupnames=c("Treatment"))
#df2$dose=as.factor(df2$dose)
head(df2)



library(ggplot2)
q <- ggplot(df2, aes(x = Treatment, y= Hb, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=Hb-sd, ymax=Hb+sd), width=.2,
                position=position_dodge(.9)) +
  theme(legend.position = "none")

print(q)





colnames(df)

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}



df2 <- data_summary(df, varname="WBC",
                    groupnames=c("Treatment"))
#df2$dose=as.factor(df2$dose)
head(df2)



library(ggplot2)
r <- ggplot(df2, aes(x = Treatment, y=WBC, fill=Treatment)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=WBC-sd, ymax=WBC+sd), width=.2,
                position=position_dodge(.9)) +
  theme(legend.position = "none")

print(r)


library(ggpubr) #load in library for multi-panel figures
#put all three plots together into one multipanel plot
multi_plot<- ggarrange(p,q,r, #plots that are going to be included in this multipanel figure
                       labels = c("A", "B", "C"), #labels given each panel 
                       ncol = 3, nrow = 1, #adjust plot space 
                       common.legend = T) #does the plot have a common legend
#add titles and labels to the multi-panel graph
multi_plot <- annotate_figure(multi_plot,
                              top = text_grob("", color = "black", face = "bold", size = 11))
multi_plot


ggsave("plot1.png",
       width = 20,
       height = 10,
       units = "cm",
       dpi = 300)

##############################################################################################################################
##################### Heat-Map PLOT - Fatty Acids ####################################################################################################

df <- read.csv("Fattyacid.csv")
colnames(df)



ggplot(data = df, aes(x = Treatment, y = Fatty.Acids)) +
  geom_tile(aes(fill = Concentration)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "Treatment", y = "Fatty Acid", title = "Fatty Acid Profile")



##############################################################################################################################
########################## Bar PLOT - Pigments ####################################################################################################

# Install these packages if you have not and load them:
library(readxl)
library(ggplot2)
library(ggthemes)
library(multcompView)
library(dplyr)

df <- read_excel("Pigment.xlsx")
colnames(df)

ggbarplot(df, x = "Pigment", y = "value", 
          add = c("mean_se", "jitter"),
          color = "Treatment", palette = "jco",
          position = position_dodge(0.8)) +
  coord_flip()

ggsave("Pigment.png",  units="in", width=11, height=7)

##############################################################################################################################
