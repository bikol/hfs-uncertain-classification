rm(list=ls())

source("results-overview.R")

library(tikzDevice)
options(tikzPdftexWarnUTF = FALSE)

env = new.env()
load("datasets/dyslexic-eval-output.RData", env)

filename = "dyslexic"

training.stats.all.perf = convertClasses(get("training.stats.all.perf", envir = env))
training.stats.all.perf.wide = convertClasses(get("training.stats.all.perf.wide", envir = env))

# ref.colors =c("#FC8D62", "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#36c7ab")


stats1 = transmute(training.stats.all.perf.wide, Method, Class, Subclass, Subsubclass, Value=`Accuracy (all)`) %>%
    filter(Subsubclass!="R", Subclass=='All', Class=='GFS') %>%
    group_by(Class, Subclass, Subsubclass) %>%
    top_n(1, jitter(Value))
stats1 = mutate(stats1, Err=0)
stats1 = bind_rows(stats1, list(Method="crisp", Subclass="all", Subsubclass='cr.', Value=0.343, Class="GFS", Err=0.0),
                  list(Method="extended", Subclass="all",Subsubclass="ext.", Value=0.579, Class="GFS", Err=0.137))

stats2 = transmute(training.stats.all.perf.wide, Method, Class, Subclass, Subsubclass, Value=`Accuracy (all)`) %>%
    filter(Subsubclass!="R", Subclass=='All', Class=='mean') %>%
    group_by(Class, Subclass, Subsubclass) %>%
    top_n(1, jitter(Value))
stats2 = mutate(stats2, Err=0)

stats3 = transmute(training.stats.all.perf.wide, Method, Class, Subclass, Subsubclass, Value=`Accuracy (all)`) %>%
    filter(Subsubclass!="R", Subclass=='All', Class=='median') %>%
    group_by(Class, Subclass, Subsubclass) %>%
    top_n(1, jitter(Value))
stats3 = mutate(stats3, Err=0)

# stats$Subclass = factor(stats$Subclass, levels=c("J_min_id", "J_SS_m5_id", "J_SS_m2_id", "J_SS_m1_id",
#                                                  "J_SS_m05_id", "J_prod_id", "J_SS_05_id", "J_luk_id",
#                                                  "J_SS_2_id", "J_SS_5_id", "crisp", "extended"))
# levels(stats$Subclass) = c("J_min", "J_SS_m5", "J_SS_m2", "J_SS_m1",
#                            "J_SS_m05", "J_prod", "J_SS_05", "J_luk",
#                            "J_SS_2", "J_SS_5", "ostra", "rozszerzona")

# a1 = ggplot(data=stats,
#            aes(x=Subsubclass, y=1-Value, group=Class, colour=Subsubclass, shape=Subsubclass, fill=Subsubclass)) +
#     geom_errorbar(aes(ymin=1-Value, ymax=1-Value+Err),
#                   width=0.9,                    # Width of the error bars
#                   position=position_dodge(.9), color="black") +
#     geom_bar(stat = "identity", color="black") +
#     scale_fill_grey(start=1, end=0.8) +
#     xlab("") +
#     ylab("error rate") +
#     theme_bw() +
#     geom_text(aes(label = round(1-Value,2)),size = 3.5, hjust=-0.3, color='black') +
#     theme(legend.position="none",
#           plot.margin=unit(c(0.005, 0.005, 0.0, 0.01), "npc"),
#           legend.margin=unit(0.05, "npc"),
#           panel.margin = unit(0.07, "npc"),
#           axis.text.x=element_text(size=9),
#           strip.text.x=element_text(size=9),
#           axis.title.x = element_text(vjust=-0.5),
#           axis.title.y = element_text(vjust=1.5),
#           title = element_text(vjust=1.2)) +
#     coord_cartesian(ylim=c(0, 0.7)) +
#     facet_grid(Class~., scales="fixed", space="free")+
#     coord_flip(ylim=c(0, 0.7))

plots = lapply(list(stats1, stats2, stats3), function(stats){
    ggplot(data=stats,
           aes(x=Subsubclass, y=1-Value, group=Class, colour=Subsubclass, shape=Subsubclass, fill=Subsubclass)) +
        geom_errorbar(aes(ymin=1-Value, ymax=1-Value+Err),
                      width=0.9,                    # Width of the error bars
                      position=position_dodge(.9), color="black") +
        geom_bar(stat = "identity", color="black") +
        scale_fill_grey(start=1, end=0.8) +
        xlab("") +
        ylab("") +
        theme_bw() +
        geom_text(aes(label = round(1-Value,2)),size = 2, hjust=-0.25, color='black') +
        theme(legend.position="none",
              plot.margin=unit(c(0.005, 0.005, 0.0, 0.01), "npc"),
              legend.margin=unit(0.05, "npc"),
              panel.margin = unit(0.07, "npc"),
              axis.text.x=element_blank(),
              strip.text.x=element_text(size=9),
              axis.title.x = element_text(vjust=-0.5),
              axis.title.y = element_text(vjust=1.5),
              title = element_text(vjust=1.2)) +
        coord_cartesian(ylim=c(0, 0.71)) +
        facet_grid(Class~., scales="free", space="free")+
        coord_flip(ylim=c(0, 0.71))
})



tikz(file=paste0("4-",filename,"-all.tex"), sanitize = T, height = 3.8, width = 3.4,
documentDeclaration = '\\documentclass[conference]{IEEEtran}')
grid.arrange(plots[[1]], plots[[2]], plots[[3]], nrow=3, heights=unit(c(0.15, 0.425, 0.425), "npc"))
dev.off()