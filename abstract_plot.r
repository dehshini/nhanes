# Load required libraries
library(tidyverse)
library(patchwork)
library(ggthemes)

theme_panel <- theme_light() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.title = element_text(size = 10)) +
    theme(legend.text = element_text(size = 10)) +
    theme(legend.key.size = unit(0.9, "cm")) +
    theme(legend.key.height = unit(0.2, "cm")) +
    theme(plot.tag.position = c(0.01, 0.99))


# P1 - Overall

highsrh_summary2 <- fread("./out/highsrh_summary2.csv")

# rename the first column
colnames(highsrh_summary2)[1] <- "cycle"

# create the plot
p1 <- ggplot(
    data = highsrh_summary2,
    aes(x = cycle, y = Proportion)
) +
    geom_line(
        aes(group = 1),
        linewidth = 1
    ) +
    geom_point(
        size = 3.5
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI
        ),
        width = 0.1
    ) +
    scale_x_discrete(
        label = xlabels.2
    ) +
    scale_y_continuous(
        breaks = seq(0, 100, 20), 
        limits = c(0, 100)
    ) +
    labs(
        x = "Year",
        y = "Proportion (%)",
        tag = "A"
    ) +
    theme_panel +
    annotate(
        geom = "text",
        x = 2,
        y = 90,
        label = c("Overall\n\n", paste("\tP trend", round(p_trend.2[[2, 5]], 3))),
        size = 5
    )

p1


# P2 - by Age

p2 <- ggplot(
    data = ageplot.2,
    aes(x = cycle, y = Proportion, color = Age)
) +
    geom_line(
        aes(group = Age),
        linewidth = 1,
        position = position_dodge(width = 0.2)
    ) +
    geom_point(
        aes(color = Age, shape = Age),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Age
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        x = "Year",
        y = "Proportion (%)",
        color = "Age",
        shape = "Age",
        tag = "B"
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        breaks = seq(0, 100, 20),
        limits = c(0, 100)
    ) +
    scale_color_manual(
        labels = age_ptrend.2,
        values = c(">=65" = "blue", "<65" = "red"),
        breaks = c(">=65", "<65")
        ) +
    scale_shape_manual(
        labels = age_ptrend.2,
        values = c(">=65" = 15, "<65" = 17),
        breaks = c(">=65", "<65")
        ) +
    theme(
        legend.position = c(0.05, 0.9),
        legend.justification = "left"
    )

p2



# P3 - by Race

p3 <- ggplot(
        data = raceplot.2,
    aes(x = cycle, y = Proportion, color = Ethnicity)
) +
    geom_line(
        aes(group = Ethnicity),
        linewidth = 1,
        position = position_dodge(width = 0.2)
    ) +
    geom_point(
        aes(color = Ethnicity, shape = Ethnicity),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI
            #color = Ethnicity
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        x = "Year",
        y = "Proportion (%)",
        color = "Race/Ethnicity",
        shape = "Race/Ethnicity",
        tag = "C"
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        breaks = seq(0, 100, 20),
        limits = c(0, 100)
    ) +
    scale_color_manual(
        labels = ptrend_race.2,
 #       values = c("#45b1ff", "#fdc41b", "#fa3838"),
        values = c("blue", "orange", "red"),
        breaks = c("White", "Black", "Hispanic")
    ) +
    scale_shape_manual(
        labels = ptrend_race.2,
        values = c(15, 16, 17),
        breaks = c("White", "Black", "Hispanic")) +
    theme(
        legend.position = c(0.05, 0.88),
        legend.justification = "left"
    )

p3



# P4 - by Income

p4 <- ggplot(
    data = incomeplot.2,
    aes(x = cycle, y = Proportion, color = Income)
) +
    geom_line(
        aes(group = Income),
        linewidth = 1,
        position = position_dodge(width = 0.2)
    ) +
    geom_point(
        aes(color = Income, shape = Income),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Income
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        x = "Year",
        y = "Proportion (%)",
        color = "Family Income",
        shape = "Family Income",
        tag = "D"
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_manual(
        labels = income_ptrend.2, 
        values = c("blue", "red")
    ) +
    scale_shape_manual(
        labels = income_ptrend.2,
        values = c(15, 17)
    ) +
    theme(
        legend.position = c(0.05, 0.9),
        legend.justification = "left"
    )

p4


# P5 - by Education
p5 <- ggplot(
        data = educationplot.2,
    aes(x = cycle, y = Proportion, color = Education)
) +
    geom_line(
        aes(group = Education),
        linewidth = 1,
        position = position_dodge(width = 0.2)
    ) +
    geom_point(
        aes(color = Education, shape = Education),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Education
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        x = "Year",
        y = "Proportion (%)",
        color = "Education",
        shape = "Education",
        tag = "E"
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_manual(
        labels = ptrend_education.2,
        values = c("blue", "orange", "red"),
        breaks = c("College", "High school", "Below high school")
        ) +
    scale_shape_manual(
        labels = ptrend_education.2,
        values = c(15, 16, 17),
        breaks = c("College", "High school", "Below high school")
        ) +
    theme(
        legend.position = c(0.05, 0.88),
        legend.justification = "left"
    )

p5


# P6 - by Insurance
p6 <- ggplot(
        data = insuranceplot.2,
    aes(x = cycle, y = Proportion, color = Insurance)
) +
    geom_line(
        aes(group = Insurance),
        linewidth = 1,
        position = position_dodge(width = 0.2)
    ) +
    geom_point(
        aes(color = Insurance, shape = Insurance),
        size = 3.5,
        position = position_dodge(width = 0.2),
    ) +
    geom_errorbar(
        aes(
            ymin = Lower_CI,
            ymax = Upper_CI,
            color = Insurance
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        x = "Year",
        y = "Proportion (%)",
        color = "Insurance",
        shape = "Insurance",
        tag = "F"
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_manual(
        labels = insurance_ptrend.2,
        values = c("blue", "red"),
        breaks = c("Uninsured", "Insured")
        ) +
    scale_shape_manual(
        labels = insurance_ptrend.2,
        values = c(15, 17),
        breaks = c("Uninsured", "Insured")
        ) +
    theme(
        legend.position = c(0.05, 0.9),
        legend.justification = "left"
    )

p6



# Combine the six plots
combined_plot <- (p1 + p2 + p3) / (p4 + p5 + p6)

combined_plot +
    plot_layout(
        axes = "collect",
        guides = "keep",
        axis_titles = "collect"
    ) +
    plot_annotation(
        tag_levels = "A"
    ) +
    labs(
        caption = "Figure. Trends in the prevalence of high self-reported health among US adults with diabetes"
    ) +
    theme(
        plot.caption = element_text(hjust = 0.5, size = rel(1.2))
    )


caption <- textGrob(
    "Figure. Trends in the Prevalence of High Self-Reported Health Among US Adults with Diabetes",
    gp = gpar(fontsize = 18), # Change fontsize here
    hjust = 0, # Left-align the text
    x = 0.01 # Adjust x to fine-tune the alignment
)

combined_plot <- gridExtra::grid.arrange(
    p1, p2, p3, p4, p5, p6,
    bottom = caption,
    ncol = 3, nrow = 2
)

# Display the combined figure
print(combined_plot)

