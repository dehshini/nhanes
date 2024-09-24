# Load required libraries
library(tidyverse)
library(patchwork)
library(ggthemes)

theme_panel <- theme_clean() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    theme(legend.title = element_text(size = 10)) +
    theme(legend.text = element_text(size = 10)) +
    theme(legend.key.size = unit(0.5, "cm")) +
    theme(legend.key.height = unit(0.2, "cm"))


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
        x = "NHANES Cycle",
        y = "Proportion (%)",
        title = "Overall"
    ) +
    theme_panel +
    annotate(
        geom = "text",
        x = 3,
        y = 18,
        label = paste("P trend", round(p_trend.2[[2, 5]], 3)),
        size = 5
    )

p1


# P2 - by Age

p2 <- ggplot(
    data = ageplot.2,
    aes(x = cycle, y = Proportion, color = Age)
) +
    geom_line(
        aes(group = Age, linetype = Age),
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
            color = Age,
            linetype = Age
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "by Age Group",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Age Group",
        linetype = "Age Group",
        shape = "Age Group",
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        breaks = seq(0, 100, 20),
        limits = c(0, 100)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = age_ptrend.2) +
    scale_linetype_discrete(labels = age_ptrend.2) +
    scale_shape_discrete(labels = age_ptrend.2) +
    theme(
        legend.position = c(0.5, 0.2) # Position inside the plot area
    )

p2



# P3 - by Race

p3 <- ggplot(
        data = raceplot.2,
    aes(x = cycle, y = Proportion, color = Ethnicity)
) +
    geom_line(
        aes(group = Ethnicity, linetype = Ethnicity),
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
            ymax = Upper_CI,
            color = Ethnicity,
            linetype = Ethnicity
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "by Race/Ethnicity",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Ethnicity",
        linetype = "Ethnicity",
        shape = "Ethnicity"
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        breaks = seq(0, 100, 20),
        limits = c(0, 100)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = ptrend_race.2) +
    scale_linetype_discrete(labels = ptrend_race.2) +
    scale_shape_discrete(labels = ptrend_race.2) +
    theme(
        legend.position = c(0.5, 0.2) # Position inside the plot area
    )

p3



# P4 - by Income

p4 <- ggplot(
    data = incomeplot.2,
    aes(x = cycle, y = Proportion, color = Income)
) +
    geom_line(
        aes(group = Income, linetype = Income),
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
            color = Income,
            linetype = Income
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "by Family Income",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Family Income",
        linetype = "Family Income",
        shape = "Family Income"
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = income_ptrend.2) +
    scale_linetype_discrete(labels = income_ptrend.2) +
    scale_shape_discrete(labels = income_ptrend.2) +
    theme(
        legend.position = c(0.5, 0.15) # Position inside the plot area
    )

p4


# P5 - by Education
p5 <- ggplot(
        data = educationplot.2,
    aes(x = cycle, y = Proportion, color = Education)
) +
    geom_line(
        aes(group = Education, linetype = Education),
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
            color = Education,
            linetype = Education
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "by Education",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Education",
        linetype = "Education",
        shape = "Education"
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = ptrend_education.2) +
    scale_linetype_discrete(labels = ptrend_education.2) +
    scale_shape_discrete(labels = ptrend_education.2) +
    theme(
        legend.position = c(0.5, 0.15) # Position inside the plot area
    )

p5


# P6 - by Insurance
p6 <- ggplot(
        data = insuranceplot.2,
    aes(x = cycle, y = Proportion, color = Insurance)
) +
    geom_line(
        aes(group = Insurance, linetype = Insurance),
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
            color = Insurance,
            linetype = Insurance
        ),
        width = 0.3,
        position = position_dodge(width = 0.2)
    ) +
    labs(
        title = "by Insurance",
        x = "NHANES Cycle",
        y = "Proportion (%)",
        color = "Insurance",
        linetype = "Insurance",
        shape = "Insurance"
    ) +
    theme_panel +
    scale_x_discrete(label = xlabels.2) +
    scale_y_continuous(
        limits = c(0, 100),
        breaks = seq(0, 100, 20)
    ) +
    scale_color_brewer(palette = "Dark2") +
    scale_color_discrete(labels = insurance_ptrend.2) +
    scale_linetype_discrete(labels = insurance_ptrend.2) +
    scale_shape_discrete(labels = insurance_ptrend.2) +
    theme(
        legend.position = c(0.5, 0.15) # Position inside the plot area
    )

p6



# Combine the six plots
combined_plot <- (p1 + p2 + p3) / (p4 + p5 + p6) +
    plot_annotation(
        title = "Trend of high SRH Among US Adults with Self-Reported Diabetes",
        theme = theme(plot.title = element_text(hjust = 0.5)),
        tag_levels = "A"
    ) +
    plot_layout(axes = "collect", guides = "keep")

combined_plot + plot_layout(axes = "collect", guides = "keep")

# Display the combined figure
print(combined_plot)

