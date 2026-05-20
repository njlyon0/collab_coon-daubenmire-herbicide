
# Template scatterplots (with CIs) ####

ggplot(cgr.csg.pltdf, aes(x = Year, y = CSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = CSG + ci, ymin = CSG - ci), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = cgr.colors) +
  labs(x = "Year", y = "Cool-Season Grass (%)") +
  pref.theme + theme(legend.position = c(0.7, 0.9))

ggplot(ugr.csg.pltdf, aes(x = Year, y = CSG, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = CSG + ci, ymin = CSG - ci), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F) +
  geom_point(position = dodge, size = 2.5) +
  scale_color_manual(values = ugr.colors) +
  labs(x = "Year", y = "Cool-Season Grass (%)") +
  pref.theme + theme(legend.position = c(0.7, 0.9))

# Template boxplots ####
ggplot(cgr, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = cgr.colors) +
  geom_text(label = "a", x = 0.9, y = 21) +
  geom_text(label = "ab", x = 1.8, y = 30) +
  geom_text(label = "b", x = 2.9, y = 34) +
  labs(x = "Post-Treatment", y = "Cool-Season Grass (%)") +
  pref.theme

ggplot(ugr, aes(x = Herbicide.Treatment, y = CSG, fill = Herbicide.Treatment)) +
  geom_boxplot(outlier.shape = 21) +
  scale_fill_manual(values = ugr.colors) +
  geom_text(label = "a", x = 0.9, y = 63) +
  geom_text(label = "ab", x = 1.8, y = 44) +
  geom_text(label = "b", x = 2.9, y = 35) +
  labs(x = "Post-Treatment", y = "Cool-Season Grass (%)") +
  pref.theme

# Template process (full but need to find/replace "fsc" and "Fescue") ####
# Get summary stats for plotting
cgr.fsc.pltdf <- summarySE(data = cgr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))
ugr.fsc.pltdf <- summarySE(data = ugr, measurevar = "Fescue", groupvars = c("Year", "Herbicide.Treatment"))

# Plots
cgr.fsc.plt <- ggplot(cgr.fsc.pltdf, aes(x = Year, y = Fescue, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "NS", x = 13.8, y = 45, color = "black") +
  scale_color_manual(values = cgr.colors) +
  #ylim(0, 75) +
  labs(x = "Year", y = "Fescue (%)", title = "Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); cgr.fsc.plt

ugr.fsc.plt <- ggplot(ugr.fsc.pltdf, aes(x = Year, y = Fescue, color = Herbicide.Treatment, shape = Herbicide.Treatment)) +
  geom_errorbar(aes(ymax = Fescue + se, ymin = Fescue - se), width = 0.3, position = dodge) +
  geom_smooth(aes(linetype = Herbicide.Treatment), method = 'lm', se = F, position = dodge) +
  geom_point(position = dodge, size = 2.5) +
  #geom_text(label = "NS", x = 13.8, y = 45, color = "black") +
  scale_color_manual(values = ugr.colors) +
  #ylim(0, 45) +
  labs(x = "Year", y = "Fescue (%)", title = "Un-Grazed") +
  scale_x_continuous(breaks = c(14, 18)) +
  pref.theme + theme(legend.position = c(0.7, 0.9)); ugr.fsc.plt

# Save these plots
plot_grid(cgr.fsc.plt, ugr.fsc.plt, labels = c("I", "II"), nrow = 1, ncol = 2)
ggplot2::ggsave("./Figures/14-vs-18 Figures/Fescue.pdf", width = 8,
                height = 6,units = 'in', plot = last_plot())






