# Figure Title: "Reasoning Patterns by Demographics and Decision Context (DWS vs. FIS)"
# 
# |-----------------------------|------------------------------|
#   |       DWS Reasoning         |       FIS Reasoning          |
#   |-----------------------------|------------------------------|
#   | Age                         | Age                          |
#   | Employment                  | Employment                   |
#   | Region                      | Region                       |
#   | Education                   | Education                    |
#   | Income                      | Income                       |
#   | Climate Science Literacy    | Climate Science Literacy     |
#   | Interpretation              | Interpretation               |
#   | Political Preference        | Political Preference         |
#   | Race                        | Race                         |
#   | Ethnicity                   | Ethnicity                    |
#   | Gender                      | Gender                       |
  
install.packages("patchwork")
library(ggplot2)
library(reshape2)
library(gridExtra)

reasons <- c("Risk Assessment", "Preparedness", "Judgment", "No Reason", "Economics", "Interpretation")
groups <- c("18–34", "35–54", "55+")

# Generate dummy percentage values
set.seed(123)
dummy_matrix <- matrix(runif(18, min=0, max=50), nrow=6, ncol=3, dimnames=list(reasons, groups))
dummy_df <- melt(dummy_matrix)
colnames(dummy_df) <- c("Reason", "Group", "Percentage")

plot_heatmap <- function(data, title_text) {
  ggplot(data, aes(x = Group, y = Reason, fill = Percentage)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(colours = c("#440154", "#31688e", "#35b779", "#fde725"),
                         limits = c(0, 50), name = "%") +
    theme_minimal(base_size = 8) +
    labs(title = title_text, x = NULL, y = NULL) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}

ggplot(data, aes(x = Group, y = Reason, fill = Percentage)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colours = c("#440154", "#31688e", "#35b779", "#fde725"),
                       limits = c(0, 50), name = "%") +
  theme_minimal(base_size = 8) +
  labs(title = title_text, x = NULL, y = NULL) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "right"
  )

# Simulate 6 panels (3 DWS, 3 FIS) using same dummy data
p_age_dws <- plot_heatmap(dummy_df, "Age: DWS")
p_age_fis <- plot_heatmap(dummy_df, "Age: FIS")
p_income_dws <- plot_heatmap(dummy_df, "Income: DWS")
p_income_fis <- plot_heatmap(dummy_df, "Income: FIS")
p_edu_dws <- plot_heatmap(dummy_df, "Education: DWS")
p_edu_fis <- plot_heatmap(dummy_df, "Education: FIS")

grid.arrange(
  p_age_dws,    p_age_fis,
  p_income_dws, p_income_fis,
  p_edu_dws,    p_edu_fis,
  ncol = 2,
  top = "Mock Layout: Reasoning by Demographic and Decision Context (DWS vs FIS)"
)

ggplot(long_data, aes(x = Group, y = Reasoning, fill = Percent)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "purple") +
  labs(title = "Reasoning by Demographic Group and Context (DWS/FIS)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
library(dplyr)

# Example reasoning types and groups
reasoning <- c("Risk", "Preparedness", "Judgment", "No Reason", "Economics", "Interpretation")
demographic_groups <- c("18-34 DWS", "18-34 FIS", "35-54 DWS", "35-54 FIS", "55+ DWS", "55+ FIS")

# Simulate percentages
set.seed(42)
data <- expand.grid(Reason = reasoning, Group = demographic_groups)
data$Percent <- runif(nrow(data), min = 0, max = 50)

ggplot(data, aes(x = Group, y = Reason, fill = Percent)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c("#440154", "#31688e", "#35b779", "#fde725"),
    limits = c(0, 50),
    name = "% of participants"
  ) +
  labs(
    title = "Reasoning Patterns by Demographic Subgroup and Decision Context",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )


install.packages("fmsb")
library(fmsb)

# Dummy reasoning percentages for one group
data <- data.frame(
  Risk = 10,
  Preparedness = 15,
  Judgment = 25,
  No_Reason = 5,
  Economics = 30,
  Interpretation = 15
)

# fmsb requires adding min and max rows
data <- rbind(
  max = rep(50, 6),
  min = rep(0, 6),
  Group = data
)

radarchart(data,
           axistype = 1,
           pcol = "blue", pfcol = scales::alpha("blue", 0.3),
           plwd = 2,
           cglcol = "grey", cglty = 1, axislabcol = "black",
           vlcex = 0.8,
           title = "Reasoning Distribution: Group A")


# Load libraries
library(ggplot2)
library(dplyr)

# -----------------------------
# STEP 1: CREATE YOUR DATA FRAME
# -----------------------------
# Define reasoning types
reasoning_types <- c("Risk", "Preparedness", "Judgment", "No Reason", "Economics", "Interpretation")

# Define subgroups for DWS and FIS (same structure)
demog_subgroups_dws <- c(
  "18–34 (Age)", "35–54 (Age)", "55+ (Age)",
  "Employed (Employment)", "Unemployed (Employment)",
  "Northeast (Region)", "Midwest (Region)", "South (Region)", "West (Region)",
  "HS or less (Education)", "Some college (Education)", "BA+ (Education)", "Grad+ (Education)",
  "<$35k (Income)", "$35–74k (Income)", "$75–149k (Income)", "$150k+ (Income)",
  "Low (Literacy)", "High (Literacy)",
  "Low (Interpretation)", "High (Interpretation)",
  "Democrat (Political ID)", "Independent (Political ID)", "Republican (Political ID)", "Other (Political ID)",
  "White (Race)", "Black (Race)", "Asian (Race)", "Other (Race)",
  "Hispanic (Ethnicity)", "Non-Hispanic (Ethnicity)",
  "Male (Gender)", "Female (Gender)", "Other (Gender)"
)

# Expand grid and simulate dummy data
set.seed(42)
dws_data <- expand.grid(Reason = reasoning_types, Group = demog_subgroups_dws)
dws_data$Percent <- runif(nrow(dws_data), min = 0, max = 50)

# Clone structure for FIS
fis_data <- dws_data
fis_data$Percent <- runif(nrow(fis_data), min = 0, max = 50)


plot_reasoning_heatmap <- function(data, title_text) {
  ggplot(data, aes(x = Group, y = Reason, fill = Percent)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colours = c("#440154", "#31688e", "#35b779", "#fde725"),
      limits = c(0, 50),
      name = "% of participants"
    ) +
    labs(
      title = title_text,
      x = NULL,
      y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm")
    )
}

# DWS Heatmap
plot_dws <- plot_reasoning_heatmap(dws_data, "Reasoning by Demographic Group – DWS")

# FIS Heatmap
plot_fis <- plot_reasoning_heatmap(fis_data, "Reasoning by Demographic Group – FIS")

# Show them in RStudio
print(plot_dws)
print(plot_fis)

# Save as PNGs (adjust size for your report layout)
ggsave("DWS_heatmap.png", plot_dws, width = 10, height = 6, dpi = 300)
ggsave("FIS_heatmap.png", plot_fis, width = 10, height = 6, dpi = 300)


# -------

# Original list
group_info <- data.frame(
  Subgroup = c(
    "18–34", "35–54", "55+",
    "Employed", "Unemployed",
    "Northeast", "Midwest", "South", "West",
    "HS or less", "Some college", "BA+", "Grad+",
    "<$35k", "$35–74k", "$75–149k", "$150k+",
    "Low", "High", "Low", "High",
    "Democrat", "Independent", "Republican", "Other",
    "White", "Black", "Asian", "Other",
    "Hispanic", "Non-Hispanic",
    "Male", "Female", "Other"
  ),
  Demographic = c(
    rep("Age", 3),
    rep("Employment", 2),
    rep("Region", 4),
    rep("Education", 4),
    rep("Income", 4),
    rep("Literacy", 2),
    rep("Interpretation", 2),
    rep("Political ID", 4),
    rep("Race", 4),
    rep("Ethnicity", 2),
    rep("Gender", 3)
  )
)

# Combine for DWS data
reasoning_types <- c("Risk", "Preparedness", "Judgment", "No Reason", "Economics", "Interpretation")

dws_data <- expand.grid(Reason = reasoning_types, ID = 1:nrow(group_info))
dws_data <- cbind(dws_data, group_info[dws_data$ID, ])
dws_data$Percent <- runif(nrow(dws_data), 0, 50)

# Create a unique group ID for plotting
dws_data$Group <- paste(dws_data$Demographic, dws_data$Subgroup, sep = "\n")

# Create a factor with blank labels between demographic categories
demographics_order <- unique(group_info$Demographic)

# Build grouped x-axis factor with spacers
group_labels <- unlist(lapply(demographics_order, function(demo) {
  subgroups <- group_info$Subgroup[group_info$Demographic == demo]
  labels <- paste(demo, subgroups, sep = "\n")
  c(labels, " ")  # Add a spacer after each demographic
}))

# Remove last spacer
group_labels <- group_labels[-length(group_labels)]

# Apply to data
dws_data$Group <- factor(dws_data$Group, levels = group_labels)

plot_reasoning_heatmap <- function(data, title_text) {
  ggplot(data, aes(y = Group, x = Reason, fill = Percent)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colours = c("#440154", "#31688e", "#35b779", "#fde725"),
      limits = c(0, 50),
      name = "% of participants"
    ) +
    labs(title = title_text, x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8, lineheight = 0.9),
      axis.text.y = element_text(size = 9),
      axis.ticks.x = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom",
      panel.grid.major = element_blank()
    )
}

plot_dws <- plot_reasoning_heatmap(dws_data, "Reasoning by Demographic Group – DWS")
print(plot_dws)

# ---------

install.packages("ggtext")
library(ggplot2)
library(dplyr)
library(ggtext)  # For rich/bold text in axis labels

# Step 1: Create grouped demographic structure
group_info <- data.frame(
  Group = c(
    rep("Age", 3),
    rep("Employment", 2),
    rep("Region", 4),
    rep("Education", 4),
    rep("Income", 4),
    rep("Literacy", 2),
    rep("Interpretation", 2),
    rep("Political ID", 3),
    rep("Race", 4),
    rep("Ethnicity", 2),
    rep("Gender", 3)
  ),
  Demographic = c(
    "Age", rep("", 2),
    "Employment", "",
    "Region", rep("", 3),
    "Education", rep("", 3),
    "Income", rep("", 3),
    "Literacy", "",
    "Interpretation", "",
    "Political ID", rep("", 2),
    "Race", rep("", 3),
    "Ethnicity", "",
    "Gender", rep("", 2)
  ),
  Subgroup = c(
    "18–34", "35–54", "55+",
    "Employed", "Unemployed",
    "Northeast", "Midwest", "South", "West",
    "HS or less", "Some college", "BA+", "Grad+",
    "<$35k", "$35–74k", "$75–149k", "$150k+",
    "Low Climate", "High Climate",
    "Low", "High",
    "Democrat", "Independent", "Republican", 
    "White", "Black", "Asian", "Other",
    "Hispanic", "Non-Hispanic",
    "Male", "Female", "Other Gender"
  )
)

# Add composite label (Subgroup + Group) for use in plotting
group_info <- group_info %>%
#  mutate(Label = paste0(Subgroup:, "\n<b>", Demographic, "</b>"))
mutate(Label = paste0("<b>", Demographic, "</b>\n", Subgroup))

# Create reasoning types
reasoning_types <- c("Risk", "Preparedness", "Judgment", "No Reason", "Economics", "Interpretation")

# Expand for full heatmap structure
set.seed(42)
dws_data <- expand.grid(
  Reason = reasoning_types,
  Label = group_info$Label
)

# Merge back the original subgroup info
dws_data <- dws_data %>%
  left_join(group_info, by = "Label")

# Simulated percentages
dws_data$Percent <- runif(nrow(dws_data), 0, 50)

# Factor the labels in order of appearance
dws_data$Label <- factor(dws_data$Label, levels = rev(group_info$Label))
# dws_data$Label <- factor(dws_data$Label, levels = rev(group_info$Label))

# Plot
ggplot(dws_data, aes(x = Reason, y = Label, fill = Percent)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(
    colours = c("#440154", "#31688e", "#35b779", "#fde725"),
    limits = c(0, 50),
    name = "% of participants"
  ) +
  scale_y_discrete(labels = function(x) x) +  # allow HTML-style labels
  labs(
    title = "Reasoning by Demographic Group – DWS",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_markdown(hjust = 1, size = 8),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_blank()
  )

ggsave("DWS_rotated_heatmap.png", width = 8, height = 10, dpi = 300)

# -----

install.packages(c("ggplot2", "dplyr", "ggtext"))
library(ggplot2)
library(dplyr)
library(ggtext)

# Define demographics and subgroups
group_info <- data.frame(
  Demographic = c(
    rep("Age", 3),
    rep("Employment", 2),
    rep("Region", 4),
    rep("Education", 4),
    rep("Income", 4),
    rep("Literacy", 2),
    rep("Interpretation", 2),
    rep("Political ID", 4),
    rep("Race", 4),
    rep("Ethnicity", 2),
    rep("Gender", 3)
  ),
  Subgroup = c(
    "18–34", "35–54", "55+",
    "Employed", "Unemployed",
    "Northeast", "Midwest", "South", "West",
    "HS or less", "Some college", "BA+", "Grad+",
    "<$35k", "$35–74k", "$75–149k", "$150k+",
    "Low", "High",
    "Low", "High",
    "Democrat", "Independent", "Republican", "Other",
    "White", "Black", "Asian", "Other",
    "Hispanic", "Non-Hispanic",
    "Male", "Female", "Other"
  )
)

# Create row ID to control order and make dividers
group_info <- group_info %>%
  mutate(
    Row = row_number(),
    Group_Label = ifelse(!duplicated(Demographic), paste0("**", Demographic, "**"), ""),
    Display_Label = paste0(Subgroup)
  )

# Reasoning types
reasoning_types <- c("Risk", "Preparedness", "Judgment", "No Reason", "Economics", "Interpretation")

# Create full heatmap data
dws_data <- expand.grid(
  Reason = reasoning_types,
  Row = group_info$Row
)

# Merge in demographic info
dws_data <- left_join(dws_data, group_info, by = "Row")

# Add fake percentage data
set.seed(42)
dws_data$Percent <- runif(nrow(dws_data), 0, 50)

# Set the factor levels for proper order
dws_data$Display_Label <- factor(dws_data$Display_Label, levels = rev(group_info$Display_Label))
dws_data$Group_Label <- factor(dws_data$Group_Label, levels = rev(unique(group_info$Group_Label)))

# Determine where to draw dividers (between demographic blocks)
divider_rows <- dws_data %>%
  group_by(Demographic) %>%
  summarise(min_row = min(Row)) %>%
  arrange(min_row) %>%
  pull(min_row)

# Remove first (no line above first group)
divider_positions <- rev(divider_rows[-1] - 0.5)

# Build the heatmap
ggplot(dws_data, aes(x = Reason, y = Row, fill = Percent)) +
  geom_tile(color = "white") +
  scale_y_continuous(
    breaks = dws_data$Row,
    labels = dws_data$Display_Label,
    expand = c(0, 0)
  ) +
  scale_fill_gradientn(
    colours = c("#440154", "#31688e", "#35b779", "#fde725"),
    limits = c(0, 50),
    name = "% of participants"
  ) +
  labs(
    title = "Reasoning by Demographic Group – DWS",
    x = NULL, y = NULL
  ) +
  geom_hline(yintercept = divider_positions, color = "gray50", size = 0.4) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y = element_text(hjust = 0, size = 8),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  ) +
  # Add the group labels on left as rich text
  geom_richtext(
    data = group_info %>% filter(Group_Label != ""),
    aes(x = 0.1, y = Row, label = Group_Label),
    inherit.aes = FALSE,
    hjust = 1,
    fill = NA, label.color = NA,
    size = 3.2
  )
    
ggsave("DWS_rotated_clean.png", width = 10, height = 10, dpi = 300)
    
# -----
# Copy structure of DWS for FIS
fis_data <- dws_data
fis_data$Percent <- runif(nrow(fis_data), 0, 50)  # Replace with real FIS data
fis_data$Condition <- "FIS"

dws_data$Condition <- "DWS"

# Combine both into one data frame
combined_data <- bind_rows(dws_data, fis_data)

# Ensure 'Row' is correctly ordered and consistent
combined_data$Row <- factor(combined_data$Row, levels = unique(combined_data$Row))
combined_data$Reason <- factor(combined_data$Reason, levels = reasoning_types)
combined_data$Condition <- factor(combined_data$Condition, levels = c("DWS", "FIS"))

library(ggtext)

ggplot(combined_data, aes(x = Reason, y = Row, fill = Percent)) +
  geom_tile(color = "white") +
  facet_wrap(~Condition, nrow = 1) +
  scale_fill_gradientn(
    colours = c("#440154", "#31688e", "#35b779", "#fde725"),
    limits = c(0, 50),
    name = "% of participants"
  ) +
  scale_y_discrete(
    breaks = combined_data$Row,
    labels = rev(combined_data$Display_Label),
    expand = c(0, 0)
  ) +
  geom_hline(yintercept = divider_positions, color = "gray50", size = 0.3) +
  labs(
    title = "Reasoning by Demographic Group and Decision Context",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y = element_text(hjust = 0, size = 8),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid = element_blank()
  ) +
  # Add group labels on left using geom_richtext
  geom_richtext(
    data = group_info %>% filter(Group_Label != ""),
    aes(x = 0.5, y = Row, label = Group_Label),
    inherit.aes = FALSE,
    hjust = 1,
    fill = NA, label.color = NA,
    size = 3.2
  )

# -------------------- DEMOGRAPHIC GROUP

library(ggplot2)
library(dplyr)
library(ggtext)  # For rich/bold text in axis labels
library(scales)

# Step 1: Create grouped demographic structure
group_info <- data.frame(
  Group = c(
    rep("Age", 3),
    rep("Employment", 7),
    rep("Region", 4),
    rep("Education", 6),
    rep("Income", 6),
    rep("Literacy", 3),
    rep("Interpretation", 3),
    rep("Political", 3),
    rep("Race", 4),
    rep("Ethnicity", 2),
    rep("Gender", 2)
  ),
  Demographic = c(
    "Age", rep("", 2),
    "Employment", rep("", 6),
    "Region", rep("", 3),
    "Education", rep("", 5),
    "Income", rep("", 5),
    "Climate literacy", rep("", 2),
    "Interpretation", rep("", 2),
    "Political", rep("", 2),
    "Race", rep("", 3),
    "Ethnicity", "",
    "Gender", ""
  ),
  Subgroup = c(
    "18–34", "35–54", "55+",
    "Other", "Retired", "Full-time", "Part-time", "Homemaker", "Student", "Unemployed",
    "South", "Midwest", "Northeast", "West",
    "Postgrad", "BA", "Associates", "Some college", "HS or GED", "Some HS or less",  
    "<$25k", "$25–49.9k", "$50-74.9k", "$75–99.9k", "100-149.9k", "$150k+",
    "Lower", "Moderate lit.", "Higher",
    "Below average", "Average", "Above average",
    "Conservative", "Moderate", "Liberal", 
    "White", "Black", "Asian", "Native American",
    "Non-Hispanic", "Hispanic",
    "Male", "Female"
  )
)

# Add composite label (Subgroup + Group) for use in plotting
group_info <- group_info %>%
  #  mutate(Label = paste0(Subgroup:, "\n<b>", Demographic, "</b>"))
  mutate(Label = paste0("<b>", Demographic, "</b>\n", Subgroup))
  # mutate(Label = Subgroup)

# Create reasoning types
# reasoning_types <- c("Flood risk", "Preparedness", "Personal judgment", 
#                      "No Reason", "Economics", "Data interp.")
reasoning_types <- c("Economics", "Flood risk", "Data interp.", "Personal\njudgment", 
                     "Preparedness", "No Reason")

# Expand for full heatmap structure
dws_data <- expand.grid(
  Reason = reasoning_types,
  Label = group_info$Label
)

# Merge back the original subgroup info
dws_data <- dws_data %>%
  left_join(group_info, by = "Label")

# Add percentages
dws_data$Percent = c(age_dws$value,
                     work_dws$value,
                     region_dws$value,
                     edu_dws$value[-grep("Prefer", edu_dws$variable)],
                     income_dws$value,
                     clim_dws$value,
                     int_dws$value,
                     pol_dws$value,
                     race_dws$value[grep("white|black|asian|indian", race_dws$variable)],
                     lat_dws$value,
                     gen_dws$value[grep("Male|Female", gen_dws$variable)])

# Factor the labels in order of appearance
dws_data$Label <- factor(dws_data$Label, levels = rev(group_info$Label))
# dws_data$Label <- factor(dws_data$Label, levels = rev(group_info$Label))
dws_data$txtcol = ifelse(dws_data$Percent < 35, "white", "black")

# Plot
ttplot = ggplot(dws_data, aes(x = Reason, y = Label, fill = Percent)) +
  geom_tile(color = "gray") +
  scale_fill_viridis_c(
    option = "inferno",
    limits = c(0, 50),
    name = "% of participants", oob=squish
  ) +
  geom_text(aes(label = round(Percent)), color = dws_data$txtcol, size = 2) +
  scale_y_discrete(labels = function(x) x) +  # allow HTML-style labels
  labs(
    title = NULL, #"Driveway Washout Scenario",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_markdown(hjust = 1, size = 7),
    axis.text.x = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    # legend.position = c(0,0),
    legend.position = "top",
    legend.title = element_text(size = 8),
    legend.box.spacing = unit(-0.2, 'cm'),
    legend.key.height = unit(0.4,"line"),
    legend.key.width = unit(1, 'cm'),
    # legend.margin=margin(0,170,100,0),
    panel.grid.major = element_blank()
  )

pdf(file="paper2/fig_DWS_reasoning.pdf", family="Helvetica", width=maximum_width, height=column_height*3)
facet(ttplot, facet.by = "Group", scales = "free_y", ncol=1, strip.position="left") +
  theme(strip.text=element_blank())# 
dev.off()

# Expand for full heatmap structure
fis_data <- expand.grid(
  Reason = reasoning_types,
  Label = group_info$Label
)

# Merge back the original subgroup info
fis_data <- fis_data %>%
  left_join(group_info, by = "Label")

# Add percentages
fis_data$Percent = c(age_fis$value,
                     work_fis$value,
                     region_fis$value,
                     edu_fis$value[-grep("Prefer", edu_fis$variable)],
                     income_fis$value,
                     clim_fis$value,
                     int_fis$value,
                     pol_fis$value,
                     race_fis$value[grep("white|black|asian|indian", race_fis$variable)],
                     lat_fis$value,
                     gen_fis$value[grep("Male|Female", gen_fis$variable)])

# Factor the labels in order of appearance
fis_data$Label <- factor(fis_data$Label, levels = rev(group_info$Label))
# dws_data$Label <- factor(dws_data$Label, levels = rev(group_info$Label))
fis_data$txtcol = ifelse(fis_data$Percent < 35, "white", "black")

# Plot
fisplot = ggplot(fis_data, aes(x = Reason, y = Label, fill = Percent)) +
  geom_tile(color = "gray") +
  scale_fill_viridis_c(
    option = "inferno",
    limits = c(0, 50),
    name = "% of participants", oob=squish
  ) +
  geom_text(aes(label = round(Percent)), color = fis_data$txtcol, size = 2) +
  scale_y_discrete(labels = function(x) x) +  # allow HTML-style labels
  labs(
    title = NULL, #"Flood insurance Scenario",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_markdown(hjust = 1, size = 7),
    axis.text.x = element_text(size = 9),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    # legend.position = c(0,0),
    legend.position = "top",
    legend.title = element_text(size = 8),
    legend.box.spacing = unit(-0.2, 'cm'),
    legend.key.height = unit(0.4,"line"),
    legend.key.width = unit(1, 'cm'),
    # legend.margin=margin(0,170,100,0),
    panel.grid.major = element_blank()
  )

pdf(file="paper2/fig_FIS_reasoning.pdf", family="Helvetica", width=maximum_width, height=column_height*3)
facet(fisplot, facet.by = "Group", scales = "free_y", ncol=1, strip.position="left") +
  theme(strip.text=element_blank())# 
dev.off()

