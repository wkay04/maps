library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)

setwd("~/Mayoral Results AD34")

cat("Creating HTML Demographics Dashboard...\n\n")

# Load the correlation data
correlations <- read.csv("outputs/tables/census_election_correlations.csv")
demographic_profiles <- read.csv("outputs/tables/demographic_profiles_by_category.csv")

# Create correlation bar chart for Support
support_plot <- correlations %>%
  mutate(Variable = reorder(Variable, Support.Correlation)) %>%
  ggplot(aes(x = Variable, y = Support.Correlation,
             fill = ifelse(Support.Correlation > 0, "Positive", "Negative"))) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-0.3, 0.3), linetype = "dashed", color = "gray50", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2166ac", "Negative" = "#b2182b"),
                    guide = "none") +
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, 0.2)) +
  labs(title = "Correlation with Zohran Support",
       subtitle = "Which demographics predict higher vote share?",
       x = NULL,
       y = "Correlation Coefficient") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/charts/correlation_support.png", support_plot,
       width = 10, height = 6, dpi = 300, bg = "white")

# Create correlation bar chart for Turnout
turnout_plot <- correlations %>%
  filter(!is.na(Turnout.Correlation)) %>%
  mutate(Variable = reorder(Variable, Turnout.Correlation)) %>%
  ggplot(aes(x = Variable, y = Turnout.Correlation,
             fill = ifelse(Turnout.Correlation > 0, "Positive", "Negative"))) +
  geom_col(alpha = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
  geom_hline(yintercept = c(-0.3, 0.3), linetype = "dashed", color = "gray50", linewidth = 0.3) +
  coord_flip() +
  scale_fill_manual(values = c("Positive" = "#2166ac", "Negative" = "#b2182b"),
                    guide = "none") +
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, 0.2)) +
  labs(title = "Correlation with Voter Turnout",
       subtitle = "Which demographics predict higher turnout?",
       x = NULL,
       y = "Correlation Coefficient") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, color = "gray40"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/charts/correlation_turnout.png", turnout_plot,
       width = 10, height = 6, dpi = 300, bg = "white")

cat("Charts saved.\n\n")

# Create HTML report
html_content <- '
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>AD34 Demographics & Election Analysis Dashboard</title>
    <style>
        * {
            margin: 0;
            padding: 0;
            box-sizing: border-box;
        }

        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            line-height: 1.6;
            color: #333;
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            padding: 20px;
        }

        .container {
            max-width: 1400px;
            margin: 0 auto;
            background: white;
            border-radius: 12px;
            box-shadow: 0 20px 60px rgba(0,0,0,0.3);
            overflow: hidden;
        }

        header {
            background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%);
            color: white;
            padding: 40px;
            text-align: center;
        }

        header h1 {
            font-size: 2.5rem;
            margin-bottom: 10px;
            font-weight: 700;
        }

        header p {
            font-size: 1.2rem;
            opacity: 0.9;
        }

        .content {
            padding: 40px;
        }

        .section {
            margin-bottom: 50px;
        }

        .section h2 {
            font-size: 1.8rem;
            color: #1e3a8a;
            margin-bottom: 20px;
            padding-bottom: 10px;
            border-bottom: 3px solid #3b82f6;
        }

        .insight-box {
            background: #f0f9ff;
            border-left: 5px solid #3b82f6;
            padding: 20px;
            margin: 20px 0;
            border-radius: 8px;
        }

        .insight-box strong {
            color: #1e3a8a;
            font-size: 1.1rem;
        }

        .warning-box {
            background: #fef3c7;
            border-left: 5px solid #f59e0b;
            padding: 20px;
            margin: 20px 0;
            border-radius: 8px;
        }

        table {
            width: 100%;
            border-collapse: collapse;
            margin: 20px 0;
            background: white;
            box-shadow: 0 2px 8px rgba(0,0,0,0.1);
            border-radius: 8px;
            overflow: hidden;
        }

        thead {
            background: linear-gradient(135deg, #1e3a8a 0%, #3b82f6 100%);
            color: white;
        }

        th {
            padding: 15px;
            text-align: left;
            font-weight: 600;
            font-size: 0.95rem;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        td {
            padding: 12px 15px;
            border-bottom: 1px solid #e5e7eb;
        }

        tr:hover {
            background-color: #f9fafb;
        }

        .positive {
            color: #059669;
            font-weight: 600;
        }

        .negative {
            color: #dc2626;
            font-weight: 600;
        }

        .neutral {
            color: #6b7280;
        }

        .strong {
            font-weight: 700;
            font-size: 1.05em;
        }

        .chart-container {
            margin: 30px 0;
            text-align: center;
        }

        .chart-container img {
            max-width: 100%;
            height: auto;
            border-radius: 8px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        }

        .quadrant-grid {
            display: grid;
            grid-template-columns: repeat(2, 1fr);
            gap: 20px;
            margin: 30px 0;
        }

        .quadrant-card {
            padding: 25px;
            border-radius: 8px;
            box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        }

        .quadrant-card h3 {
            margin-bottom: 15px;
            font-size: 1.3rem;
        }

        .quadrant-card ul {
            list-style: none;
            padding-left: 0;
        }

        .quadrant-card li {
            padding: 8px 0;
            border-bottom: 1px solid rgba(0,0,0,0.05);
        }

        .quadrant-card li:last-child {
            border-bottom: none;
        }

        .q1 {
            background: linear-gradient(135deg, #dbeafe 0%, #bfdbfe 100%);
            border-left: 5px solid #2563eb;
        }

        .q2 {
            background: linear-gradient(135deg, #fed7aa 0%, #fdba74 100%);
            border-left: 5px solid #ea580c;
        }

        .q3 {
            background: linear-gradient(135deg, #ddd6fe 0%, #c4b5fd 100%);
            border-left: 5px solid #7c3aed;
        }

        .q4 {
            background: linear-gradient(135deg, #fecaca 0%, #fca5a5 100%);
            border-left: 5px solid #dc2626;
        }

        .stat-label {
            font-size: 0.85rem;
            color: #6b7280;
            text-transform: uppercase;
            letter-spacing: 0.5px;
        }

        .stat-value {
            font-size: 1.4rem;
            font-weight: 700;
            color: #1e3a8a;
        }

        footer {
            background: #f9fafb;
            padding: 30px;
            text-align: center;
            color: #6b7280;
            border-top: 1px solid #e5e7eb;
        }

        .methodology {
            background: #f9fafb;
            padding: 20px;
            border-radius: 8px;
            margin: 20px 0;
            font-size: 0.9rem;
            color: #6b7280;
        }

        @media (max-width: 768px) {
            .quadrant-grid {
                grid-template-columns: 1fr;
            }

            header h1 {
                font-size: 1.8rem;
            }

            .content {
                padding: 20px;
            }
        }
    </style>
</head>
<body>
    <div class="container">
        <header>
            <h1>Assembly District 34</h1>
            <p>Demographics & Election Analysis Dashboard</p>
            <p style="font-size: 0.9rem; opacity: 0.8; margin-top: 10px;">2025 Democratic Mayoral Primary | ACS 2019-2023 Census Data</p>
        </header>

        <div class="content">

            <!-- Key Findings Section -->
            <div class="section">
                <h2>🎯 Key Findings</h2>

                <div class="insight-box">
                    <strong>Education is the #1 predictor of turnout</strong><br>
                    College-educated areas vote at dramatically higher rates (r=+0.78). This is the strongest demographic correlation in the district.
                </div>

                <div class="warning-box">
                    <strong>⚠️ Hispanic Turnout Gap</strong><br>
                    Hispanic neighborhoods show strong support (63%) but significantly lower turnout (r=-0.68). This represents the largest mobilization opportunity.
                </div>

                <div class="insight-box">
                    <strong>Age matters for support</strong><br>
                    Younger areas show higher support for progressive candidates (r=-0.34). Median age is a stronger predictor than race or income.
                </div>
            </div>

            <!-- Correlation Analysis Section -->
            <div class="section">
                <h2>📊 Demographic Correlations</h2>
                <p style="margin-bottom: 20px; color: #6b7280;">How different demographics correlate with Zohran support and voter turnout. Values range from -1 (strong negative) to +1 (strong positive).</p>
'

# Add correlation table manually
corr_table_html <- '<table>
<thead>
    <tr>
        <th>Demographic Factor</th>
        <th>Support Correlation</th>
        <th>Direction</th>
        <th>Turnout Correlation</th>
        <th>Direction</th>
    </tr>
</thead>
<tbody>
'

for (i in 1:nrow(correlations)) {
  row <- correlations[i,]

  # Format correlations
  supp_corr <- ifelse(is.na(row$Support.Correlation), "N/A",
                     sprintf("%.2f", row$Support.Correlation))
  turn_corr <- ifelse(is.na(row$Turnout.Correlation), "N/A",
                     sprintf("%.2f", row$Turnout.Correlation))

  # Add color classes
  supp_class <- ifelse(is.na(row$Support.Correlation), "neutral",
                      ifelse(abs(row$Support.Correlation) > 0.3, "strong",
                            ifelse(row$Support.Correlation > 0, "positive", "negative")))
  turn_class <- ifelse(is.na(row$Turnout.Correlation), "neutral",
                      ifelse(abs(row$Turnout.Correlation) > 0.3, "strong",
                            ifelse(row$Turnout.Correlation > 0, "positive", "negative")))

  corr_table_html <- paste0(corr_table_html, sprintf('
    <tr>
        <td><strong>%s</strong></td>
        <td class="%s" style="text-align: center;">%s</td>
        <td>%s</td>
        <td class="%s" style="text-align: center;">%s</td>
        <td>%s</td>
    </tr>',
    row$Variable,
    supp_class, supp_corr, row$Support.Direction,
    turn_class, turn_corr, row$Turnout.Direction))
}

corr_table_html <- paste0(corr_table_html, '
</tbody>
</table>')

html_content <- paste0(html_content, corr_table_html)

html_content <- paste0(html_content, '

                <div class="chart-container">
                    <img src="../charts/correlation_support.png" alt="Support Correlations">
                </div>

                <div class="chart-container">
                    <img src="../charts/correlation_turnout.png" alt="Turnout Correlations">
                </div>
            </div>

            <!-- Four Quadrants Section -->
            <div class="section">
                <h2>🗺️ Strategic Quadrants</h2>
                <p style="margin-bottom: 30px; color: #6b7280;">Election Districts grouped by support level and turnout, with demographic profiles and strategic recommendations.</p>

                <div class="quadrant-grid">
')

# Add quadrant cards
quadrants <- list(
  list(
    title = "High Support, High Turnout",
    class = "q1",
    eds = 18,
    registered = "27,365",
    strategy = "PROTECT BASE",
    priority = "25% of resources",
    data = demographic_profiles %>%
      filter(classification == "High Support, High Turnout")
  ),
  list(
    title = "High Support, Low Turnout",
    class = "q2",
    eds = 11,
    registered = "12,291",
    strategy = "MOBILIZE - TOP PRIORITY",
    priority = "50% of resources",
    data = demographic_profiles %>%
      filter(classification == "High Support, Low Turnout")
  ),
  list(
    title = "Low Support, High Turnout",
    class = "q3",
    eds = 11,
    registered = "10,736",
    strategy = "PERSUADE",
    priority = "25% of resources",
    data = demographic_profiles %>%
      filter(classification == "Low Support, High Turnout")
  ),
  list(
    title = "Low Support, Low Turnout",
    class = "q4",
    eds = 17,
    registered = "18,589",
    strategy = "LONG-TERM",
    priority = "0% short-term resources",
    data = demographic_profiles %>%
      filter(classification == "Low Support, Low Turnout")
  )
)

for (q in quadrants) {
  if (nrow(q$data) > 0) {
    row <- q$data[1,]

    html_content <- paste0(html_content, sprintf('
                    <div class="quadrant-card %s">
                        <h3>%s</h3>
                        <ul>
                            <li><span class="stat-label">Election Districts:</span> <span class="stat-value">%d</span></li>
                            <li><span class="stat-label">Registered Voters:</span> <span class="stat-value">%s</span></li>
                            <li><span class="stat-label">Strategy:</span> <strong>%s</strong></li>
                            <li><span class="stat-label">Resource Allocation:</span> %s</li>
                            <li style="margin-top: 15px; padding-top: 15px; border-top: 2px solid rgba(0,0,0,0.1);">
                                <div style="margin-bottom: 8px;"><strong>Demographics:</strong></div>
                                <div>%.0f%% Hispanic | %.0f%% White</div>
                                <div>%.0f%% Asian | %.0f%% Black</div>
                                <div>%.0f%% College+ | $%s income</div>
                            </li>
                            <li style="margin-top: 10px;">
                                <div style="margin-bottom: 8px;"><strong>Performance:</strong></div>
                                <div>Turnout: %.0f%% | Support: %.0f%%</div>
                                <div>Total Votes: %s | Zohran: %s</div>
                            </li>
                        </ul>
                    </div>
    ',
    q$class, q$title, q$eds, q$registered, q$strategy, q$priority,
    row$avg_hispanic, row$avg_white, row$avg_asian, row$avg_black,
    row$avg_college, format(round(row$avg_income/1000)*1000, big.mark=","),
    (row$total_votes / row$total_registered) * 100,
    (row$zohran_votes / row$total_votes) * 100,
    format(row$total_votes, big.mark=","),
    format(row$zohran_votes, big.mark=",")))
  }
}

html_content <- paste0(html_content, '
                </div>
            </div>

            <!-- Additional Visualizations Section -->
            <div class="section">
                <h2>📈 Additional Analysis</h2>

                <h3 style="margin-top: 30px; color: #1e3a8a; font-size: 1.4rem;">Demographic Composition</h3>
                <div class="chart-container">
                    <img src="../charts/quadrant_demographics.png" alt="Quadrant Demographics">
                </div>

                <h3 style="margin-top: 40px; color: #1e3a8a; font-size: 1.4rem;">Socioeconomic Indicators</h3>
                <div class="chart-container">
                    <img src="../charts/education_income_comparison.png" alt="Education and Income">
                </div>

                <div class="chart-container">
                    <img src="../charts/foreign_born_comparison.png" alt="Foreign Born Population">
                </div>

                <h3 style="margin-top: 40px; color: #1e3a8a; font-size: 1.4rem;">Turnout Analysis</h3>
                <div class="chart-container">
                    <img src="../charts/turnout_by_quadrant.png" alt="Turnout by Quadrant">
                </div>

                <h3 style="margin-top: 40px; color: #1e3a8a; font-size: 1.4rem;">Key Relationships</h3>
                <div style="display: grid; grid-template-columns: repeat(2, 1fr); gap: 20px; margin: 30px 0;">
                    <div class="chart-container">
                        <img src="../charts/scatter_education_turnout.png" alt="Education vs Turnout">
                    </div>
                    <div class="chart-container">
                        <img src="../charts/scatter_hispanic_turnout.png" alt="Hispanic vs Turnout">
                    </div>
                </div>
            </div>

            <!-- Methodology Section -->
            <div class="section">
                <h2>📋 Methodology</h2>
                <div class="methodology">
                    <p><strong>Data Sources:</strong></p>
                    <ul>
                        <li>Election results: NYC Board of Elections, 2025 Democratic Mayoral Primary</li>
                        <li>Census data: American Community Survey 2019-2023 5-year estimates (Census Tract level)</li>
                        <li>Voter registration: NYC Board of Elections, November 2025</li>
                    </ul>
                    <br>
                    <p><strong>Analysis Method:</strong></p>
                    <ul>
                        <li>Census tract data spatially joined to Election Districts using area-weighted averages</li>
                        <li>Correlations calculated using Pearson coefficient across 57 Election Districts with complete data</li>
                        <li>Quadrants defined by median splits: 55.2% support threshold, 41.7% turnout threshold</li>
                        <li>Support calculated as Zohran vote share across all ballot lines (Democratic + Working Families)</li>
                        <li>Turnout calculated as votes cast / active registered voters</li>
                        <li>All foreign-born data successfully integrated after switching from block group to tract-level geography</li>
                    </ul>
                </div>
            </div>

        </div>

        <footer>
            <p><strong>Assembly District 34 - Path to Victory Analysis</strong></p>
            <p>Generated: November 20, 2025</p>
            <p style="margin-top: 10px; font-size: 0.85rem;">Queens, NY | Jackson Heights, Corona, East Elmhurst, Astoria</p>
        </footer>
    </div>
</body>
</html>
')

# Write HTML file
dir.create("outputs/reports", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/charts", showWarnings = FALSE, recursive = TRUE)

writeLines(html_content, "outputs/reports/demographics_dashboard.html")

cat("\n✓ HTML Dashboard created: outputs/reports/demographics_dashboard.html\n")
cat("  (Open in web browser to view)\n\n")
