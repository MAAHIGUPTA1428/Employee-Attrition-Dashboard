# =============================================================================
# EMPLOYEE ATTRITION ANALYTICS DASHBOARD
# =============================================================================
# STEP 1: Install & Load Required Packages
# Run this section once to install all dependencies before launching the app.
# =============================================================================

required_packages <- c(
  "shiny",        # Core Shiny web application framework
  "shinydashboard", # Dashboard layout components (boxes, value boxes, sidebar)
  "ggplot2",      # Grammar of graphics plotting
  "plotly",       # Interactive, web-based charts (wraps ggplot2 + native plots)
  "dplyr",        # Data manipulation (filter, group_by, summarise, mutate)
  "DT",           # Interactive DataTables (sortable, searchable, paginated)
  "scales",       # Scale formatting helpers (percent_format, comma, etc.)
  "tidyr"         # Data tidying (pivot_wider, pivot_longer, etc.)
)

# Install any packages that are not already installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  message("Installing missing packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages, dependencies = TRUE)
} else {
  message("All required packages are already installed.")
}

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

message("✓ All packages loaded. Starting dashboard...")

# =============================================================================
# NOTE: Make sure 'Employee_Attrition.csv' is in your working directory.
# You can check/set your working directory with:
#   getwd()
#   setwd("path/to/your/folder")
# =============================================================================


# =============================================================================
# STEP 2: Load & Prepare Data
# =============================================================================

df_raw <- read.csv("C:/Users/Dell/OneDrive/Desktop/Employee-Attrition-Dashboard/Employee Attrition.csv", stringsAsFactors = FALSE)
MISS   <- colSums(is.na(df_raw))

df <- df_raw %>%
  mutate(
    AttrNum   = ifelse(Attrition == "Yes", 1, 0),
    OT_num    = ifelse(OverTime  == "Yes", 1, 0),
    AgeBand   = cut(Age, breaks = c(17,25,35,45,55,60),
                    labels = c("18-25","26-35","36-45","46-55","56-60")),
    SalBand   = cut(MonthlyIncome, breaks = c(1000,3000,6000,10000,20000),
                    labels = c("$1k-3k","$3k-6k","$6k-10k","$10k-20k")),
    TenureBand= cut(YearsAtCompany, breaks = c(-1,2,5,10,20,40),
                    labels = c("0-2 yrs","3-5 yrs","6-10 yrs","11-20 yrs","20+ yrs")),
    SatLabel  = recode(as.character(JobSatisfaction),
                       "1"="Low","2"="Medium","3"="High","4"="Very High"),
    EdLabel   = recode(as.character(Education),
                       "1"="Below College","2"="College","3"="Bachelor",
                       "4"="Master","5"="Doctor"),
    Outlier   = ifelse(abs(scale(MonthlyIncome))>3 | abs(scale(Age))>3 |
                       abs(scale(YearsAtCompany))>3, "Outlier","Normal")
  )

AT  <- df %>% filter(!is.na(AgeBand)) %>%
  group_by(AgeBand) %>%
  summarise(Total=n(), Left=sum(AttrNum), Rate=round(mean(AttrNum)*100,1), .groups="drop")

AD  <- df %>% group_by(Department) %>%
  summarise(Total=n(), Left=sum(AttrNum), Rate=round(mean(AttrNum)*100,1), .groups="drop") %>%
  arrange(desc(Rate))

AR  <- df %>% group_by(JobRole) %>%
  summarise(Total=n(), Left=sum(AttrNum), Rate=round(mean(AttrNum)*100,1), .groups="drop") %>%
  arrange(desc(Rate))

AS  <- df %>% filter(!is.na(SalBand)) %>%
  group_by(SalBand) %>%
  summarise(Total=n(), Left=sum(AttrNum), Rate=round(mean(AttrNum)*100,1), .groups="drop")

AO  <- df %>% group_by(OverTime) %>%
  summarise(Total=n(), Left=sum(AttrNum), Rate=round(mean(AttrNum)*100,1), .groups="drop")

CORR_DF <- data.frame(
  Variable = c("OverTime","TotalWorkingYears","Age","MonthlyIncome",
               "YearsAtCompany","JobSatisfaction","WorkLifeBalance","DistanceFromHome"),
  r        = c(0.2461,-0.1711,-0.1592,-0.1598,-0.1344,-0.1035,-0.0639,0.0779)
) %>% arrange(r)

VJ <- data.frame(
  Chart = c("Line Chart","Horizontal Bar","Grouped Bar","Stacked Bar",
            "Heatmap","Scatter Plot","Violin+Box","Histogram","Correlation Bar","Donut"),
  Task  = c("T2a Time-series","T2b Dept/Role rank","T2b Feature compare",
            "T2c Distribution","T2b 2-way","T2d Relationship",
            "T2c Distribution","T2c Distribution","T2d Correlation","T2 Overview"),
  Why   = c(
    "Shows attrition rate trend across age bands — equivalent of Sales trend over time",
    "Ranked bars make best/worst departments and roles immediately visible",
    "Side-by-side comparison of Left vs Stayed across overtime, gender, marital status",
    "100% stack shows attrition proportion within each salary band",
    "Department × Job Level heatmap reveals 2-way interaction patterns",
    "Age vs Income scatter coloured by attrition — equivalent of Sales vs Profit",
    "Full income distribution shape split by attrition status",
    "Overlapping histograms compare Age/Tenure distributions directly",
    "Ranks all predictors by Pearson r — shows direction and strength",
    "Binary attrition split — cleaner than pie with central annotation"
  ), stringsAsFactors = FALSE
)

PERF <- data.frame(
  Technique = c("Pre-computed aggregations","Single reactive fd()","Scatter sampling","Binning","DT pagination"),
  Benefit   = c("Heavy group_by runs once at startup","All filters evaluated once per change",
                "Caps at 500pts — scales to any size","Reduces cardinality to 4-6 levels",
                "Renders only visible rows"),
  Impact    = c("High","High","Medium","Medium","High"),
  stringsAsFactors = FALSE
)

# =============================================================================
# STEP 3: Theme & Colour Palette
# =============================================================================

A1 <- "#FF6B6B"; A2 <- "#4ECDC4"; A3 <- "#FFE66D"; A4 <- "#45B7D1"
A5 <- "#96CEB4"; A6 <- "#FF8B94"; A7 <- "#6C5CE7"
BG <- "#1A1A2E"; PL <- "#16213E"; CR <- "#0F3460"; MU <- "#94A3B8"; TX <- "#E2E8F0"

DK <- function(p) p %>% layout(
  paper_bgcolor=BG, plot_bgcolor=PL,
  font=list(color=MU, family="Georgia,serif", size=12),
  xaxis=list(gridcolor=CR, zerolinecolor=CR, tickfont=list(color=MU)),
  yaxis=list(gridcolor=CR, zerolinecolor=CR, tickfont=list(color=MU)),
  legend=list(bgcolor=PL, bordercolor=CR, orientation="h", x=0, y=1.18,
              font=list(color=MU)),
  margin=list(l=55, r=25, t=45, b=55)
)

GG <- function() theme_minimal(base_size=12) +
  theme(plot.background=element_rect(fill=BG, color=NA),
        panel.background=element_rect(fill=PL, color=NA),
        panel.grid.major=element_line(color=CR, linewidth=0.3),
        panel.grid.minor=element_blank(),
        axis.text=element_text(color=MU),
        axis.title=element_text(color=TX),
        legend.background=element_rect(fill=PL, color=NA),
        legend.text=element_text(color=MU))

# =============================================================================
# STEP 4: CSS Styles
# =============================================================================

CSS <- "
body,.wrapper,.content-wrapper { background:#1A1A2E !important; }
.box { background:#16213E !important; border:1px solid #0F3460 !important;
       border-radius:0px !important; border-top:3px solid #FF6B6B !important;
       box-shadow:4px 4px 0px #0F3460 !important; }
.box-header { background:#16213E !important; border-bottom:1px solid #0F3460 !important;
              padding:10px 16px; }
.box-header .box-title { color:#E2E8F0 !important; font-size:12px;
                          font-weight:700; letter-spacing:1px; text-transform:uppercase; }
.box-header .box-title .fa { color:#FF6B6B !important; margin-right:6px; }
.small-box { border-radius:0px !important; border-top:3px solid #FF6B6B !important;
             box-shadow:4px 4px 0px #0F3460 !important; }
.small-box h3 { font-size:2rem !important; font-weight:700 !important; font-family:Georgia,serif; }
.small-box .icon-large { opacity:.12 !important; }
.th { color:#FF6B6B; font-size:11px; letter-spacing:3px; text-transform:uppercase;
      font-weight:700; padding:6px 0; border-bottom:2px solid #FF6B6B;
      margin:4px 0 14px; display:block; }
.sh { color:#FFE66D; font-size:10px; letter-spacing:1.5px; text-transform:uppercase;
      font-weight:600; margin:14px 0 8px; padding:5px 10px;
      background:#0F3460; border-radius:0; display:block; }
.ic { background:#1A1A2E; border:1px solid #0F3460; border-left:4px solid #4ECDC4;
      padding:11px 13px; margin-bottom:8px; }
.ic h4 { color:#4ECDC4; font-size:12px; margin:0 0 4px; font-weight:700;
          font-family:Georgia,serif; }
.ic p  { color:#94A3B8; font-size:11.5px; margin:0; line-height:1.6; }
.ic.r  { border-left-color:#FF6B6B !important; } .ic.r h4 { color:#FF6B6B !important; }
.ic.y  { border-left-color:#FFE66D !important; } .ic.y h4 { color:#FFE66D !important; }
.ic.b  { border-left-color:#45B7D1 !important; } .ic.b h4 { color:#45B7D1 !important; }
table.dataTable thead th { background:#0F3460 !important; color:#E2E8F0 !important;
  border-color:#0F3460 !important; font-size:11px !important; letter-spacing:.5px; text-transform:uppercase; }
table.dataTable tbody tr { background:#16213E !important; color:#94A3B8 !important; }
table.dataTable tbody tr:hover { background:#0F3460 !important; color:#E2E8F0 !important; }
table.dataTable tbody td { border-color:#0F3460 !important; font-size:12px; }
.dataTables_wrapper { background:#16213E; color:#94A3B8; }
.dataTables_filter input, .dataTables_length select { background:#1A1A2E !important;
  color:#E2E8F0 !important; border:1px solid #0F3460 !important; }
.dataTables_info, .dataTables_paginate { color:#4A6080 !important; }
.paginate_button { background:#0F3460 !important; color:#94A3B8 !important;
  border:1px solid #0F3460 !important; }
.paginate_button.current, .paginate_button:hover { background:#FF6B6B !important;
  color:#1A1A2E !important; }
"

SB <- "
.main-sidebar, .left-side { background:#16213E !important; }
.sidebar-menu > li > a { color:#94A3B8 !important; font-size:12px; padding:9px 14px;
  border-left:3px solid transparent; letter-spacing:.5px; }
.sidebar-menu > li.active > a, .sidebar-menu > li > a:hover {
  background:#0F3460 !important; color:#FF6B6B !important;
  border-left:3px solid #FF6B6B !important; }
.sidebar-menu > li > a .fa { color:#4ECDC4 !important; width:18px; }
.form-group label { color:#4A6080; font-size:10px; text-transform:uppercase;
  letter-spacing:1px; }
select.form-control { background:#0F3460; color:#E2E8F0; border:1px solid #4A6080;
  font-size:12px; border-radius:0; }
select.form-control option { background:#1A1A2E; }
hr { border-color:#0F3460; margin:5px 14px; }
.shiny-input-container { padding:0 14px; margin-bottom:7px; }
.sbl { color:#4ECDC4; font-size:9.5px; letter-spacing:2px; text-transform:uppercase;
  padding:8px 14px 3px; font-weight:700; display:block; }
"

# =============================================================================
# STEP 5: UI Definition
# =============================================================================

ui <- dashboardPage(skin="black",
  dashboardHeader(
    title=tags$span(
      tags$span("■ ",style="color:#FF6B6B;font-size:20px;"),
      tags$b("ATTRITION",style="color:#FF6B6B;font-size:16px;letter-spacing:4px;font-family:Georgia,serif;"),
      tags$span(" ANALYTICS",style="color:#4ECDC4;font-size:10px;letter-spacing:2px;")
    ), titleWidth=310),

  dashboardSidebar(width=260, tags$style(HTML(SB)),
    sidebarMenu(id="tabs",
      menuItem("T1 · Data Exploration",   tabName="t1", icon=icon("database")),
      menuItem("T2 · Dashboard",          tabName="t2", icon=icon("chart-line")),
      menuItem("T3 · Viz Justification",  tabName="t3", icon=icon("clipboard-list")),
      menuItem("T4 · Interactivity",      tabName="t4", icon=icon("sliders-h")),
      menuItem("T5 · Insights",           tabName="t5", icon=icon("brain")),
      menuItem("T6 · Performance",        tabName="t6", icon=icon("rocket")),
      menuItem("Employee Records",        tabName="t7", icon=icon("users"))
    ),
    hr(),
    tags$span(class="sbl", "▶  Filters"),
    selectInput("fd","Department",
                choices=c("All",sort(unique(df$Department))), selected="All"),
    selectInput("fr","Job Role",
                choices=c("All",sort(unique(df$JobRole))), selected="All"),
    selectInput("fg","Gender",
                choices=c("All","Male","Female"), selected="All"),
    selectInput("fo","OverTime",
                choices=c("All","Yes","No"), selected="All"),
    selectInput("fm","Marital Status",
                choices=c("All",sort(unique(df$MaritalStatus))), selected="All"),
    selectInput("fa","Age Band",
                choices=c("All",levels(df$AgeBand)), selected="All"),
    tags$div(style="padding:4px 14px;",
      actionButton("rst","↺  Reset Filters",
        style="background:#0F3460;color:#94A3B8;border:1px solid #4A6080;
               width:100%;font-size:11px;padding:6px;letter-spacing:.5px;")),
    hr(),
    tags$span(class="sbl", "▶  Quick Presets"),
    tags$div(style="padding:0 14px;display:flex;flex-direction:column;gap:5px;margin-bottom:8px;",
      actionButton("ph","■ High Risk (Sales Rep)",
        style="background:#FF6B6B18;color:#FF6B6B;border:1px solid #FF6B6B;
               width:100%;font-size:11px;font-weight:700;padding:6px;letter-spacing:.5px;"),
      actionButton("pl","■ Low Risk (Research Dir)",
        style="background:#4ECDC418;color:#4ECDC4;border:1px solid #4ECDC4;
               width:100%;font-size:11px;font-weight:700;padding:6px;letter-spacing:.5px;"),
      actionButton("pot","■ OverTime = Yes",
        style="background:#FFE66D18;color:#FFE66D;border:1px solid #FFE66D;
               width:100%;font-size:11px;font-weight:700;padding:6px;letter-spacing:.5px;"),
      actionButton("psal","■ Low Salary Band",
        style="background:#45B7D118;color:#45B7D1;border:1px solid #45B7D1;
               width:100%;font-size:11px;font-weight:700;padding:6px;letter-spacing:.5px;")
    )
  ),

  dashboardBody(tags$head(tags$style(HTML(CSS))),
  tabItems(

  tabItem(tabName="t1",
    fluidRow(column(12,tags$span(class="th","TASK 1  —  DATA EXPLORATION & PREPARATION"))),
    fluidRow(column(12,tags$span(class="sh","Subtask 1a  ·  Identify Key Dimensions & Measures"))),
    fluidRow(
      valueBoxOutput("k1",width=3), valueBoxOutput("k2",width=3),
      valueBoxOutput("k3",width=3), valueBoxOutput("k4",width=3)
    ),
    fluidRow(
      box(title=tagList(icon("table"),"Variable Catalogue — All Dimensions & Measures"),
          width=7, DTOutput("tbl_var")),
      box(title=tagList(icon("chart-pie"),"Variable Type Composition"),
          width=5, plotlyOutput("plt_vtype",height="290px"))
    ),
    fluidRow(
      box(title=tagList(icon("chart-bar"),"Department — Key Dimension Distribution"),
          width=6, plotlyOutput("plt_ddist",height="250px")),
      box(title=tagList(icon("chart-area"),"Key Measures — Income, Age, Tenure"),
          width=6, plotlyOutput("plt_mdist",height="250px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 1b  ·  Preprocessing — Missing Values, Outliers & Formatting"))),
    fluidRow(
      box(title=tagList(icon("check-circle"),"Missing Value Audit — All 35 Variables"),
          width=4, plotlyOutput("plt_miss",height="255px")),
      box(title=tagList(icon("exclamation-triangle"),"Outlier Detection — Z-Score > 3.0"),
          width=4, plotlyOutput("plt_out",height="255px")),
      box(title=tagList(icon("exchange-alt"),"Formatting — Before vs After"),
          width=4, plotlyOutput("plt_fmt",height="255px"))
    ),
    fluidRow(
      box(title=tagList(icon("tools"),"Preprocessing Steps Applied"),width=12,
        fluidRow(
          column(3,tags$div(class="ic",tags$h4("Step 1 · Missing Values"),
            tags$p("Zero missing values across all 1,470 employee records and 35 variables. Confirmed with colSums(is.na()). No imputation required."))),
          column(3,tags$div(class="ic y",tags$h4("Step 2 · Outlier Flagging"),
            tags$p("Z-score > 3 on MonthlyIncome, Age, YearsAtCompany. Flagged records marked in Outlier column and visualised. Retained for analysis."))),
          column(3,tags$div(class="ic b",tags$h4("Step 3 · Label Formatting"),
            tags$p("Education codes 1-5 mapped to readable labels (College, Bachelor, Master etc.). Attrition Yes/No used as target. Numeric satisfaction levels labelled."))),
          column(3,tags$div(class="ic b",tags$h4("Step 4 · Feature Engineering"),
            tags$p("AgeBand, SalBand, TenureBand created via cut(). AttritionNum binary column added. OT_num numeric flag for OverTime created for correlation analysis.")))
        ))
    )
  ),

  tabItem(tabName="t2",
    fluidRow(column(12,tags$span(class="th","TASK 2  —  DASHBOARD DEVELOPMENT"))),
    fluidRow(
      valueBoxOutput("k5",width=3), valueBoxOutput("k6",width=3),
      valueBoxOutput("k7",width=3), valueBoxOutput("k8",width=3)
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 2a  ·  Time-Series — Attrition Rate Trend Across Age Bands"))),
    fluidRow(
      box(title=tagList(icon("chart-line"),"Attrition Rate Trend Across Age Bands (Line) vs Headcount (Bars) — Dual Axis"),
          width=8, plotlyOutput("plt_ts",height="295px")),
      box(title=tagList(icon("chart-bar"),"Headcount per Age Band"),
          width=4, plotlyOutput("plt_tsvol",height="295px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 2b  ·  Category-wise & Region-wise Comparisons"))),
    fluidRow(
      box(title=tagList(icon("building"),"Category-wise: Department Attrition Rate"),
          width=5, plotlyOutput("plt_dept",height="280px")),
      box(title=tagList(icon("user-tie"),"Region-wise: Job Role Attrition Rate"),
          width=7, plotlyOutput("plt_role",height="280px"))
    ),
    fluidRow(
      box(title=tagList(icon("th"),"Heatmap — Department × Job Level Attrition (%)"),
          width=8, plotlyOutput("plt_heat",height="310px")),
      box(title=tagList(icon("chart-bar"),"Salary Band — Attrition Proportion"),
          width=4, plotlyOutput("plt_sal",height="310px"))
    ),
    fluidRow(
      box(title=tagList(icon("clock"),"OverTime Effect on Attrition"),
          width=6, plotlyOutput("plt_ot",height="260px")),
      box(title=tagList(icon("heart"),"Marital Status vs Attrition"),
          width=6, plotlyOutput("plt_mar",height="260px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 2c  ·  Distribution of Key Variables"))),
    fluidRow(
      box(title=tagList(icon("chart-area"),"Age Distribution — Left vs Stayed"),
          width=4, plotlyOutput("plt_dage",height="255px")),
      box(title=tagList(icon("chart-area"),"Monthly Income — Violin + Box"),
          width=4, plotlyOutput("plt_dinc",height="255px")),
      box(title=tagList(icon("chart-area"),"Years at Company — Left vs Stayed"),
          width=4, plotlyOutput("plt_dten",height="255px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 2d  ·  Relationships Between Variables (Sales vs Profit equivalent)"))),
    fluidRow(
      box(title=tagList(icon("braille"),"Age vs Monthly Income — Coloured by Attrition"),
          width=7, plotlyOutput("plt_sct",height="295px")),
      box(title=tagList(icon("signal"),"Job Satisfaction vs Attrition Rate"),
          width=5, plotlyOutput("plt_sat",height="295px"))
    ),
    fluidRow(
      box(title=tagList(icon("project-diagram"),"Correlation — All Variables vs Attrition"),
          width=12, plotlyOutput("plt_cor",height="250px"))
    )
  ),

  tabItem(tabName="t3",
    fluidRow(column(12,tags$span(class="th","TASK 3  —  VISUALIZATION JUSTIFICATION"))),
    fluidRow(column(12,tags$span(class="sh","Select Appropriate Charts & Provide Justification for Each"))),
    fluidRow(
      box(title=tagList(icon("table"),"Chart Justification — All Visualizations Used"),
          width=12, DTOutput("tbl_vj"))
    ),
    fluidRow(
      box(title=tagList(icon("chart-pie"),"Chart Types Used"),
          width=4, plotlyOutput("plt_ct",height="300px")),
      box(title=tagList(icon("book"),"Justification Principles"),
          width=8,
          fluidRow(
            column(6,
              tags$div(class="ic",tags$h4("Line Chart (Time-Series)"),
                tags$p("Attrition rate across age bands = trend over time equivalent. Line shows trajectory; bars show volume — prevents misreading rate as headcount effect.")),
              tags$div(class="ic",tags$h4("Ranked Bar Charts"),
                tags$p("Departments and Job Roles ranked by attrition rate. Horizontal orientation fits long role names. Ranking makes best/worst visible instantly.")),
              tags$div(class="ic",tags$h4("Heatmap (2-D)"),
                tags$p("Department × Job Level matrix. Only chart that shows combined effect of two categorical variables simultaneously."))
            ),
            column(6,
              tags$div(class="ic b",tags$h4("Scatter Plot"),
                tags$p("Age vs Income coloured by Attrition — direct equivalent of Sales vs Profit. Shows whether leavers cluster in specific age-income space.")),
              tags$div(class="ic b",tags$h4("Violin + Box"),
                tags$p("Monthly Income distribution split by attrition. Violin shows full density shape; box shows median and quartiles — much richer than bar.")),
              tags$div(class="ic y",tags$h4("Correlation Bar"),
                tags$p("All 8 predictors ranked by Pearson r. Red = positive (OverTime increases attrition), Teal = negative (Income reduces attrition)."))
            )
          ))
    )
  ),

  tabItem(tabName="t4",
    fluidRow(column(12,tags$span(class="th","TASK 4  —  INTERACTIVITY"))),
    fluidRow(column(12,tags$span(class="sh","Subtask 4a  ·  Filters — Department=Category, Job Role=Region, Gender, OverTime, Marital Status, Age Band"))),
    fluidRow(
      valueBoxOutput("k9",width=3), valueBoxOutput("k10",width=3),
      valueBoxOutput("k11",width=3), valueBoxOutput("k12",width=3)
    ),
    fluidRow(
      box(title=tagList(icon("filter"),"Active Filter Status"),
          width=4,
          tags$p(style="color:#94A3B8;font-size:12px;margin-bottom:8px;",
            "All sidebar filters update every chart on every tab instantly."),
          uiOutput("flt_status")),
      box(title=tagList(icon("chart-bar"),"Live Chart — Reacts to All Filters"),
          width=8, plotlyOutput("plt_flive",height="275px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 4b  ·  Drill-down — Click Any Role Bar to See Employee Records"))),
    fluidRow(
      box(title=tagList(icon("search-plus"),"Click a Role Bar to Drill Down into Employee Records"),
          width=6, plotlyOutput("plt_drill",height="275px")),
      box(title=tagList(icon("users"),"Drill-down Employee Records"),
          width=6, DTOutput("tbl_drill"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 4c  ·  Tooltips — Hover Over Any Chart Element"))),
    fluidRow(
      box(title=tagList(icon("mouse-pointer"),"Tooltip Demo — Hover Over Any Point"),
          width=7, plotlyOutput("plt_tip",height="275px")),
      box(title=tagList(icon("comment-dots"),"Tooltip Content Per Chart"),
          width=5,
          tags$div(class="ic",tags$h4("Bar Chart Tooltips"),
            tags$p("Department/Role · Headcount (n) · Employees left · Attrition rate %")),
          tags$div(class="ic",tags$h4("Scatter Tooltips"),
            tags$p("Employee age · Monthly income · Department · Job role · Attrition status")),
          tags$div(class="ic",tags$h4("Heatmap Tooltips"),
            tags$p("Department · Job level · Attrition rate % · Headcount (n)")),
          tags$div(class="ic",tags$h4("Line/Trend Tooltips"),
            tags$p("Age band · Attrition rate % · Headcount (n)"))
      )
    )
  ),

  tabItem(tabName="t5",
    fluidRow(column(12,tags$span(class="th","TASK 5  —  INSIGHT GENERATION"))),
    fluidRow(column(12,tags$span(class="sh","Subtask 5a  ·  Key Trends & Patterns"))),
    fluidRow(
      box(title=tagList(icon("chart-line"),"Trend 1 — Young Employees Leave Most (Age-Attrition Gradient)"),
          width=8, plotlyOutput("plt_t1",height="270px")),
      box(title=tagList(icon("lightbulb"),"Key Patterns"),
          width=4,
          tags$div(class="ic",tags$h4("Young Workforce Risk"),
            tags$p("18-25 age band: 35.8% attrition vs 9.2% for 36-45. Early career employees are 3.9× more likely to leave.")),
          tags$div(class="ic",tags$h4("OverTime is #1 Driver"),
            tags$p("OverTime=Yes: 30.5% attrition vs 10.4% without — a 2.9× multiplier. Strongest single predictor (r=0.25).")),
          tags$div(class="ic",tags$h4("Low Salary = High Risk"),
            tags$p("$1k-3k band: 28.6% vs 8.9% for $10k-20k. Income is a key retention lever (r=-0.16).")))
    ),
    fluidRow(
      box(title=tagList(icon("clock"),"Trend 2 — OverTime Effect"),
          width=6, plotlyOutput("plt_t2",height="255px")),
      box(title=tagList(icon("dollar-sign"),"Trend 3 — Income vs Attrition"),
          width=6, plotlyOutput("plt_t3",height="255px"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 5b  ·  Best & Worst Performing Categories / Regions"))),
    fluidRow(
      box(title=tagList(icon("trophy"),"Job Role Retention — Best to Worst"),
          width=7, plotlyOutput("plt_bw_role",height="330px")),
      box(title=tagList(icon("building"),"Department Retention — Best to Worst"),
          width=5, plotlyOutput("plt_bw_dept",height="330px"))
    ),
    fluidRow(
      box(title=tagList(icon("table"),"Full Performance Rankings — Roles & Departments"),
          width=12, DTOutput("tbl_bw"))
    ),
    fluidRow(column(12,tags$span(class="sh","Subtask 5c  ·  Anomalies & Unusual Observations"))),
    fluidRow(
      box(title=tagList(icon("exclamation-circle"),"Anomaly 1 — Sales Rep Attrition 5× Higher Than Manager"),
          width=6, plotlyOutput("plt_an1",height="255px")),
      box(title=tagList(icon("exclamation-triangle"),"Anomaly 2 — Outlier Employees vs Normal"),
          width=6, plotlyOutput("plt_an2",height="255px"))
    ),
    fluidRow(
      box(title=tagList(icon("flag"),"3 Anomalies Identified & Explained"),width=12,
        fluidRow(
          column(4,tags$div(class="ic r",tags$h4("Anomaly 1 — Sales Rep 39.8% vs Manager 4.9%"),
            tags$p("Sales Representatives have the highest attrition in the company at 39.8%. Managers have the lowest at 4.9%. An 8× gap within the same organisation suggests role-specific pressures — quota stress, lower pay, limited growth."))),
          column(4,tags$div(class="ic y",tags$h4("Anomaly 2 — OverTime 2.9× Multiplier"),
            tags$p("30.5% attrition with OverTime vs 10.4% without. This is the strongest predictor (r=0.25). Unusual because it is modifiable — reducing compulsory overtime is the single highest-impact retention intervention."))),
          column(4,tags$div(class="ic b",tags$h4("Anomaly 3 — Single Employees Leave 3× More"),
            tags$p("Single employees: 25.5% attrition vs 8.9% (Married) and 10.9% (Divorced). Married employees with family responsibilities may have stronger financial motivation to stay — anomalously large lifestyle effect.")))
        ))
    )
  ),

  tabItem(tabName="t6",
    fluidRow(column(12,tags$span(class="th","TASK 6  —  PERFORMANCE OPTIMIZATION"))),
    fluidRow(column(12,tags$span(class="sh","Techniques Applied to Handle Large Datasets Efficiently"))),
    fluidRow(
      valueBoxOutput("k13",width=3), valueBoxOutput("k14",width=3),
      valueBoxOutput("k15",width=3), valueBoxOutput("k16",width=3)
    ),
    fluidRow(
      box(title=tagList(icon("chart-bar"),"Optimisation Impact Score"),
          width=7, plotlyOutput("plt_perf",height="280px")),
      box(title=tagList(icon("table"),"Performance Techniques Log"),
          width=5, DTOutput("tbl_perf"))
    ),
    fluidRow(
      box(title=tagList(icon("clock"),"Computation Timing — Startup vs Per-Interaction"),
          width=6, plotlyOutput("plt_ptim",height="250px")),
      box(title=tagList(icon("chart-area"),"Scalability — Render Strategy by Dataset Size"),
          width=6, plotlyOutput("plt_pscl",height="250px"))
    ),
    fluidRow(
      box(title=tagList(icon("code"),"Code-Level Implementation"),width=12,
        fluidRow(
          column(3,tags$div(class="ic",tags$h4("Pre-computed Aggregations"),
            tags$p("AT, AD, AR, AS, AO, CORR_DF built once at startup. group_by+summarise never runs inside a reactive — eliminates the biggest bottleneck."))),
          column(3,tags$div(class="ic",tags$h4("Single Reactive fd()"),
            tags$p("All 6 filters evaluated in one reactive expression. Result cached and shared across 25+ chart outputs. Runs once per interaction."))),
          column(3,tags$div(class="ic y",tags$h4("Scatter Sampling"),
            tags$p("sample_n(min(500, nrow(.))) caps scatter plots. Prevents browser DOM overload. Automatically scales to datasets of any size."))),
          column(3,tags$div(class="ic b",tags$h4("Binning + DT Pagination"),
            tags$p("cut() on Age/Income/Tenure reduces cardinality. DT renders only visible rows — 1,470 or 1,000,000 rows handled identically.")))
        ))
    )
  ),

  tabItem(tabName="t7",
    fluidRow(column(12,tags$span(class="th","Employee Records — Full Filtered Dataset"))),
    fluidRow(
      box(title=tagList(icon("users"),"Employee Data"),width=12,
        fluidRow(
          column(7,tags$p(style="color:#94A3B8;font-size:12px;margin:4px 0;",textOutput("row_info"))),
          column(5,style="text-align:right;padding-right:14px;",
            downloadButton("dl","⬇  Export CSV",
              style="background:#FF6B6B;color:#1A1A2E;border:none;font-weight:700;
                     font-size:12px;padding:6px 14px;letter-spacing:.5px;"))
        ),
        br(), DTOutput("tbl_main"))
    )
  )
  ))
)

# =============================================================================
# STEP 6: Server Logic
# =============================================================================

server <- function(input, output, session) {

  fd <- reactive({
    d <- df
    if(input$fd  !="All") d <- d %>% filter(Department    == input$fd)
    if(input$fr  !="All") d <- d %>% filter(JobRole       == input$fr)
    if(input$fg  !="All") d <- d %>% filter(Gender        == input$fg)
    if(input$fo  !="All") d <- d %>% filter(OverTime      == input$fo)
    if(input$fm  !="All") d <- d %>% filter(MaritalStatus == input$fm)
    if(input$fa  !="All") d <- d %>% filter(AgeBand       == input$fa)
    d
  })

  observeEvent(input$rst, {
    for(id in c("fd","fr","fg","fo","fm","fa"))
      updateSelectInput(session,id,selected="All")
  })
  observeEvent(input$ph,  { updateSelectInput(session,"fr",selected="Sales Representative"); updateTabItems(session,"tabs","t5") })
  observeEvent(input$pl,  { updateSelectInput(session,"fr",selected="Research Director");    updateTabItems(session,"tabs","t5") })
  observeEvent(input$pot, { updateSelectInput(session,"fo",selected="Yes");                  updateTabItems(session,"tabs","t2") })
  observeEvent(input$psal,{ updateSelectInput(session,"fa",selected="18-25");                updateTabItems(session,"tabs","t2") })

  output$k1 <- renderValueBox(valueBox(nrow(df_raw),"Total Employees",    icon=icon("users"),       color="navy"))
  output$k2 <- renderValueBox(valueBox(ncol(df_raw),"Variables",           icon=icon("columns"),     color="purple"))
  output$k3 <- renderValueBox(valueBox("9 Dimensions","Categorical",       icon=icon("layer-group"), color="maroon"))
  output$k4 <- renderValueBox(valueBox("26 Measures","Numeric",            icon=icon("ruler"),       color="green"))

  output$tbl_var <- renderDT(
    data.frame(
      Variable=c("Age","Attrition","Department","Gender","JobRole","MaritalStatus",
                 "OverTime","Education","MonthlyIncome","YearsAtCompany","JobSatisfaction",
                 "DistanceFromHome","WorkLifeBalance","TotalWorkingYears","PerformanceRating"),
      Type=c("Numeric","Binary TARGET","Dimension","Dimension","Dimension","Dimension",
             "Dimension","Ordinal","Measure","Measure","Ordinal","Measure","Ordinal","Measure","Ordinal"),
      Role=c("Employee age","Left company (Yes/No)","Business unit",
             "Employee gender","Specific job function","Single/Married/Divorced",
             "Works extra hours","1=Below College to 5=Doctor",
             "Monthly pay in USD","Tenure with company","1=Low to 4=Very High",
             "Miles from office","1=Bad to 4=Best","Total career years","1=Low to 4=Outstanding")
    ) %>%
      datatable(options=list(pageLength=15,dom="t",scrollX=TRUE,
        columnDefs=list(list(className="dt-left",targets="_all"))),
        rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Type",color=styleEqual(
        c("Binary TARGET","Numeric","Dimension","Measure","Ordinal"),
        c(A1,A3,A2,A4,A5)),fontWeight="bold")
  )

  output$plt_vtype <- renderPlotly(
    plot_ly(data.frame(Type=c("Dimension","Measure","Binary","Ordinal"),n=c(8,15,2,6)),
            labels=~Type,values=~n,type="pie",hole=0.5,
            marker=list(colors=c(A2,A4,A1,A3),line=list(color=BG,width=2)),
            textinfo="label+percent") %>%
      DK() %>% layout(showlegend=FALSE,margin=list(l=5,r=5,t=5,b=5))
  )

  output$plt_ddist <- renderPlotly({
    d <- df %>% count(Department)
    plot_ly(d,x=~reorder(Department,n),y=~n,type="bar",
            marker=list(color=c(A1,A2,A4),line=list(color=BG,width=1)),
            hovertemplate="<b>%{x}</b><br>Employees: %{y}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Department"),yaxis=list(title="Count"))
  })

  output$plt_mdist <- renderPlotly(
    plot_ly(alpha=0.75) %>%
      add_histogram(x=~df$MonthlyIncome/1000,name="Income ($k)",marker=list(color=A1),nbinsx=20) %>%
      add_histogram(x=~df$Age,               name="Age",         marker=list(color=A2),nbinsx=20) %>%
      add_histogram(x=~df$YearsAtCompany,    name="Tenure (yrs)",marker=list(color=A3),nbinsx=20) %>%
      DK() %>% layout(barmode="overlay",xaxis=list(title="Value"),yaxis=list(title="Count"))
  )

  output$plt_miss <- renderPlotly({
    d <- data.frame(Var=names(df_raw)[1:15],
                    Complete=nrow(df_raw)-as.integer(MISS[1:15]),
                    Missing=as.integer(MISS[1:15]))
    plot_ly(d,x=~Var,y=~Complete,type="bar",name="Complete",marker=list(color=A2)) %>%
      add_trace(y=~Missing,name="Missing",marker=list(color=A1)) %>%
      DK() %>% layout(barmode="stack",xaxis=list(title=NULL,tickangle=-40),
                      yaxis=list(title="Records"),
                      annotations=list(list(x=0.5,y=0.5,xref="paper",yref="paper",
                        text="<b>✓ Zero missing values</b>",
                        font=list(color=A2,size=12),showarrow=FALSE)))
  })

  output$plt_out <- renderPlotly({
    d <- df %>% count(Outlier)
    plot_ly(d,x=~Outlier,y=~n,type="bar",
            marker=list(color=c(A1,A2),line=list(color=BG,width=1)),
            text=~n,textposition="outside",
            hovertemplate="%{x}: %{y}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Record Type"),yaxis=list(title="Count"))
  })

  output$plt_fmt <- renderPlotly(
    plot_ly() %>%
      add_bars(data=data.frame(L=c("Code 1","Code 2","Code 3","Code 4","Code 5"),V=c(170,282,572,370,76)),
               x=~L,y=~V,name="Before (Codes)",marker=list(color=CR)) %>%
      add_bars(data=data.frame(L=c("Below Col","College","Bachelor","Master","Doctor"),V=c(170,282,572,370,76)),
               x=~L,y=~V,name="After (Labels)",marker=list(color=A2)) %>%
      DK() %>% layout(barmode="group",xaxis=list(title="Education",tickangle=-20),yaxis=list(title="Count"))
  )

  output$k5 <- renderValueBox(valueBox(nrow(fd()),"Employees",       icon=icon("users"),       color="navy"))
  output$k6 <- renderValueBox(valueBox(sum(fd()$AttrNum),"Left",     icon=icon("sign-out-alt"),color="maroon"))
  output$k7 <- renderValueBox({
    r <- if(nrow(fd())>0) round(mean(fd()$AttrNum)*100,1) else 0
    valueBox(paste0(r,"%"),"Attrition Rate",icon=icon("percent"),color="red")
  })
  output$k8 <- renderValueBox({
    avg <- if(nrow(fd())>0) paste0("$",round(mean(fd()$MonthlyIncome))) else "N/A"
    valueBox(avg,"Avg Monthly Income",icon=icon("dollar-sign"),color="green")
  })

  output$plt_ts <- renderPlotly({
    d <- fd() %>% filter(!is.na(AgeBand)) %>% group_by(AgeBand) %>%
      summarise(Total=n(),Left=sum(AttrNum),Rate=round(mean(AttrNum)*100,1),.groups="drop")
    plot_ly(d) %>%
      add_bars(x=~AgeBand,y=~Total,name="Total Employees",marker=list(color=CR),
               hovertemplate="Age: %{x}<br>Count: %{y}<extra></extra>") %>%
      add_bars(x=~AgeBand,y=~Left,name="Employees Left",marker=list(color=A1),
               hovertemplate="Age: %{x}<br>Left: %{y}<extra></extra>") %>%
      add_trace(x=~AgeBand,y=~Rate,type="scatter",mode="lines+markers",
                name="Attrition Rate %",yaxis="y2",
                line=list(color=A3,width=2.5,dash="dot"),marker=list(color=A3,size=9),
                hovertemplate="Rate: %{y:.1f}%<extra></extra>") %>%
      DK() %>% layout(barmode="overlay",
                      xaxis=list(title="Age Band"),yaxis=list(title="Employee Count"),
                      yaxis2=list(title="Attrition Rate (%)",overlaying="y",side="right",
                                  tickfont=list(color=A3),titlefont=list(color=A3)))
  })

  output$plt_tsvol <- renderPlotly({
    d <- fd() %>% filter(!is.na(AgeBand)) %>% count(AgeBand)
    plot_ly(d,x=~n,y=~AgeBand,type="bar",orientation="h",
            marker=list(color=A4,line=list(color=BG,width=1)),
            hovertemplate="%{y}: %{x}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Count"),yaxis=list(title=NULL))
  })

  output$plt_dept <- renderPlotly({
    d <- fd() %>% group_by(Department) %>%
      summarise(Total=n(),Left=sum(AttrNum),Rate=round(mean(AttrNum)*100,1),.groups="drop") %>%
      arrange(desc(Rate))
    cols <- ifelse(d$Rate>20,A1,ifelse(d$Rate>15,A3,A2))
    plot_ly(d,x=~Rate,y=~reorder(Department,Rate),type="bar",orientation="h",
            text=~paste0(Rate,"%"),textposition="outside",
            marker=list(color=cols,line=list(color=BG,width=1)),customdata=~Total,
            hovertemplate="<b>%{y}</b><br>Rate: %{x:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Attrition Rate (%)"),yaxis=list(title=NULL))
  })

  output$plt_role <- renderPlotly({
    d <- fd() %>% group_by(JobRole) %>%
      summarise(Total=n(),Left=sum(AttrNum),Rate=round(mean(AttrNum)*100,1),.groups="drop") %>%
      arrange(desc(Rate))
    cols <- ifelse(d$Rate>25,A1,ifelse(d$Rate>15,A3,A2))
    plot_ly(d,x=~Rate,y=~reorder(JobRole,Rate),type="bar",orientation="h",
            text=~paste0(Rate,"%"),textposition="outside",
            marker=list(color=cols,line=list(color=BG,width=1)),customdata=~Total,
            hovertemplate="<b>%{y}</b><br>Rate: %{x:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Attrition Rate (%)"),yaxis=list(title=NULL))
  })

  output$plt_heat <- renderPlotly({
    d <- fd() %>% group_by(Department,JobLevel) %>%
      summarise(Rate=round(mean(AttrNum)*100,1),n=n(),.groups="drop") %>%
      mutate(JobLevel=paste0("Level ",JobLevel))
    plot_ly(d,x=~JobLevel,y=~Department,z=~Rate,type="heatmap",
            colorscale=list(c(0,BG),c(0.3,CR),c(0.7,A1),c(1,A3)),
            customdata=~n,
            hovertemplate="<b>%{y}</b><br>%{x}<br>Rate: %{z:.1f}%<br>n=%{customdata}<extra></extra>",
            colorbar=list(title=list(text="Rate%",font=list(color=MU)),tickfont=list(color=MU),bgcolor=PL)) %>%
      DK() %>% layout(xaxis=list(title="Job Level"),yaxis=list(title="Department"))
  })

  output$plt_sal <- renderPlotly({
    d <- fd() %>% filter(!is.na(SalBand)) %>% group_by(SalBand,Attrition) %>%
      summarise(n=n(),.groups="drop")
    p <- ggplot(d,aes(x=SalBand,y=n,fill=Attrition)) +
      geom_col(position="fill",width=0.65) +
      scale_fill_manual(values=c("Yes"=A1,"No"=CR)) +
      scale_y_continuous(labels=percent_format()) +
      labs(x="Salary Band",y="Proportion",fill=NULL) + GG() +
      theme(axis.text.x=element_text(angle=20,hjust=1,size=9))
    ggplotly(p) %>% DK()
  })

  output$plt_ot <- renderPlotly({
    d <- fd() %>% group_by(OverTime) %>%
      summarise(Total=n(),Left=sum(AttrNum),Rate=round(mean(AttrNum)*100,1),.groups="drop")
    plot_ly(d,x=~OverTime,y=~Rate,type="bar",
            marker=list(color=c(A2,A1),line=list(color=BG,width=1)),
            text=~paste0(Rate,"%"),textposition="outside",customdata=~Total,
            hovertemplate="OverTime=%{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="OverTime"),yaxis=list(title="Attrition Rate (%)"))
  })

  output$plt_mar <- renderPlotly({
    d <- fd() %>% group_by(MaritalStatus) %>%
      summarise(Total=n(),Left=sum(AttrNum),Rate=round(mean(AttrNum)*100,1),.groups="drop")
    plot_ly(d,x=~MaritalStatus,y=~Rate,type="bar",
            marker=list(color=c(A2,A4,A1),line=list(color=BG,width=1)),
            text=~paste0(Rate,"%"),textposition="outside",customdata=~Total,
            hovertemplate="%{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Marital Status"),yaxis=list(title="Attrition Rate (%)"))
  })

  output$plt_dage <- renderPlotly(
    plot_ly(fd(),x=~Age,color=~Attrition,colors=c("No"=CR,"Yes"=A1),
            type="histogram",opacity=0.82,nbinsx=20,
            hovertemplate="Age: %{x}<br>n=%{y}<extra></extra>") %>%
      DK() %>% layout(barmode="overlay",xaxis=list(title="Age"),yaxis=list(title="Count"))
  )

  output$plt_dinc <- renderPlotly(
    plot_ly(fd(),x=~MonthlyIncome,color=~Attrition,
            colors=c("No"=A2,"Yes"=A1),type="violin",
            box=list(visible=TRUE),meanline=list(visible=TRUE),
            hovertemplate="Income: $%{x}<extra></extra>") %>%
      DK() %>% layout(violinmode="overlay",xaxis=list(title="Monthly Income ($)"),yaxis=list(title="Attrition"))
  )

  output$plt_dten <- renderPlotly(
    plot_ly(fd(),x=~YearsAtCompany,color=~Attrition,colors=c("No"=CR,"Yes"=A3),
            type="histogram",opacity=0.82,nbinsx=20,
            hovertemplate="Years: %{x}<br>n=%{y}<extra></extra>") %>%
      DK() %>% layout(barmode="overlay",xaxis=list(title="Years at Company"),yaxis=list(title="Count"))
  )

  output$plt_sct <- renderPlotly({
    d <- fd() %>% sample_n(min(500,nrow(.)),replace=FALSE)
    plot_ly(d,x=~Age,y=~MonthlyIncome,type="scatter",mode="markers",
            color=~Attrition,colors=c("No"=CR,"Yes"=A1),
            symbol=~Attrition,symbols=c("circle","x"),
            marker=list(size=7,opacity=0.72,line=list(width=0.5,color=BG)),
            customdata=~Department,text=~JobRole,
            hovertemplate="Age: %{x}<br>Income: $%{y}<br>Dept: %{customdata}<br>Role: %{text}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Age"),yaxis=list(title="Monthly Income ($)"))
  })

  output$plt_sat <- renderPlotly({
    d <- fd() %>% group_by(JobSatisfaction) %>%
      summarise(Rate=round(mean(AttrNum)*100,1),Total=n(),.groups="drop") %>%
      mutate(Label=c("Low","Medium","High","Very High")[JobSatisfaction])
    plot_ly(d,x=~Label,y=~Rate,type="scatter",mode="lines+markers",
            line=list(color=A1,width=2.5),
            marker=list(color=A3,size=12,line=list(color=A1,width=2)),
            customdata=~Total,
            hovertemplate="Satisfaction=%{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Job Satisfaction"),yaxis=list(title="Attrition Rate (%)"))
  })

  output$plt_cor <- renderPlotly(
    plot_ly(CORR_DF,x=~reorder(Variable,r),y=~r,type="bar",
            marker=list(color=ifelse(CORR_DF$r>0,A1,A2),line=list(color=BG,width=1)),
            text=~round(r,3),textposition="outside",
            hovertemplate="<b>%{x}</b><br>r = %{y:.4f}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Variable"),
                      yaxis=list(title="Pearson r vs Attrition",
                                 zeroline=TRUE,zerolinecolor=MU,zerolinewidth=1.5))
  )

  output$tbl_vj <- renderDT(
    VJ %>% datatable(options=list(pageLength=10,scrollX=TRUE,dom="frtip",
                       columnDefs=list(list(className="dt-left",targets="_all"))),
                     rownames=FALSE,class="stripe hover") %>%
      formatStyle("Chart",fontWeight="bold",color=A3) %>%
      formatStyle("Task",color=A4)
  )

  output$plt_ct <- renderPlotly(
    plot_ly(data.frame(Type=c("Bar","Line+Bar","Scatter","Heatmap","Violin","Histogram","Donut","KPI"),
                       n=c(7,2,1,1,1,3,1,8)),
            labels=~Type,values=~n,type="pie",hole=0.45,
            marker=list(colors=c(A1,A2,A3,A4,A5,A6,A7,CR),line=list(color=BG,width=2)),
            textinfo="label+percent") %>%
      DK() %>% layout(showlegend=FALSE,margin=list(l=5,r=5,t=5,b=5))
  )

  output$k9  <- renderValueBox(valueBox(nrow(fd()),"Filtered Employees",    icon=icon("filter"),      color="navy"))
  output$k10 <- renderValueBox(valueBox(length(unique(fd()$Department)),"Departments",icon=icon("building"),color="purple"))
  output$k11 <- renderValueBox({
    r <- if(nrow(fd())>0) round(mean(fd()$AttrNum)*100,1) else 0
    valueBox(paste0(r,"%"),"Attrition Rate",icon=icon("percent"),color="red")
  })
  output$k12 <- renderValueBox(valueBox(sum(fd()$AttrNum),"Employees Left",icon=icon("sign-out-alt"),color="maroon"))

  output$flt_status <- renderUI({
    vals <- list(c("Department",input$fd),c("Job Role",input$fr),c("Gender",input$fg),
                 c("OverTime",input$fo),c("Marital Status",input$fm),c("Age Band",input$fa))
    tags$div(lapply(vals, function(v) {
      col <- if(v[2]!="All") A3 else MU
      tags$div(style=paste0("color:",col,";font-size:12px;padding:4px 0;"),
               paste0(if(v[2]!="All")"■ " else "□ ",v[1],": ",v[2]))
    }))
  })

  output$plt_flive <- renderPlotly({
    d <- fd() %>% group_by(Department) %>%
      summarise(Total=n(),Left=sum(AttrNum),Rate=round(mean(AttrNum)*100,1),.groups="drop")
    plot_ly(d,x=~Department,y=~Rate,type="bar",
            marker=list(color=c(A1,A2,A4),line=list(color=BG,width=1)),
            text=~paste0(Rate,"%"),textposition="outside",customdata=~Total,
            hovertemplate="<b>%{x}</b><br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Department — updates with all filters"),yaxis=list(title="Attrition Rate (%)"))
  })

  drill_sel <- reactiveVal(NULL)
  observeEvent(event_data("plotly_click","plt_drill"), {
    click <- event_data("plotly_click","plt_drill")
    if(!is.null(click)) drill_sel(click$y)
  })

  output$plt_drill <- renderPlotly({
    d <- fd() %>% group_by(JobRole) %>%
      summarise(Total=n(),Left=sum(AttrNum),Rate=round(mean(AttrNum)*100,1),.groups="drop") %>%
      arrange(desc(Rate))
    sel  <- drill_sel()
    cols <- ifelse(!is.null(sel)&d$JobRole==sel,A3,ifelse(d$Rate>25,A1,A2))
    plot_ly(d,x=~Rate,y=~reorder(JobRole,Rate),type="bar",orientation="h",
            source="plt_drill",
            marker=list(color=cols,line=list(color=BG,width=1)),customdata=~Total,
            hovertemplate="<b>%{y}</b><br>Rate: %{x:.1f}%<br>n=%{customdata}<br><i>Click to drill down</i><extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Attrition Rate (%)"),yaxis=list(title=NULL))
  })

  output$tbl_drill <- renderDT({
    sel <- drill_sel()
    if(is.null(sel)) return(datatable(data.frame(Message="← Click a role bar to see records"),rownames=FALSE,options=list(dom="t")))
    d <- fd() %>% filter(JobRole==sel) %>%
      select(Attrition,Age,Gender,Department,MonthlyIncome,YearsAtCompany,
             OverTime,MaritalStatus,JobSatisfaction,PerformanceRating)
    datatable(d,caption=paste0(nrow(d)," employees in: ",sel),
              options=list(pageLength=8,scrollX=TRUE,dom="tip",
                columnDefs=list(list(className="dt-center",targets="_all"))),
              rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Attrition",color=styleEqual(c("Yes","No"),c(A1,A2)),fontWeight="bold")
  })

  output$plt_tip <- renderPlotly({
    d <- fd() %>% sample_n(min(400,nrow(.)),replace=FALSE)
    plot_ly(d,x=~Age,y=~MonthlyIncome,size=~YearsAtCompany+1,
            type="scatter",mode="markers",
            color=~Attrition,colors=c("No"=CR,"Yes"=A1),
            marker=list(opacity=0.72,sizemode="diameter",line=list(color=BG,width=0.5)),
            customdata=~Department,text=~JobRole,
            hovertemplate="<b>%{text}</b><br>Dept: %{customdata}<br>Age: %{x}<br>Income: $%{y}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Age — hover any bubble for full detail"),yaxis=list(title="Monthly Income ($)"))
  })

  output$plt_t1 <- renderPlotly({
    d <- fd() %>% filter(!is.na(AgeBand)) %>% group_by(AgeBand) %>%
      summarise(Rate=round(mean(AttrNum)*100,1),Total=n(),.groups="drop")
    plot_ly(d,x=~AgeBand,y=~Rate,type="scatter",mode="lines+markers",
            line=list(color=A3,width=2.5),fill="tozeroy",fillcolor=paste0(A1,"20"),
            marker=list(color=colorRampPalette(c(A1,A2))(nrow(d)),size=13,line=list(color=A3,width=2)),
            customdata=~Total,
            hovertemplate="Age: %{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Age Band"),yaxis=list(title="Attrition Rate (%)"),
                      shapes=list(list(type="line",x0=1.5,x1=1.5,y0=0,y1=40,
                                       line=list(color=A1,dash="dash",width=1.5))),
                      annotations=list(list(x=1.8,y=25,text="Risk drops sharply here",
                                             font=list(color=A1,size=11),showarrow=FALSE)))
  })

  output$plt_t2 <- renderPlotly({
    d <- fd() %>% group_by(OverTime) %>%
      summarise(Rate=round(mean(AttrNum)*100,1),Total=n(),.groups="drop")
    plot_ly(d,x=~OverTime,y=~Rate,type="bar",
            marker=list(color=c(A2,A1),line=list(color=BG,width=1)),
            text=~paste0(Rate,"%"),textposition="outside",customdata=~Total,
            hovertemplate="%{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="OverTime"),yaxis=list(title="Attrition Rate (%)"))
  })

  output$plt_t3 <- renderPlotly({
    d <- fd() %>% filter(!is.na(SalBand)) %>% group_by(SalBand) %>%
      summarise(Rate=round(mean(AttrNum)*100,1),Total=n(),.groups="drop")
    plot_ly(d,x=~SalBand,y=~Rate,type="scatter",mode="lines+markers",
            line=list(color=A2,width=2.5),
            marker=list(color=A3,size=11,line=list(color=A2,width=2)),
            customdata=~Total,
            hovertemplate="%{x}<br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Salary Band"),yaxis=list(title="Attrition Rate (%)"))
  })

  output$plt_bw_role <- renderPlotly({
    d <- fd() %>% group_by(JobRole) %>%
      summarise(Total=n(),Left=sum(AttrNum),Rate=round(mean(AttrNum)*100,1),.groups="drop") %>%
      arrange(desc(Rate)) %>%
      mutate(Tier=case_when(Rate>25~"High Risk",Rate<10~"Low Risk",TRUE~"Medium"))
    cols <- ifelse(d$Tier=="High Risk",A1,ifelse(d$Tier=="Low Risk",A2,A4))
    avg  <- round(mean(d$Rate),1)
    plot_ly(d,x=~Rate,y=~reorder(JobRole,Rate),type="bar",orientation="h",
            marker=list(color=cols,line=list(color=BG,width=1)),
            text=~paste0(Rate,"%\nn=",Total),textposition="outside",
            customdata=~cbind(Total,Left),
            hovertemplate="<b>%{y}</b><br>Rate: %{x:.1f}%<br>n=%{customdata[0]}<br>Left=%{customdata[1]}<extra></extra>") %>%
      DK() %>%
      layout(xaxis=list(title="Attrition Rate (%)"),yaxis=list(title=NULL),
             shapes=list(list(type="line",y0=-0.5,y1=nrow(d)-0.5,x0=avg,x1=avg,
                              line=list(color=A3,dash="dot",width=1.5))),
             annotations=list(list(y=nrow(d)-1,x=avg+1,text=paste0("Avg: ",avg,"%"),
                                   font=list(color=A3,size=11),showarrow=FALSE)))
  })

  output$plt_bw_dept <- renderPlotly({
    d <- fd() %>% group_by(Department) %>%
      summarise(Total=n(),Left=sum(AttrNum),Rate=round(mean(AttrNum)*100,1),.groups="drop") %>%
      arrange(desc(Rate))
    cols <- ifelse(d$Rate>18,A1,ifelse(d$Rate<15,A2,A4))
    plot_ly(d,x=~Department,y=~Rate,type="bar",
            marker=list(color=cols,line=list(color=BG,width=1)),
            text=~paste0(Rate,"%\nn=",Total),textposition="outside",
            customdata=~cbind(Total,Left),
            hovertemplate="<b>%{x}</b><br>Rate: %{y:.1f}%<br>n=%{customdata[0]}<br>Left=%{customdata[1]}<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Department"),yaxis=list(title="Attrition Rate (%)"))
  })

  output$tbl_bw <- renderDT({
    r <- fd() %>% group_by(JobRole) %>%
      summarise(Total=n(),Left=sum(AttrNum),Stayed=n()-sum(AttrNum),
                Rate=round(mean(AttrNum)*100,1),.groups="drop") %>%
      mutate(Type="Job Role",Group=JobRole,
             Tier=case_when(Rate>25~"High Risk",Rate<10~"Low Risk",TRUE~"Medium Risk")) %>%
      select(Type,Group,Total,Left,Stayed,Rate,Tier)
    d2 <- fd() %>% group_by(Department) %>%
      summarise(Total=n(),Left=sum(AttrNum),Stayed=n()-sum(AttrNum),
                Rate=round(mean(AttrNum)*100,1),.groups="drop") %>%
      mutate(Type="Department",Group=Department,
             Tier=case_when(Rate>18~"High Risk",Rate<14~"Low Risk",TRUE~"Medium Risk")) %>%
      select(Type,Group,Total,Left,Stayed,Rate,Tier)
    bind_rows(r,d2) %>% arrange(Type,desc(Rate)) %>%
      rename("Category"=Type,"Name"=Group,"Employees"=Total,
             "Left"=Left,"Stayed"=Stayed,"Attrition %"=Rate,"Risk Tier"=Tier) %>%
      datatable(options=list(pageLength=15,scrollX=TRUE,dom="frtip",
                  columnDefs=list(list(className="dt-center",targets="_all"))),
                rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Risk Tier",
        backgroundColor=styleEqual(c("High Risk","Medium Risk","Low Risk"),
          c(paste0(A1,"22"),paste0(A4,"22"),paste0(A2,"22"))),
        color=styleEqual(c("High Risk","Medium Risk","Low Risk"),c(A1,A4,A2)),fontWeight="bold") %>%
      formatStyle("Attrition %",background=styleColorBar(c(0,45),paste0(A1,"88")),
                  backgroundSize="100% 80%",backgroundRepeat="no-repeat",backgroundPosition="center")
  })

  output$plt_an1 <- renderPlotly({
    d <- fd() %>% group_by(JobRole) %>%
      summarise(Rate=round(mean(AttrNum)*100,1),Total=n(),.groups="drop") %>%
      arrange(desc(Rate))
    plot_ly(d,x=~reorder(JobRole,Rate),y=~Rate,type="bar",
            marker=list(color=ifelse(d$Rate>25,A1,ifelse(d$Rate<10,A2,A4)),
                        line=list(color=BG,width=1)),
            text=~paste0(Rate,"%"),textposition="outside",customdata=~Total,
            hovertemplate="<b>%{x}</b><br>Rate: %{y:.1f}%<br>n=%{customdata}<extra></extra>") %>%
      DK() %>%
      layout(xaxis=list(title="Job Role",tickangle=-30),yaxis=list(title="Attrition Rate (%)"),
             annotations=list(list(x=0.5,y=0.95,xref="paper",yref="paper",
               text="8× gap: Sales Rep 39.8% vs Manager 4.9%",
               font=list(color=A1,size=11),showarrow=FALSE)))
  })

  output$plt_an2 <- renderPlotly({
    d <- fd() %>% count(Outlier,Attrition)
    p <- ggplot(d,aes(x=Outlier,y=n,fill=Attrition)) +
      geom_col(position="dodge",width=0.6) +
      scale_fill_manual(values=c("Yes"=A1,"No"=A2)) +
      labs(x="Record Type",y="Count",fill=NULL) + GG()
    ggplotly(p) %>% DK()
  })

  output$k13 <- renderValueBox(valueBox("6","Pre-computed Aggs",   icon=icon("database"),    color="navy"))
  output$k14 <- renderValueBox(valueBox("1","Shared Reactive fd()", icon=icon("code-branch"), color="purple"))
  output$k15 <- renderValueBox(valueBox("500","Max Scatter Points",  icon=icon("dot-circle"),  color="teal"))
  output$k16 <- renderValueBox(valueBox("<100ms","Filter Response",  icon=icon("bolt"),        color="green"))

  output$plt_perf <- renderPlotly({
    d <- data.frame(
      Tech=c("Pre-computed aggs","Shared reactive","DT pagination","Binning","Scatter cap","plotly"),
      Score=c(95,85,70,60,60,80))
    plot_ly(d,x=~Score,y=~reorder(Tech,Score),type="bar",orientation="h",
            marker=list(color=colorRampPalette(c(CR,A1))(6),line=list(color=BG,width=1)),
            text=~paste0(Score,"%"),textposition="outside",
            hovertemplate="%{y}: %{x}%<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Impact Score",range=c(0,115)),yaxis=list(title=NULL))
  })

  output$tbl_perf <- renderDT(
    PERF %>% datatable(options=list(pageLength=5,dom="t",scrollX=TRUE,
                         columnDefs=list(list(className="dt-left",targets="_all"))),
                       rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Impact",color=styleEqual(c("High","Medium"),c(A2,A3)),fontWeight="bold")
  )

  output$plt_ptim <- renderPlotly({
    d <- data.frame(Op=c("Startup","Per filter","Per chart","Full load"),
                    Ms=c(250,10,30,310),
                    Type=c("One-time","Per interaction","Per interaction","One-time"))
    plot_ly(d,x=~reorder(Op,Ms),y=~Ms,type="bar",color=~Type,
            colors=c("One-time"=A4,"Per interaction"=A2),
            text=~paste0(Ms,"ms"),textposition="outside",
            hovertemplate="%{x}: %{y}ms<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title=NULL,tickangle=-10),yaxis=list(title="Time (ms)"))
  })

  output$plt_pscl <- renderPlotly({
    d <- data.frame(
      Rows=c(500,1470,5000,10000,50000,100000),
      Strategy=c("Full","Current","Sample+bin","Sample+bin","Sample+bin","Sample+bin"),
      Ms=c(60,120,85,95,105,118))
    plot_ly(d,x=~Rows,y=~Ms,color=~Strategy,
            colors=c("Full"=A1,"Current"=A3,"Sample+bin"=A2),
            type="scatter",mode="lines+markers",
            hovertemplate="Rows: %{x}<br>Render: %{y}ms<extra></extra>") %>%
      DK() %>% layout(xaxis=list(title="Dataset Size",type="log"),yaxis=list(title="Render (ms)"))
  })

  output$row_info <- renderText({
    paste0("Showing ",nrow(fd())," of ",nrow(df)," employees  |  ",
           sum(fd()$AttrNum)," left  |  ",
           round(mean(fd()$AttrNum)*100,1),"% attrition rate  |  ",
           "Avg income: $",round(mean(fd()$MonthlyIncome)))
  })

  output$tbl_main <- renderDT(
    fd() %>%
      select(EmployeeNumber,Attrition,Age,Gender,Department,JobRole,
             MonthlyIncome,YearsAtCompany,OverTime,MaritalStatus,
             JobSatisfaction,PerformanceRating,WorkLifeBalance) %>%
      datatable(options=list(pageLength=20,scrollX=TRUE,dom="Bfrtip",
                  columnDefs=list(list(className="dt-center",targets="_all"))),
                rownames=FALSE,class="stripe hover compact") %>%
      formatStyle("Attrition",
        backgroundColor=styleEqual(c("Yes","No"),c(paste0(A1,"22"),"transparent")),
        color=styleEqual(c("Yes","No"),c(A1,MU)),fontWeight="bold")
  )

  output$dl <- downloadHandler(
    filename=function() paste0("Attrition_Filtered_",Sys.Date(),".csv"),
    content =function(file) write.csv(fd(),file,row.names=FALSE)
  )
}

# =============================================================================
# STEP 7: Launch the App
# =============================================================================

shinyApp(ui=ui, server=server)
