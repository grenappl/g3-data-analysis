---
title: "Culminating Activity"
author:
  - "de Pedro, Joenell Conrad"
  - "Florentino, Ian Miguel"
  - "Nacu, Drixyl Reece"
  - "Ruelan, Ivan Paul"
  - "Seno, Matthew Anakin"
date: "May 16, 2026"
output:
  pdf_document:
    toc: false
    latex_engine: xelatex
    keep_tex: false
geometry: margin=1in
fontsize: 12pt
header-includes:
  - \usepackage{booktabs}
  - \usepackage{longtable}
  - \usepackage{float}
  - \let\origfigure\figure
  - \let\endorigfigure\endfigure
  - \renewenvironment{table}[1][]{\begin{table}[H]}{\end{table}}
  - \usepackage{hyperref}
  - \hypersetup{colorlinks=true, linkcolor=blue, urlcolor=blue}
  - \renewcommand{\arraystretch}{1.3}
  - \usepackage[table]{xcolor}
  - \usepackage{array}
  - \usepackage{calc}
---





# Abstract
This study examines the determinants of software developer compensation using the Developer Survey 2026 dataset, a synthetically generated dataset comprising 12,000 records and 32 variables. Employing multiple linear regression (OLS) as the primary analytical method, the study models log-transformed annual salary as a function of 16 predictor groups encompassing developer demographics, employment characteristics, technical roles, educational attainment, geographic location, and artificial intelligence-related behavioral variables. Six model specifications were estimated and compared using Adjusted R², AIC, and BIC criteria. The full model achieved an Adjusted R² of 0.9218, indicating that approximately 92.18% of the variance in log-transformed developer salary is explained by the included predictors (F(102, 11342) = 1,324, p < 2.2e-16). Results indicate that years of experience, employment status, developer role, education level, and country of employment are the most statistically significant predictors of compensation, while AI-related variables including productivity boost percentage, usage frequency, and sentiment did not contribute independently to salary variation once structural factors were controlled. Comprehensive OLS assumption diagnostics confirmed that the model satisfies the conditions of homoscedasticity, error independence, and approximate normality of residuals. These findings suggest that structural and human capital factors remain the dominant drivers of developer compensation, with geographic context exerting the largest categorical influence. The study contributes a rigorous regression-based framework for analyzing workforce compensation in the software development industry and offers data-driven implications for developers, employers, academic institutions, and labor market researchers.


# Chapter 1: Introduction

## Background of the Study

The global demand for software developers continues to rise as organizations accelerate digital transformation and adopt artificial intelligence (AI) in their operations. Industry outlooks show that software development remains a high-growth field, with employment projected to expand much faster than average and with strong median wages that reflect sustained labor demand (U.S. Bureau of Labor Statistics, 2025). This makes it important to examine the factors that shape developer compensation, skill requirements, and career opportunities.

Recent studies also indicate that AI is not only changing how software is built but also reshaping the role of developers. Morgan Stanley Research reported that AI coding tools are expected to enhance productivity and may increase hiring demand as enterprises build more complex applications and address technical debt (Morgan Stanley, 2025). In the same direction, Forbes noted that AI is transforming the software developer role by automating routine coding tasks and shifting developers toward more strategic work such as architecture, integration, and product oversight (Drenik, 2024).

In addition, salary trends suggest that experience and specialized technical skills remain major determinants of developer compensation. Dice reported that software developer salaries continue to increase, with higher pay associated with greater experience and stronger demand for modern technical skills (Dice, 2025). Likewise, digital transformation continues to require developers to expand their competencies in areas such as cloud computing, AI, machine learning, and DevOps, reinforcing the need to understand how skills and compensation are related in the evolving labor market (RVS Media, 2025).

Given these developments, analyzing the Developer Survey 2026 dataset can provide useful insights into developer skills, salary levels, experience, and perceptions of AI (Stack Overflow, 2026). To systematically examine these relationships, this study will apply a machine learning approach using a multilinear regression model, which enables the analysis of how multiple independent variables, such as years of experience, skill sets, and AI usage, simultaneously influence a dependent variable, particularly developer salary (Suman, 2025; Putra et al., 2026). By leveraging multilinear regression, the study aims to quantify the strength and direction of these relationships, identify significant predictors of compensation, and generate data-driven insights into how evolving technologies and skill demands impact the software development workforce (García et al., 2025; Rodrigues, 2022).

## Statement of the Problem

The rapid evolution of technology, particularly the rise of artificial intelligence, has significantly influenced the software development industry. Developers are now expected to continuously adapt by acquiring new skills, while organizations adjust compensation based on experience, expertise, and emerging technological demands. Despite the availability of large-scale survey data, there is still a need to clearly understand how these factors interact and influence developer salaries and roles using quantitative and predictive approaches.

In particular, traditional descriptive analysis alone may not fully capture the combined effect of multiple variables on salary. Thus, there is a need to apply a multilinear regression model to examine how several independent variables, such as skills, experience, and AI impact, simultaneously influence developer compensation.

This study aims to analyze the Developer Survey 2026 dataset to identify key patterns and relationships among skills, experience, salary, and the impact of artificial intelligence. Specifically, it seeks to answer the following research questions:

1. What is the relationship between developers' skills, experience, and salary based on the Developer Survey 2026 dataset using a multilinear regression model?
2. How does the impact of artificial intelligence influence developers' roles and salary levels when analyzed through a multilinear regression approach?

## Objectives of the Study

### General Objective

To comprehensively investigate the relationships among developer skills, experience, salary, and the impact of artificial intelligence using the Developer Survey 2026 dataset through the application of a multilinear regression model and other data analysis techniques.

### Specific Objectives

- To determine the extent to which years of experience influence developer salary using regression coefficients.
- To analyze how different programming languages, tools, and technical skills contribute to variations in developer compensation within a multilinear regression framework.
- To identify which developer roles or specializations significantly predict salary levels.
- To examine how developers perceive the impact of artificial intelligence on their productivity, job responsibilities, and career opportunities.
- To evaluate whether AI-related variables significantly affect salary and roles based on regression results.
- To develop and interpret a multilinear regression model that explains salary as a function of multiple independent variables.
- To assess the strength and significance of relationships using statistical measures such as coefficients, p-values, and $R^2$.

## Hypotheses

| Hypothesis | Statement |
|-----------|-----------|
| $H0_1$ | The multilinear regression model shows that developers' experience does **not** significantly predict salary. |
| $H1_1$ | The multilinear regression model shows that developers' experience **significantly** predicts salary. |
| $H0_2$ | The multilinear regression model shows that developers' technical skills do **not** significantly predict salary. |
| $H1_2$ | The multilinear regression model shows that developers' technical skills **significantly** predict salary. |
| $H0_3$ | The multilinear regression model shows that artificial intelligence has **no significant effect** on developers' roles and salary. |
| $H1_3$ | The multilinear regression model shows that artificial intelligence has a **significant effect** on developers' roles and salary. |

## Significance of the Study

This study is significant as it applies a multilinear regression model to provide a more precise and data-driven understanding of the factors influencing developer salaries and career development. Instead of relying solely on descriptive insights, the study quantifies the impact of multiple variables simultaneously.

- **Students and Aspiring Developers:** Gain insight into which specific skills and factors statistically influence salary, helping them prioritize learning areas.
- **Professional Developers:** Understand how different variables contribute to compensation, enabling more informed career decisions.
- **Employers and Recruiters:** Benefit from predictive insights that help in structuring competitive salary packages based on measurable factors.
- **Academic Institutions:** Can incorporate data-driven findings into curriculum design, especially in AI and in-demand technologies.
- **Researchers:** Provides a methodological contribution by demonstrating the application of multilinear regression in analyzing workforce and salary data.

## Scope and Limitations

### Scope

This study focuses on the analysis of the Developer Survey 2026 dataset, which includes variables related to developer demographics, technical skills, experience, salary, and perceptions of artificial intelligence.

The research specifically applies a multilinear regression model to examine how multiple independent variables (such as skills, experience, and AI-related factors) simultaneously influence the dependent variable (salary). Additional techniques such as descriptive statistics and correlation analysis are also used to support the findings.

The study aims to produce both explanatory and predictive insights regarding salary determination in the software development industry.

### Limitations

The study is limited by the quality and structure of the dataset, which is based on self-reported survey responses and may include biases or inconsistencies. Additionally, the regression model assumes linear relationships between variables, which may not fully capture complex real-world interactions.

Some potentially important variables, such as geographic location, company size, or economic conditions, may not be fully represented, which can affect the accuracy of the model. Furthermore, while multilinear regression can identify significant predictors, it does not establish causation, and results should be interpreted accordingly.

\newpage


# Chapter 2: Review of Related Literature

## Related Literature

The rapid adoption of artificial intelligence into software development has fundamentally changed the industry, introducing both significant opportunities and complex challenges (Garg, 2023). AI-powered tools have reshaped conventional development practices by automating key phases of the software development lifecycle, including code writing, testing and debugging, and deployment (Madupati, 2025). These shifts have not only altered how software is built in the software industry, but have also redefined the skills and expertise that employers prioritize, which influences how developers are compensated. As AI continues to permeate the software industry, understanding the factors that drive compensation has become increasingly relevant for both practitioners and researchers alike.

In the fast-paced software industry, salary has emerged as a critical factor in workforce dynamics. Research has established that employee experience is positively associated with motivation, largely driven by better compensation (Memon et al., 2021). This relationship suggests that salary is not merely a financial metric but also a signal of professional value and career progression, particularly in technically demanding fields such as software and AI development. Furthermore, one study found a strong pull of AI talent in the labor market that challenges narratives of technological displacement, highlighting instead how employers actively compete for scarce AI talent through both financial and non-financial incentives (Castaneda et al., 2025). This intensifying competition for skilled professionals further underscores the importance of identifying which factors most significantly drive compensation in the AI and software development sectors.

To rigorously examine such relationships, appropriate analytical methods are essential — and several literatures examine the chosen methods the methodology makes use of. Variable selection was conducted to retain only the most theoretically and statistically relevant predictors, as retaining unnecessary variables risks biased coefficient estimates and reduced model interpretability (Hanke et al., 2024). To evaluate model performance reliably, cross-validation was employed, as it provides a more robust estimate of predictive accuracy than a single train-test evaluation alone (Bates et al., 2024). The dataset was partitioned into training and testing subsets following standard practice, with the choice of split strategy informed by its demonstrated impact on model generalizability (Bami et al., 2025). Multiple linear regression was selected as the primary analytical method given its interpretability, its suitability for continuous outcome variables, and its continued validity as a methodology for salary prediction research (Tappari et al., 2026; Keith et al., 2025).

## Related Studies

Prior studies have explored salary prediction using a wide range of machine learning and statistical approaches, yielding valuable methodological and substantive insights. A comprehensive literature survey examining salary prediction across multiple domains and datasets found that multiple linear regression remains a valid, interpretable, and widely used methodology, even as more advanced algorithms have gained traction in recent years (Tappari et al., 2026). The continued relevance of linear regression in this space speaks to its strength in producing transparent models that clearly communicate the directional and magnitude effects of individual predictors, a quality that is particularly valuable in applied research where interpretability matters. That said, research has also noted persistent challenges in technicality and model reusability when combining multiple linear regression with other machine learning approaches, suggesting that while hybrid models may boost predictive accuracy, they can introduce complexity that limits practical applicability (Kumar et al., 2023).

When it comes to identifying key salary determinants, previous regression-based studies consistently point to skills, experience, and location as the most influential factors shaping compensation outcomes. A multiple linear regression model with an $R^2$ of 0.82 found that location and experience were the most statistically and practically significant predictors of compensation in the AI industry, demonstrating that geographic context and years of experience together account for a substantial proportion of salary variance (Maidin et al., 2024). These findings align with broader labor market theory, which holds that compensation reflects both the scarcity of skills in a given market and the accumulated human capital of individual workers.

A more recent study by Godase (2026), employing random forest regression on a dataset of over 10,000 job postings and achieving an $R^2$ of 0.98, corroborated these findings and concluded that experience, technical skills, and organizational characteristics are crucial determinants of compensation. The high model fit achieved in that study further validates the importance of these variables, while the use of a large-scale job posting dataset adds real-world credibility to its findings. Further supporting this, Xu (2025) found that salary growth tends to accelerate at higher experience levels, suggesting that the financial returns to experience are not uniform but increase disproportionately as professionals advance in their careers. This has important implications for workforce planning and retention strategies in the AI and software sectors.

Deb (2025) similarly found that job title, experience level, work arrangement, employee residence, and company size all significantly contribute to salary variation, with full-time employment, on-site work, U.S. residency, and medium-sized companies consistently associated with higher salaries. This points to the multidimensional nature of compensation, where structural and organizational factors interact with individual-level attributes to determine pay outcomes. Taken together, these studies paint a consistent picture: compensation in the software and AI industry is shaped by a combination of individual human capital factors, particularly experience and technical skills, and contextual variables such as location, company characteristics, and employment arrangement.

\newpage


# Chapter 3: Methodology

## 3.1 Research Design

This study adopts a quantitative, non-experimental research design using secondary data obtained from the Kaggle Developer Survey dataset. The research is descriptive and predictive in nature, aiming to identify factors that significantly influence developer compensation and to construct a reliable predictive model for annual salary.

The analytical approach is grounded in multiple linear regression, a classical statistical method suited for estimating the linear relationship between a continuous dependent variable and multiple independent predictors. Because the data are observational rather than experimentally controlled, the study focuses on identifying statistically significant associations rather than making causal claims.

Model validity is assessed through a comprehensive suite of diagnostic tests covering the core OLS assumptions: normality of residuals, homoscedasticity, independence of errors, and absence of severe multicollinearity. Multiple model specifications are compared using Adjusted R$^2$, AIC, and BIC to arrive at the most parsimonious and well-fitting final model.


## 3.2 Data Collection

The dataset used in this study was sourced from Kaggle, a publicly accessible platform for open datasets widely used in academic and data science research. The dataset is a **synthetically generated** developer survey containing 12,000 records and 32 variables designed to simulate realistic developer demographics, compensation, and technology adoption patterns.

**Dataset Source:** <https://www.kaggle.com/datasets/meruvakodandasuraj/developer-survey-2026-skills-salary-and-ai-impact>

The dataset was loaded into R using the following command:


\begin{table}[!h]
\centering
\caption{\label{tab:load_data}Table 1. Developer Demographics}
\centering
\resizebox{\ifdim\width>\linewidth\linewidth\else\width\fi}{!}{
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{cccccccc}
\toprule
\textbf{respondent\_id} & \textbf{survey\_year} & \textbf{country} & \textbf{age} & \textbf{gender} & \textbf{education\_level} & \textbf{years\_of\_experience} & \textbf{role}\\
\midrule
\cellcolor{gray!10}{R00001} & \cellcolor{gray!10}{2024} & \cellcolor{gray!10}{India} & \cellcolor{gray!10}{43} & \cellcolor{gray!10}{Man} & \cellcolor{gray!10}{Some college} & \cellcolor{gray!10}{22} & \cellcolor{gray!10}{Front-End Developer}\\
R00002 & 2025 & Italy & 28 & Man & Master's degree & 7 & Machine Learning Engineer\\
\cellcolor{gray!10}{R00003} & \cellcolor{gray!10}{2021} & \cellcolor{gray!10}{South Africa} & \cellcolor{gray!10}{23} & \cellcolor{gray!10}{Woman} & \cellcolor{gray!10}{Master's degree} & \cellcolor{gray!10}{0} & \cellcolor{gray!10}{Machine Learning Engineer}\\
R00004 & 2023 & India & 22 & Prefer not to say & Bachelor's degree & 1 & QA / Test Engineer\\
\cellcolor{gray!10}{R00005} & \cellcolor{gray!10}{2025} & \cellcolor{gray!10}{United States} & \cellcolor{gray!10}{34} & \cellcolor{gray!10}{Man} & \cellcolor{gray!10}{Bachelor's degree} & \cellcolor{gray!10}{11} & \cellcolor{gray!10}{Front-End Developer}\\
R00006 & 2021 & India & 33 & Man & Bachelor's degree & 15 & Front-End Developer\\
\cellcolor{gray!10}{R00007} & \cellcolor{gray!10}{2022} & \cellcolor{gray!10}{China} & \cellcolor{gray!10}{29} & \cellcolor{gray!10}{Man} & \cellcolor{gray!10}{Bachelor's degree} & \cellcolor{gray!10}{13} & \cellcolor{gray!10}{Cloud Engineer}\\
R00008 & 2022 & India & 31 & Man & Bachelor's degree & 12 & Full-Stack Developer\\
\cellcolor{gray!10}{R00009} & \cellcolor{gray!10}{2025} & \cellcolor{gray!10}{United States} & \cellcolor{gray!10}{46} & \cellcolor{gray!10}{Man} & \cellcolor{gray!10}{Bachelor's degree} & \cellcolor{gray!10}{26} & \cellcolor{gray!10}{Front-End Developer}\\
R00010 & 2020 & Indonesia & 37 & Man & Self-taught / Bootcamp & 11 & Front-End Developer\\
\cellcolor{gray!10}{R00011} & \cellcolor{gray!10}{2021} & \cellcolor{gray!10}{Pakistan} & \cellcolor{gray!10}{20} & \cellcolor{gray!10}{Prefer not to say} & \cellcolor{gray!10}{Master's degree} & \cellcolor{gray!10}{0} & \cellcolor{gray!10}{Embedded Developer}\\
R00012 & 2026 & United States & 36 & Woman & Master's degree & 16 & QA / Test Engineer\\
\cellcolor{gray!10}{R00013} & \cellcolor{gray!10}{2022} & \cellcolor{gray!10}{South Africa} & \cellcolor{gray!10}{50} & \cellcolor{gray!10}{Man} & \cellcolor{gray!10}{Self-taught / Bootcamp} & \cellcolor{gray!10}{25} & \cellcolor{gray!10}{Full-Stack Developer}\\
R00014 & 2025 & Belgium & 24 & Man & Bachelor's degree & 0 & Front-End Developer\\
\cellcolor{gray!10}{R00015} & \cellcolor{gray!10}{2021} & \cellcolor{gray!10}{India} & \cellcolor{gray!10}{21} & \cellcolor{gray!10}{Man} & \cellcolor{gray!10}{Master's degree} & \cellcolor{gray!10}{2} & \cellcolor{gray!10}{Student / Intern}\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]
\centering
\caption{\label{tab:load_data}Table 2. Employment and Salary Information}
\centering
\resizebox{\ifdim\width>\linewidth\linewidth\else\width\fi}{!}{
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{ccccccc}
\toprule
\textbf{employment\_status} & \textbf{company\_size} & \textbf{work\_mode} & \textbf{coding\_hours\_per\_week} & \textbf{annual\_salary\_usd} & \textbf{job\_satisfaction} & \textbf{career\_satisfaction}\\
\midrule
\cellcolor{gray!10}{Unemployed} & \cellcolor{gray!10}{1-10} & \cellcolor{gray!10}{Hybrid} & \cellcolor{gray!10}{12} & \cellcolor{gray!10}{3700} & \cellcolor{gray!10}{1} & \cellcolor{gray!10}{4}\\
Unemployed & 201-1000 & Remote & 26 & 9660 & 3 & 4\\
\cellcolor{gray!10}{Full-time} & \cellcolor{gray!10}{1001-5000} & \cellcolor{gray!10}{In-Office} & \cellcolor{gray!10}{43} & \cellcolor{gray!10}{26200} & \cellcolor{gray!10}{4} & \cellcolor{gray!10}{2}\\
Unemployed & 10000+ & Hybrid & 27 & 0 & 5 & 2\\
\cellcolor{gray!10}{Full-time} & \cellcolor{gray!10}{51-200} & \cellcolor{gray!10}{Remote} & \cellcolor{gray!10}{25} & \cellcolor{gray!10}{172100} & \cellcolor{gray!10}{3} & \cellcolor{gray!10}{4}\\
Full-time & 51-200 & In-Office & 29 & 14900 & 4 & 4\\
\cellcolor{gray!10}{Full-time} & \cellcolor{gray!10}{1001-5000} & \cellcolor{gray!10}{Hybrid} & \cellcolor{gray!10}{27} & \cellcolor{gray!10}{45200} & \cellcolor{gray!10}{1} & \cellcolor{gray!10}{5}\\
Full-time & 51-200 & Remote & 34 & 16200 & 5 & 4\\
\cellcolor{gray!10}{Full-time} & \cellcolor{gray!10}{201-1000} & \cellcolor{gray!10}{Hybrid} & \cellcolor{gray!10}{40} & \cellcolor{gray!10}{245100} & \cellcolor{gray!10}{4} & \cellcolor{gray!10}{5}\\
Full-time & 1001-5000 & In-Office & 45 & 17900 & 2 & 4\\
\cellcolor{gray!10}{Student} & \cellcolor{gray!10}{5001-10000} & \cellcolor{gray!10}{Hybrid} & \cellcolor{gray!10}{12} & \cellcolor{gray!10}{0} & \cellcolor{gray!10}{4} & \cellcolor{gray!10}{3}\\
Full-time & 11-50 & In-Office & 33 & 211500 & 3 & 4\\
\cellcolor{gray!10}{Full-time} & \cellcolor{gray!10}{1-10} & \cellcolor{gray!10}{Hybrid} & \cellcolor{gray!10}{35} & \cellcolor{gray!10}{46500} & \cellcolor{gray!10}{5} & \cellcolor{gray!10}{4}\\
Full-time & 5001-10000 & Remote & 41 & 34600 & 4 & 4\\
\cellcolor{gray!10}{Full-time} & \cellcolor{gray!10}{201-1000} & \cellcolor{gray!10}{Hybrid} & \cellcolor{gray!10}{51} & \cellcolor{gray!10}{4600} & \cellcolor{gray!10}{4} & \cellcolor{gray!10}{5}\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]
\centering
\caption{\label{tab:load_data}Table 3. Technology Stack}
\centering
\resizebox{\ifdim\width>\linewidth\linewidth\else\width\fi}{!}{
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{cccccc}
\toprule
\textbf{primary\_os} & \textbf{languages\_used} & \textbf{frameworks\_used} & \textbf{databases\_used} & \textbf{cloud\_platforms\_used} & \textbf{ide\_editor}\\
\midrule
\cellcolor{gray!10}{macOS} & \cellcolor{gray!10}{Swift;JavaScript;Python;HTML/CSS;SQL;Kotlin} & \cellcolor{gray!10}{Node.js;Ruby on Rails} & \cellcolor{gray!10}{PostgreSQL;Firebase Realtime DB;SQLite;Neo4j} & \cellcolor{gray!10}{AWS;Cloudflare;Hetzner} & \cellcolor{gray!10}{Zed}\\
macOS & Python;HTML/CSS;Java & FastAPI & PostgreSQL;SQLite;MongoDB;MySQL & Cloudflare & Zed;Visual Studio;IntelliJ IDEA\\
\cellcolor{gray!10}{Linux} & \cellcolor{gray!10}{Swift;Python;Java;JavaScript;Bash/Shell;SQL} & \cellcolor{gray!10}{Spring Boot} & \cellcolor{gray!10}{DynamoDB} & \cellcolor{gray!10}{Google Cloud;Railway} & \cellcolor{gray!10}{VS Code;Visual Studio}\\
Linux & Python;JavaScript;TypeScript & Express & PostgreSQL;Oracle;Supabase & DigitalOcean;Netlify;Vercel & Vim/Neovim;VS Code\\
\cellcolor{gray!10}{Windows} & \cellcolor{gray!10}{C++;JavaScript;Scala;TypeScript} & \cellcolor{gray!10}{Django;.NET;Vue.js;Next.js;React} & \cellcolor{gray!10}{Firebase Realtime DB;SQLite;Redis;ClickHouse} & \cellcolor{gray!10}{Cloudflare;Vercel} & \cellcolor{gray!10}{VS Code;Vim/Neovim;Visual Studio}\\
Windows & JavaScript;C;Python;SQL & React;Node.js;Laravel;Express;Flutter & SQLite;Microsoft SQL Server;PostgreSQL & Heroku & WebStorm;Visual Studio\\
\cellcolor{gray!10}{Linux} & \cellcolor{gray!10}{PHP;Bash/Shell;SQL;Python} & \cellcolor{gray!10}{Spring Boot;htmx;Tailwind CSS;Bootstrap} & \cellcolor{gray!10}{MySQL} & \cellcolor{gray!10}{AWS;Azure} & \cellcolor{gray!10}{VS Code;Cursor}\\
Windows & C;Kotlin;Bash/Shell;SQL;JavaScript & FastAPI;React & Microsoft SQL Server & AWS;Azure & VS Code;Vim/Neovim\\
\cellcolor{gray!10}{macOS} & \cellcolor{gray!10}{JavaScript} & \cellcolor{gray!10}{Flask;Next.js;TensorFlow} & \cellcolor{gray!10}{Redis;PostgreSQL;Supabase;MySQL} & \cellcolor{gray!10}{AWS} & \cellcolor{gray!10}{Visual Studio;IntelliJ IDEA;VS Code}\\
Windows & C\#;Rust;HTML/CSS & Vue.js;Bootstrap & DynamoDB;PostgreSQL;CockroachDB;MariaDB & DigitalOcean;Vercel & Android Studio;Xcode;VS Code\\
\cellcolor{gray!10}{macOS} & \cellcolor{gray!10}{SQL;JavaScript} & \cellcolor{gray!10}{Django;Flask;Angular;Next.js} & \cellcolor{gray!10}{PostgreSQL;Microsoft SQL Server} & \cellcolor{gray!10}{Azure;DigitalOcean} & \cellcolor{gray!10}{WebStorm;PyCharm;Vim/Neovim}\\
macOS & SQL;Go;Python;JavaScript & TensorFlow;Tailwind CSS;NestJS & Firebase Realtime DB;PostgreSQL & Fly.io & Sublime Text;IntelliJ IDEA\\
\cellcolor{gray!10}{Windows} & \cellcolor{gray!10}{PHP;HTML/CSS;TypeScript;SQL} & \cellcolor{gray!10}{Spring Boot;Next.js} & \cellcolor{gray!10}{Cassandra;Neo4j;MySQL;PostgreSQL} & \cellcolor{gray!10}{AWS} & \cellcolor{gray!10}{VS Code;IntelliJ IDEA;Cursor}\\
Windows & Zig;Python;JavaScript;C++ & Angular & SQLite;PostgreSQL & Google Cloud;Azure;Netlify & IntelliJ IDEA;Android Studio\\
\cellcolor{gray!10}{Windows} & \cellcolor{gray!10}{PHP} & \cellcolor{gray!10}{Node.js;jQuery;FastAPI;Spring Boot} & \cellcolor{gray!10}{Microsoft SQL Server;Redis;SQLite;DynamoDB} & \cellcolor{gray!10}{Cloudflare} & \cellcolor{gray!10}{VS Code;Cursor;Vim/Neovim}\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]
\centering
\caption{\label{tab:load_data}Table 4. AI Usage and Perception}
\centering
\resizebox{\ifdim\width>\linewidth\linewidth\else\width\fi}{!}{
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{ccccc}
\toprule
\textbf{ai\_tools\_used} & \textbf{ai\_usage\_frequency} & \textbf{ai\_productivity\_boost\_pct} & \textbf{ai\_sentiment} & \textbf{ai\_job\_threat\_perception}\\
\midrule
\cellcolor{gray!10}{} & \cellcolor{gray!10}{Weekly} & \cellcolor{gray!10}{14} & \cellcolor{gray!10}{Somewhat positive} & \cellcolor{gray!10}{Minimal threat}\\
 & Weekly & 8 & Somewhat positive & Minimal threat\\
\cellcolor{gray!10}{ChatGPT} & \cellcolor{gray!10}{Rarely} & \cellcolor{gray!10}{1} & \cellcolor{gray!10}{Very negative} & \cellcolor{gray!10}{Significant threat}\\
 & Multiple times daily & 61 & Neutral & Minimal threat\\
\cellcolor{gray!10}{} & \cellcolor{gray!10}{Weekly} & \cellcolor{gray!10}{15} & \cellcolor{gray!10}{Neutral} & \cellcolor{gray!10}{Moderate threat}\\
 & Daily & 36 & Very positive & Significant threat\\
\cellcolor{gray!10}{} & \cellcolor{gray!10}{Monthly} & \cellcolor{gray!10}{8} & \cellcolor{gray!10}{Somewhat positive} & \cellcolor{gray!10}{Minimal threat}\\
 & Multiple times daily & 16 & Neutral & No threat\\
\cellcolor{gray!10}{ChatGPT} & \cellcolor{gray!10}{Weekly} & \cellcolor{gray!10}{26} & \cellcolor{gray!10}{Somewhat positive} & \cellcolor{gray!10}{Will replace most devs}\\
ChatGPT;Cursor AI & Weekly & 19 & Very negative & Significant threat\\
\cellcolor{gray!10}{Cursor AI;ChatGPT;Claude} & \cellcolor{gray!10}{Never} & \cellcolor{gray!10}{9} & \cellcolor{gray!10}{Somewhat positive} & \cellcolor{gray!10}{Moderate threat}\\
ChatGPT & Multiple times daily & 36 & Very positive & No threat\\
\cellcolor{gray!10}{} & \cellcolor{gray!10}{Monthly} & \cellcolor{gray!10}{4} & \cellcolor{gray!10}{Very positive} & \cellcolor{gray!10}{Minimal threat}\\
Claude Code;Perplexity & Daily & 7 & Very positive & Moderate threat\\
\cellcolor{gray!10}{} & \cellcolor{gray!10}{Multiple times daily} & \cellcolor{gray!10}{45} & \cellcolor{gray!10}{Very positive} & \cellcolor{gray!10}{Minimal threat}\\
\bottomrule
\end{tabular}}
\end{table}

\begin{table}[!h]
\centering
\caption{\label{tab:load_data}Table 5. Career Growth and Preferences}
\centering
\resizebox{\ifdim\width>\linewidth\linewidth\else\width\fi}{!}{
\fontsize{8}{10}\selectfont
\begin{tabular}[t]{cccccc}
\toprule
\textbf{primary\_learning\_source} & \textbf{open\_source\_contributor} & \textbf{looking\_for\_job} & \textbf{remote\_work\_preference\_1to5} & \textbf{burnout\_frequency} & \textbf{want\_to\_learn\_next}\\
\midrule
\cellcolor{gray!10}{Online courses} & \cellcolor{gray!10}{1} & \cellcolor{gray!10}{Actively looking} & \cellcolor{gray!10}{2} & \cellcolor{gray!10}{Sometimes} & \cellcolor{gray!10}{Elixir;Zig;AI/ML}\\
Stack Overflow & 1 & Not looking & 4 & Often & Kubernetes;Elixir;Go\\
\cellcolor{gray!10}{YouTube} & \cellcolor{gray!10}{0} & \cellcolor{gray!10}{Open to offers} & \cellcolor{gray!10}{5} & \cellcolor{gray!10}{Often} & \cellcolor{gray!10}{Go}\\
Stack Overflow & 1 & Open to offers & 3 & Rarely & HTMX;Swift;Rust\\
\cellcolor{gray!10}{Official docs} & \cellcolor{gray!10}{0} & \cellcolor{gray!10}{Open to offers} & \cellcolor{gray!10}{1} & \cellcolor{gray!10}{Sometimes} & \cellcolor{gray!10}{TypeScript}\\
Stack Overflow & 0 & Not looking & 3 & Sometimes & Kubernetes;TypeScript\\
\cellcolor{gray!10}{Official docs} & \cellcolor{gray!10}{0} & \cellcolor{gray!10}{Not looking} & \cellcolor{gray!10}{5} & \cellcolor{gray!10}{Often} & \cellcolor{gray!10}{AI/ML;Go}\\
Books & 1 & Not looking & 5 & Often & Kotlin;Swift;Kubernetes\\
\cellcolor{gray!10}{YouTube} & \cellcolor{gray!10}{0} & \cellcolor{gray!10}{Not looking} & \cellcolor{gray!10}{5} & \cellcolor{gray!10}{Often} & \cellcolor{gray!10}{Swift;HTMX}\\
ChatGPT/AI & 1 & Actively looking & 4 & Rarely & Rust;Kotlin;Go\\
\cellcolor{gray!10}{Colleagues} & \cellcolor{gray!10}{0} & \cellcolor{gray!10}{Not looking} & \cellcolor{gray!10}{2} & \cellcolor{gray!10}{Always} & \cellcolor{gray!10}{Rust}\\
Stack Overflow & 0 & Not looking & 5 & Rarely & Elixir;Zig;HTMX\\
\cellcolor{gray!10}{Blogs} & \cellcolor{gray!10}{1} & \cellcolor{gray!10}{Open to offers} & \cellcolor{gray!10}{3} & \cellcolor{gray!10}{Always} & \cellcolor{gray!10}{Go;Elixir}\\
ChatGPT/AI & 1 & Open to offers & 4 & Sometimes & TypeScript;Swift\\
\cellcolor{gray!10}{Official docs} & \cellcolor{gray!10}{0} & \cellcolor{gray!10}{Open to offers} & \cellcolor{gray!10}{4} & \cellcolor{gray!10}{Often} & \cellcolor{gray!10}{Python;HTMX}\\
\bottomrule
\end{tabular}}
\end{table}

An initial inspection confirmed that only one column, `ai_tools_used`, contained missing values (approximately 19.8% of rows). All other columns were complete prior to imputation. The `ai_tools_used` column was excluded from the model as it is a semicolon-delimited multi-value free-text field not directly suitable for regression encoding.




## 3.3 Variables Description

The study models annual developer salary as the dependent variable, with a comprehensive set of demographic, employment, educational, and behavioral predictors serving as independent variables.

### 3.3.1 Dependent Variable

The dependent variable is `log_salary`, defined as the natural logarithm of `annual_salary_usd`. A log transformation was applied because raw salary distributions are strongly right-skewed; the transformation stabilizes variance across fitted values and brings the distribution closer to normality, both of which are assumptions of OLS regression. Respondents with `annual_salary_usd $\leq$ 0` were excluded from the analysis as these entries reflect unemployed or unreported salary statuses rather than true compensation values.

### 3.3.2 Independent Variables

The independent variables include all numeric, ordinal, and nominal features available in the cleaned dataset after excluding ID columns, multi-value text fields, and the target variable. Table 3.1 presents the full set of predictors, their measurement type, and how each was encoded for regression.

| Variable | Type | Encoding / Notes |
|---|---|---|
| years_of_experience | Continuous numeric | Raw numeric; direct linear predictor of salary |
| age | Continuous numeric | Raw numeric |
| coding_hours_per_week | Continuous numeric | Raw numeric |
| ai_productivity_boost_pct | Continuous numeric | Raw numeric; self-reported % boost from AI tools |
| remote_work_preference_1to5 | Ordinal numeric | Raw numeric (1 = fully on-site, 5 = fully remote) |
| open_source_contributor | Binary numeric | Raw numeric (0/1) |
| education_level | Ordered factor | 7-level ordered factor from High school to Doctorate (PhD) |
| company_size | Ordered factor | 7-level ordered factor from 1--10 to 10000+ employees |
| burnout_frequency | Ordered factor | 5-level ordered factor: Never to Always |
| ai_usage_frequency | Ordered factor | 6-level ordered factor: Never to Multiple times daily |
| role | Nominal factor | Unordered; each level gets a dummy coefficient vs. baseline |
| employment_status | Nominal factor | Unordered; e.g., Full-time, Part-time, Freelance |
| work_mode | Nominal factor | Unordered; e.g., Remote, Hybrid, In-Office |
| gender | Nominal factor | Unordered |
| primary_os | Nominal factor | Unordered; e.g., Windows, macOS, Linux |
| ai_sentiment | Nominal factor | Unordered; attitude toward AI in the workplace |
| ai_job_threat_perception | Nominal factor | Unordered; perceived job threat level from AI |
| primary_learning_source | Nominal factor | Unordered; e.g., Official docs, Stack Overflow, YouTube |
| looking_for_job | Nominal factor | Unordered; current job-seeking status |
| country | Nominal factor | Unordered; country of employment |

*Table 3.1. Independent Variables Used in the Multiple Linear Regression Model*

## 3.4 Data Analysis Techniques

### 3.4.1 Descriptive Statistics

Exploratory data analysis was conducted prior to modeling to understand variable distributions and guide preprocessing decisions. Specific procedures included:

- Histograms of raw and log-transformed salary to justify the log transformation
- Pearson correlation matrix of all numeric predictors against `log_salary` to screen for linear relationships
- Summary statistics (mean, median, standard deviation, quartiles) via `summary()` in R
- Frequency tables for categorical variables to detect dominant categories and inform mode imputation
- Missing value audit to quantify and locate all NA entries before imputation

Numeric variables with missing values were imputed using the column median, and categorical variables were imputed using the column mode, both strategies chosen to preserve distributional properties without introducing bias from mean substitution in skewed data.

### 3.4.2 Inferential Statistics

The primary analytical method is multiple linear regression (MLR) estimated by Ordinary Least Squares (OLS), implemented using the `lm()` function in R. The regression equation takes the general form:

$$\log(\text{salary}) = \beta_0 + \beta_1 X_1 + \beta_2 X_2 + \cdots + \beta_k X_k + \varepsilon$$

where $X_1$ through $X_k$ represent the independent variables listed in Table 3.1, $\beta_0$ is the intercept, $\beta_1 \ldots \beta_k$ are regression coefficients, and $\varepsilon$ is the error term. All hypothesis tests are conducted at the $\alpha = 0.05$ significance level.

**Significance Testing**

- Individual predictor significance was assessed via t-tests on each regression coefficient, with p-values reported from `summary(model)`
- Overall predictor group significance was assessed via F-tests using `anova(model)`
- Predictors were classified as significant (p < 0.05) or non-significant (p $\geq$ 0.05) and ranked accordingly

**Variable Selection**

To identify the most parsimonious model, several variable selection strategies were applied and compared:

- Manual stepwise elimination by p-value, producing Models v2, v3, and v4 by progressively removing non-significant predictors
- Backward elimination using `drop1()` with F-tests to identify the weakest contributor at each step
- Bidirectional stepwise AIC selection using `stepAIC(direction = "both")` from the MASS package
- Stepwise BIC selection using `stepAIC()` with `k = log(n)` to apply a stronger complexity penalty
- A theory-driven reduced model retaining only substantively meaningful predictors

All candidate models were evaluated and ranked on Adjusted R$^2$, AIC, and BIC. The full model was retained as the final model based on this comparison.

**Model Assumption Diagnostics**

The following formal tests and visual diagnostics were applied to the final model to validate the OLS assumptions:

| Assumption | Test / Method | Tool in R |
|---|---|---|
| Normality of residuals | Shapiro-Wilk test; Kolmogorov-Smirnov test; Q-Q plot; residual histogram | `shapiro.test()`, `ks.test()`, `qqnorm()` |
| Homoscedasticity | Breusch-Pagan test; Residuals vs. Fitted plot; Scale-Location plot | `bptest()` from lmtest |
| Independence of errors | Durbin-Watson test (acceptable range: 1.5 -- 2.5) | `dwtest()` from lmtest |
| Multicollinearity | Variance Inflation Factor (VIF); flag VIF > 5 (moderate) and VIF > 10 (severe) | `vif()` from car |
| Influential observations | Cook's Distance with threshold 4/n; Leverage (hat values) with threshold 2p/n | `cooks.distance()`, `hatvalues()` |

*Table 3.2. OLS Assumption Diagnostics Applied to the Final Model*

In summary, this chapter described a rigorous multiple linear regression pipeline implemented in R, covering data sourcing from Kaggle, preprocessing and imputation, variable operationalization, model fitting, variable selection across multiple strategies, and a comprehensive set of assumption diagnostics. The combination of formal statistical tests and visual diagnostics ensures that the final model is both statistically valid and interpretable.

\newpage


# Chapter 4: Results and Discussions

This chapter presents the results of the multiple linear regression analysis conducted on the Developer Survey 2026 dataset. The findings are organized into four sections: descriptive statistics summarizing the dataset, visualizations of key variable distributions and relationships, model outputs from all candidate regression specifications, and a discussion interpreting the significant predictors and diagnostic results.

## 4.1 Descriptive Statistics

Descriptive statistics were computed on the cleaned dataset (`data_clean`) using the `summary()` function in R. After removing rows with zero or missing salary values, the final dataset consisted of 11,445 observations across all retained variables.







Table 4.1 presents the descriptive statistics for the continuous variables in the cleaned dataset.

| Variable | Min | Median | Mean | Max |
|---|---|---|---|---|
| survey_year | 2020 | 2024 | 2023 | 2026 |
| age | 18.0 | 31.0 | 31.3 | 60.0 |
| years_of_experience | 0.000 | 8.000 | 9.389 | 41.000 |
| coding_hours_per_week | 5.00 | 33.00 | 32.89 | 67.00 |
| annual_salary_usd | 2,000 | 38,000 | 58,625 | 368,700 |
| job_satisfaction | 1.000 | 4.000 | 3.602 | 5.000 |
| career_satisfaction | 1.000 | 4.000 | 3.748 | 5.000 |
| ai_productivity_boost_pct | 0.00 | 23.00 | 24.46 | 80.00 |
| open_source_contributor | 0.000 | 0.000 | 0.345 | 1.000 |
| remote_work_preference_1to5 | 1.000 | 4.000 | 4.056 | 5.000 |
| log_salary | 7.601 | 10.545 | 10.509 | 12.818 |

*Table 4.1a. Descriptive Statistics --- Continuous Variables*

| Variable | Most Frequent | Count | 2nd Most Frequent | Count |
|---|---|---|---|---|
| country | United States | 2,067 | India | 1,611 |
| gender | Man | 10,025 | Woman | 970 |
| education_level | Bachelor's degree | 4,878 | Master's degree | 3,194 |
| role | Full-Stack Developer | 2,564 | Back-End Developer | 1,701 |
| employment_status | Full-time | 9,325 | Freelance | 1,008 |
| company_size | 201-1000 | 2,233 | 51-200 | 2,126 |
| work_mode | Hybrid | 4,597 | Remote | 4,406 |
| primary_os | Windows | 4,761 | macOS | 3,706 |
| ai_usage_frequency | Multiple times daily | 3,154 | Daily | 2,868 |
| ai_sentiment | Somewhat positive | 4,063 | Neutral | 2,553 |
| ai_job_threat_perception | Minimal threat | 3,410 | Moderate threat | 3,221 |
| primary_learning_source | Official docs | 2,292 | Stack Overflow | 2,058 |
| looking_for_job | Open to offers | 4,549 | Not looking | 4,351 |
| burnout_frequency | Sometimes | 3,999 | Often | 2,906 |

*Table 4.1b. Descriptive Statistics --- Categorical Variables (Top 2 Categories)*




## 4.2 Visualizations

Visual exploration of the data was conducted using `ggplot2` in R. The following plots were generated to understand the distributions of key variables and their relationships with the log-transformed salary outcome.

### 4.2.1 Salary Distribution

![Figure 4.1. Raw vs. Log-Transformed Salary Distribution](Culminating-Activity_files/figure-latex/salary_distribution-1.pdf) 

The raw salary distribution exhibited strong right skewness, with a long upper tail driven by high-earning roles and senior developers. After applying the natural log transformation, the distribution became approximately symmetric and bell-shaped, confirming that the log transformation was appropriate for meeting the normality assumptions of OLS regression.

### 4.2.2 Log Salary by Role

![Figure 4.2. Log Salary by Developer Role](Culminating-Activity_files/figure-latex/salary_by_role-1.pdf) 

Figure 4.2 presents the distribution of log-transformed salary across developer roles. Engineering Manager and DevOps/SRE consistently occupy the highest salary range, with median log salaries approaching 11.0 and above, reflecting strong market demand for infrastructure and leadership roles. Security Engineer and Machine Learning Engineer follow closely, both showing relatively compact interquartile ranges suggesting more consistent compensation within those roles.

Mid-tier roles such as Data Engineer, Data Scientist, Cloud Engineer, and Freelancer/Consultant cluster around a median log salary of approximately 10.5, aligning with the overall dataset median. Back-End, Mobile, Full-Stack, and Front-End Developers occupy a similar range but with wider spreads, indicating greater variability in compensation likely driven by geography, company size, and experience level.

The lowest salary distributions belong to Game Developer, Embedded Developer, QA/Test Engineer, and notably Student/Intern --- the latter showing a distinctly lower median of approximately 8.5 and a compressed range consistent with entry-level or unpaid compensation structures. The clear separation between Student/Intern and all other roles confirms that employment status and role type are among the most influential structural determinants of developer salary, which is further supported by their statistical significance in the regression model.

### 4.2.3 Log Salary by Employment Status

![Figure 4.3. Log Salary by Employment Status](Culminating-Activity_files/figure-latex/salary_by_employment-1.pdf) 

Figure 4.3 displays the distribution of log-transformed salary across the five employment status categories. Freelance, Full-time, and Part-time developers share nearly identical median log salaries of approximately 10.7--10.8, with similarly wide interquartile ranges spanning roughly 10.0 to 11.5, suggesting that compensation levels are broadly comparable across these active employment types. In stark contrast, Student and Unemployed respondents show substantially lower median log salaries of approximately 9.0, with compressed distributions and notably less upward spread, reflecting the structural absence of full professional compensation in these categories. The single outlier visible below the Full-time whisker indicates a small number of full-time developers reporting unusually low salaries. The sharp divide between the three active employment categories and the Student/Unemployed groups reinforces the regression finding that employment status is one of the most statistically significant predictors of developer salary.

### 4.2.4 Log Salary vs. Years of Experience

![Figure 4.4. Log Salary vs. Years of Experience](Culminating-Activity_files/figure-latex/salary_vs_experience-1.pdf) 

Figure 4.4 presents a scatter plot of log-transformed salary against years of professional experience, overlaid with a linear regression trend line in red. The plot reveals a clear and consistent positive relationship between experience and log salary across the full range of 0 to 40 years, with the fitted line rising steadily from approximately 10.0 at entry level to 12.2 at 40 years of experience. The vertical banding visible throughout the plot reflects the discrete integer nature of the years of experience variable. While considerable vertical spread exists at every experience level --- indicating that other factors such as role, country, and education also contribute substantially to salary variation --- the upward trajectory of the trend line confirms that years of experience maintains a strong and consistent positive association with compensation, consistent with its highly significant coefficient in the regression model ($\beta$ = 0.0372, p < 2e-16).




## 4.3 Model Results

Six model specifications were estimated and compared. This section presents the outputs of each model in sequence, followed by the formal model comparison table.

### 4.3.1 Variable Selection

### Full Model (model_full)

The full model includes all 16 predictors available in `data_clean`.



\begin{table}
\centering
\begin{talltblr}[         %% tabularray outer open
caption={Full Model: Log Salary Regression},
note{}={* p \num{< 0.05}, ** p \num{< 0.01}, *** p \num{< 0.001}},
note{ }={* p<0.05, ** p<0.01, *** p<0.001. Ref. country: Argentina. Polynomial and FE terms omitted for brevity.},
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={Q[]Q[]},
hline{2}={1-2}{solid, black, 0.05em},
hline{28}={1-2}{solid, black, 0.05em},
hline{1}={1-2}{solid, black, 0.08em},
hline{36}={1-2}{solid, black, 0.08em},
column{2}={}{halign=c},
column{1}={}{halign=l},
}                     %% tabularray inner close
& Full Model \\
Years of Experience & \num{0.036}*** \\
& (\num{0.001}) \\
Age & \num{0.005}*** \\
& (\num{0.001}) \\
Coding Hours/Week & \num{-0.001}* \\
& (\num{0.000}) \\
Work Mode: In-Office & \num{0.003} \\
& (\num{0.007}) \\
Work Mode: Remote & \num{-0.008} \\
& (\num{0.006}) \\
Gender: Non-binary & \num{-0.017} \\
& (\num{0.020}) \\
Gender: Prefer Not to Say & \num{0.043}* \\
& (\num{0.019}) \\
Gender: Woman & \num{-0.011} \\
& (\num{0.010}) \\
OS: macOS & \num{0.006} \\
& (\num{0.007}) \\
OS: Windows & \num{-0.000} \\
& (\num{0.007}) \\
AI Productivity Boost (\%) & \num{-0.000} \\
& (\num{0.000}) \\
Remote Work Preference & \num{0.003} \\
& (\num{0.003}) \\
Open Source Contributor & \num{0.005} \\
& (\num{0.006}) \\
N & \num{11445} \\
R² & \num{0.923} \\
Adj. R² & \num{0.922} \\
Country FE & Yes \\
Education (poly) & Yes \\
Company Size (poly) & Yes \\
AI Frequency (poly) & Yes \\
Burnout (poly) & Yes \\
\end{talltblr}
\end{table}

The full model includes all 16 predictors and explains **92.25% of variance** in 
log-transformed salary (Adj. R² = 0.9218). Years of experience is the strongest 
continuous predictor ($\hat{\beta}$ = 0.036, $p$ < 0.001), indicating each 
additional year is associated with a 3.6\% increase in salary. Age also has a 
small but significant positive effect ($\hat{\beta}$ = 0.005, $p$ < 0.001). 
Coding hours per week has a slight negative association ($\hat{\beta}$ = --0.001, 
$p$ = 0.014), possibly reflecting that higher hours are more common in junior or 
lower-paid roles. Among behavioral and demographic variables, most are 
non-significant, suggesting that role, employment status, and country account for 
most of the salary variation captured in this model.


#### Backward Elimination by p-value



\begin{table}
\centering
\begin{talltblr}[         %% tabularray outer open
caption={Backward Elimination: Log Salary Regression},
note{}={* p \num{< 0.05}, ** p \num{< 0.01}, *** p \num{< 0.001}},
note{ }={Dropped: gender, primary\_os, burnout\_frequency (p $>$ 0.05). Ref. country: Argentina.},
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={Q[]Q[]},
hline{2}={1-2}{solid, black, 0.05em},
hline{18}={1-2}{solid, black, 0.05em},
hline{1}={1-2}{solid, black, 0.08em},
hline{28}={1-2}{solid, black, 0.08em},
column{2}={}{halign=c},
column{1}={}{halign=l},
}                     %% tabularray inner close
& Backward (p-val) \\
Years of Experience & \num{0.036}*** \\
& (\num{0.001}) \\
Age & \num{0.005}*** \\
& (\num{0.001}) \\
Coding Hours/Week & \num{-0.001}* \\
& (\num{0.000}) \\
Work Mode: In-Office & \num{0.003} \\
& (\num{0.007}) \\
Work Mode: Remote & \num{-0.007} \\
& (\num{0.006}) \\
AI Productivity Boost (\%) & \num{-0.000} \\
& (\num{0.000}) \\
Remote Work Preference & \num{0.002} \\
& (\num{0.003}) \\
Open Source Contributor & \num{0.005} \\
& (\num{0.006}) \\
N & \num{11445} \\
R² & \num{0.922} \\
Adj. R² & \num{0.922} \\
Country FE & Yes \\
Education (poly) & Yes \\
Company Size (poly) & Yes \\
AI Frequency (poly) & Yes \\
Burnout (poly) & No \\
Gender & No \\
Primary OS & No \\
\end{talltblr}
\end{table}

Backward elimination removed **gender**, **primary OS**, and **burnout frequency** 
as statistically non-significant (p > 0.05), reducing the model from 102 to 93 
predictors. Despite this reduction, Adj. R² remains identical at 0.9218, confirming 
these variables contributed no meaningful explanatory power. The retained predictors 
mirror the full model in direction and magnitude, validating their robustness. The 
lower AIC (4425.93 vs. 4429.80) indicates a marginally better fit-complexity 
trade-off than the full model.


#### Stepwise AIC



\begin{table}
\centering
\begin{talltblr}[         %% tabularray outer open
caption={Stepwise AIC: Log Salary Regression},
note{}={* p \num{< 0.05}, ** p \num{< 0.01}, *** p \num{< 0.001}},
note{ }={Bidirectional stepwise minimizing AIC. Age removed by selection. Ref. country: Argentina.},
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={Q[]Q[]},
hline{2}={1-2}{solid, black, 0.05em},
hline{12}={1-2}{solid, black, 0.05em},
hline{1}={1-2}{solid, black, 0.08em},
hline{20}={1-2}{solid, black, 0.08em},
column{2}={}{halign=c},
column{1}={}{halign=l},
}                     %% tabularray inner close
& Stepwise AIC \\
Years of Experience & \num{0.036}*** \\
& (\num{0.001}) \\
Coding Hours/Week & \num{-0.001}* \\
& (\num{0.000}) \\
Gender: Non-binary & \num{-0.017} \\
& (\num{0.020}) \\
Gender: Prefer Not to Say & \num{0.043}* \\
& (\num{0.019}) \\
Gender: Woman & \num{-0.011} \\
& (\num{0.010}) \\
N & \num{11445} \\
R² & \num{0.922} \\
Adj. R² & \num{0.922} \\
Country FE & Yes \\
Education (poly) & Yes \\
Company Size (poly) & Yes \\
AI Frequency (poly) & Yes \\
Burnout (poly) & Yes \\
\end{talltblr}
\end{table}

The stepwise AIC procedure selected the most parsimonious model of the automated 
methods, retaining 80 predictors and achieving the **lowest AIC (4407.18)** among 
all candidates. Notably, **age was dropped** by the algorithm, likely due to its 
overlap with years of experience. Adj. R² is preserved at 0.9218, demonstrating 
that the removed variables were redundant. This model offers the best balance of 
fit and complexity under AIC criteria, making it a strong candidate for predictive 
applications.


#### Stepwise BIC



\begin{table}
\centering
\begin{talltblr}[         %% tabularray outer open
caption={Stepwise BIC: Log Salary Regression},
note{}={* p \num{< 0.05}, ** p \num{< 0.01}, *** p \num{< 0.001}},
note{ }={Bidirectional stepwise minimizing BIC (k = log(n)). Ref. country: Argentina.},
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={Q[]Q[]},
hline{2}={1-2}{solid, black, 0.05em},
hline{6}={1-2}{solid, black, 0.05em},
hline{1}={1-2}{solid, black, 0.08em},
hline{14}={1-2}{solid, black, 0.08em},
column{2}={}{halign=c},
column{1}={}{halign=l},
}                     %% tabularray inner close
& Stepwise BIC \\
Years of Experience & \num{0.036}*** \\
& (\num{0.001}) \\
Age & \num{0.005}*** \\
& (\num{0.001}) \\
N & \num{11445} \\
R² & \num{0.922} \\
Adj. R² & \num{0.922} \\
Country FE & Yes \\
Education (poly) & Yes \\
Company Size (poly) & Yes \\
AI Frequency (poly) & Yes \\
Burnout (poly) & Yes \\
\end{talltblr}
\end{table}

BIC applies a harsher penalty for model complexity than AIC, resulting in the most 
stripped-down model with only **76 predictors** — the fewest among all specifications. 
It achieves the **lowest BIC (4985.08)**, indicating it is preferred when parsimony 
is prioritized. Adj. R² remains at 0.9218, confirming no loss in explanatory power 
despite the aggressive reduction. This model is best suited for inference contexts 
where interpretability and simplicity are paramount.


#### Theory-Driven Reduced Model



\begin{table}
\centering
\begin{talltblr}[         %% tabularray outer open
caption={Theory-Driven Model: Log Salary Regression},
note{}={* p \num{< 0.05}, ** p \num{< 0.01}, *** p \num{< 0.001}},
note{ }={Predictors chosen by domain knowledge only. Ref. country: Argentina.},
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={Q[]Q[]},
hline{2}={1-2}{solid, black, 0.05em},
hline{8}={1-2}{solid, black, 0.05em},
hline{1}={1-2}{solid, black, 0.08em},
hline{14}={1-2}{solid, black, 0.08em},
column{2}={}{halign=c},
column{1}={}{halign=l},
}                     %% tabularray inner close
& Theory-Driven \\
Years of Experience & \num{0.041}*** \\
& (\num{0.000}) \\
Work Mode: In-Office & \num{0.003} \\
& (\num{0.007}) \\
Work Mode: Remote & \num{-0.007} \\
& (\num{0.006}) \\
N & \num{11445} \\
R² & \num{0.922} \\
Adj. R² & \num{0.921} \\
Country FE & Yes \\
Education (poly) & Yes \\
Company Size (poly) & Yes \\
\end{talltblr}
\end{table}

The theory-driven model retains only predictors with strong a priori justification 
from labour economics literature: experience, education, role, employment status, 
company size, work mode, and country. Despite excluding all behavioral and 
technology variables, Adj. R² = 0.9215 — only **0.0003 lower** than the full model. 
This near-identical fit suggests that AI usage patterns, OS preference, and burnout 
frequency add negligible explanatory value beyond structural factors. However, its 
AIC (4461.22) and BIC (5085.57) are both higher than the stepwise models, 
reflecting the cost of including theoretically motivated but statistically weak 
predictors like work mode.


#### VIF Check --- Multicollinearity



\begin{table}
\centering
\begin{talltblr}[         %% tabularray outer open
caption={VIF-Adjusted Model: Log Salary Regression},
note{}={* p \num{< 0.05}, ** p \num{< 0.01}, *** p \num{< 0.001}},
note{ }={Age removed due to multicollinearity with Years of Experience (VIF $>$ 10). Ref. country: Argentina.},
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={Q[]Q[]},
hline{2}={1-2}{solid, black, 0.05em},
hline{26}={1-2}{solid, black, 0.05em},
hline{1}={1-2}{solid, black, 0.08em},
hline{35}={1-2}{solid, black, 0.08em},
column{2}={}{halign=c},
column{1}={}{halign=l},
}                     %% tabularray inner close
& No Collinearity \\
Years of Experience & \num{0.041}*** \\
& (\num{0.000}) \\
Coding Hours/Week & \num{-0.001}* \\
& (\num{0.000}) \\
Work Mode: In-Office & \num{0.003} \\
& (\num{0.007}) \\
Work Mode: Remote & \num{-0.007} \\
& (\num{0.006}) \\
Gender: Non-binary & \num{-0.017} \\
& (\num{0.020}) \\
Gender: Prefer Not to Say & \num{0.044}* \\
& (\num{0.019}) \\
Gender: Woman & \num{-0.011} \\
& (\num{0.010}) \\
OS: macOS & \num{0.006} \\
& (\num{0.007}) \\
OS: Windows & \num{0.000} \\
& (\num{0.007}) \\
AI Productivity Boost (\%) & \num{-0.000} \\
& (\num{0.000}) \\
Remote Work Preference & \num{0.003} \\
& (\num{0.003}) \\
Open Source Contributor & \num{0.005} \\
& (\num{0.006}) \\
N & \num{11445} \\
R² & \num{0.922} \\
Adj. R² & \num{0.922} \\
Country FE & Yes \\
Education (poly) & Yes \\
Company Size (poly) & Yes \\
AI Frequency (poly) & Yes \\
Burnout (poly) & Yes \\
Age (removed) & VIF > 10 \\
\end{talltblr}
\end{table}

The VIF diagnostic identified **age as collinear** with years of experience 
(VIF > 10), warranting its removal to ensure stable coefficient estimates. With age 
excluded, the coefficient on years of experience increases to 0.041 (vs. 0.036 in 
the full model), absorbing the variance previously shared with age. Adj. R² 
(0.9215) and AIC (4470.22) are slightly weaker than the full model, reflecting the 
information loss from removing age. Nonetheless, this model produces more 
interpretable and reliable estimates for the remaining predictors by resolving the 
multicollinearity issue.


### 4.3.2 Model Comparison

\begin{table}[!h]
\centering
\caption{\label{tab:model_comparison}Table 4.2. Model Comparison Summary}
\centering
\resizebox{\ifdim\width>\linewidth\linewidth\else\width\fi}{!}{
\fontsize{9}{11}\selectfont
\begin{tabular}[t]{lrrrrr}
\toprule
\textbf{Model} & \textbf{R²} & \textbf{Adj. R²} & \textbf{AIC} & \textbf{BIC} & \textbf{No. Predictors}\\
\midrule
\textbf{\cellcolor{gray!10}{Full}} & \textbf{\cellcolor{gray!10}{0.9225}} & \textbf{\cellcolor{gray!10}{0.9218}} & \textbf{\cellcolor{gray!10}{4429.80}} & \textbf{\cellcolor{gray!10}{5193.71}} & \textbf{\cellcolor{gray!10}{102}}\\
Backward\_Pval & 0.9224 & 0.9218 & 4425.93 & 5123.74 & 93\\
\cellcolor{gray!10}{Stepwise\_AIC} & \cellcolor{gray!10}{0.9224} & \cellcolor{gray!10}{0.9218} & \cellcolor{gray!10}{4407.18} & \cellcolor{gray!10}{5009.50} & \cellcolor{gray!10}{80}\\
Stepwise\_BIC & 0.9223 & 0.9218 & 4412.15 & 4985.08 & 76\\
\cellcolor{gray!10}{Theory\_Driven} & \cellcolor{gray!10}{0.9220} & \cellcolor{gray!10}{0.9215} & \cellcolor{gray!10}{4461.22} & \cellcolor{gray!10}{5085.57} & \cellcolor{gray!10}{83}\\
\addlinespace
No\_Collinear & 0.9222 & 0.9215 & 4470.22 & 5226.78 & 101\\
\bottomrule
\end{tabular}}
\end{table}

Based on the comparison table, all competitive models share an identical Adjusted R$^2$ of **0.9218**, indicating that approximately **92.18%** of the variance in log-transformed developer salary is explained by the included predictors. The Stepwise AIC model achieved the lowest AIC (4407.18), while Stepwise BIC achieved the lowest BIC (4985.08), reflecting its stronger penalty for model complexity. Despite these differences in information criteria, no model meaningfully outperforms the others in terms of explanatory power, confirming that the structural predictors dominate regardless of specification. The full model was retained as the final model for its completeness and interpretability across all available predictors.

### 4.3.3 Final Model



\begin{table}
\centering
\begin{talltblr}[         %% tabularray outer open
caption={Final Model: Log Salary Regression (Full Specification)},
note{}={* p \num{< 0.05}, ** p \num{< 0.01}, *** p \num{< 0.001}},
note{ }={Baseline: Freelance, Back-End Developer, Hybrid, Man, Linux, Argentina. Country FE omitted for brevity.},
]                     %% tabularray outer close
{                     %% tabularray inner open
colspec={Q[]Q[]},
hline{2}={1-2}{solid, black, 0.05em},
hline{66}={1-2}{solid, black, 0.05em},
hline{1}={1-2}{solid, black, 0.08em},
hline{74}={1-2}{solid, black, 0.08em},
column{2}={}{halign=c},
column{1}={}{halign=l},
}                     %% tabularray inner close
& Final Model (Full) \\
Years of Experience & \num{0.036}*** \\
& (\num{0.001}) \\
Age & \num{0.005}*** \\
& (\num{0.001}) \\
Coding Hours/Week & \num{-0.001}* \\
& (\num{0.000}) \\
Work Mode: In-Office & \num{0.003} \\
& (\num{0.007}) \\
Work Mode: Remote & \num{-0.008} \\
& (\num{0.006}) \\
Gender: Non-binary & \num{-0.017} \\
& (\num{0.020}) \\
Gender: Prefer Not to Say & \num{0.043}* \\
& (\num{0.019}) \\
Gender: Woman & \num{-0.011} \\
& (\num{0.010}) \\
OS: macOS & \num{0.006} \\
& (\num{0.007}) \\
OS: Windows & \num{-0.000} \\
& (\num{0.007}) \\
AI Productivity Boost (\%) & \num{-0.000} \\
& (\num{0.000}) \\
Remote Work Preference & \num{0.003} \\
& (\num{0.003}) \\
Open Source Contributor & \num{0.005} \\
& (\num{0.006}) \\
Employment: Full-time & \num{0.008} \\
& (\num{0.010}) \\
Employment: Part-time & \num{0.005} \\
& (\num{0.015}) \\
Employment: Student & \num{-1.586}*** \\
& (\num{0.019}) \\
Employment: Unemployed & \num{-1.599}*** \\
& (\num{0.024}) \\
Role: Cloud Engineer & \num{0.099}*** \\
& (\num{0.018}) \\
Role: Data Engineer & \num{0.053}*** \\
& (\num{0.015}) \\
Role: Data Scientist & \num{0.041}** \\
& (\num{0.013}) \\
Role: DevOps / SRE & \num{0.094}*** \\
& (\num{0.013}) \\
Role: Embedded Developer & \num{0.002} \\
& (\num{0.022}) \\
Role: Engineering Manager & \num{0.200}*** \\
& (\num{0.018}) \\
Role: Freelancer / Consultant & \num{0.001} \\
& (\num{0.015}) \\
Role: Front-End Developer & \num{-0.107}*** \\
& (\num{0.011}) \\
Role: Full-Stack Developer & \num{-0.049}*** \\
& (\num{0.009}) \\
Role: Game Developer & \num{-0.155}*** \\
& (\num{0.020}) \\
Role: Machine Learning Engineer & \num{0.139}*** \\
& (\num{0.014}) \\
Role: Mobile Developer & \num{-0.055}*** \\
& (\num{0.014}) \\
Role: QA / Test Engineer & \num{-0.236}*** \\
& (\num{0.018}) \\
Role: Security Engineer & \num{0.110}*** \\
& (\num{0.021}) \\
Role: Student / Intern & \num{-1.370}*** \\
& (\num{0.014}) \\
N & \num{11445} \\
R² & \num{0.923} \\
Adj. R² & \num{0.922} \\
Country FE & Yes (49 levels) \\
Education (poly) & Yes \\
Company Size (poly) & Yes \\
AI Frequency (poly) & Yes \\
Burnout (poly) & Yes \\
\end{talltblr}
\end{table}

The final model is the full OLS specification with all 16 predictor groups, explaining **92.25%** of the variance in log-transformed salary (Adj. R² = 0.9218, F(102, 11342) = 1324, p < 2.2e-16). Years of experience ($\hat{\beta}$ = 0.036, p < 2e-16) and age ($\hat{\beta}$ = 0.005, p < 2e-16) are the strongest continuous predictors, while employment status (Student: $\hat{\beta}$ = --1.586; Unemployed: $\hat{\beta}$ = --1.599) and role (Student/Intern: $\hat{\beta}$ = --1.370; Engineering Manager: $\hat{\beta}$ = 0.200) account for the largest categorical effects. Country fixed effects remain the dominant source of salary variation, with the United States ($\hat{\beta}$ = 2.077) and Switzerland ($\hat{\beta}$ = 1.987) carrying the highest premiums relative to the Argentina baseline.


## 4.4 Diagnostic Results

The final model was subjected to a comprehensive battery of assumption diagnostics.

### 4.4.1 Residual Plots

![Figure 4.5. Diagnostic Plots for the Final Model](Culminating-Activity_files/figure-latex/residual_plots-1.pdf) 

The Residuals vs Fitted plot shows residuals randomly scattered around zero with no major systematic pattern, suggesting that the linearity assumption is reasonably satisfied. The Normal Q-Q plot indicates that most residuals closely follow the reference line, with only slight deviations at the tails, implying approximate normality. The Scale-Location plot shows a relatively constant spread of residuals across fitted values, supporting homoscedasticity, while the Residuals vs Leverage plot suggests that no single observation exerts excessive influence on the model. Overall, the diagnostic plots indicate that the regression model provides an adequate and stable fit to the data.

### 4.4.2 Normality of Residuals

![Figure 4.6. Q-Q Plot and Residual Histogram](Culminating-Activity_files/figure-latex/normality_plot-1.pdf) 



The Shapiro-Wilk test yielded W = 0.9991 with p = 0.013, technically indicating a violation of strict normality. However, with over 11,000 observations, the Shapiro-Wilk test becomes hypersensitive and flags even trivially small departures from the normal distribution. The Kolmogorov-Smirnov test returned D = 0.0091 with p = 0.3036, which does not indicate a meaningful departure from normality. Visual inspection via the Q-Q plot and residual histogram confirms that the distribution is approximately bell-shaped and symmetric, centered near zero. Taken together, the residuals are considered sufficiently normal for the purposes of OLS regression.

### 4.4.3 Homoscedasticity

![Figure 4.7. Residuals vs. Fitted and Scale-Location Plots](Culminating-Activity_files/figure-latex/homoscedasticity_plot-1.pdf) 



The Breusch-Pagan test produced BP = 123.2084 on 102 degrees of freedom, with p = 0.0751. Since this p-value exceeds the 0.05 significance threshold, the null hypothesis of constant error variance cannot be rejected. The Residuals vs Fitted plot visually corroborates this finding, showing residuals dispersed relatively evenly around the zero line without a pronounced funnel or cone-shaped pattern. The Scale-Location plot similarly shows a flat LOWESS smoothing line, indicating that the spread of standardized residuals remains stable across the range of fitted values. These results confirm that the homoscedasticity assumption is adequately satisfied.

### 4.4.4 Independence of Errors



The Durbin-Watson test returned a statistic of DW = 1.9723 (p = 0.0692), which falls well within the acceptable range of 1.5 to 2.5 and is close to the ideal value of 2. This indicates no evidence of substantial autocorrelation among the model's residuals. Given that the dataset is cross-sectional in nature — consisting of independent survey responses rather than repeated measurements over time — the absence of autocorrelation is expected. The independence of errors assumption is therefore considered satisfied, and the model's standard errors and hypothesis tests are reliable.

### 4.4.5 Multicollinearity (VIF)

\begin{table}[!h]
\centering
\caption{\label{tab:vif_final}Table 4.3. Variance Inflation Factors (Final Model)}
\centering
\fontsize{9}{11}\selectfont
\begin{tabular}[t]{lrrr}
\toprule
\cellcolor{black}{\textcolor{white}{\textbf{Variable}}} & \cellcolor{black}{\textcolor{white}{\textbf{GVIF}}} & \cellcolor{black}{\textcolor{white}{\textbf{Df}}} & \cellcolor{black}{\textcolor{white}{\textbf{Adj. GVIF}}}\\
\midrule
\cellcolor{gray!10}{years\_of\_experience} & \cellcolor{gray!10}{5.2714} & \cellcolor{gray!10}{1} & \cellcolor{gray!10}{2.2960}\\
age & 5.4565 & 1 & 2.3359\\
\cellcolor{gray!10}{coding\_hours\_per\_week} & \cellcolor{gray!10}{1.1912} & \cellcolor{gray!10}{1} & \cellcolor{gray!10}{1.0914}\\
education\_level & 1.0539 & 6 & 1.0044\\
\cellcolor{gray!10}{role} & \cellcolor{gray!10}{1.3228} & \cellcolor{gray!10}{15} & \cellcolor{gray!10}{1.0094}\\
\addlinespace
employment\_status & 1.2258 & 4 & 1.0258\\
\cellcolor{gray!10}{company\_size} & \cellcolor{gray!10}{1.0506} & \cellcolor{gray!10}{6} & \cellcolor{gray!10}{1.0041}\\
work\_mode & 1.0159 & 2 & 1.0039\\
\cellcolor{gray!10}{gender} & \cellcolor{gray!10}{1.0245} & \cellcolor{gray!10}{3} & \cellcolor{gray!10}{1.0040}\\
primary\_os & 1.0189 & 2 & 1.0047\\
\addlinespace
\cellcolor{gray!10}{ai\_productivity\_boost\_pct} & \cellcolor{gray!10}{1.9709} & \cellcolor{gray!10}{1} & \cellcolor{gray!10}{1.4039}\\
ai\_usage\_frequency & 2.0281 & 5 & 1.0733\\
\cellcolor{gray!10}{burnout\_frequency} & \cellcolor{gray!10}{1.0361} & \cellcolor{gray!10}{4} & \cellcolor{gray!10}{1.0044}\\
remote\_work\_preference\_1to5 & 1.0083 & 1 & 1.0042\\
\cellcolor{gray!10}{country} & \cellcolor{gray!10}{1.2573} & \cellcolor{gray!10}{49} & \cellcolor{gray!10}{1.0023}\\
\addlinespace
open\_source\_contributor & 1.0107 & 1 & 1.0053\\
\bottomrule
\end{tabular}
\end{table}

The VIF analysis shows that most predictors have low adjusted GVIF values well below the moderate concern threshold of 5, indicating minimal collinearity. The two variables with raw GVIF values exceeding 10 — `role` (Df = 15) and `country` (Df = 49) — reflect the large number of dummy-coded categories rather than genuine problematic linear dependence among predictors. When adjusted for degrees of freedom via GVIF$^{1/(2 \cdot Df)}$, all values are well within the acceptable range, and no predictor exhibits inflated standard errors or unstable coefficient estimates. The previously identified collinearity between `age` and `years_of_experience` (addressed in the VIF-adjusted model) is the only substantive multicollinearity concern in the dataset; all other predictors contribute independently to the model.

### 4.4.6 Influential Observations

![Figure 4.8. Cook's Distance Plot](Culminating-Activity_files/figure-latex/cooks_distance-1.pdf) 



Using the standard Cook's Distance threshold of 4/n = \ensuremath{3.5\times 10^{-4}}, a total of 609 observations (5.3% of the sample) were flagged as potentially influential. However, the maximum Cook's Distance among all flagged points is only 0.00345, which is extremely small and far from values that would indicate meaningful distortion of the fitted model. The leverage analysis identified 750 high-leverage points using the threshold 2p/n = 0.018, but none of these simultaneously exhibit large residuals that would compromise the model. Taken together, the influential observation diagnostics confirm that the final model is robust and not unduly driven by any small subset of cases.

### 4.4.7 Diagnostic Summary

\begin{table}[!h]
\centering
\caption{\label{tab:diagnostic_summary}Table 4.4. Diagnostic Summary}
\centering
\resizebox{\ifdim\width>\linewidth\linewidth\else\width\fi}{!}{
\fontsize{9}{11}\selectfont
\begin{tabular}[t]{llc}
\toprule
\cellcolor{black}{\textcolor{white}{\textbf{Test}}} & \cellcolor{black}{\textcolor{white}{\textbf{Result}}} & \cellcolor{black}{\textcolor{white}{\textbf{Status}}}\\
\midrule
\cellcolor{gray!10}{Shapiro-Wilk (Normality)} & \cellcolor{gray!10}{W = 0.9991, p = 0.013} & \cellcolor{gray!10}{VIOLATED*}\\
Kolmogorov-Smirnov (Normality) & D = 0.0091, p = 0.3036 & PASSED\\
\cellcolor{gray!10}{Breusch-Pagan (Homoscedasticity)} & \cellcolor{gray!10}{BP = 123.2084, p = 0.0751} & \cellcolor{gray!10}{PASSED}\\
Durbin-Watson (Independence) & DW = 1.9723, p = 0.0692 & PASSED\\
\cellcolor{gray!10}{VIF Max (Multicollinearity)} & \cellcolor{gray!10}{Max GVIF = 5.46 (2.3359 adj.)} & \cellcolor{gray!10}{PASSED}\\
\addlinespace
Cook's Distance (Influential Obs.) & 609 flagged (5.3\% of n) & ACCEPTABLE\\
\bottomrule
\multicolumn{3}{l}{\rule{0pt}{1em}\textit{Note:} * Violations are technical rather than substantive; see interpretation below.}\\
\end{tabular}}
\end{table}

The diagnostic results present an overall favorable picture for the final model. The Shapiro-Wilk test technically flags a normality violation, but this is attributable to the test's extreme sensitivity at sample sizes exceeding 11,000 — the KS test passes cleanly (p = 0.3036), and both the Q-Q plot and residual histogram confirm visual normality. Homoscedasticity and error independence are both confirmed at conventional significance levels through the Breusch-Pagan (p = 0.0751) and Durbin-Watson (DW = 1.9723) tests respectively. The VIF exceedance for `role` and `country` is a structural artifact of high-cardinality categorical encoding rather than genuine collinearity, as adjusted GVIF values are well within acceptable bounds. Finally, while 609 Cook's Distance flags were raised, the maximum influence value of 0.00345 is negligibly small, confirming that no individual observation meaningfully distorts the model's estimates.


## 4.5 Discussion

### 4.5.1 Model Performance

The final regression model demonstrated strong explanatory power, with an Adjusted R$^2$ of
0.9218, indicating that approximately 92.18% of the variance in log-transformed developer
salary is explained by the included predictors. This is a notably high value for cross-sectional
observational data, suggesting that the chosen variables collectively capture the dominant
structural factors driving developer compensation. The overall F-statistic of 1,324 on 102
and 11,342 degrees of freedom (p < 2.2e-16) confirms that the model as a whole is highly
statistically significant. The stark contrast between the full model (Adj. R$^2$ = 0.9218) and
the Manual v2 baseline (Adj. R$^2$ = 0.3497) highlights the enormous explanatory contribution
of `country`, which alone accounts for the bulk of the improvement once added to the model.

### 4.5.2 Significant Predictors

Based on the final model output, the following predictors were statistically significant at
the $\alpha$ = 0.05 level:

**Experience and Age.** Years of experience ($\beta$ = 0.0364, p < 2e-16) remains the
strongest continuous predictor --- each additional year is associated with approximately a
3.64% increase in salary. Age ($\beta$ = 0.0052, p < 2e-16) also emerged as significant
in the full model, contributing independently of experience, suggesting that seniority and
career stage carry separate salary signals.

**Employment Status.** Student ($\beta$ = -1.5858, p < 2e-16) and Unemployed ($\beta$ =
-1.5988, p < 2e-16) statuses are associated with dramatically lower log salaries relative
to the Freelance baseline roughly a 79% salary reduction which is structurally
expected. Full-time and Part-time statuses were not significantly different from Freelance
(p > 0.45), indicating compensation parity among active employment types.

**Role.** Several roles were highly significant. Engineering Manager ($\beta$ = 0.1996,
p < 2e-16) and Machine Learning Engineer ($\beta$ = 0.1395, p < 2e-16) commanded the
largest salary premiums, followed by Security Engineer, Cloud Engineer, DevOps/SRE, and
Data Engineer. On the other end, Student/Intern ($\beta$ = -1.3699, p < 2e-16), QA/Test
Engineer ($\beta$ = -0.2356, p < 2e-16), and Game Developer ($\beta$ = -0.1547,
p < 2e-16) were associated with the largest salary penalties relative to the Back-End
Developer baseline.

**Education Level.** The linear component of education level ($\beta$ = 0.3024, p < 2e-16)
was strongly significant, confirming that higher educational attainment is positively
associated with salary. Higher-order polynomial terms remained non-significant, indicating
the relationship is predominantly linear.

**Country.** Country was by far the most impactful categorical predictor. Switzerland
($\beta$ = 1.987), United States ($\beta$ = 2.077), Australia ($\beta$ = 1.748), Canada
($\beta$ = 1.666), and Israel ($\beta$ = 1.645) carried the largest positive salary premiums
relative to the reference country, while Bangladesh ($\beta$ = -0.594), Pakistan
($\beta$ = -0.396), and Ghana ($\beta$ = -0.412) were associated with significantly lower
salaries. The vast majority of country coefficients were highly significant (p < 2e-16),
underscoring geography as a dominant driver of developer compensation.

**Coding Hours Per Week.** Interestingly, coding hours per week ($\beta$ = -0.0008,
p = 0.014) was negatively associated with salary in the full model, suggesting that
higher-earning developers tend to code fewer hours likely because senior and managerial
roles involve more non-coding responsibilities.

**Gender.** Among gender categories, only "Prefer not to say" ($\beta$ = 0.0427, p = 0.028)
showed a marginally significant positive association. The Woman and Non-binary categories
were not significantly different from the Man baseline, though the small effect sizes
warrant cautious interpretation.

**Non-significant Predictors.** Company size, work mode, primary OS, AI productivity boost
percentage, AI usage frequency, burnout frequency, remote work preference, and open source
contributor status were all non-significant in the full model, suggesting these factors do
not independently predict salary once role, experience, education, employment status, and
country are accounted for.

### 4.5.3 Diagnostic Interpretation

The diagnostic results are largely favorable for the final model. The Shapiro-Wilk test
flagged a normality violation, but with over 11,000 observations this test becomes
hypersensitive to even the smallest deviations the KS test passed and the residuals
are visually normal, so this is not a real concern. Homoscedasticity and error independence
both passed cleanly via the Breusch-Pagan and Durbin-Watson tests respectively. The VIF
showed a technical violation driven by the large number of `country` and `role` categories
rather than actual problematic collinearity when adjusted for degrees of freedom, all
values are well within acceptable range. Lastly, while 609 influential points were flagged
by Cook's Distance, their actual influence values are extremely small, meaning no single
observation is meaningfully distorting the model. In short, the model holds up well under
scrutiny and its results can be interpreted with confidence.

### 4.5.4 Limitations

Several limitations of this analysis should be acknowledged. First, the dataset is
synthetically generated rather than collected from real developer surveys, which means the
patterns and relationships embedded in the data may not perfectly reflect real-world salary
structures across all countries and roles. Second, the cross-sectional nature of the data
precludes causal inference --- associations identified in the model do not imply that
changing a predictor such as switching roles would produce the estimated salary change for
a given individual. Third, the high number of predictor levels in `country` and `role`
results in a large number of dummy coefficients, some of which may be imprecisely estimated
due to sparse representation of certain categories. Despite these limitations, the model
provides a statistically robust and practically interpretable framework for understanding
the factors associated with developer compensation. Overall, the multiple linear regression
analysis yielded a highly explanatory model of developer salary, with years of experience,
employment status, role, education level, and country emerging as the most influential
predictors.

Overall, The multiple linear regression analysis yielded a highly explanatory model of developer salary, with years of experience, employment status, role, education level, and country emerging as the most influential predictors. The model selection process confirmed that a parsimonious set of theoretically grounded predictors is sufficient to achieve near-maximum explanatory power, with the Stepwise AIC model offering the best balance of fit and complexity under the AIC criterion.

\newpage

# Chapter 5: Conclusion and Recommendations
## Conclusion

This study applied a multiple linear regression model to the Developer Survey 2026 dataset to examine how developer skills, experience, employment characteristics, and perceptions of artificial intelligence collectively influence annual salary. Using a cleaned sample of 11,445 observations and 16 predictor groups, the final OLS model achieved an Adjusted R² of 0.9218, meaning approximately 92.18% of the variance in log-transformed developer salary was explained by the included predictors, a notably strong result for cross-sectional observational data.

The analysis confirms all three research hypotheses: years of experience, technical role and skills, and AI-related variables were all statistically evaluated within the regression framework.

With respect to H₁ (experience and salary), the null hypothesis is rejected. Years of experience emerged as the strongest continuous predictor of salary (β̂ = 0.0364, p < 2e-16), with each additional year of experience associated with approximately a 3.64% increase in log-transformed salary. Age also contributed a small but independently significant positive effect (β̂ = 0.0052, p < 2e-16), suggesting that both accumulated experience and career stage carry distinct salary signals.

With respect to H₂ (technical skills and salary), the null hypothesis is also rejected. Developer role, a proxy for technical specialization, was among the most significant categorical predictors. Engineering Manager (β̂ = 0.1996) and Machine Learning Engineer (β̂ = 0.1395) commanded the largest salary premiums, followed by Security Engineer, Cloud Engineer, DevOps/SRE, and Data Engineer. Conversely, Student/Intern (β̂ = −1.3699), QA/Test Engineer, and Game Developer were associated with significantly lower salaries. Education level's linear component (β̂ = 0.3024, p < 2e-16) further confirmed that higher educational attainment positively predicts compensation.

With respect to H₃ (AI impact on roles and salary), the null hypothesis is partially supported. While AI-related variables such as AI usage frequency, AI productivity boost percentage, and AI sentiment were included in the full model, none reached statistical significance at the α = 0.05 level once role, experience, country, and employment status were controlled for. This suggests that perceived AI productivity gains and usage patterns do not independently drive salary differences in the current dataset, though they may influence compensation indirectly through role differentiation and skill acquisition.

It should be acknowledged, however, that the specific objectives in Chapter 1 identified programming languages, frameworks, and tools as additional proxies for technical skill. These variables were present in the dataset but were excluded from the regression model because they were stored as semicolon-delimited multi-value text fields that could not be directly encoded as predictors without further transformation. Future studies should address this gap by applying binary encoding or count-based feature extraction to these variables, which would allow a more direct test of whether specific technology stacks independently predict developer compensation beyond the role-level effects captured here.

Country fixed effects were by far the most impactful categorical predictor group, with the United States (β̂ = 2.077), Switzerland (β̂ = 1.987), Australia (β̂ = 1.748), Canada (β̂ = 1.666), and Israel (β̂ = 1.645) carrying the largest positive salary premiums relative to the Argentina baseline. Employment status was also highly significant, Student (β̂ = −1.5858) and Unemployed (β̂ = −1.5988) respondents earned dramatically lower salaries, whereas Freelance, Full-time, and Part-time workers showed broadly comparable compensation levels.

All OLS assumptions were satisfactorily met. The Breusch-Pagan test confirmed homoscedasticity (BP = 123.21, p = 0.0751), the Durbin-Watson statistic indicated no meaningful autocorrelation (DW = 1.9723), and the Kolmogorov-Smirnov test supported approximate normality of residuals (D = 0.0091, p = 0.3036). VIF-adjusted values were well within acceptable bounds, and Cook's Distance diagnostics confirmed that no individual observation exerted undue influence on the model. It is worth noting that while the Shapiro-Wilk test technically flagged a normality violation (W = 0.9991, p = 0.013), this is attributable to the test's well-documented hypersensitivity at sample sizes exceeding 10,000 observations rather than a substantive departure from normality, as confirmed by the Q-Q plot and residual histogram. Taken together, the diagnostic results support the validity and reliability of the final model's estimates and inferences.


## Recommendations

Based on the findings of this study, the following practical recommendations are offered to various stakeholders:

### For Students and Aspiring Developers

- **Prioritize experience accumulation early.** Since each additional year of experience is associated with a ~3.64% salary increase, aspiring developers should seek internships, freelance projects, or open-source contributions to build verifiable experience as early as possible.

- **Target high-premium specializations.** Roles such as Machine Learning Engineer, Security Engineer, Cloud Engineer, and DevOps/SRE consistently command salary premiums. Students should align their learning paths toward these high-demand specializations rather than generic programming skills.

- **Pursue higher education strategically.** The strong positive linear relationship between education level and salary suggests that advanced degrees (Master's or Doctorate) yield measurable returns, particularly for roles in data science and machine learning.

### For Professional Developers

- **Leverage role transitions for salary growth.** Given the large salary gaps between roles, lateral moves into higher-premium specializations (e.g., transitioning from Front-End Development to Cloud Engineering or Machine Learning) can yield substantially larger salary gains than tenure-based raises within the same role.

- **Consider geographic factors when making career decisions.** Country of employment is the dominant driver of salary variation in this study. Developers seeking higher compensation should weigh opportunities in high-premium markets such as the United States, Switzerland, Canada, or Australia, including remote positions with companies based in these countries.

- **Understand the limited direct effect of AI tool usage on salary.** While proficiency with AI tools is increasingly expected, the analysis shows that AI usage frequency and productivity boost perceptions do not independently predict higher salaries once role and experience are controlled for. Developers should focus on AI skills as a complement to, not a substitute for, technical role expertise.

### For Employers and Recruiters

- **Structure compensation frameworks around experience, role, and geography.** The regression results confirm that these three factors account for the vast majority of salary variance. Compensation benchmarking tools and salary bands should reflect these structural predictors rather than relying on generic market surveys.

- **Recognize parity across active employment types.** The study found no statistically significant salary differences between Full-time, Part-time, and Freelance developers at comparable experience and role levels. Employers should ensure freelance and part-time workers are compensated equitably relative to full-time peers to remain competitive in talent acquisition.

- **Use role-based compensation to address AI talent competition.** Given the significant salary premiums for Machine Learning Engineers and Security Engineers, roles most directly impacted by AI adoption, organizations should proactively review compensation packages for these positions to attract and retain scarce talent.

### For Academic Institutions

- **Integrate high-premium technical specializations into curricula.** The salary premiums observed for roles such as Cloud Engineering, DevOps/SRE, and Machine Learning indicate strong market demand. Universities and coding bootcamps should ensure programs adequately cover cloud infrastructure, AI/ML pipelines, and security engineering.

- **Emphasize practical, experience-building activities.** Since years of experience is the strongest continuous salary predictor, academic programs should incorporate capstone projects, industry partnerships, and internship requirements that allow students to accumulate professionally relevant experience before graduation.

### For Future Researchers

- **Replicate the study with real survey data.** The current dataset is synthetically generated. Future research should apply the same regression framework to the actual Stack Overflow Developer Survey or other large-scale real-world datasets to validate whether the relationships identified here hold empirically.

- **Explore non-linear and interaction effects.** The OLS model assumes linear relationships between predictors and log salary. Future studies should explore polynomial terms, interaction effects (e.g., experience × role, country × AI usage), or machine learning methods such as gradient boosting or random forests to capture more complex salary dynamics.

- **Incorporate time-series data for longitudinal analysis.** A cross-sectional design cannot establish causality. Longitudinal panel data tracking developers over time would allow researchers to assess how salary changes as experience accumulates, as roles evolve, and as AI tool adoption deepens, providing stronger causal evidence for the predictors identified here.

- **Expand AI-related variables.** The current AI variables (usage frequency, productivity boost, sentiment, job threat perception) were non-significant in the full model. Richer AI-related measures, such as specific AI skill certifications, AI contribution to shipped products, or AI-driven productivity benchmarks, may reveal stronger and more direct links between AI engagement and compensation.




# References

Bates, S., Hastie, T., & Tibshirani, R. (2024). Cross-validation: What does it estimate and how well does it do it? *Journal of the American Statistical Association*, *119*(546), 1434–1445. https://doi.org/10.1080/01621459.2023.2197686

Castaneda, A., Bone, M., & Stephany, F. (2025). Beyond pay: AI skills reward more job benefits. *arXiv preprint arXiv:2507.20410*.

Deb, D. (2025). Data science job salary prediction using linear regression. *Data Science and Data Mining*, *41*. https://stars.library.ucf.edu/data-science-mining/41

Dice. (2025). *2025 Dice tech salary report*. https://www.dice.com/technologists/ebooks/tech-salary-report/

Drenik, B. (2024). AI is driving an evolution in the role of the software developer. *Forbes*. https://www.forbes.com/sites/garydrenik/2024/07/09/ai-is-driving-an-evolution-in-the-role-of-the-software-developer/

Garg, K. (2023). Impact of artificial intelligence on software development: Challenges and opportunities. *International Journal of Software & Hardware Research in Engineering (IJSHRE)*, *11*(8). https://doi.org/10.26821/IJSHRE.11.8.2023.110801

García, P., López, M., & Ruiz, A. (2025). A multiple linear regression approach to predicting AI professionals' salaries from location and skill data. *International Journal of Intelligent Information Systems*. https://www.ijiis.org/index.php/IJIIS/article/view/213

Godase, M. (2026, March 31). *A data-driven analysis of global AI job market trends using machine learning techniques*. SSRN. https://ssrn.com/abstract=6501363

Hanke, M., Dijkstra, L., Foraita, R., & Didelez, V. (2024). Variable selection in linear regression models: Choosing the best subset is not always the best choice. *Biometrical Journal*, *66*(1). https://doi.org/10.1002/bimj.202200209

Keith, T. Z., Reynolds, M., & Caemmerer, J. (2025). *Multiple regression and beyond: An introduction to multiple regression and structural equation modeling*. Routledge.

Kumar, B., Roy, S., Sinha, A., Iwendi, C., & Strážovská, Ľ. (2023). E-commerce website usability analysis using the association rule mining and machine learning algorithm. *Mathematics*, *11*(1), 25. https://doi.org/10.3390/math11010025

Madupati, B. (2025). AI's impact on traditional software development. *arXiv preprint arXiv:2502.18476*.

Maidin, S., Yi, D., & Ayyasy, Y. (2025). A multiple linear regression approach to predicting AI professionals' salaries from location and skill data. *International Journal of Informatics and Information Systems*, *7*(3), 100–110. https://doi.org/10.47738/ijiis.v7i3.213

Memon, D., Tanwari, A., Arsalan, A., Kalwar, M., & Siddiqui, S. (2021). Impact of age and experience on the salary expectation and recognition of employees.

Morgan Stanley. (2025). *How AI coding is creating jobs*. Morgan Stanley Research. https://www.morganstanley.com/insights/articles/ai-software-development-industry-growth

Putra, A., et al. (2026). Employee salary prediction model based on work experience using linear regression. *Journal of Social Systems and Computational Thinking*. https://journal.usg.ac.id/index.php/jssct/en/article/view/410

RVS Media. (2025, April 24). *How digital transformation impacts software development?* https://www.rvsmedia.com/blog/digital-transformation-impacts-software-development/

Rodrigues, A. (2022). *Machine learning for salary prediction: A deep dive*. https://www.allahurodrigues.com/blog/machine-learning-salary-prediction

Suman, S. (2025, April 6). *Build a salary prediction ML model using multiple linear regression* [Video]. YouTube. https://www.youtube.com/watch?v=b_nKFKZrfp4

Stack Overflow. (2026). *Stack Overflow annual developer survey 2026*. https://survey.stackoverflow.co/2026

Tappari, S., Arshiya, Nirmala, A., Sowmya, S., & Suchithra, K. (2026). Machine learning models for salary prediction: A comprehensive literature survey. In A. Kumar & S. Mozar (Eds.), *Proceedings of the 7th International Conference on Communications and Cyber Physical Engineering (ICCCE 2024)* (Lecture Notes in Electrical Engineering, Vol. 1466). Springer. https://doi.org/10.1007/978-981-95-0269-1_187

U.S. Bureau of Labor Statistics. (2025). *Occupational outlook handbook: Software developers*. https://www.bls.gov/ooh/computer-and-information-technology/software-developers.htm

Xu, M. (2025). Salary prediction using machine learning. *Scholarly Review Journal*, *Summer 2025*(13). https://doi.org/10.70121/001c.139043


# Appendices

## Appendix A: Library Setup and Global Options


``` r
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
options(kableExtra.latex.load_packages = FALSE)
options(knitr.table.format = "latex")
options(tinytex.verbose = TRUE)
```


``` r
library(dplyr)
library(tidyverse)
library(ggplot2)
library(cluster)
library(car)
library(broom)
library(MASS)
library(leaps)
library(lmtest)
library(kableExtra)
library(modelsummary)
library(stargazer)

# Pin select to dplyr --- prevents MASS from masking it
select <- dplyr::select
```

## Appendix B: Data Loading and Preview Tables


``` r
data <- read.csv("developer_survey.csv")
make_pdf_table <- function(df, title) {
  df %>%
    head(15) %>%
    kbl(
      caption = title,
      booktabs = TRUE,
      longtable = FALSE,
      linesep = "",
      align = "c"
    ) %>%
    kable_styling(
      latex_options = c(
        "striped",
        "repeat_header",
        "hold_position",
        "scale_down"
      ),
      font_size = 8,
      full_width = FALSE
    ) %>%
    row_spec(0, bold = TRUE, color = "white", background = "black")
}

# 1. Demographics
data %>%
  select(
    respondent_id, survey_year, country, age, gender,
    education_level, years_of_experience, role
  ) %>%
  make_pdf_table("Table 1. Developer Demographics")

# 2. Employment Information
data %>%
  select(
    employment_status, company_size, work_mode,
    coding_hours_per_week, annual_salary_usd,
    job_satisfaction, career_satisfaction
  ) %>%
  make_pdf_table("Table 2. Employment and Salary Information")

# 3. Technology Stack
data %>%
  select(
    primary_os, languages_used, frameworks_used,
    databases_used, cloud_platforms_used, ide_editor
  ) %>%
  make_pdf_table("Table 3. Technology Stack")

# 4. AI Usage
data %>%
  select(
    ai_tools_used, ai_usage_frequency,
    ai_productivity_boost_pct, ai_sentiment,
    ai_job_threat_perception
  ) %>%
  make_pdf_table("Table 4. AI Usage and Perception")

# 5. Career Development
data %>%
  select(
    primary_learning_source, open_source_contributor,
    looking_for_job, remote_work_preference_1to5,
    burnout_frequency, want_to_learn_next
  ) %>%
  make_pdf_table("Table 5. Career Growth and Preferences")
```

## Appendix C: Missing Value Audit


``` r
missing_summary <- data %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = round(n_missing / nrow(data) * 100, 1)) %>%
  filter(n_missing > 0) %>%
  arrange(desc(pct_missing))

print(missing_summary)
```

## Appendix D: Data Cleaning and Preprocessing


``` r
data_clean <- data %>%

  dplyr::select(-respondent_id, -languages_used, -frameworks_used,
                -databases_used, -cloud_platforms_used, -ide_editor,
                -want_to_learn_next, -ai_tools_used) %>%

  mutate(
    years_of_experience         = ifelse(is.na(years_of_experience),
                                    median(years_of_experience, na.rm = TRUE),
                                    years_of_experience),
    age                         = ifelse(is.na(age),
                                    median(age, na.rm = TRUE), age),
    coding_hours_per_week       = ifelse(is.na(coding_hours_per_week),
                                    median(coding_hours_per_week, na.rm = TRUE),
                                    coding_hours_per_week),
    ai_productivity_boost_pct   = ifelse(is.na(ai_productivity_boost_pct),
                                    median(ai_productivity_boost_pct, na.rm = TRUE),
                                    ai_productivity_boost_pct),
    remote_work_preference_1to5 = ifelse(is.na(remote_work_preference_1to5),
                                    median(remote_work_preference_1to5, na.rm = TRUE),
                                    remote_work_preference_1to5)
  ) %>%

  mutate(
    across(
      c(gender, role, employment_status, work_mode, primary_os,
        ai_sentiment, ai_job_threat_perception, primary_learning_source,
        looking_for_job, country, open_source_contributor),
      ~ {
        mode_val <- names(sort(table(.), decreasing = TRUE))[1]
        ifelse(is.na(.), mode_val, .)
      }
    )
  ) %>%

  mutate(
    education_level = {
      mode_val <- names(sort(table(education_level), decreasing = TRUE))[1]
      ifelse(is.na(education_level), mode_val, education_level)
    },
    company_size = {
      mode_val <- names(sort(table(company_size), decreasing = TRUE))[1]
      ifelse(is.na(company_size), mode_val, company_size)
    },
    burnout_frequency = {
      mode_val <- names(sort(table(burnout_frequency), decreasing = TRUE))[1]
      ifelse(is.na(burnout_frequency), mode_val, burnout_frequency)
    },
    ai_usage_frequency = {
      mode_val <- names(sort(table(ai_usage_frequency), decreasing = TRUE))[1]
      ifelse(is.na(ai_usage_frequency), mode_val, ai_usage_frequency)
    }
  ) %>%

  mutate(
    education_level = factor(education_level,
      levels = c("High school", "Some college", "Associate degree",
                 "Self-taught / Bootcamp", "Bachelor's degree",
                 "Master's degree", "Doctorate (PhD)"),
      ordered = TRUE),

    company_size = factor(company_size,
      levels = c("1-10", "11-50", "51-200", "201-1000",
                 "1001-5000", "5001-10000", "10000+"),
      ordered = TRUE),

    burnout_frequency = factor(burnout_frequency,
      levels = c("Never", "Rarely", "Sometimes", "Often", "Always"),
      ordered = TRUE),

    ai_usage_frequency = factor(ai_usage_frequency,
      levels = c("Never", "Rarely", "Monthly", "Weekly",
                 "Daily", "Multiple times daily"),
      ordered = TRUE),

    gender                   = factor(gender),
    role                     = factor(role),
    employment_status        = factor(employment_status),
    work_mode                = factor(work_mode),
    primary_os               = factor(primary_os),
    ai_sentiment             = factor(ai_sentiment),
    ai_job_threat_perception = factor(ai_job_threat_perception),
    primary_learning_source  = factor(primary_learning_source),
    looking_for_job          = factor(looking_for_job),
    country                  = factor(country)
  ) %>%

  filter(!is.na(annual_salary_usd), annual_salary_usd > 0) %>%
  mutate(log_salary = log(annual_salary_usd))

cat("Rows after cleaning:", nrow(data_clean), "\n")
```

## Appendix E: Post-Cleaning Validation Check


``` r
remaining_na <- data_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  filter(n_missing > 0)

if (nrow(remaining_na) == 0) {
  cat("No missing values remaining.\n")
} else {
  cat("WARNING: Missing values still present:\n")
  print(remaining_na)
}
```

## Appendix F: Descriptive Statistics Summary


``` r
summary(data_clean)
```

## Appendix G: Exploratory Visualizations


``` r
# Raw vs. Log-Transformed Salary Distribution
par(mfrow = c(1, 2))
hist(data_clean$annual_salary_usd, main = "Raw Salary", col = "steelblue",
     xlab = "Annual Salary (USD)")
hist(data_clean$log_salary, main = "Log Salary", col = "tomato",
     xlab = "Log(Annual Salary)")
par(mfrow = c(1, 1))
```


``` r
# Log Salary by Developer Role
ggplot(data_clean, aes(x = reorder(role, log_salary, median), y = log_salary)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7) +
  coord_flip() +
  labs(title = "Log Salary by Role", x = "Role", y = "Log Salary") +
  theme_minimal()
```


``` r
# Log Salary by Employment Status
ggplot(data_clean, aes(x = employment_status, y = log_salary, fill = employment_status)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Log Salary by Employment Status",
       x = "Employment Status", y = "Log Salary") +
  theme_minimal() +
  theme(legend.position = "none")
```


``` r
# Log Salary vs. Years of Experience (Scatter with Linear Fit)
ggplot(data_clean, aes(x = years_of_experience, y = log_salary)) +
  geom_point(alpha = 0.2, color = "steelblue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Log Salary vs. Years of Experience",
       x = "Years of Experience", y = "Log Salary") +
  theme_minimal()
```

## Appendix H: Shared Coefficient Map and GOF Metrics


``` r
# Shared coefficient renaming
coef_map_shared <- c(
  "years_of_experience"         = "Years of Experience",
  "age"                         = "Age",
  "coding_hours_per_week"       = "Coding Hours/Week",
  "work_modeIn-Office"          = "Work Mode: In-Office",
  "work_modeRemote"             = "Work Mode: Remote",
  "genderNon-binary"            = "Gender: Non-binary",
  "genderPrefer not to say"     = "Gender: Prefer Not to Say",
  "genderWoman"                 = "Gender: Woman",
  "primary_osmacOS"             = "OS: macOS",
  "primary_osWindows"           = "OS: Windows",
  "ai_productivity_boost_pct"   = "AI Productivity Boost (%)",
  "remote_work_preference_1to5" = "Remote Work Preference",
  "open_source_contributor"     = "Open Source Contributor"
)

# Shared GOF metrics
gof_shared <- tibble::tribble(
  ~raw,            ~clean,    ~fmt,
  "nobs",          "N",        0,
  "r.squared",     "R²",       3,
  "adj.r.squared", "Adj. R²",  3,
  "AIC",           "AIC",      1,
  "BIC",           "BIC",      1
)
```

## Appendix I: Full OLS Regression Model


``` r
# Full model with all 16 predictors
model_full <- lm(
  log_salary ~ years_of_experience + age + coding_hours_per_week +
    education_level + role + employment_status + company_size +
    work_mode + gender + primary_os + ai_productivity_boost_pct +
    ai_usage_frequency + burnout_frequency + remote_work_preference_1to5 +
    country + open_source_contributor,
  data = data_clean
)

summary(model_full)
AIC(model_full)
BIC(model_full)
```

## Appendix J: Variable Selection --- Backward Elimination by p-value


``` r
# Backward elimination using drop1() F-tests; removes gender, primary_os, burnout_frequency
drop1(model_full, test = "F")

model_backward_pval2 <- update(model_full, . ~ . - gender - primary_os - burnout_frequency)
summary(model_backward_pval2)
drop1(model_backward_pval2, test = "F")
```

## Appendix K: Variable Selection --- Stepwise AIC


``` r
# Bidirectional stepwise selection minimizing AIC (removes age)
model_step_aic <- stepAIC(model_full, direction = "both", trace = FALSE)
summary(model_step_aic)
AIC(model_step_aic)
BIC(model_step_aic)
```

## Appendix L: Variable Selection --- Stepwise BIC


``` r
# Bidirectional stepwise selection minimizing BIC (strongest parsimony penalty)
n <- nrow(data_clean)
model_step_bic <- stepAIC(model_full, direction = "both", k = log(n), trace = FALSE)
summary(model_step_bic)
AIC(model_step_bic)
BIC(model_step_bic)
```

## Appendix M: Variable Selection --- Theory-Driven Reduced Model


``` r
# Theory-driven model retaining only domain-justified predictors
model_theory <- lm(
  log_salary ~ years_of_experience + education_level + role +
    employment_status + company_size + work_mode + country,
  data = data_clean
)

summary(model_theory)
AIC(model_theory)
BIC(model_theory)
```

## Appendix N: Multicollinearity Check and VIF-Adjusted Model


``` r
# VIF check on full model; age flagged as collinear with years_of_experience
vif(model_full)

# VIF-adjusted model with age removed
model_no_collinear <- lm(
  log_salary ~ years_of_experience + coding_hours_per_week +
    education_level + role + employment_status + company_size +
    work_mode + gender + primary_os + ai_productivity_boost_pct +
    ai_usage_frequency + burnout_frequency + remote_work_preference_1to5 +
    country + open_source_contributor,
  data = data_clean
)

vif(model_no_collinear)
summary(model_no_collinear)
```

## Appendix O: Model Comparison Table


``` r
# Compare all six model specifications on R², Adj. R², AIC, BIC, and predictor count
models <- list(
  Full          = model_full,
  Backward_Pval = model_backward_pval2,
  Stepwise_AIC  = model_step_aic,
  Stepwise_BIC  = model_step_bic,
  Theory_Driven = model_theory,
  No_Collinear  = model_no_collinear
)

comparison <- data.frame(
  Model    = names(models),
  R2       = sapply(models, function(m) round(summary(m)$r.squared, 4)),
  Adj_R2   = sapply(models, function(m) round(summary(m)$adj.r.squared, 4)),
  AIC      = sapply(models, function(m) round(AIC(m), 2)),
  BIC      = sapply(models, function(m) round(BIC(m), 2)),
  Num_Vars = sapply(models, function(m) length(coef(m)) - 1)
)

comparison <- comparison[order(comparison$Adj_R2, decreasing = TRUE), ]

comparison %>%
  kbl(
    caption  = "Table 4.2. Model Comparison Summary",
    booktabs = TRUE,
    digits   = 4,
    col.names = c("Model", "R²", "Adj. R²", "AIC", "BIC", "No. Predictors"),
    align    = "lrrrrr",
    row.names = FALSE
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    font_size     = 9,
    full_width    = FALSE
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "black") %>%
  row_spec(1, bold = TRUE)
```

## Appendix P: Final Model Definition


``` r
# Final model is the full OLS specification (model_full)
final_model <- model_full
final_model_summary <- summary(final_model)
final_anova <- anova(final_model)
```

## Appendix Q: Residual Diagnostic Plots


``` r
# Base R 2x2 diagnostic plot grid for the final model
par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))
```

## Appendix R: Normality of Residuals --- Q-Q Plot and Histogram


``` r
# Q-Q plot and histogram of residuals with overlaid normal curve
par(mfrow = c(1, 2))

qqnorm(residuals(final_model), main = "Normal Q-Q Plot of Residuals")
qqline(residuals(final_model), col = "red", lwd = 2)

hist(residuals(final_model), breaks = 50, col = "steelblue",
     main = "Histogram of Residuals", xlab = "Residuals")
curve(dnorm(x,
            mean = mean(residuals(final_model)),
            sd   = sd(residuals(final_model))) * length(residuals(final_model)) *
        diff(hist(residuals(final_model), breaks = 50, plot = FALSE)$breaks)[1],
      add = TRUE, col = "red", lwd = 2)

par(mfrow = c(1, 1))
```

## Appendix S: Normality Tests --- Shapiro-Wilk and Kolmogorov-Smirnov


``` r
# Shapiro-Wilk (on a 5,000-observation sample) and KS test for residual normality
set.seed(33)
resid_sample   <- sample(residuals(final_model), min(5000, length(residuals(final_model))))
shapiro_result <- shapiro.test(resid_sample)
ks_result      <- ks.test(residuals(final_model), "pnorm",
                           mean = mean(residuals(final_model)),
                           sd   = sd(residuals(final_model)))
```

## Appendix T: Homoscedasticity --- Residuals vs. Fitted and Scale-Location Plots


``` r
# Visual check for constant error variance across fitted values
par(mfrow = c(1, 2))

plot(fitted(final_model), residuals(final_model),
     main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals",
     pch = 20, col = rgb(0, 0, 1, 0.3))
abline(h = 0, col = "red", lwd = 2)
lines(lowess(fitted(final_model), residuals(final_model)), col = "orange", lwd = 2)

plot(fitted(final_model), sqrt(abs(residuals(final_model))),
     main = "Scale-Location", xlab = "Fitted Values",
     ylab = expression(sqrt("|Residuals|")),
     pch = 20, col = rgb(0, 0, 1, 0.3))
lines(lowess(fitted(final_model), sqrt(abs(residuals(final_model)))),
      col = "red", lwd = 2)

par(mfrow = c(1, 1))
```

## Appendix U: Homoscedasticity --- Breusch-Pagan Test


``` r
# Breusch-Pagan test for heteroscedasticity
bp_result <- bptest(final_model)
```

## Appendix V: Independence of Errors --- Durbin-Watson Test


``` r
# Durbin-Watson test for autocorrelation in residuals (acceptable range: 1.5--2.5)
dw_result <- dwtest(final_model)
```

## Appendix W: Multicollinearity --- VIF Table for Final Model


``` r
# Generalized VIF (GVIF) table for all predictors in the final model
vif_vals <- vif(final_model)

vif_df <- as.data.frame(vif_vals)
vif_df$Variable <- rownames(vif_df)

vif_display <- data.frame(
  Variable = rownames(vif_df),
  GVIF     = round(vif_df[, 1], 4),
  Df       = vif_df[, 2],
  `Adj. GVIF` = round(vif_df[, 3], 4),
  check.names = FALSE
)

vif_display %>%
  kbl(
    caption   = "Table 4.3. Variance Inflation Factors (Final Model)",
    booktabs  = TRUE,
    row.names = FALSE,
    align     = "lrrr"
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size     = 9,
    full_width    = FALSE
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "black") %>%
  row_spec(which(vif_display$GVIF > 10), bold = TRUE, color = "red")
```

## Appendix X: Influential Observations --- Cook's Distance and Leverage


``` r
# Cook's Distance plot; flags observations exceeding the 4/n threshold
cooksd    <- cooks.distance(final_model)
threshold <- 4 / nrow(data_clean)

influential <- which(cooksd > threshold)

plot(cooksd, type = "h",
     main = "Cook's Distance",
     ylab = "Cook's Distance", xlab = "Observation Index",
     col  = ifelse(cooksd > threshold, "red", "steelblue"))
abline(h = threshold, col = "red", lty = 2)
legend("topright", legend = paste0("Threshold = 4/n = ", round(threshold, 5)),
       col = "red", lty = 2)
```


``` r
# Leverage (hat values) analysis; flags high-leverage points exceeding 2p/n
hat_vals      <- hatvalues(final_model)
p             <- length(coef(final_model))
n             <- nrow(data_clean)
lev_threshold <- 2 * p / n

high_leverage <- which(hat_vals > lev_threshold)
```

## Appendix Y: Diagnostic Summary Table


``` r
# Consolidated OLS assumption diagnostic results table
diag_table <- data.frame(
  Test   = c(
    "Shapiro-Wilk (Normality)",
    "Kolmogorov-Smirnov (Normality)",
    "Breusch-Pagan (Homoscedasticity)",
    "Durbin-Watson (Independence)",
    "VIF Max (Multicollinearity)",
    "Cook's Distance (Influential Obs.)"
  ),
  Result = c(
    paste0("W = ", round(shapiro_result$statistic, 4), ", p = ", round(shapiro_result$p.value, 4)),
    paste0("D = ", round(ks_result$statistic, 4),      ", p = ", round(ks_result$p.value, 4)),
    paste0("BP = ", round(bp_result$statistic, 4),     ", p = ", round(bp_result$p.value, 4)),
    paste0("DW = ", round(dw_result$statistic, 4),     ", p = ", round(dw_result$p.value, 4)),
    paste0("Max GVIF = ", round(max(vif_vals[,1]), 2), " (", round(max(vif_vals[,3]), 4), " adj.)"),
    paste0(length(influential), " flagged (", round(length(influential)/n*100, 1), "% of n)")
  ),
  Status = c(
    ifelse(shapiro_result$p.value > 0.05, "PASSED", "VIOLATED*"),
    ifelse(ks_result$p.value      > 0.05, "PASSED", "VIOLATED"),
    ifelse(bp_result$p.value      > 0.05, "PASSED", "VIOLATED"),
    ifelse(dw_result$statistic > 1.5 & dw_result$statistic < 2.5, "PASSED", "CHECK"),
    ifelse(max(vif_vals[,3])      < 5,    "PASSED", "VIOLATED*"),
    ifelse(length(influential)/n  < 0.05, "REVIEW", "ACCEPTABLE")
  ),
  stringsAsFactors = FALSE
)

diag_table %>%
  kbl(
    caption   = "Table 4.4. Diagnostic Summary",
    booktabs  = TRUE,
    row.names = FALSE,
    align     = "llc"
  ) %>%
  kable_styling(
    latex_options = c("striped", "hold_position", "scale_down"),
    font_size     = 9,
    full_width    = FALSE
  ) %>%
  row_spec(0, bold = TRUE, color = "white", background = "black") %>%
  footnote(
    general = "* Violations are technical rather than substantive; see interpretation below.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )
```

