# Enhancing NYC 311: Identifying Key Drivers of Dissatisfaction & Trends

This project investigates **why NYC 311 users are dissatisfied** and how complaint patterns evolve over time. This repository combines **statistical analysis, natural language processing, and time-series modeling** to uncover operational, linguistic, and seasonal drivers of dissatisfaction in New York City’s 311 service system.

The project integrates NYC Open Data service request records with resolution satisfaction survey data to generate **actionable insights for improving public service transparency, communication quality, and resource allocation**.

---

## 🔍 Project Motivation

NYC 311 is one of the largest civic service platforms in the world, yet resident satisfaction remains persistently low. While prior analyses often focus on complaint volumes or response times, fewer studies examine **how agencies communicate resolutions** and how that language shapes public perception.

This project addresses that gap by:

* Analyzing **which complaint types and boroughs drive dissatisfaction**
* Evaluating the impact of **vague or boilerplate resolution language** on satisfaction
* Identifying **seasonal and long-term trends** in high-volume complaint categories

---

## 🎯 Research Questions

**RQ1.** Which complaint types and borough-level characteristics are associated with low satisfaction or unresolved cases?

**RQ2.** How does resolution description language correlate with citizen satisfaction, and how does this vary by complaint type and agency?

**RQ3.** How have complaint volumes changed over time, and what seasonal or trend patterns can inform proactive resource planning?

---

## 📊 Data Sources

* **NYC 311 Service Requests (2022–Present)**
  Complaint type, agency, borough, timestamps, and resolution descriptions

* **NYC 311 Resolution Satisfaction Survey**
  Post-resolution satisfaction responses linked to complaint characteristics

Both datasets are sourced from **NYC Open Data** and aligned temporally and categorically for analysis.

---

## 🧠 Analytical Methods

### 1️⃣ Descriptive & Statistical Analysis

* Borough- and complaint-level complaint distribution
* Dissatisfaction and unresolved rate comparisons
* Logistic regression modeling to identify predictors of dissatisfaction

### 2️⃣ Natural Language Processing (NLP)

* Text preprocessing and boilerplate phrase detection
* **LDA topic modeling** on resolution descriptions
* Classification of resolution language into **vague vs. action-oriented themes**
* Regression analysis linking linguistic patterns to satisfaction outcomes

### 3️⃣ Time-Series Analysis & Forecasting

* Monthly aggregation of top complaint types
* STL decomposition to extract trend and seasonality
* ETS and ARIMA forecasting models to evaluate predictability and operational value

---

## 🔑 Key Findings

* **Noise and illegal parking complaints** consistently drive high dissatisfaction, especially in Brooklyn and Manhattan
* Infrastructure-related complaints show **higher unresolved rates** despite lower dissatisfaction
* Resolution descriptions containing **vague or non-actionable language** (e.g., “no violation observed”, “attempted to inspect”) significantly reduce satisfaction
* Complaint volumes exhibit **strong seasonal patterns** (e.g., winter heating complaints, summer noise issues), enabling proactive planning

---

## 🛠️ Tech Stack

* **Languages**: R
* **Statistical Modeling**: Logistic Regression
* **NLP**: LDA Topic Modeling
* **Time Series**: STL Decomposition, ETS, ARIMA
* **Visualization**: ggplot2
* **Data Source**: NYC Open Data

---

## 🚀 Impact & Recommendations

This project demonstrates that **how agencies communicate** is as important as what they do. Improving resolution clarity, reducing vague language, and aligning staffing with seasonal demand can materially improve public trust and satisfaction.

The framework can be extended to other cities or integrated into operational dashboards to support **data-driven public service reform**.

---

## 👤 Authors

**Minkyung Kim**, Boni Vasius Rosen, Minqi Huang, Jiarong Ying
Columbia University – Applied Analytics 

---

## 📄 License

This project is for academic and research purposes only.
