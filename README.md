# Heart Disease Risk Prediction & Resource Prioritization

**Can routine patient data help a clinic decide who to see first?**
This project predicts heart disease risk and uses those predictions to optimize which waiting-list patients get follow-up appointments under a fixed budget and slot limit.

**Tools:** R · caret · lpSolve · ggplot2 | **Data:** UCI Cleveland Heart Disease dataset (297 patients, 14 variables)

## 🔑 Key Results
- **Logistic regression** was the best model: **85.4% test accuracy, 0.908 AUC, 80.5% sensitivity**
- **Optimized scheduling found ~36% more expected cases** than first-come-first-served (21.06 vs. 15.51) at the same $9,000 budget
- Asymptomatic patients had a **72.5% disease rate**, far higher than patients reporting chest pain

## 📊 Project Workflow
| Step | What I did | File |
|---|---|---|
| 1. Descriptive | Cleaned data, summary stats, outlier checks, collinearity (GVIF), disease rate by chest pain type | `Heart_Disease_Descriptive.R` |
| 2. Predictive | Compared logistic regression, decision tree and random forest with 10-fold CV and a 70/30 test split | `Heart_Disease_Predictive (1).R` |
| 3. Prescriptive | Binary integer program (lpSolve) to choose patients that maximize expected cases within 35 slots and $9,000; budget sensitivity analysis | `HeartDisease_Prescriptive.R` |

## 📈 Model Comparison (test set, n = 89)
| Model | Accuracy | Sensitivity | Specificity | AUC |
|---|---|---|---|---|
| Logistic Regression | 0.854 | 0.805 | 0.896 | 0.908 |
| Random Forest (tuned) | 0.798 | 0.732 | 0.854 | 0.907 |
| Decision Tree (tuned) | 0.775 | 0.634 | 0.896 | 0.774 |

## 🏥 Scheduling Results ($9,000 budget, 35 slots)
| Plan | Patients Seen | Expected Cases | Actual Cases |
|---|---|---|---|
| **Optimized** | 30 | **21.06** | **20** |
| Highest-risk-first | 23 | 20.19 | 19 |
| First-come-first-served | 33 | 15.51 | 15 |

📄 Full write-up: `Final_Analysis_of_heart_disease_prediction.pdf`

## 📚 Data Source
Janosi, A., Steinbrunn, W., Pfisterer, M., & Detrano, R. (1989). *Heart Disease* [Dataset]. UCI Machine Learning Repository.
