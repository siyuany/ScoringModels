# ScoringModels - 自动进行评分卡建模的R包

## 功能 Features
----------------------------------------------

* 数据概览：生成数据集概览xlsx文件，涵盖所有字段名、（推断）类型、值枚举、唯一值统计、分布统计等(**完成**)

* 数据预处理：根据xlsx文件对数据集进行整理，并提供数据预处理接口函数，生成最终入模变量概览xlsx文件(**完成**)

* 变量相关性分析：支持Pearson/Spearman/Kendall相关系数计算，用于变量初步筛选，`pairwise.complete.obs = TRUE`

* 变量自动分箱（可选）：根据目标变量对解释变量进行监督式分箱，WoE/IV

* 变量二次筛选（可选）：IV值筛选、Boruta

* 模型拟合：支持 Logistic Regression 及 xgboost

* 模型验证：验证样本集

* 评分代码生成（pending）
