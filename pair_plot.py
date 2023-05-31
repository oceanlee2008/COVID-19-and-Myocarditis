# -*- coding: utf-8 -*-
"""
@Env: Python3.7
@Time: 2022/8/8 18:18
@Author: Haiyang Li
@Function： Genetic Algorithm - Random Forest（RF)
遗传算法下的随机森林二分类
参考文献：
[1] UCI. wine[DB/OL].https://archive.ics.uci.edu/ml/machine-learning-databases/wine.
"""

import pandas as pd
import numpy 
import numpy as np
import random
import math
import collections
import matplotlib.pyplot as plt
import seaborn as sns

print(np.__version__)
print(pd.__version__)




df = pd.read_excel("positive-INDEX.xlsx" ,sheet_name='paircopy')   #读取文件
print(df)

plt.figure(figsize=(40, 50))
sns.set(style="ticks",font= "Arial",font_scale = 2)
g=sns.pairplot(df,hue='label', diag_kind="kde",kind="kde",palette="pastel",plot_kws={"alpha":0.15})
g.map_upper(sns.kdeplot, levels=6, color=".3")
g.map_lower(sns.scatterplot, s=30, color=".3", edgecolor="w")
# plt.text(5, 5, "H_cancer", ha="center", va="center", size=30, color="Black")
# plt.text(5, 6, "F_cancer", ha="center", va="center", size=30, color="Black")

plt.savefig('./2023.5.8_pair3.pdf')
g = sns.PairGrid(df, hue='label',palette="pastel",grid_kws={'size':3})
g.map_upper(sns.kdeplot, levels=6, color=".4")
g.map_lower(sns.scatterplot, s=50, color=".4", edgecolor="w")
g.map_diag()

plt.savefig('./2023.5.5_pair_covid.pdf')

# df = df[df['label'].isin([1, 6])].sample(frac=1, random_state=3333).reset_index(drop=True)
# plt.savefig('pair_W_H_C22.pdf')