# KNN 예제 1)
import seaborn as sns
iris = sns.load_dataset('iris')

y = iris['species']
X = iris.drop('species',axis=1)

from sklearn.preprocessing import LabelEncoder
y = LabelEncoder().fit_transform(y)

# from sklearn.preprocessing import StandardScaler
# sc = StandardScaler(옵션)
# sc.fit(자료) # 표준화 식 완성 => 평균, 표준편차 계산
# sc.transform(자료) # 자료를 표준화

# train => 표준화 => 모델링 
# 검증시 test 자료는 train 자료 표준화식에 대입하여 표준화
# line 27~30 참고


from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0,
                 stratify=y)

from sklearn.preprocessing import StandardScaler
sc = StandardScaler().fit(X_train)
X_train_std = sc.transform(X_train)
X_test_std = sc.transform(X_test)


from sklearn.neighbors import KNeighborsClassifier
knn = KNeighborsClassifier(n_neighbors=5).fit(X_train, y_train)
y_train_pred = knn.predict(X_train)
y_test_pred = knn.predict(X_test)

from sklearn.neighbors import KNeighborsClassifier
knn_std = KNeighborsClassifier(n_neighbors=5).fit(X_train_std, y_train)
y_train_pred_std = knn_std.predict(X_train_std)
y_test_pred_std = knn_std.predict(X_test_std)



from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred)
print(classification_report(y_train, y_train_pred))
confusion_matrix(y_test, y_test_pred)
print(classification_report(y_test, y_test_pred))

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred_std)
print(classification_report(y_train, y_train_pred_std))
confusion_matrix(y_test, y_test_pred_std)
print(classification_report(y_test, y_test_pred_std))


from sklearn.metrics import recall_score, precision_score, f1_score
#예측하고자 하는 결과가 3개 이상인 경우
# average=None 지정
recall_score(y_test, y_test_pred, average=None)
precision_score(y_test, y_test_pred, average=None)
f1_score(y_test, y_test_pred, average=None)


# KNN 예제 2)
import os
os.chdir("D:/파이썬 중급")

import pandas as pd
df = pd.read_csv('허리둘레.csv', encoding='cp949')
import numpy as np
df['허리둘레'] = df['허리둘레(윗허리)'].replace(0, np.nan)
df = df.dropna(subset=['허리둘레'], axis=0)
df['BMI'] = df['몸무게']/((df['키']/100)**2)
df['BMI_c'] = pd.cut(df['BMI'], bins=[0,18.5,23,100],
       labels=['저체중', '정상', '비만'], right=False)

y = df['BMI_c']
X = df[['엉덩이둘레', '허리둘레', '머리둘레']]

from sklearn.preprocessing import LabelEncoder
y = LabelEncoder().fit_transform(y)

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0,
                 stratify=y)

from sklearn.preprocessing import StandardScaler
sc = StandardScaler().fit(X_train)
X_train_std = sc.transform(X_train)
X_test_std = sc.transform(X_test)


from sklearn.neighbors import KNeighborsClassifier
knn = KNeighborsClassifier(n_neighbors=5).fit(X_train, y_train)
y_train_pred = knn.predict(X_train)
y_test_pred = knn.predict(X_test)


from sklearn.neighbors import KNeighborsClassifier
knn_std = KNeighborsClassifier(n_neighbors=5).fit(X_train_std, y_train)
y_train_pred_std = knn_std.predict(X_train_std)
y_test_pred_std = knn_std.predict(X_test_std)


from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred)
print(classification_report(y_train, y_train_pred))
confusion_matrix(y_test, y_test_pred)
print(classification_report(y_test, y_test_pred))

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred_std)
print(classification_report(y_train, y_train_pred_std))
confusion_matrix(y_test, y_test_pred_std)
print(classification_report(y_test, y_test_pred_std))














# SVM 예제 1)
import seaborn as sns
iris = sns.load_dataset('iris')

y = iris['species']
X = iris.drop('species',axis=1)

from sklearn.preprocessing import LabelEncoder
y = LabelEncoder().fit_transform(y)

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0,
                 stratify=y)

# from sklearn.svm import SVC
# 객체 = SVC(kernel='linear'/'rbf', C= 오차허용정도(실수값),
#     gamma = 곡률정도(실수값), random_state=난수값(정수값),
#     probability=True(확률값 제공)).fit(독립, 종속)
# 객체.predict(독립) : 예측 종속값
# 객체.predict_proba(독립): 예측 종속 확률값(probability=True인 경우만)

from sklearn.svm import SVC
svm1 = SVC(kernel='linear', C=1, probability=True).fit(X_train, y_train)
y_train_pred = svm1.predict(X_train)
y_test_pred = svm1.predict(X_test)
y_train_prob = svm1.predict_proba(X_train)[:,1]

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred)
print(classification_report(y_train, y_train_pred))
confusion_matrix(y_test, y_test_pred)
print(classification_report(y_test, y_test_pred))


svm2 = SVC(kernel='rbf', C=1, gamma = 1, probability=True).fit(X_train, y_train)
y_train_pred = svm2.predict(X_train)
y_test_pred = svm2.predict(X_test)
y_train_prob = svm2.predict_proba(X_train)[:,1]

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred)
print(classification_report(y_train, y_train_pred))
confusion_matrix(y_test, y_test_pred)
print(classification_report(y_test, y_test_pred))


# 초모수 결정 방법 - GridSearchCV
# from sklearn.model_selection import GridSearchCV

# 객체 = GridSearchCV(모델링 명령어, param_grid= 적용하고자 하는 모수값들,
#              scoring='accuracy', cv = kfold의 k값).fit(독립, 종속)
# 객체.best_score_ : 가장 좋은 정분류률 결과 출력
# 객체.best_params_ : 가장 좋은 결과를 나타내는 옵션들 값 출력
# 객체.best_estimator_ : 가장 좋은 옵션을 넣은 모델링 실행

# 모수값들: [{'모델링 옵션1': [값1, 값2, ...],
#           '모델링 옵션2': [값1, 값2, ...]}]


from sklearn.model_selection import GridSearchCV
param_score = [0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000]
param_grid = [{'C': param_score, 'kernel': ['linear']},
           {'C': param_score, 'gamma': param_score, 'kernel': ['rbf']}]

gs = GridSearchCV(SVC(random_state=0), param_grid= param_grid,
             scoring='accuracy', cv = 10).fit(X_train, y_train)
gs.best_score_
gs.best_params_
svm1 = gs.best_estimator_.fit(X_train, y_train)

y_train_pred = svm1.predict(X_train)
y_test_pred = svm1.predict(X_test)

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred)
print(classification_report(y_train, y_train_pred))
confusion_matrix(y_test, y_test_pred)
print(classification_report(y_test, y_test_pred))


from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import make_pipeline
pipe = make_pipeline(StandardScaler(), SVC(random_state=0))

param_score = [0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000]
param_grid = [{'C': param_score, 'kernel': ['linear']},
           {'C': param_score, 'gamma': param_score, 'kernel': ['rbf']}]

gs = GridSearchCV(pipe, param_grid= param_grid,
             scoring='accuracy', cv = 10).fit(X_train, y_train)

# 중첩 교차검증
# from sklearn.model_selection import GridSearchCV, cross_val_score
# 이너루프 모델링 함수 = GridSearchCV(모델링 명령어, param_grid= 적용하고자 하는 모수값들,
#              scoring='accuracy', cv = kfold의 k값)
# cross_val_score(이너루프 모델링 함수, 독립, 종속, scoring='accuracy',
#                 cv = 아웃루프의 k값) : 아웃루프

from sklearn.model_selection import GridSearchCV, cross_val_score
from sklearn.model_selection import StratifiedKFold
param_score = [0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000]
param_grid = [{'C': param_score, 'kernel': ['linear']},
           {'C': param_score, 'gamma': param_score, 'kernel': ['rbf']}]

kfold = StratifiedKFold(n_splits=5)
gs = GridSearchCV(SVC(random_state=0), param_grid= param_grid,
             scoring='accuracy', cv = 3)
score = cross_val_score(gs, X, y, scoring='accuracy', cv=kfold)
import numpy as np
np.mean(score)
np.std(score)









# SVM 예제 2)
import os
os.chdir("D:/파이썬 중급")

import pandas as pd
df = pd.read_csv('허리둘레.csv', encoding='cp949')
import numpy as np
df['허리둘레'] = df['허리둘레(윗허리)'].replace(0, np.nan)
df = df.dropna(subset=['허리둘레'], axis=0)
df['BMI'] = df['몸무게']/((df['키']/100)**2)
df['BMI_c'] = pd.cut(df['BMI'], bins=[0,18.5,23,100],
       labels=['저체중', '정상', '비만'], right=False)

df['BMI_c_1'] = np.where(df['BMI_c']!='정상',0,1)
y = df['BMI_c_1']
X = df[['엉덩이둘레', '허리둘레', '머리둘레']]

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0,
                 stratify=y)


from sklearn.svm import SVC
svm1 = SVC(kernel='linear', C=1, probability=True).fit(X_train, y_train)
y_train_pred = svm1.predict(X_train)
y_test_pred = svm1.predict(X_test)
y_train_prob = svm1.predict_proba(X_train)[:,1]

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred)
print(classification_report(y_train, y_train_pred))
confusion_matrix(y_test, y_test_pred)
print(classification_report(y_test, y_test_pred))


svm2 = SVC(kernel='rbf', C=1, gamma = 1, probability=True).fit(X_train, y_train)
y_train_pred = svm2.predict(X_train)
y_test_pred = svm2.predict(X_test)
y_train_prob = svm2.predict_proba(X_train)[:,1]

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred)
print(classification_report(y_train, y_train_pred))
confusion_matrix(y_test, y_test_pred)
print(classification_report(y_test, y_test_pred))



from sklearn.model_selection import GridSearchCV
param_score = [0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000]
param_grid = [{'C': param_score, 'kernel': ['linear']},
           {'C': param_score, 'gamma': param_score, 'kernel': ['rbf']}]

gs = GridSearchCV(SVC(random_state=0), param_grid= param_grid,
             scoring='accuracy', cv = 10).fit(X_train, y_train)
gs.best_score_
gs.best_params_
svm1 = gs.best_estimator_.fit(X_train, y_train)

y_train_pred = svm1.predict(X_train)
y_test_pred = svm1.predict(X_test)

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred)
print(classification_report(y_train, y_train_pred))
confusion_matrix(y_test, y_test_pred)
print(classification_report(y_test, y_test_pred))





# DTC 예제 1)
import seaborn as sns
iris = sns.load_dataset('iris')

y = iris['species']
X = iris.drop('species',axis=1)

from sklearn.preprocessing import LabelEncoder
y = LabelEncoder().fit_transform(y)

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0,
                 stratify=y)

# from sklearn.tree import DecisionTreeClassifier
# 객체 = DecisionTreeClassifier(criterion='gini'/'entropy',
#                        max_depth=나무깊이(정수값, None),
#                        random_state=난수값(정수)).fit(독립, 종속)
# 객체.predict(독립) : 분류 결과 확인

from sklearn.tree import DecisionTreeClassifier
dtc = DecisionTreeClassifier(criterion='gini',
                       max_depth=None,
                       random_state=0).fit(X_train, y_train)
y_train_hat = dtc.predict(X_train)
y_test_hat = dtc.predict(X_test)


pip install pydotplus
from pydotplus import graph_from_dot_data
from sklearn.tree import export_graphviz
dot_data = export_graphviz(dtc, filled=True, rounded=True,
                           class_names = ['setosa', 'versicolor', 'virginica'],
                           feature_names=X_train.columns)
graph = graph_from_dot_data(dot_data)

from IPython.display import Image
Image(graph.create_png())
graph.write_png('tree.png')


from sklearn.model_selection import GridSearchCV
param_score = [1, 2, 3, 4, 5, 6, None]
param_grid = [{'max_depth': param_score, 'criterion': ['gini', 'entropy']}]

gs = GridSearchCV(DecisionTreeClassifier(random_state=0), param_grid= param_grid,
             scoring='accuracy', cv = 10).fit(X_train, y_train)
gs.best_score_
gs.best_params_
dtc1 = gs.best_estimator_.fit(X_train, y_train)


# 군집분석

from scipy.cluster.hierarchy import linkage
clusters = linkage(y=X, method='average', metric='euclidean')
clusters

from scipy.cluster.hierarchy import dendrogram
import matplotlib.pyplot as plt
plt.figure(figsize = (15, 6)) # 그래프 사이즈를 조정
dendrogram(clusters, leaf_rotation=90, leaf_font_size=12)
plt.tight_layout()
plt.ylabel('Euclidean distrance')
plt.show() 

from scipy.cluster.hierarchy import fcluster
cut_tree = fcluster(clusters, t=3.8, criterion='distance')
cut_tree



from sklearn.cluster import KMeans
model = KMeans(n_clusters=3, random_state=0, init='k-means++').fit(X)
pred = model.predict(X)

# 불러오기
import pandas as pd
data = pd.read_csv('서울지하철.csv', encoding='cp949')
data.info()
# 변수생성
data['출근승차'] = data['04시-05시 승차인원']+data['05시-06시 승차인원']
data['출근하차'] = data['04시-05시 하차인원']+data['05시-06시 하차인원']
data['퇴근승차'] = data['18시-19시 승차인원']+data['19시-20시 승차인원']
data['퇴근하차'] = data['18시-19시 하차인원']+data['19시-20시 하차인원']
X = data[['출근승차', '출근하차', '퇴근승차', '퇴근하차']]

# 군집분석 수행 - 계층적
from scipy.cluster.hierarchy import linkage, dendrogram
clusters = linkage(X, method='average', metric='euclidean')
import matplotlib.pyplot as plt
plt.figure(figsize=(15,6))
dendrogram(clusters, leaf_rotation=90, leaf_font_size=10)

# 근집분석 수행 - 비계층적
from sklearn.cluster import KMeans
model = KMeans(n_clusters=3, init='k-means++').fit(X)
pred = model.predict(X)


data['출근승차'] = data.iloc[:,[3,5]].sum(axis=1)
data['출근하차'] = data.iloc[:,[4,6]].sum(axis=1)
data['퇴근승차'] = data.iloc[:,[31,33]].sum(axis=1)
data['퇴근하차'] = data.iloc[:,[32,34]].sum(axis=1)



