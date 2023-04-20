import os
os.chdir("D:/파이썬 중급")

import pandas as pd
df1 = pd.read_excel("stock price.xlsx")
df2 = pd.read_excel("stock valuation.xlsx")
df = pd.merge(df1, df2, on="id", how='left')

pd.concat([df1, df2, df3, df4], join='inner')

# 1) 2011(data1), 2012(data2) 해상조난사고데이터 불러오기
import pandas as pd
data1 = pd.read_excel("2011년 해상조난사고 상세데이터.xlsx")
data2 = pd.read_excel("2012년 해상조난사고 상세데이터.xlsx")
# 2) 두개의 자료를 결합(data)
# pd.concat()
data = pd.concat([data1, data2], join='outer', axis=0)
# 3) 발생일시 변수에서 연도자료 추출 year 변수 추가
# dt.year
data['year'] = data['발생일시'].dt.year
# 4) 구조, 부상, 사망 인원의 결측 자료 삭제
# .dropna()
data.dropna(subset=['구조인원', '부상인원', '사망인원'],
            axis=0, how='any', inplace=True)
data.count()
data.shape
import seaborn as sns
mpg = sns.load_dataset('mpg')

sns.heatmap(mpg.corr(),
            vmin=-1,vmax=1,
            cmap='vlag',
            annot=True
            )

# sklearn의 명령어 특징
# 객체 = 모델링명령어(옵션들).fit(독립데이터, 종속데이터)
# 객체.모델링 결과 확인 명령어
# coef_: 계수 확인
# .predict(독립데이터): 예측결과 확인
# .predict_proba(독립데이터): 예측 확률 확인
y = mpg['mpg']
X = mpg[['acceleration', 'weight']]
from sklearn.linear_model import LinearRegression
result = LinearRegression().fit(X, y)
result.intercept_
result.coef_
y_hat = result.predict(X)
result.score(X,y) # R^2
from sklearn.metrics import mean_squared_error
mean_squared_error(y, y_hat) #MSE
from sklearn.metrics import mean_absolute_error
mean_absolute_error(y, y_hat) #MAE

# VIF
from statsmodels.stats.outliers_influence import variance_inflation_factor
vif = pd.DataFrame()
vif['VIF'] = [variance_inflation_factor(X.values, i) for i in range(X.shape[1])]
vif['Variable'] = X.columns

# Hold-Out
# from sklearn.model_selection import train_test_split
# 4개의 객체 = train_test_split(자료1, 자료2, test_size=테스트 비율,
#                  random_state=난수번호,
#                  stratify=층화변수(분류예측))
# 실행결과
# => 자료1의 train, 자료1의 test, 
# => 자료2의 train, 자료2의 test
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0)

from sklearn.linear_model import LinearRegression
result = LinearRegression().fit(X_train, y_train)
result.intercept_
result.coef_
y_train_hat = result.predict(X_train)
y_test_hat = result.predict(X_test)
result.score(X_train,y_train) # R^2
result.score(X_test,y_test) # R^2
from sklearn.metrics import mean_squared_error
mean_squared_error(y_train, y_train_hat) #MSE
mean_squared_error(y_test, y_test_hat) #MSE
from sklearn.metrics import mean_absolute_error
mean_absolute_error(y_train, y_train_hat) #MAE
mean_absolute_error(y_test, y_test_hat) #MAE

# 1) 허리 둘레.csv 불러오기
df = pd.read_csv('허리둘레.csv', encoding='cp949')
# 2) 허리둘레(윗허리) : 0은 결측자료
import numpy as np
df['허리둘레'] = df['허리둘레(윗허리)'].replace(0, np.nan)
df = df.dropna(subset=['허리둘레'], axis=0)
# 3) BMI 계산: 몸무게/(키(m)**2)
df['BMI'] = df['몸무게']/((df['키']/100)**2)

# 4) BMI등급화: <18.5 저체중, 18.5<= < 23: 정상, 23<=비만
df['BMI_c'] = pd.cut(df['BMI'], bins=[0,18.5,23,100],
       labels=['저체중', '정상', '비만'], right=False)

# 5) BMI를 더미변수
df = pd.get_dummies(df, columns=['BMI_c'])

# 6) 머리둘레: 종속변수, BMI 더미, 허리둘레, 엉덩이 둘레: 독립변수
y = df['머리둘레']
X = df[['엉덩이둘레', '허리둘레', 'BMI_c_저체중', 'BMI_c_정상', 'BMI_c_비만']]

# 7) 선형회귀분석(sklearn 모듈 이용) - train: test = 7:3
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0)

from sklearn.linear_model import LinearRegression
result = LinearRegression().fit(X_train, y_train)
result.intercept_
result.coef_
y_train_hat = result.predict(X_train)
y_test_hat = result.predict(X_test)
result.score(X_train,y_train) # R^2
result.score(X_test,y_test) # R^2

from sklearn.metrics import mean_squared_error
mean_squared_error(y_train, y_train_hat) #MSE
mean_squared_error(y_test, y_test_hat) #MSE
from sklearn.metrics import mean_absolute_error
mean_absolute_error(y_train, y_train_hat) #MAE
mean_absolute_error(y_test, y_test_hat) #MAE


# import statsmodels.formula.api as smf
# 객체 = smf.ols(formula = '종속변수 ~ 독립변수1+독립변수2', data=분석데이터).fit()
# 객체.summary() : 종합결과
# 객체.predict(독립데이터) : 예측 종속변수 값

from sklearn.model_selection import train_test_split
train, test = train_test_split(mpg, test_size=0.3,
                               random_state=0)

import statsmodels.formula.api as smf
result = smf.ols(formula='mpg ~ acceleration + weight',
                 data = train).fit()
result.summary()


# import statsmodels.api as sm
# 객체 = sm.OLS(종속데이터, 독립데이터).fit()
# 객체.summary() : 종합결과
# 객체.predict(독립데이터) : 예측 종속변수 값

y = mpg['mpg']
X = mpg[['acceleration', 'weight']]
import statsmodels.api as sm
X_ad = sm.add_constant(X)
result = sm.OLS(y, X_ad).fit()
result.summary()

# sklearn.linear_model
# LinearRegression().fit()
# 장점: 빠르게 계산, 출력
# 단점: 유의확률 알 수 없음

# statsmodels.formula.api
# smf.ols().fit()
# 장점: R과 유사, 유의확률 알 수 있음, 자료 분리 필요 없음, 결측 삭제 안해도 됨
# 단점: sklearn 사용하려면 데이터 추가 전처리 필요

# statsmodels.api
# sm.OLS().fit()
# 장점: sklearn과 유사, 유의확률 알 수 있음
# 단점: 절편항 추가 시 add_constant() 사용, 결측자료 미리 삭제 


y = df['머리둘레']
X = df[['엉덩이둘레', '허리둘레', 'BMI_c_저체중', 'BMI_c_정상', 'BMI_c_비만']]

formula = '머리둘레 ~ 엉덩이둘레 + 허리둘레 + BMI_c_저체중  +BMI_c_정상 + BMI_c_비만'
import statsmodels.formula.api as smf
result = smf.ols(formula=formula, data = df).fit()
result.summary()

import statsmodels.api as sm
X_ad = sm.add_constant(X)
result = sm.OLS(y,X_ad).fit()
result.summary()

from sklearn.linear_model import LogisticRegression
객체 = LogisticRegression(옵션들).fit(독립, 종속)
객체.coef_ : 회귀계수
객체.predict(독립) : 예측값(분류된)
객체.predict_proba(독립) : 예측값(확률)
!!옵션들
penalty='l2', 'l1', None(통계패키지와 같은 결과)
tol = 수렴 판단 기준값
solver = 수치해석 방법(뉴튼랩슨 방법 등...)
'lbfgs', 'liblinear', 'newton-cg', 'newton-cholesky', 'sag', 'saga'

import seaborn as sns
titanic = sns.load_dataset('titanic')
titanic.info()

# 종속: survived
# 독립변수: fare(연속), class(범주)
data = pd.get_dummies(titanic, columns=['class'])
data.info()

y = data['survived']
X = data[['fare', 'class_First', 
          'class_Second', 'class_Third']]

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, 
                 y, test_size=0.3, random_state=0,
                 stratify=y)

from sklearn.linear_model import LogisticRegression
LR = LogisticRegression(penalty=None).fit(X_train, y_train)
LR.coef_ # 회귀계수
import numpy as np
np.exp(LR.coef_) # 오즈비 계산
y_test_hat1 = LR.predict(X_test) # 확률값 0.5를 기준으로 분류
y_test_prob = LR.predict_proba(X_test)[:,1] # P(y=1)

# import numpy as np
# np.where(조건, 참일때, 거짓일 때)
y_test_hat2 = np.where(y_test_prob > 0.4, 1, 0)
y # 실제값
y_hat1  # 예측값
y_hat2  # 예측값

# from sklearn.metrics import confusion_matrix
# confusion_matrix(실제값, 예측값)
from sklearn.metrics import confusion_matrix
confusion_matrix(y_test, y_test_hat1)

# from sklearn.metrics import classification_report
# classification_report(실제값, 예측값)
from sklearn.metrics import classification_report
print(classification_report(y_test, y_test_hat1))


# 1) origin에서 'usa', 'japan' 인 자료 추출(europe 제외)
df = mpg.loc[mpg['origin']!='europe']

# 2) origin 예측하는 모델링 (자료가 숫자여야 함)
from sklearn.preprocessing import LabelEncoder
classle = LabelEncoder()
df['origin'] = classle.fit_transform(df['origin'])
# 문자 -> 숫자
# 문자에 숫자 넘버링 기준: a(0) -> z(1)


# 3) 종속변수(origin)
#    독립변수(mpg, weight)
y = df['origin']
X = df[['mpg','weight']]

# 4) train: test = 7:3
from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0,
                 stratify=y)

from sklearn.linear_model import LogisticRegression
LR = LogisticRegression(penalty=None).fit(X_train, y_train)
LR.coef_
import numpy as np
np.exp(LR.coef_)
y_test_hat = LR.predict(X_test)

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_test, y_test_hat)
print(classification_report(y_test, y_test_hat))


# ROC curve
from sklearn.metrics import roc_curve, auc
y_train_prob = LR.predict_proba(X_train)[:,1]
# fpr, tpr, _ = roc_curve(y 실제값, y=1확률)
# fpr, tpr 계산
fpr, tpr, _ = roc_curve(y_train, y_train_prob)
roc_auc = auc(fpr, tpr) #AUC 계산

import matplotlib.pyplot as plt
# plt.plot(x축값, y축값, label= 범례 내용)
plt.plot(fpr, tpr, 'b', label = 'AUC=%.2f' %roc_auc)
plt.plot([0,1],[0,1],'--')
plt.legend(loc='best')




y = df['origin']
X = df[['mpg','weight']]

import statsmodels.api as sm
X_ad = sm.add_constant(X)

from sklearn.model_selection import train_test_split
X_train, X_test, y_train, y_test = train_test_split(X_ad, 
                 y, test_size=0.3, random_state=0,
                 stratify=y)

import statsmodels.api as sm
LR = sm.Logit(y_train, X_train).fit()
LR.summary()
y_train_prob = LR.predict(X_train)
y_train_pred = np.where(y_train_prob>0.5,1,0)
y_test_prob = LR.predict(X_test)
y_test_pred = np.where(y_test_prob>0.5,1,0)

from sklearn.metrics import confusion_matrix, classification_report
confusion_matrix(y_train, y_train_pred)
print(classification_report(y_train, y_train_pred))
confusion_matrix(y_test, y_test_pred)
print(classification_report(y_test, y_test_pred))



