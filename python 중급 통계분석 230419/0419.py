df = [[10, 20],[5, 2]]

import pandas as pd
data = pd.DataFrame(df, columns=['Y', 'N'],
             index= ['A', 'B'])

# 데이터프레임.to_excel('내보낼 위치')
data.to_excel('실습.xlsx')
import seaborn as sns
df = sns.load_dataset('mpg')
df.describe().to_excel('실습1.xlsx')

df = pd.read_csv('auto-mpg.csv',
            header=None)
# 변수명 일괄적으로 변경
df.columns = ['mpg', 'cylinders', 
              'displacement', 
 'horsepower', 'weight', 
 'acceleration', 'model year', 
 'origin', 'name']

# 변수명 중 일부만 변경
# inplace=True : 원본 변경 저장
# 원본 변경 저장: 새로운 객체에 다시 재할당
df.rename(columns={기존이름:새이름})
df1 = df.rename(columns={'model year':'year'})
df.rename(columns={'model year':'year'},
          inplace=True)

# 변수 삭제
# 행열 선택 옵션
# axis = 0(행, 기본), 1(열)
df.drop([삭제할 변수명], axis=1)
df1.drop('name', axis=1)
# drop 용어: 삭제를 의미

# 변수선택
df1.mpg # 1개의 변수만 선택가능
df1['mpg'] # 여러개의 변수를 선택가능
df1[['mpg', 'year']]

# 종속변수: mpg, 독립변수: 나머지
y = df1['mpg']
X = df1.drop('mpg', axis=1)

df1.loc[행이름,열이름]
df1.iloc[행번호,열번호]
# 불린인덱싱(조건식을 활용 데이터 가져오기)
# df1.loc[조건식]
df1['mpg']>20 (O)
'mpg'>20 (X)
df1.loc[df1['mpg']>20]

# 조건식이 2개 이상인 경우
# (조건식1) & (조건식2) : 교집합
# (조건식1) | (조건식2) : 합집합
# ==(같다), != (다르다)
(df1['mpg']>20) & (df1['cylinders']>6)

# 변수 추가 - 기존변수를 활용하여 
#            새변수 추가
# 새로운 데이터 자리에 하나의 숫자만 입력
# => 모두 동일한 자료로 입력
# df1['새로운 변수명'] = 새로운 데이터

df1[['mpg', 'cylinders']].mean(axis=1)
df1['mpg_1'] = df1[['mpg', 'cylinders']].mean(axis=1)

# 행과 열 위치 변경
df1.describe().T

# 인덱스 별 정렬 (기본: 오름차순)
# ascending=False :내림차순
df1.sort_index(ascending=False)
df1['origin'].value_counts().sort_index()

# 최빈값
df1['origin'].value_counts().idxmax()

# 변수에 따른 값 정렬.
df1.sort_values(by=기준변수명)
df1.sort_values(by='origin')

# 피어슨 상관행렬 => 유의확률 제공 안함
# 상관계수값만 제공
df1.corr()
# 순위 상관행렬
df1.corr(method='spearman')

# 상관계수 유의성 검정
import scipy.stats as stats
stats.pearsonr(df1['mpg'], df1['acceleration'])
stats.spearmanr(df1['mpg'], df1['acceleration'])

# 패키지 추가 설치
import pingouin as pg
pg.corr(df1['mpg'], df1['acceleration'],
        alternative='greater',
        method='spearman')

import seaborn as sns
data = sns.load_dataset('titanic')
data.count()
data1 = data.dropna(axis=1,thresh=450)
data1.count()
data1.dropna(axis=0, 
            subset=['age','embarked', 'embark_town'],
            how='any', inplace=True)
data.count()
data['age'].fillna(채울값)

data['age_1'] = data['age'].fillna(data['age'].mean())
data.count()

data['deck'].fillna(method='ffill')
# 1) 불러오기 : pd.read_excel()
data = pd.read_excel('남북한발전전력량.xlsx')
data.info()
# 2) 변수명 변경 : .rename(columns={'전력량 (억㎾h)':})
data1 = data.rename(columns={'전력량 (억㎾h)':'전력량'})
# 전력량 (억㎾h) => 전력량
# 3) 채우기 : .fillna(method='ffill')
data1['전력량'].fillna(method='ffill', inplace=True)
# 전력량의 결측자료 ffill로 채우기
# 4) 케이스 선택 : .loc[ == '남한']
data1.loc[ data1['전력량'] == '남한' ]
# 전력량이 남한인 경우만 선택

# 자료 유형 확인
df.dtypes
# Out[121]: 
# mpg             float64
# cylinders         int64
# displacement    float64
# horsepower       object
# weight          float64
# acceleration    float64
# model year        int64
# origin            int64
# name             object

# 특정문자(?)로 인해 문자형으로지정된 경우
# => 문자를 결측자료로 변경
# => 결측자료 제거
# ? => NaN => 제거
# 특정값 변경
.replace(대상값, 변경할 값)
.replace({대상값1:변경값1, 대상값2:변경값2, ...})
import numpy as np
df['horsepower'] = 
df['horsepower'].replace('?', np.nan)
# 자료 유형 변경
# 자료 유형: 정수형(int), 실수형(float), 문자형(str)
# 재할당 방법으로 자료 유형 변경 유지
df['horsepower']=df['horsepower'].astype('float')
# 변수 구성 고유값 출력
df['origin'].unique()

df['mpg']
< 15: 저연비
15<= mpg < 25: 보통연비
25<= mpg   : 고연비

df['mpg_c'] = pd.cut(df['mpg'], bins=[0,15,25,100],
       labels=['저', '보통', '고'],
       right=False)

# 더미변수 - 원핫인코딩
pd.get_dummies(df['mpg_c'])
# 더미변수 - 차원축소
pd.get_dummies(df['mpg_c'],
               drop_first=True)
# 기준변수 변경 - drop 기준변수 삭제
df[['저연비', '보통연비']] = pd.get_dummies(df['mpg_c']).drop('고', axis=1)

df2 = pd.get_dummies(df, columns=['mpg_c'])


# 1) horsepower의 결측을 제거
df1 = df.dropna(axis=0, subset='horsepower', how='any')
# 2) horsepower 의 구간 3개로
# ~<75, 75<= <100, 100<=
# 저, 중간, 고
df1['horsepower_c'] = pd.cut(df1['horsepower'], bins=[0,75,100,500], labels=['저','중간','고'], right=False)

# 3) 더미변수 변경
df1 = pd.get_dummies(df1, columns=['horsepower_c'])

date = '2017-07-02'
# 년 - 월 - 일

date.str.split('-').str.get(0)
# 2017
dt.year
date = pd.read_excel('주가데이터.xlsx')
date['연월일'].dt.second
# year(연), month, day, hour, minute, second
# weekday(요일)


