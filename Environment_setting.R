# 설치할 외부 패키지
"Ecdat" %in% installed.packages() ## 설치가 되어있는지 확인하는 코드

# 다른 패키지들도 설치되어 있는지 확인하기 

"AER" %in% installed.packages()
"lmtest" %in% installed.packages()
"sandwich" %in% installed.packages()
"wooldridge" %in% installed.packages()

# 없으면 install.packages("Packagename")으로 설치하기 
