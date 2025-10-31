# Python 3 の正規表現機能についての実験
# a?a?a?aaa のような正規表現の aaa のような文字列への適用に要する時間の測定
# a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?a?aaaaaaaaaaaaaaaaaaaaaaaaa

import re
from time import time

def matchBenchmark(pattern, string):
    time_start = time()
    pattern.match(string)
    return time() - time_start


i = 1
while True:
    t = matchBenchmark(re.compile('a?' * i + 'a' * i), 'a' * i)
    print('{:>2}: {:2.2f}s'.format(i, t))
    if t > 30: break
    i = i + 1

'''
ベンチマークの実行結果

実行環境
    Python 3.13.1

    MacBook Air (Apple M3, 2024 model, Apple M3, 16GB)

 1: 0.00s
 2: 0.00s
 3: 0.00s
 4: 0.00s
 5: 0.00s
 6: 0.00s
 7: 0.00s
 8: 0.00s
 9: 0.00s
10: 0.00s
11: 0.00s
12: 0.00s
13: 0.00s
14: 0.00s
15: 0.00s
16: 0.00s
17: 0.00s
18: 0.01s
19: 0.01s
20: 0.02s
21: 0.03s
22: 0.06s
23: 0.13s
24: 0.26s
25: 0.53s
26: 1.08s
27: 2.18s
28: 4.48s
29: 9.09s
30: 18.46s
31: 37.21s'''
