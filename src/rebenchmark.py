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
    Python 3.11.4 (main, Jun 20 2023, 17:23:00) [Clang 14.0.3 (clang-1403.0.22.14.1)] on darwin
    MacBook Air (M1, 2020 model, Apple M1, 16GB 1600 MHz DDR3)

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
21: 0.04s
22: 0.09s
23: 0.18s
24: 0.36s
25: 0.73s
26: 1.48s
27: 3.02s
28: 6.15s
29: 12.58s
30: 25.52s
31: 52.78s'''
