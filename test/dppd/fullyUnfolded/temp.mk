:: list xs = [h t ys: xs === ((h % t) % ys)]

:: callZero x = {call x zero (succ x)}


:: boxedZero x = x === <box: zero>  

:: boxedOne x = x === <box : succ zero>