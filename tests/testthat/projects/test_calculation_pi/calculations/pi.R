# Compute pi using a series: http://functions.wolfram.com/Constants/Pi/06/01/01/
Reduce(`+`, 4 * vapply(seq_len(1000) - 1, function(k) { (-1)^k / (2 * k + 1) }, double(1)))

