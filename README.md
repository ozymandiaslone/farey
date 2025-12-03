# Quick Motivation
While on vacation, sitting by the beach, I began reading *Exploring the Number Jungle: A Journey into Diophantine Analysis* 
by Edward B Burger. Within the first dozen or so pages, it introduced the concept of Farey sequences, and 
asked the reader to play around with them. After messing around a bit in my notebook, the first thing that jumped out to me was:<br>

$$(1) \sum_{n=1}^i (n + 1) - a(n) = |\mathcal{F}_i|$$<br>

Where a(n) is [OEIS A322366](https://oeis.org/A322366) - also known as the centrifuge balance problem.
a(n) = Number of integers k in {0,1,...,n} such that k identical test tubes can be balanced in a centrifuge
with n equally spaced holes.

This actually turns out to be an interesting sequence in and of itself with such properties as Ishwar's theorem:

"You can balance k identical test tubes, 1 ≤ k ≤ n, in an n-hole centrifuge if and only if both
k and n-k can be expressed as a sum of prime divisors of n."

BUT as soon as I got some lisp code running to verify (1), I realized that the equality does NOT hold. After n = 12, the sum 
is 2 off. After n = 18, it is 6 off, and so on.

In fact, the absolute differences look like this:

(0 0 0 0 0 0 0 0 0 0 0 0 2 2 2 2 2 2 6 6 10 10 10 10 16 16 16 16 22 22 28 28 28
 28 28 28 38 38 38 38 50 50 60 60 70 86 86 86 100 100 116 116 128 128 144 144
 162 162 162 162 176 176 176 200 200 200 218 218 234 234 254 254 276 276 276
 308 326 326 348 348 376 376 376 376 398 398 398 398 428 428 450 450 472 472
 472 472 502 502 538 578 614 ...)

Although there are clearly errors accumulating in the sum: what really catches my eye is the seeming structure here. At first
there is 0 error, then the error jumps to 2, and stays there for a bit, then jumps to 6, then to 10, and so on. Let's look at
the values of n where the error increases:

(12 18 20 24 28 30 36 40 42 44 45 48 50 52 54 56 60 63 66 68 70 72 75 76 78 80 84 ... )

Interestingly, there seems to be another pattern here! These appear to be all the numbers that are neither prime powers, nor semiprimes!
(This is OEIS A102467)[https://oeis.org/A102467].

This is where I find myself today - belive these n values are *where* the error increases, but I have not yet been able to determine
*what* the error will be. 







