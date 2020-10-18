simple x = x

calcChange owed given = if change > 0
                        then change
                        else 0
  where change = given - owed

inc n = n + 1
double n = n * 2
square n = n * n

calc n = if even n
         then n - 2
         else 3 * n + 1
