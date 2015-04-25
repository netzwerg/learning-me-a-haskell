doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 
                      then x 
                      else x * 2

list = [1,2,3,4] ++ [5,6,7,8]
listCons = 0:list
index6 = list !! 6
