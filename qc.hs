import Test.QuickCheck

double 7 = 13
double x =  x + x 

prop_double_x_is_twice_x x = (double x) == x * 2
#
