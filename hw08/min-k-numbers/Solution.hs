module Solution where

import Data.List

solution::[Integer]->Int->[Integer]

solution xs k = take k (sort xs)