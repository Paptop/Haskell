-- Welcome to first haskel program
-- Excercise 6

itemTotal :: [(String,Float)] -> [(String,Float)]

getItemPriceSum :: String -> [(String,Float)] -> Float
getItemPriceSum name (selected:items)
    | null selected = 0
    | length(items) == 0 && selectedName == name = price
    | length(items) == 0 = 0
    | selectedName == name = price + getItemPriceSum name items
    | otherwise = 0 + getItemPriceSum name items
    where price = snd selected
          selectedName = fst selected
          
itemTotalInner :: [(String,Float)] -> [(String,Float)] -> [String] -> [(String,Float)] -> [(String,Float)]
itemTotalInner (selected:items) immutableList visited total
    | elem selectedName visited && length(items) == 0 = total
    | elem selectedName visited = itemTotalInner items immutableList visited total
    | length(items) == 0 = (newItem:total)
    | otherwise = itemTotalInner items immutableList (selectedName:visited) (newItem:total)
    where selectedName = fst selected
          newItem = (selectedName,newPrice)
          newPrice = getItemPriceSum selectedName immutableList 
    
itemTotal list = itemTotalInner list list [] []


itemDiscount :: String -> Integer -> [(String,Float)] -> [(String,Float)]
itemDiscountInner :: String -> Integer -> [(String,Float)] -> [(String,Float)] -> [(String,Float)]
itemDiscountInner name disc (selected:list) newList
    | sameName && empty = (newItem:newList)
    | empty = newList
    | sameName = itemDiscountInner name disc list (newItem:newList)
    | otherwise = itemDiscountInner name disc list (selected:newList)
    where selectedName = fst selected
          sameName = selectedName == name
          empty = length(list) == 0
          selectedPrice = snd selected
          newPrice = selectedPrice * (100-fromIntegral(disc))/100
          newItem = (selectedName, newPrice)

itemDiscount name disc list = itemDiscountInner name disc list []
