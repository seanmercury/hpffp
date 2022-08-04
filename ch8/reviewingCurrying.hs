module ReviewingCurring where

cattyConny :: String -> String -> String 
cattyConny x y = x ++ " mrow " ++ y

-- flippy :: (String -> String -> String) -> String -> String
flippy = flip cattyConny

-- appedCatty :: String -> String
appedCatty = cattyConny "woops"

-- frappe :: (String -> String -> String) -> String
frappe = flippy "haha"


main = do
  -- Reviewing currying

  
  -- 1. 
  putStrLn $ appedCatty "woohoo!"
  --    "woops mrow woohoo!"
  -- 2.
  putStrLn $ frappe "1"
  --    "1 mrow haha"
  -- 3. frappe (appedCatty "2")
  putStrLn $ frappe (appedCatty "2")
  --    flippy "haha" (appedCatty "2")
  --    flip cattyConny "haha" (appedCatty "2")
  --    flip cattyConny "haha" (cattyConny "woops" "2")
  --    flip cattyConny "haha" "woops mrow 2"
  --    "woops mrow 2 mrow haha"

  -- 4. appedCatty (frappe "blue")
  putStrLn $ appedCatty (frappe "blue")
  --    cattyConny "woops" (flippy "haha" "blue")
  --    cattyConny "woops" (flip cattyConny "haha" "blue")
  --    cattyConny "woops" "blue mrow haha"
  --    "woops mrow blue mrow haha"

  -- 5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
  putStrLn $ cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))
  --    cattyConny (flippy "haha" "pink") (cattyConny "green" (cattyConny "woops" "blue"))
  --    cattyConny (flippy "haha" "pink") (cattyConny "green" "woops mrow blue")
  --    cattyConny (flippy "haha" "pink") "green mrow woops mrow blue"
  --    cattyConny "pink mrow haha" "green mrow woops mrow blue"
  --    "pink mrow haha mrow green mrow woops mrow blue"

  -- 6. cattyConny (flippy "Pugs" "are") "awesome"
  putStrLn $ cattyConny (flippy "Pugs" "are") "awesome"
  --    cattyConny "are mrow Pugs" "awesome"
  --    "are mrow Pugs mrow awesome"

  