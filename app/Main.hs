module Main where

import Solutions

main :: IO ()
main = do
  putStrLn $ (show . lastBox) [1..4]
  putStrLn $ (show . lastButOne) [1..4]
  putStrLn $ (show . (kth 3)) [1..4]
  putStrLn $ (show . len) [1..4]
  putStrLn $ (show . rev) [1..5]
  putStrLn $ "Is palindrome 1: " ++ (show . isPalindrome) "this is not a palindrome"
  putStrLn $ "Is palindrome 2: " ++ (show . isPalindrome) "xamax"
  putStrLn $ (show . flat) [[1..5], [6..9]]
  putStrLn $ (show . compress) "aaaabccaadeee"
  putStrLn $ (show . pack) "aaaabccaadeee"
  putStrLn $ (show . runLength) "aaaabccaadeee"
  putStrLn $ (show . decompress . runLength) "aaaabccaadeee"
  putStrLn $ (show . runLength') "aaaabccaadeee"
  putStrLn $ (show . dupli) [1..5]
