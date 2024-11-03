module KVDB_Tests (tests) where

import Control.Concurrent (forkIO, threadDelay)
import KVDB
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "KVDB Tests"
    [ testCase "Basic put and get" $ do
        db <- startKVDB
        kvPut db (1 :: Int) (42 :: Int)
        val <- kvGet db 1
        val @?= 42,
      
      testCase "Get waits for put" $ do
        db <- startKVDB
        -- Start a thread that will put after a delay
        _ <- forkIO $ do
          threadDelay 100000  -- 0.1 second delay
          kvPut db (2 :: Int) (123 :: Int)
        -- Get should wait for the put
        val <- kvGet db 2
        val @?= 123,
      
      testCase "Put updates existing value" $ do
        db <- startKVDB
        kvPut db (3 :: Int) (1 :: Int)
        kvPut db (3 :: Int) (2 :: Int)
        val <- kvGet db 3
        val @?= 2,

      testCase "Multiple concurrent gets" $ do
        db <- startKVDB
        -- Start two gets that will wait
        _ <- forkIO $ do
          val <- kvGet db (4 :: Int)
          val @?= (42 :: Int)
        _ <- forkIO $ do
          val <- kvGet db 4
          val @?= 42
        threadDelay 100000
        -- Put the value they're waiting for
        kvPut db 4 42
    ]