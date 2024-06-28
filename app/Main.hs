-- SPDX-License-Identifier: BSD-3-Clause

module Main (main) where

import ProgCon qualified (main)

main :: IO ()
main = do
  ProgCon.main
