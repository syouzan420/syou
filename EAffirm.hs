module EAffirm (affr) where

import Affirm (affirm)
import Libs (selectData)

affr :: IO String
affr = do
  let txs = lines affirm
  affrmText <- selectData 1 txs
  return (last affrmText)
