module EAffirm (affr) where

import Affirm (affirm)
import Libs (selectData)

affr :: Int -> IO (String,Int)
affr g = do
  let txs = lines affirm
  (affrmText,ng) <- selectData 1 g txs
  return (last affrmText,g)
