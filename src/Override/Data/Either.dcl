definition module Override.Data.Either

import Data.Error

:: Either a b :== MaybeError a b

Left a :== Error a
Right b :== Ok b

// isLeft :: !(Either a b) -> Bool
isLeft e :== isError e
// isRight :: !(Either a b) -> Bool
isRight e :== isOk e

// fromLeft :: !(Either .a .b) -> .a
fromLeft e :== fromError e
// fromRight :: !(Either .a .b) -> .a
fromRight e :== fromOk e

