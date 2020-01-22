module Idrboot

export
readBootCode : IO (Provider String)
readBootCode =
  case !(readFile "../rosetta-lisp/boot.lisp") of
    Right r => pure $ Provide r
    Left err => pure $ Error $ show err

