let data Foo = Foo Char
in let data Foo = Foo Int
   in case Foo 3 of
         Foo x -> x
