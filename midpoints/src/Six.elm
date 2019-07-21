module Six exposing (Six, from, map, pairedOffset1, toList)


type alias Six a =
    { e0 : a, e1 : a, e2 : a, e3 : a, e4 : a, e5 : a }


map : (a -> b) -> Six a -> Six b
map f { e0, e1, e2, e3, e4, e5 } =
    Six (f e0) (f e1) (f e2) (f e3) (f e4) (f e5)


pairedOffset1 : (a -> a -> b) -> Six a -> Six b
pairedOffset1 f { e0, e1, e2, e3, e4, e5 } =
    Six (f e0 e1) (f e1 e2) (f e2 e3) (f e3 e4) (f e4 e5) (f e5 e0)


toList : Six a -> List a
toList { e0, e1, e2, e3, e4, e5 } =
    [ e0, e1, e2, e3, e4, e5 ]


from : (Int -> a) -> Six a
from f =
    Six (f 0) (f 1) (f 2) (f 3) (f 4) (f 5)
