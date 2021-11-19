module RunningStat exposing (RunningStat, empty, push, mean, variance, std, normalize)

type alias RunningStat =
    { count: Int
    , m: Float
    , s: Float
    }

empty : RunningStat
empty = RunningStat 0 0 0

push : Float -> RunningStat -> RunningStat
push x stat =
    let 
        count = stat.count + 1
        newM = case count of
                1 -> x
                _ -> stat.m + (x - stat.m) / (toFloat count)
        newS = case count of 
                1 -> 0
                _ -> stat.s + (x - stat.m) * (x - newM)
    in
        { stat | count = count, m = newM, s = newS }

mean : RunningStat -> Float
mean stat = stat.m 

variance : RunningStat -> Float
variance stat = 
    case countOver 1 stat of
        False-> 0
        True -> stat.s / (toFloat (stat.count - 1))

countOver : Int -> RunningStat -> Bool
countOver limit stat = stat.count > limit

std : RunningStat -> Float
std stat = sqrt (variance stat)

normalize : Float -> RunningStat -> Float
normalize x stat =
    case countOver 1 stat of
        False -> 0
        True -> (x - (mean stat)) / (std stat)