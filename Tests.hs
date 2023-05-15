import PRF

main :: IO ()
main = do
    print(addR (Succ Zero) (Succ Zero))
    print(pow (Succ (Succ Zero)) (Succ (Succ Zero)))