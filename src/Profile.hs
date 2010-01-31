import LifeMatrix

main = putStrLn . show =<< return . (!! 20) =<< randomGame 60
