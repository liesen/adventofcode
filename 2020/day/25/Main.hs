import Data.List

determineLoopSize target = elemIndex target (transform 7)

transform subjectNumber = iterate (\value -> (value * subjectNumber) `mod` 20201227) 1

calculateEncryptionKey (cardKey, deviceKey) = do
    loopSize <- determineLoopSize cardKey
    return (transform deviceKey !! loopSize)

main = do
    input <- readFile "input.txt"
    let [cardKey, deviceKey] = map read (lines input)
        Just encryptionKey = calculateEncryptionKey (cardKey, deviceKey)
    print encryptionKey
