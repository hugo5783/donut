import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector
import qualified Data.List as List

renderFrame :: Double -> Double -> IO ()
renderFrame a b = do
    let cosA = cos a
        sinA = sin a
        cosB = cos b
        sinB = sin b
        charOutput = replicate (screenHeight + 1) (replicate (screenWidth + 1) ' ')
        zBuffer = replicate (screenHeight + 1) (replicate (screenWidth + 1) 0)
        theta = 0
    renderFrameLoop theta cosA sinA cosB sinB charOutput zBuffer

renderFrameLoop :: Double -> Double -> Double -> Double -> Double -> [[Char]] -> [[Double]] -> IO ()
renderFrameLoop theta cosA sinA cosB sinB charOutput zBuffer = do
    if theta < 2 * pi
        then do
            let costheta = cos theta
                sintheta = sin theta
                phi = 0
            renderFrameInnerLoop phi costheta sintheta cosA sinB charOutput zBuffer
            renderFrameLoop (theta + thetaSpacing) cosA sinA cosB sinB charOutput zBuffer
        else do
            printFrame charOutput
            where
                thetaSpacing = 0.07

renderFrameInnerLoop :: Double -> Double -> Double -> Double -> Double -> [[Char]] -> [[Double]] -> IO ()
renderFrameInnerLoop phi costheta sintheta cosA sinB charOutput zBuffer = do
    if phi < 2 * pi
        then do
            let cosphi = cos phi
                sinphi = sin phi
                circlex = r2 + r1 * costheta
                circley = r1 * sintheta
                x = circlex * (cosB * cosphi + sinA * sinB * sinphi) - circley * cosA * sinB
                y = circlex * (sinB * cosphi - sinA * cosB * sinphi) + circley * cosA * cosB
                z = k2 + cosA * circlex * sinphi + circley * sinA
                ooz = 1 / z
                xp = floor (fromIntegral screenWidth / 2 + k1 * ooz * x)
                yp = floor (fromIntegral screenHeight / 2 - k1 * ooz * y)
                l = cosphi * costheta * sinB - cosA * costheta * sinphi - sinA * sintheta + cosB * (cosA * sintheta - costheta * sinA * sinphi)
            if l > 0
                then do
                    let luminanceIndex = floor (l * 8)
                    if ooz > (zBuffer !! xp) !! yp
                        then do
                            let updatedCharOutput = update2DList charOutput xp yp ('.,-~:;=!*#$@' !! luminanceIndex)
                                updatedZBuffer = update2DList zBuffer xp yp ooz
                            renderFrameInnerLoop (phi + phiSpacing) costheta sintheta cosA sinB updatedCharOutput updatedZBuffer
                        else do
                            renderFrameInnerLoop (phi + phiSpacing) costheta sintheta cosA sinB charOutput zBuffer
                else do
                    renderFrameInnerLoop (phi + phiSpacing) costheta sintheta cosA sinB charOutput zBuffer
        else do
            renderFrameLoop theta cosA sinA cosB sinB charOutput zBuffer
            where
                phiSpacing = 0.02

printFrame :: [[Char]] -> IO ()
printFrame charOutput = do
    putStrLn "\x1b[H"
    mapM_ putStrLn charOutput

update2DList :: [[a]] -> Int -> Int -> a -> [[a]]
update2DList list x y value = List.transpose (List.transpose list // [(x, (list !! x) // [(y, value)])])

main :: IO ()
main = do
    putStrLn "\x1b[2J"
    let a = 1.0
        b = 1.0
    renderFrame a b
    where
        r1 = 1
        r2 = 2
        k2 = 5
        screenWidth = 35
        screenHeight = 35
        k1 = fromIntegral screenWidth * k2 * 3 / (8 * (r1 + r2))
