import Controller (withYesodBBS)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = withYesodBBS $ run 3000
