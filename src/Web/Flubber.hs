module Web.Flubber (serveFlubber)

import Network.Wai.Handler.Warp (Port, run)
import Servant (Proxy)
import Servant.Server (serve)

serveFlubber :: Port -> IO ()
serveFlubber port = run port app
  where app = serve api server
        api = Proxy
        server = undefined
