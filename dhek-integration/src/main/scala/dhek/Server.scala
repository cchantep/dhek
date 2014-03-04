package dhek

import org.jboss.netty.channel.ChannelHandlerContext
import unfiltered.netty.{ Http, async }
import unfiltered.request.{ GET, POST, Path }
import unfiltered.response.{ ResponseString, NotFound }

object Server extends async.Plan with Html {
  def intent = {
    case req @ GET(Path("/")) ⇒
      req.respond(ResponseString(index))
    case req @ POST(Path("/upload")) ⇒

    case req ⇒
      req.respond(NotFound)
  }

  def onException(ctx: ChannelHandlerContext, t: Throwable) {
    print(t)
    throw t
    //ctx.getChannel.write(ResponseString("Oups ! Something bad happend"))
  }
}

object Main {
  def main(args: Array[String]) {
    Http(3000).plan(Server).run()
  }
}
