package dhek

import org.jboss.netty.channel.ChannelHandlerContext
import unfiltered.netty.{ Http, async }
import unfiltered.request.{ GET, Path }
import unfiltered.response.ResponseString

object Server extends async.Plan {
  def intent = {
    case req @ GET(Path("/")) ⇒
      req.respond(ResponseString("Hello world"))
  }

  def onException(ctx: ChannelHandlerContext, t: Throwable) {
    ctx.getChannel.write(ResponseString("Oups ! Something bad happend"))
  }
}

object Main {
  def main(args: Array[String]) {
    Http(3000).plan(Server).run()
  }
}
