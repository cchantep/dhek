package dhek

import java.io.File
import java.util.EnumSet
import javax.servlet.{ Filter, DispatcherType }

import org.eclipse.jetty.server.{ Server ⇒ JettyServer, Connector, Handler, ServerConnector }
import org.eclipse.jetty.server.handler.{ ContextHandlerCollection }
import org.eclipse.jetty.servlet.{ FilterHolder, ServletContextHandler, ServletHolder }
import org.eclipse.jetty.servlets.MultiPartFilter

final class Server(
  router: Filter, host: String = "localhost", port: Int = 3000) {

  private val inner = new JettyServer()

  def run(): this.type = {
    val handlers = new ContextHandlerCollection()

    // Wrap router
    val holder = new FilterHolder(router)
    holder.setName("Router")

    // Prepare context
    val ctx = new ServletContextHandler(handlers, "/")
    ctx.getServletContext.
      setAttribute("javax.servlet.context.tempdir", new File("tmp"))

    ctx.addFilter(new FilterHolder(new MultiPartFilter()), "/*",
      EnumSet.of(DispatcherType.REQUEST))
    ctx.addFilter(holder, "/*", EnumSet.of(DispatcherType.REQUEST))

    handlers.addHandler(ctx) // register

    // Prepare connector
    val conn = new ServerConnector(inner)
    conn.setPort(port)
    conn.setHost(host)

    // Run Jetty server
    inner.setHandler(handlers)
    inner.addConnector(conn)
    //inner.setStopAtShutdown(true)
    inner.start()
    this
  }

  def stop(): this.type = {
    inner.stop()
    this
  }
}

object Runner {
  import reactivemongo.api.MongoDriver

  def main(args: Array[String]) {
    val server = new Server(Plan(new MongoDriver().connection(List("localhost")))).run()

    readLine() // wait any key to be pressed
    server.stop()
  }
}
