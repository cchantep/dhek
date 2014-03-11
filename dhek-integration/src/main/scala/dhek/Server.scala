package dhek

import java.util.EnumSet
import javax.servlet.{ Filter, DispatcherType }

import org.eclipse.jetty.server.{ Server ⇒ JettyServer, Connector, Handler, ServerConnector }
import org.eclipse.jetty.server.handler.{ ContextHandlerCollection }
import org.eclipse.jetty.servlet.{ FilterHolder, ServletContextHandler, ServletHolder }

object Server {
  val inner = new JettyServer()

  lazy val handlers = new ContextHandlerCollection()

  lazy val contextHandler = {
    val ctx = new ServletContextHandler(handlers, "/")
    val holder = new ServletHolder(classOf[org.eclipse.jetty.servlet.DefaultServlet])

    holder.setName("Servlet")
    handlers.addHandler(ctx)
    ctx
  }

  def filter(f: Filter): this.type = {
    val holder = new FilterHolder(f)

    holder.setName("Filter")
    contextHandler.addFilter(holder, "/*", EnumSet.of(DispatcherType.REQUEST))

    this
  }

  def run(host: String = "localhost", port: Int = 3000): this.type = {
    val conn = new ServerConnector(inner)

    conn.setPort(port)
    conn.setHost(host)
    inner.setHandler(handlers)
    inner.addConnector(conn)
    inner.setStopAtShutdown(true)
    inner.start()
    this
  }

  def stop(): this.type = {
    inner.stop()
    this
  }

  def destroy(): this.type = {
    inner.destroy()
    this
  }

  def join(): this.type = {
    inner.join()
    this
  }
}

object Main {
  def main(args: Array[String]) {
    val server = Server.filter(App).run()

    readLine()
    server.stop().destroy().join()
  }
}
