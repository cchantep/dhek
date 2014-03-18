package dhek

import java.io.File
import java.util.EnumSet
import javax.servlet.{ Filter, DispatcherType }

import org.eclipse.jetty.server.{ Server ⇒ JettyServer, Connector, Handler, ServerConnector }
import org.eclipse.jetty.server.handler.{ ContextHandlerCollection }
import org.eclipse.jetty.servlet.{ FilterHolder, ServletContextHandler, ServletHolder }
import org.eclipse.jetty.servlets.MultiPartFilter

object Server {
  val inner = new JettyServer()

  lazy val handlers = new ContextHandlerCollection()

  lazy val contextHandler = {
    val ctx = new ServletContextHandler(handlers, "/")
    ctx.getServletContext.setAttribute("javax.servlet.context.tempdir", new File("tmp"))
    // val holder = new ServletHolder(classOf[org.eclipse.jetty.servlet.DefaultServlet])

    // holder.setName("Servlet")
    // ctx.addServlet(holder, "/")
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

  private val resourcesHolder = new FilterHolder(Resources)
  private val multiPartHolder = new FilterHolder(new MultiPartFilter())

  contextHandler.addFilter(resourcesHolder, "/*", EnumSet.of(DispatcherType.REQUEST))
  contextHandler.addFilter(multiPartHolder, "/*", EnumSet.of(DispatcherType.REQUEST))
}

object Main {
  def main(args: Array[String]) {
    val server = Server.filter(App).run()

    readLine()
    server.stop().destroy().join()
  }
}
