package dhek

trait Routing {
  type A

  def route(env: Env): Option[A]

  def callback(input: A, env: Env): Unit
}

object Routing {
  def apply[B](r: Env => Option[B], c: (B, Env) => Unit) = new Routing {
    type A = B

    def route(env: Env) = r(env)
    def callback(input: B, env: Env) = c(input, env)
  }
}

