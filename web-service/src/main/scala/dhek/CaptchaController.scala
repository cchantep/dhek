package dhek

import argonaut._, Argonaut._
import argonaut.{ Json ⇒ ArgJson }

object CaptchaController {
  /** Returns captcha information */
  def info(env: Env): Unit = env async { complete ⇒
    env.resp.setContentType("application/json")

    env.writer acquireAndGet { w ⇒
      val info = captcha(env).temporalCaptcha(8)
      w.print(ArgJson(
        "code" -> info.code.asJson,
        "value" -> info.value.asJson).nospaces)
      complete()
    }
  }

  /** @param text Previously generated [[scaptcha.CaptchaInfo.value]] */
  def image(env: Env)(text: String): Unit = env async { complete ⇒
    env.resp.setContentType("image/png")

    env.outputStream acquireAndGet { o ⇒
      Binaries.writeTo(captcha(env).textImage(text), o)
      complete()
    }
  }

  /**
   * @param code Previously generated [[scaptcha.CaptchaInfo.code]]
   * @param text Text typed by user in form
   */
  def matches(env: Env)(code: Int, text: String): Unit = env async { complete ⇒
    env.resp.setContentType("application/json")

    env.writer acquireAndGet { w ⇒
      w.print(captcha(env).matches(code, text).toString)
      complete()
    }
  }

  private def captcha(env: Env) = new scaptcha.Captcha {
    val seed = env.settings.secretKey.mkString("")
  }
}
