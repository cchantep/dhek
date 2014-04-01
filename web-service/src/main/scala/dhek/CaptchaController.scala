package dhek

object CaptchaController {
  /** Returns captcha information */
  def info(env: Env): Unit = env async { complete ⇒
    env.resp.setContentType("application/json")

    env.writer acquireAndGet { w ⇒
      w.print(captcha(env).temporalCaptcha(8) /* TODO: As JSON */ )
      complete()
    }
  }

  /** @param text Previously generated [[scaptcha.CaptchaInfo.value]] */
  def image(env: Env)(text: String): Unit = env async { complete ⇒
    env.resp.setContentType("image/png")

    env.writer acquireAndGet { w ⇒
      val in: java.io.InputStream = captcha(env).textImage(text)
      sys.error("TODO: Write `in` to `w`")
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
