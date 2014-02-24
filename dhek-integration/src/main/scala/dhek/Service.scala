package dhek

import akka.actor.Actor
import spray.routing._
import spray.http._
import MediaTypes._

// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class ServiceActor extends Actor with Service {

  // the HttpService trait defines only one abstract member, which
  // connects the services environment to the enclosing actor or test
  def actorRefFactory = context

  // this actor only runs our route, but you could add
  // other things here, like request stream processing
  // or timeout handling
  def receive = runRoute(route)
}


// this trait defines our service behavior independently from the service actor
trait Service extends HttpService {

  val route =
    path("") {
      get {
        respondWithMediaType(`text/html`) { // XML is marshalled to `text/xml` by default, so we simply override here
          complete {
            <html>
              <body>
                <h1>Étape 1 - Modèle de fusion</h1>
                <form method="post" enctype="multipart/form-data" action="/">
                  <table>
                    <tr>
                      <td><div><label for="pdf">Document PDF:</label></div><input name="pdf" type="file" /></td>
                    </tr>
                    <tr>
                      <td><div><label for="json">Modèle JSON:</label></div><input name="json" type="file" /></td>
                    </tr>
                    <tr>
                      <td><input type="submit" value="Continuer" /></td>
                    </tr>
                  </table>
                </form>
              </body>
            </html>
          }
        }
      } ~
      post {
        formFields('pdf, 'json) { (pdf, json) =>
          complete("pdf and json model submitted !")
        }
      }
    }
}
