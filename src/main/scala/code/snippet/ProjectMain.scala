package code.snippet

import scala.xml.{ NodeSeq, Text }
import net.liftweb.util._
import net.liftweb.common._
import java.util.Date
import code.lib._
import Helpers._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.provider.HTTPCookie
import code.model._

class ProjectMain extends Loggable {

  private var name: String = ""
  private var description: String = ""

  // capture from whence the user came so we can send them back
  private val whence = S.referer openOr "/"
  
  def getProjects = {

    User.currentUser match {
      case Full(user) => {
        val myprojects = user.getProjects;
        "#lines *" #> myprojects.map(value => {
          "#updated *" #> value.updated.defaultValue.getTime().toString() &
          "#created *" #> value.created.defaultValue.getTime().toString() &
          "#name *" #> value.name &
          "#edit *" #> <a href="#">Edit</a> &
          "#update *" #> <a href="#">Update</a> &
          "#delete *" #> <a class="delete" href="#">Delete</a>
        })
      }
      case _ => {
        "#lines *" #> <tr><td>No Projects, please add one!</td></tr>
      }
    }

    //"#my_th *" #> { "Firstname" :: "Lastname" :: "email" :: "archived" :: Nil } &
    //"#my_tbody *" #> listUsers

  }


  def addProject = {

    "#name" #> SHtml.text("", name = _) &
    "#description" #> SHtml.textarea("", description = _) &
    "type=submit" #> SHtml.ajaxSubmit("Add", processAdd) andThen SHtml.makeFormsAjax
  }

  def processAdd(): JsCmd = {

    for { user <- User.currentUser ?~ "User does not exist" } {
      val newProject = Project.createRecord.
        name(name).
        description(description);

      newProject.validate match {
        case Nil => {
          user.projects.associate(newProject)
          S.redirectTo(whence)
          S.notice("Project successfully added.")
        }
        case xs => {
          S.error(xs);
        }
      }

    }

  }
}