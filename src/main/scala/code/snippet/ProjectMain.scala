package code.snippet

import scala.xml.{ NodeSeq, Text, Attribute }
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

object projectVar extends RequestVar[Project](null)

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
          "#edit *" #> SHtml.link("/projects/edit", () => projectVar(value), Text("Edit")) &
          "#delete *" #> SHtml.a( () => {
								  JsCmds.Confirm("Are you sure you want to delete?", {
								    SHtml.ajaxInvoke(() => {
								      //Logic here to delete
								      deleteProject(value.id)
								      JsCmds.After(3 seconds, JsCmds.Reload)
								    })._2
								  })
								}, Text("Delete"), "class" -> "delete")
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
        name(name.trim()).
        description(description.trim());

      newProject.validate match {
        case Nil => {
          user.projects.associate(newProject)
          
          S.redirectTo("/projects/", () => S.notice("Project successfully added"))
        }
        case xs => {
          S.error(xs);
        }
      }
    }
  }
  
  def editProject = {
    
    val proj = projectVar.is
    
    "#name" #> JsCmds.FocusOnLoad(SHtml.text(proj.name.is, proj.name(_))) &
    "#description" #> SHtml.text(proj.description.is, proj.description(_)) &
    "type=submit" #> SHtml.ajaxSubmit("Save", processSave) andThen SHtml.makeFormsAjax
  }
  
  def processSave(): JsCmd = {

    for { user <- User.currentUser ?~ "User does not exist" } {
      
      val project = projectVar.is
      //project.name(name.trim()).description(description.trim())
      
      project.validate match {
        case Nil => {
          MySchema.projects.update(project)
          
          S.redirectTo("/projects/", () => S.notice("Project successfully updated"))
          //TODO: Add change tracking?
        }
        case xs => S.error(xs)
      }
    }
  }
  
  def deleteProject(projId:Long) = {
    for { user <- User.currentUser ?~ "User does not exist" } {

      val returnVal = user.deleteProject(projId)
      logger.info("Deleted Project (Id:"+projId+"): "+returnVal)
      if(returnVal == 1)
        S.notice("Project Deleted.")
      else
        S.error("Could not delete!")
    } 
  }
  
}