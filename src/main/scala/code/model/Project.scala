package code.model

import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.record.field.{LongField, LongTypedField, StringField, OptionalEmailField, TextareaField}
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Helpers
import net.liftweb.http.SessionVar
import net.liftweb.http.S
import net.liftweb.common.Loggable
import net.liftweb.common.Box
import net.liftweb.common.Empty
import net.liftweb.common.Full
import org.squeryl.Session
import org.squeryl.Query
import org.squeryl.annotations.Column
import code.lib._
import net.liftweb.record.field.DateTimeField

class Project private () extends Record[Project] with KeyedRecord[Long] {
  def meta = Project
  
  @Column(name="id")
  val idField = new LongField(this, 1)
  
  val name = new StringField(this, 45) {
      override def validations = valMinLen(1, "Project name is required") _ :: super.validations
      override def setFilter = trim _ :: super.setFilter
  }
  
  val description = new TextareaField(this, 2048) {
      override def textareaRows = 2
      override def textareaCols = 50
  }
  
  val updated = new DateTimeField(this)
  
  val created = new DateTimeField(this)
  
  @Column(name="users_id")
  val userId = new LongField(this)
  
  lazy val updates = MySchema.projectsToUpdates.left(this)
  
  def getUpdates = {
      from(MySchema.updates)((up) => where (up.userId === idField) select (up) orderBy(up.created)).toList
  }

}

/**
 * The singleton that has methods for accessing the database
 */
object Project extends Project with MetaRecord[Project] with Loggable {
  
    /**
     * Find projects belonging to a user
     */
    def getProjectsForUser(userId : Long) : List[Project] = {
  		 from(MySchema.projects)(p => where(p.userId === userId) select (p)).toList
    }
    
    /**
     * Add Project Update
     */
    def addUpdate(project: Project, user: User, update: Update) {
      MySchema.updates.insert(update)
      user.updates.associate(update)
      project.updates.associate(update)
    }
}