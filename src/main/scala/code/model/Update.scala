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

class Update private () extends Record[Update] with KeyedRecord[Long] {
  def meta = Update
  
  @Column(name="id")
  val idField = new LongField(this, 1)
  
  val update = new StringField(this, 2048) {
      override def validations = valMinLen(1, "Update content is required") _ :: super.validations
      override def setFilter = trim _ :: super.setFilter
  }
  
  val created = new DateTimeField(this)
  
  @Column(name="users_id")
  val userId = new LongField(this)
  
  @Column(name="projects_id")
  val projectId = new LongField(this)
}

/**
 * The singleton that has methods for accessing the database
 */
object Update extends Update with MetaRecord[Update] {
  
    /**
     * Find updates belonging to a project
     */
    def getUpdatesForProject(projectId : Long) : List[Update] = {
  		 from(MySchema.updates)(up => where(up.projectId === projectId) select (up)).toList
    }
    
    /**
     * Find updates belonging to a user
     */
    def getUpdatesForUser(userId : Long) : List[Update] = {
  		 from(MySchema.updates)(up => where(up.userId === userId) select (up)).toList
    }
  
}