package code.lib

import org.squeryl.Schema
import code.model._
import net.liftweb.squerylrecord.RecordTypeMode._
import org.squeryl.ForeignKeyDeclaration

trait SchemaWrapper {
  def doTransaction[A <: Any](code: => A): A = {
    transaction[A](code)
  }
}

object MySchema extends Schema with SchemaWrapper {
  
  // Tables
  val users = table[User] { "users" }
  val projects = table[Project] { "projects" }
  val updates = table[Update] { "updates" }
  
  // Relations
  val usersToProjects = oneToManyRelation(users, projects).via((u, p) => u.idField === p.userId)
  val usersToUpdates = oneToManyRelation(users, updates).via((u, up) => u.idField === up.userId)
  val projectsToUpdates = oneToManyRelation(projects, updates).via((p, up) => p.idField === up.projectId)
  
  // The default constraint for all foreign keys in this schema : 
  override def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration) = foreignKeyDeclaration.constrainReference
  projectsToUpdates.foreignKeyDeclaration.constrainReference(onDelete.cascade)
}