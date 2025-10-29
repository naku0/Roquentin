package Roquentin.Core

import Roquentin.Core.{ClassInfo, ClassSelector}

case object AllClassesSelector extends ClassSelector{
  override def select(classes: List[ClassInfo]): List[ClassInfo] = classes
  override def description: String = "Classes"
}

case class PackageSelector(packageName: String) extends ClassSelector{
  def select(classes: List[ClassInfo]): List[ClassInfo] = {
    classes.filter(_.packageName.startsWith(packageName))
  }
  override def description: String = s"classes in package '$packageName'"
}

case class AnnotatedSelector(annotation: String) extends ClassSelector{
  override def select(classes: List[ClassInfo]): List[ClassInfo] = {
    classes.filter(_.annotations.contains(annotation))
  }
  override def description: String = s"classes annotated with '@$annotation'"
}

case class ClassNameSelector(classNamePattern: String) extends ClassSelector{
  override def select(classes: List[ClassInfo]): List[ClassInfo] = {
    classes.filter(_.name.matches(classNamePattern))
  }
  override def description: String = s"classes with name matching '$classNamePattern'"
}