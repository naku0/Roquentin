import Roquentin.API.Sugar.*
import Roquentin.API.{accessClassesThat, beAnnotatedWith}

@main
def main(): Unit = {
  val rule1 =
    classesThat inPackage "com.domain" should beAnnotatedWith("Service") because "Domain services"

  val rule2 =
    noClasses should accessClassesThat.resideIn("java.util.logging") because "We use SLF4J"

  val rule3 =
    classesThat.inPackage("com.service").should(beAnnotatedWith("Component")).because("Services")
}