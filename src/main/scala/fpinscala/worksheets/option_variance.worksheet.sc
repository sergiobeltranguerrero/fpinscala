enum MyOption[+A]:
  case Some(get: A)
  case None

  def getOrElse[B >: A](default: => B): B = this match
    case Some(a) => a
    case None    => default

trait Animal
class Cat extends Animal
class Dog extends Animal

val animal: Animal = new Dog

val maybeAnimal: MyOption[Animal] =
  MyOption.Some(new Dog)

val anotherMaybeAnimal: Animal =
  maybeAnimal.getOrElse(new Cat) // compiles

// // If MyOption is covariant any MyOption[Dog] is valid as a MyOption[Animal]

val maybeDog: MyOption[Dog] =
  MyOption.Some(new Dog)

val anotherMaybeAnimal2: Animal =
  maybeDog.getOrElse(new Cat) // does not compile

// def function(optionAnimal: MyOption[Animal]): Animal =
//   optionAnimal.getOrElse(new Cat)

// function(maybeAnimal)
// function(maybeDog)
