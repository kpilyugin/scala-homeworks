import scala.runtime.TraitSetter;

public interface Base {
  int foo();

  int goo();

  int x();

  @TraitSetter
  void setX(int x);

  int y();
}