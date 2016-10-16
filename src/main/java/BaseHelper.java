public abstract class BaseHelper {
  public static void init(Base that) {
    that.setX(1);
  }

  public static int y(Base that) {
    return that.goo();
  }

  public static int goo(Base that) {
    return that.x() + 2;
  }
}
