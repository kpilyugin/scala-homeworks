import scala.runtime.TraitSetter;

public class Inh implements Base {
  private volatile boolean yInitialized;
  private int x;
  private int y;

  public Inh() {
    BaseHelper.init(this);
  }

  public static void run() {
    InhObject.OBJECT.run();
  }

  public int goo() {
    return BaseHelper.goo(this);
  }

  public int x() {
    return x;
  }

  @TraitSetter
  public void setX(int x) {
    this.x = x;
  }

  public int y() {
    return this.yInitialized ? this.y : yLazyCompute();
  }

  public int foo() {
    return 3;
  }

  private int yLazyCompute() {
    synchronized (this) {
      if (!yInitialized) {
        y = BaseHelper.y(this);
        yInitialized = true;
      }
    }
    return this.y;
  }
}
