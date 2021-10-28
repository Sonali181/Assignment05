import com.knoldus.Nat.{Succ, Zero}
import org.scalatest.funsuite.AnyFunSuite

class NatTestCase extends AnyFunSuite {

  trait NatTest {
    val zero = Zero
    val one = new Succ(Zero)
    val two = new Succ(one)
  }

  test("zero isZero is true") {
    assert(Zero.isZero)
  }


  test("zero plus zero is zero") {
    assert((Zero + Zero) == Zero)
  }

  test("zero plus one is one") {
    new NatTest {
      assert((Zero + one) === one)
    }
  }

  test("zero minus zero is zero") {
    assert((Zero - Zero) === Zero)
  }

  test("one isZero is false") {
    new NatTest {
      assert(!one.isZero)
    }
  }

  test("predecessor of one is zero") {
    new NatTest {
      assert(one.predecessor === Zero)
    }
  }


  test("one minus one is zero") {
    new NatTest {
      assert((one - one) === Zero)
    }
  }

  test("two minus one is one") {
    new NatTest {
      assert((two - one) === one)
    }
  }

}