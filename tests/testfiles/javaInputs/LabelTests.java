/**

   There are a few tests that I want to add that are difficult to
   construct.  We don't have the source for the original methods
   that demonstrated these issues.

   1) A test where a wide value (long or double) is stored in a
   register pair (v1, v2) and v2 is later re-used on a different
   branch for a non-wide type.  If register pairs aren't properly
   handled, this can cause phantom empty phi nodes to pop up as
   arguments to other phi nodes because there is a phantom definition
   in v2 that should have been overwritten by the use of (v1, v2) as a
   wide register pair.

   2) An example where part of an exception handler is eliminated as
   dead code in the *middle* of the instruction stream.  This happens
   when the java compiler decides to share part of a basic block, but
   the prefix of the shared block is actually dead code (e.g., due to
   unreachable exception handlers).  This caused a problem with the
   way we were computing offsets into the raw Dalvik instruction
   stream.

 */
class LabelTests {
  private int field1;
  private int field2;

  public int localCopies(int x1, int x2) {
    int t1 = x1;
    int t2 = t1;
    int t3 = t2;
    int t4 = t3;
    return t4;
  }

  public int simpleBranch(int x1, int x2) {
    int z;
    if(x2 > 100)
      z = x2;
    else
      z = x1;

    return z;
  }

  public int simplePackedSwitch(int x1, int x2, int x3) {
    int z;
    switch(x1) {
    case 1:
      z = x2;
      break;
    case 2:
      z = x3;
      break;
    default:
      z = 0;
    }

    return z;
  }

  public int simpleSparseSwitch(int x1, int x2, int x3) {
    int z;
    switch(x1) {
    case 100:
      z = x2;
      break;
    case 201:
      z = x3;
      break;
    default:
      z = 0;
    }

    return z;
  }

  /**
     Should return a phi node
  */
  public int simpleLoop(int x1, int x2) {
    int z = x1;
    while(x2 > 0) {
      z = z + x1;
      --x2;
    }

    return z;
  }

  /**
     Despite the loop body, we are just returning one of the arguments
  */
  public int loopNopBody(int x1, int x2) {
    int z = x1;
    while(x2 > 0) {
      z = z + x1;
      -- x2;
    }

    return x1;
  }

  public int nestedLoops(int x1, int x2) {
    int z = x1;
    for(int i = 0; i < x2 + 1; ++i) {
      for(int j = 0; j < i; ++ j) {
        z = z + i;
      }

      z = z + 2;
    }

    return z;
  }
  
  /**
     Since the exception is not caught, a simple value should be
     returned
  */
  public int uncaughtNPE(String[] s) {
    int x;
    x = s.length;
    return x;
  }

  /**
     Since we have exceptional control flow available,
     this test should return a phi node if everything is working.
  */
  public int simpleNPECatchNPE(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(NullPointerException ex) {
      x = 0;
    }

    return x;
  }

  /**
     Runtime exception handlers subsume NPEs, so this should also
  return a phi node
  */
  public int simpleNPECatchRuntimeEx(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(RuntimeException ex) {
      x = 0;
    }

    return x;
  }

  /**
     Likewise Exception
  */
  public int simpleNPECatchException(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(Exception ex) {
      x = 0;
    }

    return x;
  }

  /**
     And throwable
  */
  public int simpleNPECatchThrowable(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(Throwable ex) {
      x = 0;
    }

    return x;
  }

  /**
     In this case, there should be no edge to the handler
     because arraylength cannot produce ArithmeticExceptions.
  */
  public int simpleNPECatchArithEx(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(ArithmeticException ex) {
      x = 0;
    }

    return x;
  }

  /**
     We want to make sure that we only get an edge to the first
     applicable handler (in CHA order).  Here, the phi node should
     only have two values.
   */
  public int simpleNPEOnlyFirstHandler(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(NullPointerException ex) {
      x = 0;
    }
    catch(Exception ex) {
      x = 1;
    }

    return x;
  }

  /**
     Due to the finally block, this always returns zero (a SimpleLabel)
   */
  public int simpleNPEWithHandlerAndFinally(String[] s) {
    int x;
    try {
      x = s.length;
    }
    catch(NullPointerException ex) {
      System.out.println("Foo");
    }
    finally {
      x = 0;
    }

    return x;
  }

  /**
     Returns a simple label since there are no handlers.
   */
  public int divideNoCatch(int x1, int x2) {
    return x1 / x2;
  }

  /**
     This handler should be dead code
  */
  public int arithNoDivision(int x1, int x2) {
    int z;

    try {
      z = x1 + x2 * 2 * x1 >> 5;
    }
    catch(ArithmeticException ex) {
      z = 0;
    }

    return z;
  }

  /**
     Right now, will have an edge to the handler since we don't prove
     anything about x2 and zero.  In the future, we might want to do
     that and then this case would change (the returned phi would have
     two values instead of three).
   */
  public int safeDivideWithCatch(int x1, int x2) {
    if(x2 == 0) return 0;

    int z;
    try {
      z = x1 / x2;
    }
    catch(ArithmeticException ex) {
      z = 5;
    }

    return z;
  }

  /**
     Should return a phi node
  */
  public int divisionCatchArithEx(int x1, int x2) {

    int z;
    try {
      z = x1 / x2;
    }
    catch(ArithmeticException ex) {
      z = 5;
    }

    return z;
  }

  public int divisionCatchRuntimeEx(int x1, int x2) {
    int z;
    try {
      z = x1 / x2;
    }
    catch(RuntimeException ex) {
      z = 5;
    }

    return z;
  }

  public int divisionCatchException(int x1, int x2) {
    int z;
    try {
      z = x1 / x2;
    }
    catch(Exception ex) {
      z = 5;
    }

    return z;
  }

  public int divisionCatchThrowable(int x1, int x2) {
    int z;
    try {
      z = x1 / x2;
    }
    catch(Throwable ex) {
      z = 5;
    }

    return z;
  }

  public int divisionCatchNPE(int x1, int x2) {
    int z;
    try {
      z = x1 / x2;
    }
    catch(NullPointerException ex) {
      z = 5;
    }

    return z;
  }

  public int checkCastNoHandler(Object o) {
    String s = (String)o;
    return s.length();
  }

  public int checkCastHandleCCE(Object o) {
    int z;
    try {
      String s = (String)o;
      z = 1;
    }
    catch(ClassCastException ex) {
      z = 0;
    }
    return z;
  }

  public int checkCastHandleRuntimeException(Object o) {
    int z;
    try {
      String s = (String)o;
      z = 1;
    }
    catch(RuntimeException ex) {
      z = 0;
    }
    return z;
  }

  public int checkCastHandleException(Object o) {
    int z;
    try {
      String s = (String)o;
      z = 1;
    }
    catch(Exception ex) {
      z = 0;
    }
    return z;
  }

  public int checkCastHandleThrowable(Object o) {
    int z;
    try {
      String s = (String)o;
      z = 1;
    }
    catch(Throwable ex) {
      z = 0;
    }
    return z;
  }

  /**
     This handler is dead code
   */
  public int checkCastHandleArithException(Object o) {
    int z;
    try {
      String s = (String)o;
      z = 5;
    }
    catch(ArithmeticException ex) {
      z = 0;
    }
    return z;
  }

  /**
     Since this has an invoke, any exception could be thrown.  The phi
     node should have as many values as there are handlers.
   */
  public int invokeToAllHandlers(Object o) {
    int z;
    try {
      String s = (String)o;
      z = s.length();
    }
    catch(UnsupportedOperationException ex) {
      z = 1;
    }
    catch(NullPointerException ex) {
      z = 2;
    }
    catch(RuntimeException ex) {
      z = 3;
    }
    catch(Exception ex) {
      z = 4;
    }
    catch(Throwable ex) {
      z = 0;
    }
    return z;
  }

  public int returnThisFieldNoHandler() {
    return field1;
  }

  public int returnThisFieldHandler() {
    int x;
    try {
      x = field1;
    }
    catch(NullPointerException ex) {
      x = 0;
    }
    return x;
  }

  public int returnOtherFieldNoHandler(LabelTests t) {
    return t.field1;
  }

  public int returnOtherFieldHandler(LabelTests t) {
    int x;
    try {
      x = t.field1;
    }
    catch(NullPointerException ex) {
      x = 0;
    }
    return x;
  }

  public int arrayReadNoHandler(int[] is, int ix) {
    return is[ix];
  }

  public int arrayReadHandler(int[] is, int ix) {
    int x;
    try {
      x = is[ix];
    }
    catch(ArrayIndexOutOfBoundsException ex) {
      x = 0;
    }

    return x;
  }

  public int arrayReadHandlerThrowable(int[] is, int ix) {
    int x;
    try {
      x = is[ix];
    }
    catch(Throwable ex) {
      x = 0;
    }

    return x;
  }

  public int arrayWriteNoHandler(Object[] os, Object o, int ix) {
    os[ix] = o;
    return ix;
  }

  public int arrayWriteHandler(Object[] os, Object o, int ix) {
    int x;
    try {
      os[ix] = o;
      x = ix;
    }
    catch(ArrayStoreException ex) {
      x = 1;
    }
    catch(NullPointerException ex) {
      x = 2;
    }
    return x;
  }

  public double[] newArrayUnchecked(int len) {
    return new double[len];
  }

  // One day we might be able to do better here with a bit of proving
  // about the length
  public double[] newArrayChecked(int len) {
    double[] res;
    try {
      res = new double[len];
    }
    catch(NegativeArraySizeException ex) {
      res = new double[0];
    }

    return res;
  }

  public double[] newArrayCheckedThrowable(int len) {
    double[] res;
    try {
      res = new double[len];
    }
    catch(Throwable ex) {
      res = new double[0];
    }

    return res;
  }

  public double[] newArrayOOMChecked(int len) {
    double[] res;
    try {
      res = new double[len];
    }
    catch(OutOfMemoryError ex) {
      res = null;
    }

    return res;
  }

  public String newInstanceUnchecked() {
    return new String("Foo");
  }

  /**
     This test is kind of useful, but not completely.  It doesn't seem
     possible to write a test that separates the newinstance
     instruction from its subsequent constructor call (which could
     throw any runtime exception or error).

     Looking at the resulting CFG at the time of this writing, it is
     actually right and the exception handler has two edges to it, as
     expected (one for the newinstance instruction and the other for
     the invoke).
   */
  public String newInstanceChecked() {
    String s;

    try {
      s = new String("Foo");
    }
    catch(OutOfMemoryError ex) {
      s = null;
    }

    return s;
  }

  /**
     Testing fill-array-data to ensure it gets its null pointer edge.
   */
  public int[] newArrayFilledUnchecked() {
    return new int[] {1,2,3,4};
  }

  /**
     This test catches a regression in `insnCodeUnits`.  Multiple
     adjacent fill-array-data-payload instructions expose the error
     and will lead to an explosion while translating to SSA form.
   */
  public int[] newArrayFilledUncheckedMulti(int x) {
    if(x < 0)
      return new int[] {1,2,3,4};
    else if(x == 0)
      return new int[] {6,7,8,9};
    else
      return new int[] {10,11,12,13};
  }

  /**
     fill-array-data could never be executed on a null pointer from
     compiler-generated code.  Hand-generated code could do it,
     though, and the verifier can't check that.

     NOTE: Apparently this test isn't useful.  The compiler inserts a
     fill-array instruction after the allocation, but the fill array
     is not guarded by the try block.  Apparently there is some
     guarantee provided by the compiler here that means there can't be
     an NPE.
   */
  /*
  public Object newArrayFilledCheckedNPE() {
    Object o;

    try {
      o = new int[]{1,2,3,4};
    }
    catch(NullPointerException ex) {
      o = null;
    }

    return o;
  }
  */
  public Object newMultiArrayUnchecked(short x1, short x2) {
    short[][] arr = new short[x1][x2];
    return arr;
  }

  public Object newMultiArrayCheckedOOM(short x1, short x2) {
    short[][] arr;
    try {
      arr = new short[x1][x2];
    }
    catch(OutOfMemoryError ex) {
      arr = null;
    }

    return arr;
  }

  public String passLongToStaticCallee(long val) {
    return String.valueOf(val);
  }

  public String passLongToVirtualCallee(StringBuilder sb, long val) {
    sb.append(val);
    return sb.toString();
  }

  public int varargsTest(int i1, long l2) {
    System.out.format("%d%d\n", i1, l2);
    return i1 + (int)l2;
  }
}
