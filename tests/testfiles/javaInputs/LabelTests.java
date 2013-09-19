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


}
