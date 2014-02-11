class ConditionalStringsAssigned {
  static Object target;
  static Object entry(int x) {
    String s;
    if(x > 0)
      s = "foo";
    else
      s = "bar";

    target = s;
    return s;
  }
}
