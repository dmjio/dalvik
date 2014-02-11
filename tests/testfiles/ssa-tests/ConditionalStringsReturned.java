class ConditionalStringsReturned {
  static Object entry(int x) {
    String s;
    if(x > 0)
      s = "foo";
    else
      s = "bar";

    return s;
  }
}
