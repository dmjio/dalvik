class EnsureExceptionBlock {
  static String entry(Object o) {
    String s = "bar";
    try {
      s = o.toString();
    }
    catch(NullPointerException ex) {
      s = "nullstr";
    }

    return s;
  }
}
