class ConditionalInts {
  static int target;
  static void entry(int x) {
    int i;
    if(x > 0)
      i = x;
    else
      i = 0;

    target = i;
  }
}
