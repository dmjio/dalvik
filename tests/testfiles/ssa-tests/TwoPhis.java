class TwoPhis {
  static int target;
  static int entry(int x) {
    int i1;
    int i2;
    if(x > 0) {
      i1 = x;
      i2 = 12;
    }
    else {
      i1 = 0;
      i2 = 22;
    }

    target = i1;
    return i2;
  }
}
