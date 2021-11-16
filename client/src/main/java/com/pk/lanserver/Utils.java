package com.pk.lanserver;

public class Utils {
  private Utils() {}

  public static byte[] wrapMsg(String msg) {
    return ("checkers:" + msg + "!").getBytes();
  }
}
