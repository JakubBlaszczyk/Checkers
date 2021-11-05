package com.pk.server;

public class Utils {
  private Utils(){}

  public static byte[] wrapMsg(String msg) {
    return ("checkers:" + msg + "!").getBytes();
  }
}
