package com.pk.lanserver;

import com.pk.lanserver.models.Invite;
import com.pk.lanserver.models.Move;

import java.util.concurrent.BlockingQueue;

public class ServerDetails {
  private static WebTcpClient wts;

  private static BlockingQueue<String> bQS;

  private static BlockingQueue<Move> bQM;

  private static BlockingQueue<Invite> bQI;

  private static String username;

  public static String getUsername() {
    return username;
  }

  public static void setUsername(String username) {
    ServerDetails.username = username;
  }

  public static WebTcpClient getWts() {
    return wts;
  }

  public static void setWts(WebTcpClient wts) {
    ServerDetails.wts = wts;
  }

  public static BlockingQueue<String> getbQS() {
    return bQS;
  }

  public static void setbQS(BlockingQueue<String> bQS) {
    ServerDetails.bQS = bQS;
  }

  public static BlockingQueue<Move> getbQM() {
    return bQM;
  }

  public static void setbQM(BlockingQueue<Move> bQM) {
    ServerDetails.bQM = bQM;
  }

  public static BlockingQueue<Invite> getbQI() {
    return bQI;
  }

  public static void setbQI(BlockingQueue<Invite> bQI) {
    ServerDetails.bQI = bQI;
  }
}
