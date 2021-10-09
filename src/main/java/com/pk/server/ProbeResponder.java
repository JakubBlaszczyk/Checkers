package com.pk.server;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ProbeResponder implements Runnable {
  @Override
  public void run() {
    byte[] buf = new byte[100];
    try (DatagramSocket s = createSocket(10000)) {
      InetAddress addr = InetAddress.getByName("255.255.255.255");
      DatagramPacket dp = new DatagramPacket(buf, 100);
      for (;;) {
        s.receive(dp);
        log.info("ProbeResponder got msg");
        if (verifyProbe(dp)) {
          buf = String.format("checkers:probeResp %s %s", "testowyNik", "testowyIMG").getBytes();
          DatagramPacket packet = new DatagramPacket(buf, buf.length, addr, 10000);
          s.send(packet);
        }
      }
    } catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  protected DatagramSocket createSocket(Integer port) throws SocketException {
    return new DatagramSocket(port);
  }

  private boolean verifyProbe(DatagramPacket dp) {
    String msg = new String(dp.getData(), 0, dp.getLength());
    try {
      if (msg.equals("checkers:probe")) {
        log.info("Got valid probe");
        return true;
      }
      log.info("Got invalid probe");
    } catch (IndexOutOfBoundsException e) {
      log.warn("Exception: ", e);
    }
    return false;
  }
}
