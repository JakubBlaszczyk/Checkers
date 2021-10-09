package com.pk.server;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.ArrayList;
import java.util.List;

import com.pk.server.models.Packet;
import com.pk.server.models.Player;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class BasicUdpServer implements UdpServer {  
  @Override
  public List<Player> findPlayers() {
    try (DatagramSocket s = new DatagramSocket(10000)) {
      byte[] buf = new byte[BUFFER_SIZE];
      InetAddress hostAddress = InetAddress.getByName("255.255.255.255");
      DatagramPacket dp = new DatagramPacket(buf, buf.length, hostAddress, APP_PORT);
      buf = "checkers:probe".getBytes();
      DatagramPacket out = new DatagramPacket(buf, buf.length, hostAddress, APP_PORT);
      log.info("Sending probe");
      s.send(out);
      s.setSoTimeout(TIMEOUT);
      s.getChannel();
      return getActivePlayers(listenBroadcast(s, dp));
    } catch (SocketException e) {
      log.error("Socket closed ", e);
    } catch (IOException e) {
      log.error("IOException", e);
    }
    return new ArrayList<>();
  }

  private List<Packet> listenBroadcast(DatagramSocket s, DatagramPacket dp) {
    List<Packet> packets = new ArrayList<>();
    while (true) {
      try {
        s.receive(dp);
        packets.add(new Packet(dp.getAddress(), dp.getPort(), new String(dp.getData())));
        log.info("Got message");
      } catch (SocketTimeoutException e) {
        log.info("Timeout: " + e);
        return packets;
      } catch (IOException e) {
        log.error("IOException", e);
        return new ArrayList<>();
      }
    }
  }

  private List<Player> getActivePlayers(List<Packet> packets) {
    List<Player> players = new ArrayList<>();
    for (Packet packet : packets) {
      String msg = packet.getMsg();
      if (msg.length() < 9) {
        log.warn("Msg len invalid: ", msg.length());
        continue;
      }
      if (!msg.substring(0, 9).equals("checkers:")) {
        log.warn(String.format("Invalid substring, string: %s, substring: %s", msg, msg.substring(0, 9)));
        continue;
      }
      // _ == space, profileImg == base64
      // checkers:resp_nick_profileImg
      log.info("Msg_pre: ", msg);
      msg = msg.substring(9);
      log.info("Msg_post: ", msg);
      log.info("Substring: ", msg.substring(9, 13));
      if (!msg.substring(0, 9).equals("probeResp")) {
        log.warn(String.format("Invalid substring, string: %s, substring: %s", msg, msg.substring(0, 9)));
        continue;
      }
      String[] items = msg.substring(10).split(" ");
      if (items.length != 2) {
        log.warn("Invalid size: ", items.length, ", items: ", items[0]);
        continue;
      }
      log.info(String.valueOf(items.length));
      for (String item : items) {
        log.info(item);
      }
      Player player = new Player(packet.getIp(), items[0], items[1]);
      log.info(player.toString());
      players.add(player);
    }
    return players;
  }
}
