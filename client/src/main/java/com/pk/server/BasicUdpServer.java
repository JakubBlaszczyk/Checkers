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

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@AllArgsConstructor
public class BasicUdpServer implements UdpServer {
  private final DatagramSocket socket;

  @Override
  public List<Player> findPlayers() {
    try {
      byte[] buf = new byte[BUFFER_SIZE];
      InetAddress hostAddress = InetAddress.getByName("255.255.255.255");
      DatagramPacket dp = new DatagramPacket(buf, buf.length, hostAddress, APP_PORT);
      buf = "checkers:probe".getBytes();
      DatagramPacket out = new DatagramPacket(buf, buf.length, hostAddress, APP_PORT);
      log.info("Sending probe");
      sendPacket(out);
      socket.setSoTimeout(TIMEOUT);
      return getActivePlayers(listenBroadcast(dp));
    } catch (SocketException e) {
      log.error("Socket closed ", e);
    } catch (IOException e) {
      log.error("IOException", e);
    }
    return new ArrayList<>();
  }

  private List<Packet> listenBroadcast(DatagramPacket dp) {
    List<Packet> packets = new ArrayList<>();
    while (true) {
      try {
        recvPacket(dp);
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

  // FIXME simplify parsing with Exception handler
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
      log.info("Msg_pre: " + msg);
      msg = msg.substring(9);
      log.info("Msg_post: " + msg);
      log.info("Substring: ", msg.substring(9, 13));
      if (!msg.substring(0, 10).equals("probeResp ")) {
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

  /**
   * Receive packet from socket
   * 
   * @param in buffer which will store received data
   * @throws IOException thrown if socket is closed
   */
  protected void recvPacket(DatagramPacket in) throws IOException {
    socket.receive(in);
  }

  /**
   * Send packet to socket
   * 
   * @param out buffer containing data
   * @throws IOException thrown if socket is closed
   */
  protected void sendPacket(DatagramPacket out) throws IOException {
    socket.send(out);
  }
}
