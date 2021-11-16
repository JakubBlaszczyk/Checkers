package com.pk.lanserver;

import com.pk.lanserver.models.Packet;
import com.pk.lanserver.models.Player;
import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Future;
import lombok.extern.slf4j.Slf4j;

@Slf4j
public class BasicUdpServer implements UdpServer {
  private String nick;
  private String profileImg;
  private DatagramSocket ds;
  private Integer remotePort;
  private CompletableFuture<List<Player>> futureActivePlayers = null;
  private Instant start = null;
  private Vector<Player> qActivePlayers = null;
  private Set<String> setLocalIps;

  private static final Integer TIMEOUT = 4;

  public BasicUdpServer(String nick, String profileImg, Integer localPort, Integer remotePort) throws SocketException {
    setNick(nick);
    setProfileImg(profileImg);
    this.remotePort = remotePort;
    ds = createSocket(localPort);
    ds.setBroadcast(true);
    setLocalIps = new HashSet<>();

    Enumeration<NetworkInterface> nets = NetworkInterface.getNetworkInterfaces();
    for (NetworkInterface netint : Collections.list(nets)) {
      log.info("Display name: {}", netint.getDisplayName());
      log.info("Name: {}", netint.getName());
      Enumeration<InetAddress> inetAddresses = netint.getInetAddresses();
      for (InetAddress inetAddress : Collections.list(inetAddresses)) {
        log.info("InetAddress: {}", inetAddress);
        setLocalIps.add(inetAddress.getHostAddress());
      }
    }
  }

  public void setNick(String nick) {
    if (nick.length() == 0) {
      throw new IllegalArgumentException("Nick is empty");
    }
    this.nick = nick;
  }

  public void setProfileImg(String profileImg) {
    try {
      Base64.getDecoder().decode(profileImg);
    } catch (IllegalArgumentException iae) {
      throw new IllegalArgumentException("Nick is not base64 encoded");
    }
    if (nick.length() == 0) {
      throw new IllegalArgumentException("Nick is empty");
    }
    this.profileImg = profileImg;
  }

  /** */
  @Override
  public Integer call() throws Exception {
    try {
      ds.setSoTimeout(500);
      for (; ; ) {
        try {
          DatagramPacket dp = recvMsg(ds);
          InetAddress addr = dp.getAddress();
          if (setLocalIps.contains(addr.getHostAddress())) {
            log.info("Its our message, ignoring");
            continue;
          }
          String msg = new String(dp.getData(), 0, dp.getLength()).strip();
          log.info("Got msg: <{}>", msg);
          if (start != null && futureActivePlayers != null) {
            // Magic number
            if (Duration.between(start, Instant.now()).toSeconds() > TIMEOUT) {
              futureActivePlayers.complete(qActivePlayers);
              futureActivePlayers = null;
              start = null;
            }
          }
          if (msg.equals("checkers:probe")) {
            DatagramPacket dp2 = prepareResponse(msg, addr);
            if (dp2 == null) {
              log.info("dp is null");
              continue;
            }
            log.info("DP: {}", new String(dp2.getData()));
            ds.send(dp2);
          } else if (msg.startsWith("checkers:probeResp ")) {
            if (start != null && futureActivePlayers != null) {
              if (qActivePlayers.size() > 200) {
                qActivePlayers.clear();
              }
              Packet packet = new Packet(dp.getAddress(), dp.getPort(), new String(dp.getData()));
              Player player = getActivePlayers(packet);
              if (player != null) {
                qActivePlayers.add(player);
              }
            }
          } else {
            log.info("Got unknown message: <{}>", msg);
          }
        } catch (SocketTimeoutException ignore) {
          // Magic number
          if (start != null && futureActivePlayers != null) {
            if (Duration.between(start, Instant.now()).toSeconds() > TIMEOUT) {
              futureActivePlayers.complete(qActivePlayers);
              futureActivePlayers = null;
              start = null;
            }
          }
        }
      }
    } catch (Exception e) {
      log.error("UDP server is down, ", e);
      return -1;
    }
  }

  /**
   * Receive message from broadcast and return it via ds.
   *
   * @param ds input data source.
   * @return received message
   * @throws IOException placeholder
   */
  protected DatagramPacket recvMsg(DatagramSocket ds) throws IOException {
    byte[] buf = new byte[100];
    DatagramPacket dp = new DatagramPacket(buf, buf.length);
    ds.receive(dp);
    log.info("Got packet in ProbeResponder, data: <{}>", new String(dp.getData()));
    return dp;
  }

  /**
   * Creates DatagramPacket ready to send with probe response containing nickName and profileImg b64
   * encoded.
   *
   * @param msg message received from broadcast
   * @return packet to send or null if msg does not contain valid probe
   * @throws UnknownHostException never thrown (// TODO)
   */
  protected DatagramPacket prepareResponse(String msg, InetAddress addr)
      throws UnknownHostException {
    if (!verifyProbe(msg)) {
      return null;
    }
    byte[] buf = String.format("checkers:probeResp %s %s", nick, profileImg).getBytes();
    return new DatagramPacket(buf, buf.length, addr, remotePort);
  }

  protected DatagramSocket createSocket(Integer port) throws SocketException {
    return new DatagramSocket(port);
  }

  private boolean verifyProbe(String msg) {
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

  @Override
  public Future<List<Player>> getActivePlayers() {
    try {
      String msg = "checkers:probe";
      DatagramPacket dp =
          new DatagramPacket(
              msg.getBytes(), msg.length(), InetAddress.getByName("255.255.255.255"), remotePort);
      ds.send(dp);
    } catch (IOException e) {
      return CompletableFuture.failedFuture(e);
    }
    qActivePlayers = new Vector<>();
    futureActivePlayers = new CompletableFuture<>();
    start = Instant.now();
    return futureActivePlayers;
  }

  // FIXME simplify parsing with Exception handler
  private Player getActivePlayers(Packet packet) {
    String msg = packet.getMsg();
    if (msg.length() < 9) {
      log.warn("Msg len invalid: ", msg.length());
      return null;
    }
    if (!msg.startsWith("checkers:")) {
      log.warn(
          String.format("Invalid substring, string: %s, substring: %s", msg, msg.substring(0, 9)));
      return null;
    }
    log.info("Msg_pre: " + msg);
    msg = msg.substring(9);
    log.info("Msg_post: " + msg);
    log.info("Substring: ", msg.substring(9, 13));
    if (!msg.startsWith("probeResp ")) {
      log.warn(
          String.format("Invalid substring, string: %s, substring: %s", msg, msg.substring(0, 9)));
      return null;
    }
    String[] items = msg.substring(10).split(" ");
    if (items.length != 2) {
      log.warn("Invalid size: ", items.length, ", items: ", Arrays.toString(items));
      return null;
    }
    log.info(String.valueOf(items.length));
    for (String item : items) {
      log.info(item);
    }
    InetAddress ipx = packet.getIp();
    Player player = new Player(ipx, items[0], items[1], ipToInvCode(ipx.getHostAddress()));
    log.info(player.toString());
    return player;
  }

  private String ipToInvCode(String ip) {
    StringBuilder sb = new StringBuilder();
    for (String part : ip.split("\\.")) {
      if (part.length() == 3) {
        sb.append(part);
      } else if (part.length() == 2) {
        sb.append("0" + part);
      } else {
        sb.append("00" + part);
      }
    }
    log.info("ProbeResponder created inviteCode: {}", sb.toString());
    return sb.toString();
  }
}
